/**
 * idris-rn-shim.js — runtime bridge for the Idris2 react-native backend.
 *
 * Implements the `__idrisRN` object that RN.Runtime's `%foreign "react-native:..."`
 * declarations call into. It maps the Idris-side Node tree onto real React
 * elements and drives a tiny re-render loop through React state.
 *
 * This file is the FFI edge: React, React Native primitives, and the
 * component registry live here, NOT in the typed Idris2 layer. Inject it
 * before the Idris2-generated bundle (the generated entry point does this).
 *
 * Expects React and react-native to be resolvable in the host bundle.
 */
const React = require('react');
const {
  AppRegistry,
  View,
  Text,
  Button,
  ScrollView,
  TextInput,
  Pressable,
  ActivityIndicator,
  SafeAreaView,
  Image,
} = require('react-native');

// Registry mapping Idris component name strings -> RN components.
const COMPONENTS = {
  View,
  Text,
  Button,
  ScrollView,
  TextInput,
  Pressable,
  ActivityIndicator,
  SafeAreaView,
  Image,
};

// Internet Identity WebView host: when global.__idrisAuth (the generated auth
// bridge) reports a login is in progress, the root renders a full-screen WebView
// at the II authorize URL and relays its postMessages back to the bridge. Loaded
// lazily so an app without II / without react-native-webview installed still
// runs (the host simply never renders). The bridge contract:
//   __idrisAuth._loginActive() : Bool       — is a login handshake in progress
//   __idrisAuth._iiUrl()       : String     — initial WebView source URL
//   __idrisAuth._injectedJS()  : String     — relay JS injected into the page
//   __idrisAuth._attachWebView(ref)          — register the WebView ref
//   __idrisAuth._onMessage(rawString)        — feed onMessage events
let __WebView = null;
try { __WebView = require('react-native-webview').WebView; } catch (e) { __WebView = null; }

function authWebViewElement() {
  const auth = (typeof global !== 'undefined' ? global : globalThis).__idrisAuth;
  if (!__WebView || !auth || typeof auth._loginActive !== 'function' || !auth._loginActive()) {
    return null;
  }
  // A top bar with a Cancel button (so the WebView is never a dead end) + the
  // II WebView itself with the settings Internet Identity needs (DOM storage,
  // third-party cookies, modern UA so the page doesn't serve a blank fallback).
  const cancel = () => { try { if (auth.cancelLogin) auth.cancelLogin(); } catch (e) {} };
  return React.createElement(View,
    { key: 'ii-webview', style: { position: 'absolute', top: 0, left: 0, right: 0, bottom: 0, backgroundColor: '#1a1a1a' } },
    React.createElement(View,
      { style: { flexDirection: 'row', alignItems: 'center', paddingTop: 36, paddingBottom: 10, paddingHorizontal: 14, backgroundColor: '#141414' } },
      React.createElement(Text, { style: { color: '#ececec', fontSize: 14, fontWeight: '600', flex: 1 } }, 'Internet Identity'),
      React.createElement(Text, { onPress: cancel, style: { color: '#d97757', fontSize: 14, fontWeight: '700' } }, 'Cancel')),
    React.createElement(__WebView, {
      ref: (r) => { try { auth._attachWebView(r); } catch (e) {} },
      source: { uri: auth._iiUrl() },
      injectedJavaScript: auth._injectedJS(),
      onMessage: (ev) => { try { auth._onMessage(ev.nativeEvent.data); } catch (e) {} },
      onError: (ev) => { try { auth._onMessage(JSON.stringify({ data: { kind: 'authorize-client-failure', text: 'webview error: ' + (ev.nativeEvent && ev.nativeEvent.description) } })); } catch (e) {} },
      javaScriptEnabled: true,
      domStorageEnabled: true,
      thirdPartyCookiesEnabled: true,
      sharedCookiesEnabled: true,
      originWhitelist: ['https://*'],
      userAgent: 'Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Mobile Safari/537.36',
      style: { flex: 1 },
      startInLoadingState: true,
    }));
}

// A single force-update hook handle, set when the root component mounts.
let __forceRerender = null;
// Rerender requests that arrived before/while unmounted; flushed on mount.
let __pendingTicks = 0;
// The Idris-side render thunk: () -> RElem (a React element).
let __renderThunk = null;

const __idrisRN = {
  // ---- props builder ----
  newProps() {
    return {};
  },
  setProp(p, k, v) {
    // RN's `style` prop expects an object/array. If the caller passes a JSON
    // string for style, parse it; otherwise set the prop verbatim.
    if (k === 'style' && typeof v === 'string') {
      try { p.style = JSON.parse(v); return; } catch (_) { /* fall through */ }
    }
    p[k] = v;
  },
  setHandler(p, k, cb) {
    // cb is an Idris closure expecting unit; invoke it on the RN event.
    p[k] = () => { cb()(); };
  },
  setStrHandler(p, k, cb) {
    // cb is an Idris closure expecting a String (e.g. TextInput onChangeText).
    // Apply with the event's text, then invoke the returned IO thunk.
    p[k] = (text) => { cb(text)(); };
  },

  // ---- children accumulator ----
  newChildren() {
    return [];
  },
  pushChild(arr, c) {
    // Skip empty/null children: a bare "" or null among <View> children
    // crashes RN's NativeViewHierarchyManager.manageChildren. React ignores
    // null/false children, so we drop them here.
    if (c !== null && c !== undefined && c !== '') arr.push(c);
    return arr;
  },

  // ---- element creation ----
  createElement(comp, props, kids) {
    const Comp = COMPONENTS[comp] || comp;
    const children = (kids || []).filter((c) => c !== null && c !== undefined && c !== '');
    // Lift a `key` prop into React's reserved key slot (React ignores props.key
    // and warns; array children NEED a real key or RN-Paper crashes on mount).
    if (props && props.key != null) {
      const key = props.key;
      delete props.key;
      return React.createElement(Comp, { ...props, key }, ...children);
    }
    return React.createElement(Comp, props, ...children);
  },
  textNode(s) {
    // Bare strings are valid React children inside <Text>.
    return s;
  },
  emptyNode() {
    // Renders nothing; React skips null children (safe among View children).
    return null;
  },

  // ---- run loop ----
  requestRerender() {
    // Count every request. If the component is mounted, flush immediately;
    // otherwise the pending count is reconciled on mount so no update (e.g. an
    // async canister reply that lands before useEffect ran) is ever dropped.
    __pendingTicks++;
    if (__forceRerender) __forceRerender();
  },
  run(renderThunk) {
    __renderThunk = renderThunk;

    function IdrisRoot() {
      const [tick, setTick] = React.useState(0);
      React.useEffect(() => {
        __forceRerender = () => setTick((t) => t + 1);
        // Flush any rerenders requested before this effect ran.
        if (__pendingTicks > 0) setTick((t) => t + 1);
        return () => { __forceRerender = null; };
      }, []);
      __pendingTicks = 0; // consumed by this render
      // The MVU loop rebuilds the element tree each render. We wrap it in a
      // Fragment with a STABLE key so React reconciles the new tree against the
      // previous one in place, rather than unmounting and remounting it.
      //
      // Why stable (not a per-tick key): a changing key tears down the whole
      // native subtree every render, which destroys the focused <TextInput>'s
      // native view — the soft keyboard dismisses and keystrokes are dropped on
      // every change (the chat composer becomes unusable). Reconciliation keeps
      // the TextInput instance alive across renders, preserving focus, cursor,
      // and IME (e.g. Japanese) composition.
      //
      // The "unknown view tag" crash that motivated the old full-remount is
      // avoided by giving every list-rendered element a stable `key` in the
      // Idris view layer (RN.Node.key): keyed reconciliation matches elements by
      // type+key, so the native registry stays consistent without remounting.
      // Render the Idris MVU tree, with the II WebView host overlaid on top when
      // a login handshake is active (null otherwise — zero cost when not logging in).
      return React.createElement(React.Fragment, { key: 'idris-root' },
        __renderThunk()(),
        authWebViewElement());
    }

    // Let the generated auth bridge force a root rerender (to show/hide the II
    // WebView when login starts/finishes) via the same hook the MVU loop uses.
    {
      const g = (typeof global !== 'undefined') ? global : globalThis;
      if (g.__idrisAuth) g.__idrisAuth._requestRerender = () => { if (__forceRerender) __forceRerender(); };
    }

    AppRegistry.registerComponent('IdrisApp', () => IdrisRoot);
  },
};

// Expose globally so the Idris bundle's %foreign lambdas can reach it.
if (typeof global !== 'undefined') {
  global.__idrisRN = __idrisRN;
} else if (typeof globalThis !== 'undefined') {
  globalThis.__idrisRN = __idrisRN;
}

module.exports = __idrisRN;
