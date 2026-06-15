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
} = require('react-native');

// Registry mapping Idris component name strings -> RN components.
const COMPONENTS = {
  View,
  Text,
  Button,
  ScrollView,
};

// A single force-update hook handle, set when the root component mounts.
let __forceRerender = null;
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

  // ---- children accumulator ----
  newChildren() {
    return [];
  },
  pushChild(arr, c) {
    arr.push(c);
    return arr;
  },

  // ---- element creation ----
  createElement(comp, props, kids) {
    const Comp = COMPONENTS[comp] || comp;
    // React.createElement(type, props, ...children)
    return React.createElement(Comp, props, ...(kids || []));
  },
  textNode(s) {
    // Bare strings are valid React children inside <Text>.
    return s;
  },

  // ---- run loop ----
  requestRerender() {
    if (__forceRerender) __forceRerender();
  },
  run(renderThunk) {
    __renderThunk = renderThunk;

    function IdrisRoot() {
      const [, setTick] = React.useState(0);
      React.useEffect(() => {
        __forceRerender = () => setTick((t) => t + 1);
        return () => { __forceRerender = null; };
      }, []);
      // Each render asks Idris for the current element tree.
      return __renderThunk()();
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
