# idris2-react-native

A React Native backend for Idris2: write your mobile app's **UI and logic in
Idris2**, compile to a Metro-ready JS bundle, and run it under React Native /
Hermes. The motivating use case is EtherClaw's "smartphone-complete" dev
experience (chat → IP proposal → solo auto-vote → Builder → audit) where the
client is a native app but stays inside the Idris2-only toolchain policy.

## How it works

React Native runs on a JavaScript engine (Hermes / JSC), so the stock Idris2
ES backend output runs there almost verbatim. This package is therefore a thin,
**non-invasive plugin** (registered via `mainWithCodegens`, the same mechanism
idris2-evm uses) — it does **not** patch the forked compiler.

| Layer | What it is |
|-------|-----------|
| `Compiler.ReactNative.Codegen` | `codegenReactNative`: calls the existing `compileToES` with `ccTypes = ["react-native","browser","javascript"]`, so `%foreign "react-native:..."` wins, falling back to browser/javascript. |
| `RNMain` | `main = mainWithCodegens [("react-native", codegenReactNative)]` → the `idris2-rn` executable. |
| `RN.Node` | dom-mvc-shaped UI tree (`El`/`Str`/`Empty`, `Attr = Prop \| Handler`), parameterised by the event type. Components are plain strings (`"View"`, `"Text"`), **not** HTML-tag proofs — RN primitives are not HTML. |
| `RN.Runtime` | `renderNode` (Node → `React.createElement`) and `runMVU` (Elm/dom-mvc loop). All React interaction is behind `%foreign "react-native:..."`. |
| `RN.Scaffold` | Emits `package.json` / `app.json` / `metro.config.js` / `babel.config.js` / `index.js` from Idris2 — the scaffold is generated, not hand-written. |
| `rn-runtime/idris-rn-shim.js` | The FFI edge: implements `__idrisRN` over React + RN primitives, drives `AppRegistry` and re-render. |

## Build

```bash
./build.sh            # stages 1-4: produces poc-app/ (node only)
```

## Stage 5 — iOS simulator (needs CocoaPods + Xcode)

```bash
cd poc-app
npm install
(cd ios && pod install)
npx react-native run-ios
```

## PoC status

- **Stage 1** ✅ `idris2-rn --cg react-native` emits a JS bundle.
- **Stage 2** ✅ Verified under real Hermes (`hermes-engine-cli`): BigInt
  (`Integer`) and arrow functions run. The single `class IdrisError` line is
  the only ES6-class usage; modern RN-Hermes supports it (an old standalone
  Hermes CLI does not — a one-line prototype shim covers that case).
- **Stage 3** ✅ All `react-native:` FFI lambdas resolve into the output
  (proving `searchForeign` picks this backend over the fallbacks).
- **Stage 4** ✅ The MVU demo renders a real React element tree and **state
  updates on press** (`tapped: N`), verified headlessly with
  `react-test-renderer`. The Idris2-generated `index.js` wiring renders too.
- **Stage 5** ⏳ Requires CocoaPods + Xcode on the build host.

## A minimal app (UI + logic in Idris2)

```idris
import RN.Node
import RN.Runtime

data Msg = Tapped
record Model where constructor MkModel; count : Nat

update : Msg -> Model -> Model
update Tapped m = { count := S m.count } m

view : Model -> Node Msg
view m = view [testID "root"]
  [ text [] "Hello from Idris2 on React Native"
  , text [] ("tapped: " ++ show m.count)
  , button [onPress Tapped] "Tap me" ]

main : IO ()
main = runMVU update view Tapped (MkModel 0)
```
```
