||| Idris2-ReactNative compiler entry point.
||| A standard Idris2 driver with the `react-native` codegen registered as an
||| out-of-tree plugin (same mechanism as idris2-evm's `YulMain`).
|||
||| Usage:  idris2-rn --cg react-native -o out.js Main.idr
module RNMain

import Compiler.ReactNative.Codegen
import Compiler.Common

import Idris.Driver

%default covering

||| Main entry point.
main : IO ()
main = mainWithCodegens [("react-native", codegenReactNative)]
