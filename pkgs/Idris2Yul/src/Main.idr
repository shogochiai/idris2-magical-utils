||| Idris2-EVM Compiler Entry Point
||| Custom Idris2 with EVM/Yul backend
module Main

import Compiler.EVM.Yul
import Compiler.Common

import Idris.Driver

%default covering

||| Main entry point
main : IO ()
main = mainWithCodegens [("evm", codegenEVM), ("yul", codegenYul)]
