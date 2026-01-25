||| SloadReturn - Test SLOAD then return
module Main

import EVM.Primitives

main : IO ()
main = do
  val <- sload 0
  mstore 0 val
  evmReturn 0 32
