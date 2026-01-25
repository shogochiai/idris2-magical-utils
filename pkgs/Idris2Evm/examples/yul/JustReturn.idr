||| JustReturn - Minimal test: just return 42
module Main

import EVM.Primitives

main : IO ()
main = do
  mstore 0 42
  evmReturn 0 32
