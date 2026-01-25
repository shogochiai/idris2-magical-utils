||| JustCalldataSize - Test calldatasize only
module Main

import EVM.Primitives

main : IO ()
main = do
  cdSize <- calldatasize
  mstore 0 cdSize
  evmReturn 0 32
