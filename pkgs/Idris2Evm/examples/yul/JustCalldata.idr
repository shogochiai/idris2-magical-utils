||| JustCalldata - Test calldatasize/calldatacopy only
module Main

import EVM.Primitives

main : IO ()
main = do
  cdSize <- calldatasize
  calldatacopy 0 0 cdSize
  mstore 64 cdSize
  evmReturn 64 32
