||| SimpleProxy - Ultra minimal proxy for debugging
module Main

import EVM.Primitives

-- Main: Always return (assumes DELEGATECALL succeeds)
main : IO ()
main = do
  dict <- sload 0
  cdSize <- calldatasize
  calldatacopy 0 0 cdSize
  g <- gas
  _ <- delegatecall g dict 0 cdSize 0 0
  rdSize <- returndatasize
  returndatacopy 0 0 rdSize
  evmReturn 0 rdSize
