||| DelegateTest - Minimal DELEGATECALL test
module Main

import EVM.Primitives

main : IO ()
main = do
  -- Get target address from storage[0]
  target <- sload 0

  -- Get calldata size and copy to memory
  cdSize <- calldatasize
  calldatacopy 0 0 cdSize

  -- Call with all available gas
  g <- gas

  -- DELEGATECALL
  success <- delegatecall g target 0 cdSize 0 0

  -- Get return data and copy to memory
  rdSize <- returndatasize
  returndatacopy 0 0 rdSize

  -- Just return, assuming success
  evmReturn 0 rdSize
