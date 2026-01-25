||| PreDelegateTest - Return 99 before DELEGATECALL
module Main

import EVM.Primitives

main : IO ()
main = do
  -- Get target address from storage[0]
  target <- sload 0

  -- Return the target address for debugging
  mstore 0 target
  evmReturn 0 32
