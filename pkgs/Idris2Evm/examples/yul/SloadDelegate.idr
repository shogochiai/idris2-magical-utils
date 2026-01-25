||| SloadDelegate - SLOAD then DELEGATECALL
module Main

import EVM.Primitives

main : IO ()
main = do
  -- Get target from storage
  target <- sload 0

  -- Get gas
  g <- gas

  -- DELEGATECALL
  success <- delegatecall g target 0 0 0 0

  -- Return success value
  mstore 0 success
  evmReturn 0 32
