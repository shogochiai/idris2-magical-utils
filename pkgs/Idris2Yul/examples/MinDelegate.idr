||| MinDelegate - Absolute minimum DELEGATECALL test
module Main

import EVM.Primitives

main : IO ()
main = do
  -- Get gas
  g <- gas

  -- DELEGATECALL to address 0x2000 with empty calldata
  success <- delegatecall g 0x2000 0 0 0 0

  -- Return success value
  mstore 0 success
  evmReturn 0 32
