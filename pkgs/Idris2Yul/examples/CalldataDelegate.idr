||| CalldataDelegate - With calldata handling
module Main

import EVM.Primitives

main : IO ()
main = do
  target <- sload 0
  cdSize <- calldatasize
  calldatacopy 0 0 cdSize
  g <- gas
  success <- delegatecall g target 0 cdSize 0 0
  mstore 0 success
  evmReturn 0 32
