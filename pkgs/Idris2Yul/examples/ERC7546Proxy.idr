||| ERC-7546 UCS Proxy Contract
|||
||| A proxy that queries Dictionary for implementation by selector,
||| then DELEGATECALLs to the returned implementation.
|||
||| Flow:
||| 1. Extract selector from calldata (first 4 bytes)
||| 2. STATICCALL dictionary.getImplementation(selector)
||| 3. DELEGATECALL to returned implementation
||| 4. Return/revert based on result
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Main

import EVM.Primitives

-- =============================================================================
-- ERC-7546 Constants
-- =============================================================================

||| Storage slot for dictionary address
||| keccak256("erc7546.proxy.dictionary") - 1
||| = 0x267691be3525af8a813d30db0c9e2bad08f63baecf6dceb85e2cf3676cff56f4
DICTIONARY_SLOT : Integer
DICTIONARY_SLOT = 0x267691be3525af8a813d30db0c9e2bad08f63baecf6dceb85e2cf3676cff56f4

||| Function selector for getImplementation(bytes4)
||| keccak256("getImplementation(bytes4)")[:4] = 0xdc9cc645
SEL_GET_IMPL : Integer
SEL_GET_IMPL = 0xdc9cc645

-- =============================================================================
-- ERC-7546 Proxy Logic
-- =============================================================================

||| Forward call to implementation looked up from dictionary
||| This is the main proxy logic
forwardToImplementation : IO ()
forwardToImplementation = do
  -- Get selector from calldata (first 4 bytes, right-aligned in 32 bytes)
  selector <- getSelector

  -- Load dictionary address from storage
  dictionary <- sload DICTIONARY_SLOT

  -- Prepare STATICCALL to dictionary.getImplementation(bytes4 selector)
  -- Calldata layout: [SEL_GET_IMPL (4 bytes)][selector padded to 32 bytes]
  shifted <- shl 224 SEL_GET_IMPL
  mstore 0 shifted
  mstore 4 selector

  -- Get available gas
  availableGas <- gas

  -- STATICCALL(gas, addr, argsOffset, argsSize, retOffset, retSize)
  success <- staticcall availableGas dictionary 0 36 0 32

  if success == 0
    then evmRevert 0 0  -- Dictionary call failed
    else do
      -- Load implementation address from return data
      implAddr <- mload 0

      if implAddr == 0
        then evmRevert 0 0  -- No implementation found
        else do
          -- Copy original calldata to memory
          cdSize <- calldatasize
          calldatacopy 0 0 cdSize

          -- DELEGATECALL to implementation
          availableGas2 <- gas
          delegateSuccess <- delegatecall availableGas2 implAddr 0 cdSize 0 0

          -- Copy return data
          rdSize <- returndatasize
          returndatacopy 0 0 rdSize

          -- Return or revert based on success
          returnOrRevert delegateSuccess 0 rdSize

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = forwardToImplementation
