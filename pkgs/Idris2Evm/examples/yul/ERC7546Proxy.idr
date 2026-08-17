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
  -- Calldata layout: [SEL_GET_IMPL (4 bytes)][selector as bytes4, LEFT-aligned]
  --
  -- The selector must be shifted back left before it is stored. getSelector
  -- returns it RIGHT-aligned (it divides calldataload(0) by 2^224), while ABI
  -- encoding puts a bytes4 in the HIGH-order bytes of its word -- which is what
  -- Dictionary reads, since its getImplementation takes calldataload(4) and
  -- divides by 2^224 to recover the value. Passing the right-aligned form made
  -- every lookup ask for selector 0x00000000, so the dictionary answered address
  -- zero and the proxy reverted delegatecalling nothing.
  --
  -- Measured on Base mainnet 2026-08-18 against the deployed pair, two eth_calls
  -- differing only in alignment:
  --   dc9cc645 a9059cbb000…  -> 0x3178737d…  (the registered implementation)
  --   dc9cc645 000…a9059cbb  -> 0x0000000…   (nothing registered)
  -- Both contracts were self-consistent, so this was invisible until they were
  -- composed on chain.
  shifted <- shl 224 SEL_GET_IMPL
  selectorArg <- shl 224 selector
  mstore 0 shifted
  mstore 4 selectorArg

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
