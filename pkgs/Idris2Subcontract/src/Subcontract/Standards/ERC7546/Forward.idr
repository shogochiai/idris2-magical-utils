||| ERC-7546 Proxy Forwarding Logic
|||
||| Contains the core forwarding functions for ERC-7546 UCS pattern:
||| - getDictionary / setDictionary
||| - queryDictionary (STATICCALL to get implementation)
||| - forwardToImplementation (full proxy flow)
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Subcontract.Standards.ERC7546.Forward

import EVM.Primitives
import Subcontract.Standards.ERC7546.Slots

-- =============================================================================
-- Dictionary Access
-- =============================================================================

||| Get the dictionary address from storage
export
getDictionary : IO Integer
getDictionary = sload DICTIONARY_SLOT

||| Set the dictionary address in storage
export
setDictionary : Integer -> IO ()
setDictionary addr = sstore DICTIONARY_SLOT addr

-- =============================================================================
-- Dictionary Query
-- =============================================================================

||| Query dictionary for implementation address via STATICCALL
||| Calls dictionary.getImplementation(bytes4 selector)
|||
||| @param selector - The function selector to look up
||| @return Implementation address, or 0 if not found
export
queryDictionary : Integer -> IO Integer
queryDictionary selector = do
  dictionary <- getDictionary

  -- Prepare calldata for getImplementation(bytes4)
  -- Layout: [selector (4 bytes)][padding (28 bytes)][argument (32 bytes)]
  mstore 0 (SEL_GET_IMPL * 0x100000000000000000000000000000000000000000000000000000000)
  mstore 4 selector

  availableGas <- gas

  -- staticcall(gas, addr, argsOffset, argsSize, retOffset, retSize)
  success <- staticcall availableGas dictionary 0 36 0 32

  if success == 1
    then mload 0  -- Return implementation address
    else pure 0

-- =============================================================================
-- Proxy Forwarding
-- =============================================================================

||| Forward call to dictionary via DELEGATECALL
||| This is the simple forwarding pattern where dictionary handles dispatch.
|||
||| 1. Load dictionary address from DICTIONARY_SLOT
||| 2. Copy calldata to memory
||| 3. DELEGATECALL to dictionary with all gas
||| 4. Copy return data
||| 5. Return or revert based on success
export
forwardToDictionary : IO ()
forwardToDictionary = do
  -- Get dictionary address
  dictionary <- getDictionary

  -- Get calldata size and copy to memory at 0
  cdSize <- calldatasize
  calldatacopy 0 0 cdSize

  -- Get available gas
  availableGas <- gas

  -- DELEGATECALL to dictionary
  success <- delegatecall availableGas dictionary 0 cdSize 0 0

  -- Get return data size and copy to memory
  rdSize <- returndatasize
  returndatacopy 0 0 rdSize

  -- Return or revert based on success
  returnOrRevert success 0 rdSize

||| Forward call to implementation (correct ERC-7546 flow)
|||
||| 1. Extract selector from calldata
||| 2. Query dictionary for implementation address (STATICCALL)
||| 3. DELEGATECALL to implementation with original calldata
export
forwardToImplementation : IO ()
forwardToImplementation = do
  selector <- getSelector
  implAddr <- queryDictionary selector

  if implAddr == 0
    then evmRevert 0 0  -- No implementation found
    else do
      cdSize <- calldatasize
      calldatacopy 0 0 cdSize
      availableGas <- gas
      success <- delegatecall availableGas implAddr 0 cdSize 0 0
      rdSize <- returndatasize
      returndatacopy 0 0 rdSize
      returnOrRevert success 0 rdSize

-- =============================================================================
-- Upgrade Helpers
-- =============================================================================

||| Upgrade dictionary to a new address
||| Emits DictionaryUpgraded event
export
upgradeDictionary : Integer -> IO ()
upgradeDictionary newDictionary = do
  setDictionary newDictionary
  -- Emit event: log1(offset, size, topic)
  mstore 0 newDictionary
  log1 0 32 EVENT_DICTIONARY_UPGRADED
