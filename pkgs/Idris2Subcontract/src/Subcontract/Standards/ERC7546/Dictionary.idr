||| ERC-7546 Dictionary Contract
|||
||| Manages function selector → implementation address mappings.
||| Part of the UCS (Upgradeable Clone for Scalable contracts) pattern.
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Subcontract.Standards.ERC7546.Dictionary

import public EVM.Primitives
import EVM.Storage.Namespace
import Subcontract.Standards.ERC7546.Slots

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for owner address
SLOT_OWNER : Integer
SLOT_OWNER = 0

||| Base slot for implementations mapping
||| Actual slot = keccak256(selector . SLOT_IMPLEMENTATIONS_BASE)
SLOT_IMPLEMENTATIONS_BASE : Integer
SLOT_IMPLEMENTATIONS_BASE = 1

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if caller is owner
export
isOwner : IO Bool
isOwner = do
  owner <- sload SLOT_OWNER
  callerAddr <- caller
  pure (owner == callerAddr)

||| Require caller to be owner
export
requireOwner : IO ()
requireOwner = do
  ownerCheck <- isOwner
  if ownerCheck
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Implementation Mapping
-- =============================================================================

||| Calculate storage slot for a function selector's implementation
export
getImplSlot : Integer -> IO Integer
getImplSlot selector = mappingSlot SLOT_IMPLEMENTATIONS_BASE selector

||| Get implementation address for a function selector
||| Returns 0 if not set
export
getImplementation : Integer -> IO Integer
getImplementation selector = do
  slot <- getImplSlot selector
  sload slot

||| Set implementation address for a function selector
||| Only owner can call
export
setImplementation : Integer -> Integer -> IO ()
setImplementation selector implAddr = do
  requireOwner
  slot <- getImplSlot selector
  sstore slot implAddr

||| Batch set multiple implementations
||| Only owner can call
export
batchSetImplementation : List (Integer, Integer) -> IO ()
batchSetImplementation [] = pure ()
batchSetImplementation ((sel, impl) :: rest) = do
  setImplementation sel impl
  batchSetImplementation rest

-- =============================================================================
-- Owner Management
-- =============================================================================

||| Get owner address
export
getOwner : IO Integer
getOwner = sload SLOT_OWNER

||| Transfer ownership (only owner can call)
export
transferOwnership : Integer -> IO ()
transferOwnership newOwner = do
  requireOwner
  sstore SLOT_OWNER newOwner

||| Initialize owner (should only be called once during deployment)
export
initializeOwner : Integer -> IO ()
initializeOwner owner = do
  currentOwner <- sload SLOT_OWNER
  if currentOwner == 0
    then sstore SLOT_OWNER owner
    else evmRevert 0 0  -- Already initialized

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Main entry point for Dictionary contract
||| Dispatches to appropriate function based on selector
export
dictionaryMain : IO ()
dictionaryMain = do
  selector <- getSelector

  if selector == SEL_GET_IMPL
    then do
      -- getImplementation(bytes4 selector)
      arg <- calldataload 4
      impl <- getImplementation arg
      returnUint impl

    else if selector == SEL_SET_IMPL
    then do
      -- setImplementation(bytes4 selector, address impl)
      sel <- calldataload 4
      impl <- calldataload 36
      setImplementation sel impl
      evmReturn 0 0

    else if selector == SEL_OWNER
    then do
      -- owner()
      owner <- getOwner
      returnUint owner

    else if selector == SEL_TRANSFER
    then do
      -- transferOwnership(address newOwner)
      newOwner <- calldataload 4
      transferOwnership newOwner
      evmReturn 0 0

    else evmRevert 0 0  -- Unknown function
