||| MC Core: Dictionary Contract
||| Manages function selector → implementation address mappings
|||
||| Based on UCS (Upgradeable Clone Standard) pattern
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for owner address
SLOT_OWNER : Integer
SLOT_OWNER = 0

||| Storage slot for implementation count (for enumeration)
SLOT_IMPL_COUNT : Integer
SLOT_IMPL_COUNT = 1

||| Base slot for implementations mapping
||| Actual slot = keccak256(selector . SLOT_IMPLEMENTATIONS_BASE)
SLOT_IMPLEMENTATIONS_BASE : Integer
SLOT_IMPLEMENTATIONS_BASE = 2

-- =============================================================================
-- Storage Helpers
-- =============================================================================

||| Calculate storage slot for a function selector's implementation
||| slot = keccak256(abi.encode(selector, baseSlot))
getImplSlot : Integer -> IO Integer
getImplSlot selector = do
  -- Store selector at memory 0
  mstore 0 selector
  -- Store base slot at memory 32
  mstore 32 SLOT_IMPLEMENTATIONS_BASE
  -- keccak256(0, 64) = hash of (selector, baseSlot)
  keccak256 0 64

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if caller is owner
isOwner : IO Bool
isOwner = do
  owner <- sload SLOT_OWNER
  callerAddr <- caller
  pure (owner == callerAddr)

||| Revert if caller is not owner
requireOwner : IO ()
requireOwner = do
  ownerCheck <- isOwner
  if ownerCheck
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Dictionary Functions
-- =============================================================================

||| Get implementation address for a function selector
||| Returns 0 if not set
getImplementation : Integer -> IO Integer
getImplementation selector = do
  slot <- getImplSlot selector
  sload slot

||| Set implementation address for a function selector
||| Only owner can call
setImplementation : Integer -> Integer -> IO ()
setImplementation selector implAddr = do
  requireOwner
  slot <- getImplSlot selector
  sstore slot implAddr

||| Get owner address
getOwner : IO Integer
getOwner = sload SLOT_OWNER

||| Transfer ownership (only owner can call)
transferOwnership : Integer -> IO ()
transferOwnership newOwner = do
  requireOwner
  sstore SLOT_OWNER newOwner

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- getImplementation(bytes4) -> 0x...
SEL_GET_IMPL : Integer
SEL_GET_IMPL = 0xdc9cc645

-- setImplementation(bytes4,address) -> 0x...
SEL_SET_IMPL : Integer
SEL_SET_IMPL = 0x2c3c3e4e

-- owner() -> 0x8da5cb5b
SEL_OWNER : Integer
SEL_OWNER = 0x8da5cb5b

-- transferOwnership(address) -> 0xf2fde38b
SEL_TRANSFER : Integer
SEL_TRANSFER = 0xf2fde38b

-- =============================================================================
-- Entry Point
-- =============================================================================

returnAddress : Integer -> IO ()
returnAddress addr = do
  mstore 0 addr
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_GET_IMPL
    then do
      -- getImplementation(bytes4 selector)
      funcSelector <- calldataload 4
      -- Mask to 4 bytes (bytes4)
      let funcSelector4 = funcSelector `div` (256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256)
      impl <- getImplementation funcSelector4
      returnAddress impl

    else if selector == SEL_SET_IMPL
    then do
      -- setImplementation(bytes4 selector, address implementation)
      funcSelector <- calldataload 4
      let funcSelector4 = funcSelector `div` (256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256)
      impl <- calldataload 36
      setImplementation funcSelector4 impl

    else if selector == SEL_OWNER
    then do
      owner <- getOwner
      returnAddress owner

    else if selector == SEL_TRANSFER
    then do
      newOwner <- calldataload 4
      transferOwnership newOwner

    else evmRevert 0 0
