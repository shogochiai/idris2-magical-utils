||| TokenPJ Upgrade Script
|||
||| Upgrades the TokenPJ implementation:
||| 1. Deploy new TokenImpl v2
||| 2. Update Dictionary mappings to point to v2
||| 3. (Optional) Migrate storage if schema changed
|||
||| ERC-7546 allows granular upgrades:
||| - Upgrade single function: Dictionary.setImplementation(selector, newImpl)
||| - Upgrade all functions: Update all mappings
||| - Add new function: Dictionary.setImplementation(newSelector, impl)
|||
||| Storage remains in Proxy, unaffected by implementation changes.
module Upgrade

import EVM.Primitives
import Subcontract.Standards.ERC7546.Forward
import Main.Storages.Schema

-- =============================================================================
-- Upgrade Patterns
-- =============================================================================

||| Pattern 1: Full Implementation Upgrade
||| Replace all functions with new implementation
|||
||| 1. Deploy new TokenImplV2
||| 2. For each selector, call Dictionary.setImplementation(sel, v2Addr)
|||
||| Storage is preserved because it lives in Proxy.

||| Pattern 2: Single Function Upgrade
||| Only upgrade one function (e.g., add feature to transfer)
|||
||| 1. Deploy TransferV2 with just the new transfer function
||| 2. Dictionary.setImplementation(0xa9059cbb, transferV2Addr)
|||
||| Other functions still point to original impl.

||| Pattern 3: Add New Function
||| Add functionality without touching existing code
|||
||| 1. Deploy NewFeature with new function
||| 2. Dictionary.setImplementation(newSelector, newFeatureAddr)
|||
||| Existing functions unchanged.

-- =============================================================================
-- Access Control
-- =============================================================================

||| Only owner can upgrade
requireOwner : IO ()
requireOwner = do
  owner <- getOwner
  callerAddr <- caller
  if owner == callerAddr
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Upgrade Functions
-- =============================================================================

||| Transfer ownership (for upgrade authority)
||| Selector: transferOwnership(address) => 0xf2fde38b
export
transferOwnership : IO ()
transferOwnership = do
  requireOwner
  newOwner <- calldataload 4
  setOwner newOwner
  stop

-- =============================================================================
-- Upgrade Notes
-- =============================================================================

-- Upgrade steps (execute via forge/cast):
--
-- 1. Build new implementation:
--    ./scripts/build-contract.sh src/Main.idr  # with changes
--
-- 2. Deploy new implementation:
--    NEW_IMPL=$(cast send --create $NEW_IMPL_BYTECODE ...)
--
-- 3. Update Dictionary (for each selector):
--    cast send $DICTIONARY "setImplementation(bytes4,address)" \
--      0xa9059cbb $NEW_IMPL --rpc-url $RPC --private-key $PK
--
-- Storage Migration (if schema changed):
--
-- If adding new fields, they're automatically available at new offsets.
-- If changing field types/order, need migration function:
--
-- migrationEntry : Entry migrationSig
-- migrationEntry = MkEntry migrationSel $ do
--   -- Read old format
--   oldValue <- sload OLD_SLOT
--   -- Write new format
--   _ <- schemaSetValue NewSchema "newField" (transform oldValue)
--   stop
