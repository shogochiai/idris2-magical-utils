||| Subcontract Standard Function: FeatureToggle
|||
||| Allows admin to enable/disable function selectors.
module Subcontract.Std.Functions.FeatureToggle

import EVM.Primitives
import EVM.Storage.Namespace

-- =============================================================================
-- Storage Slots (ERC-7201)
-- =============================================================================

||| Admin slot (shared with subcontract.std.admin)
export
SLOT_ADMIN : Integer
SLOT_ADMIN = 0xc87a8b268af18cef58a28e8269c607186ac6d26eb9fb11e976ba7fc83fbc5b00

||| Feature toggle mapping base slot
||| keccak256(keccak256("subcontract.std.featureToggle") - 1) & ~0xff
export
SLOT_FEATURE_TOGGLE : Integer
SLOT_FEATURE_TOGGLE = 0xfbe5942bf8b77a2e1fdda5ac4fad2514a8894a997001808038d8cb6785c1d500

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if caller is admin
export
isAdmin : IO Bool
isAdmin = do
  admin <- readAddress SLOT_ADMIN
  callerAddr <- caller
  pure (admin == callerAddr)

||| Require caller to be admin
export
requireAdmin : IO ()
requireAdmin = do
  adminCheck <- isAdmin
  if adminCheck
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Feature Toggle Functions
-- =============================================================================

||| Check if a feature (selector) is disabled
export
isFeatureDisabled : Integer -> IO Bool
isFeatureDisabled selector = do
  slot <- mappingSlot SLOT_FEATURE_TOGGLE selector
  readBool slot

||| Check if feature should be active, revert if disabled
||| Library function: FeatureToggle.shouldBeActive(bytes4)
export
shouldBeActive : Integer -> IO ()
shouldBeActive selector = do
  disabled <- isFeatureDisabled selector
  if disabled
    then evmRevert 0 0  -- FeatureNotActive error
    else pure ()

||| Toggle a feature's enabled/disabled status
||| Only admin can call
export
featureToggle : Integer -> IO ()
featureToggle selector = do
  requireAdmin
  slot <- mappingSlot SLOT_FEATURE_TOGGLE selector
  currentVal <- readBool slot
  writeBool slot (not currentVal)

||| Enable a specific feature
export
enableFeature : Integer -> IO ()
enableFeature selector = do
  requireAdmin
  slot <- mappingSlot SLOT_FEATURE_TOGGLE selector
  writeBool slot False  -- disabled = false means enabled

||| Disable a specific feature
export
disableFeature : Integer -> IO ()
disableFeature selector = do
  requireAdmin
  slot <- mappingSlot SLOT_FEATURE_TOGGLE selector
  writeBool slot True  -- disabled = true
