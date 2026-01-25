||| Test importing idris2-mc API from idris2-yul
|||
||| Verifies that MC.Std.Storage.ERC7201 and MC.Std.Storage.Schema
||| can be imported and used
module Main

import MC.Std.Storage.ERC7201
import MC.Std.Storage.Schema

import EVM.Primitives

-- =============================================================================
-- Test: Use pre-computed MC slots
-- =============================================================================

||| Read admin address using MC's pre-computed slot
testReadAdmin : IO Integer
testReadAdmin = do
  sload SLOT_MC_ADMIN

||| Read dictionary address using MC's pre-computed slot
testReadDictionary : IO Integer
testReadDictionary = do
  sload SLOT_MC_CLONE

-- =============================================================================
-- Test: Use Schema API
-- =============================================================================

||| Test the AdminSchema definition
testAdminSchema : Integer
testAdminSchema = schemaRoot AdminSchema

||| Test the CloneSchema definition
testCloneSchema : Integer
testCloneSchema = schemaRoot CloneSchema

-- =============================================================================
-- Test: Mapping slot calculation
-- =============================================================================

||| Calculate mapping slot for a feature toggle
testMappingSlot : Integer -> IO Integer
testMappingSlot selector = do
  mappingSlot SLOT_MC_FEATURE_TOGGLE selector

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = do
  admin <- testReadAdmin
  dict <- testReadDictionary

  -- Store results for verification
  mstore 0 admin
  mstore 32 dict
  evmReturn 0 64
