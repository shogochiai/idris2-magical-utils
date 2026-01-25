||| Schema Upgrade Check Tests
|||
||| Demonstrates compile-time and runtime validation of schema upgrades.
||| Compile-time tests pass if the file compiles; runtime tests verify
||| the pure validation logic.
module SchemaCheckTest

import Subcontract.Core.Schema
import Subcontract.Core.SchemaCompat
import Subcontract.Core.SchemaCheck

%language ElabReflection

-- =============================================================================
-- Test Fixtures
-- =============================================================================

testRoot : Integer
testRoot = 0x1234567890abcdef

||| Version 1: Baseline schema
SchemaV1 : Schema
SchemaV1 = MkSchema "test.storage" testRoot
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  ]

||| Version 2 (SAFE): Append-only extension
SchemaV2_Safe : Schema
SchemaV2_Safe = MkSchema "test.storage" testRoot
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  , Value "paused" TBool         -- NEW (appended)
  ]

||| Version 2 (UNSAFE): Field removed
SchemaV2_Removed : Schema
SchemaV2_Removed = MkSchema "test.storage" testRoot
  [ Value "owner" TAddress
  -- totalSupply removed!
  ]

||| Version 2 (UNSAFE): Field inserted (causes reorder)
SchemaV2_Inserted : Schema
SchemaV2_Inserted = MkSchema "test.storage" testRoot
  [ Value "owner" TAddress
  , Value "paused" TBool         -- INSERTED - pushes totalSupply down
  , Value "totalSupply" TUint256
  ]

||| Version 2 (UNSAFE): Type changed
SchemaV2_TypeChanged : Schema
SchemaV2_TypeChanged = MkSchema "test.storage" testRoot
  [ Value "owner" TUint256       -- Was TAddress!
  , Value "totalSupply" TUint256
  ]

||| Version 2 (UNSAFE): Namespace changed
SchemaV2_NsChanged : Schema
SchemaV2_NsChanged = MkSchema "test.storage.v2" testRoot  -- Different namespace!
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  ]

||| Version 2 (UNSAFE): Root slot changed
SchemaV2_RootChanged : Schema
SchemaV2_RootChanged = MkSchema "test.storage" 0xDEADBEEF  -- Different root!
  [ Value "owner" TAddress
  , Value "totalSupply" TUint256
  ]

-- =============================================================================
-- Compile-Time Tests (these compile = tests pass)
-- =============================================================================

||| Test: Identity upgrade is always safe
test_identity : ()
test_identity = %runElab checkUpgrade SchemaV1 SchemaV1

||| Test: Append-only upgrade is safe
test_append : ()
test_append = %runElab checkUpgrade SchemaV1 SchemaV2_Safe

-- =============================================================================
-- Compile Error Examples (uncomment to see detailed error messages)
-- =============================================================================

-- test_removed : ()
-- test_removed = %runElab checkUpgrade SchemaV1 SchemaV2_Removed
-- Error: FIELD_REMOVED - Field 'totalSupply' at slot+1 was removed

-- test_inserted : ()
-- test_inserted = %runElab checkUpgrade SchemaV1 SchemaV2_Inserted
-- Error: FIELD_REORDERED - Expected 'totalSupply' but found 'paused'

-- test_typeChanged : ()
-- test_typeChanged = %runElab checkUpgrade SchemaV1 SchemaV2_TypeChanged
-- Error: TYPE_CHANGED - Type changed from address to uint256

-- test_nsChanged : ()
-- test_nsChanged = %runElab checkUpgrade SchemaV1 SchemaV2_NsChanged
-- Error: NAMESPACE_CHANGED - Namespace changed from 'test.storage' to 'test.storage.v2'

-- test_rootChanged : ()
-- test_rootChanged = %runElab checkUpgrade SchemaV1 SchemaV2_RootChanged
-- Error: ROOT_SLOT_CHANGED - Root slot changed

-- =============================================================================
-- Runtime Tests (pure validation logic)
-- =============================================================================

||| Test: Valid append-only upgrade
test_compat_valid : Bool
test_compat_valid = isCompatible (checkSchemaCompat SchemaV1 SchemaV2_Safe)

||| Test: Field removed is detected
test_compat_removed : Bool
test_compat_removed =
  case checkSchemaCompat SchemaV1 SchemaV2_Removed of
    Incompatible cs => any (\c => c.collisionType == FieldRemoved) cs
    Compatible => False

||| Test: Field reorder is detected
test_compat_inserted : Bool
test_compat_inserted =
  case checkSchemaCompat SchemaV1 SchemaV2_Inserted of
    Incompatible cs => any (\c => c.collisionType == FieldReordered) cs
    Compatible => False

||| Test: Type change is detected
test_compat_typeChanged : Bool
test_compat_typeChanged =
  case checkSchemaCompat SchemaV1 SchemaV2_TypeChanged of
    Incompatible cs => any (\c => c.collisionType == TypeChanged) cs
    Compatible => False

||| Test: Namespace change is detected
test_compat_nsChanged : Bool
test_compat_nsChanged =
  case checkSchemaCompat SchemaV1 SchemaV2_NsChanged of
    Incompatible cs => any (\c => c.collisionType == NamespaceChanged) cs
    Compatible => False

||| Test: Root slot change is detected
test_compat_rootChanged : Bool
test_compat_rootChanged =
  case checkSchemaCompat SchemaV1 SchemaV2_RootChanged of
    Incompatible cs => any (\c => c.collisionType == RootSlotChanged) cs
    Compatible => False

-- =============================================================================
-- Test Runner
-- =============================================================================

||| All runtime tests
export
allTests : List (String, Bool)
allTests =
  [ ("COMPAT: valid append-only", test_compat_valid)
  , ("COMPAT: field removed detected", test_compat_removed)
  , ("COMPAT: field reorder detected", test_compat_inserted)
  , ("COMPAT: type change detected", test_compat_typeChanged)
  , ("COMPAT: namespace change detected", test_compat_nsChanged)
  , ("COMPAT: root slot change detected", test_compat_rootChanged)
  ]

||| Run all tests and return (passed, total)
export
runTests : (Nat, Nat)
runTests =
  let results = map snd allTests
      passed = length (filter id results)
  in (passed, length allTests)

||| Check if all tests pass
export
allTestsPass : Bool
allTestsPass = fst runTests == snd runTests
