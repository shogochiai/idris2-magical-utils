||| Core Module Tests - Conflict and FR coverage
|||
||| Tests for Conflict type, FR encoding/decoding, severity, and category functions
module Subcontract.Core.Tests.AllTests

import Subcontract.Core.Conflict
import Subcontract.Core.FR
import Subcontract.Core.Evidence
import Subcontract.Core.Outcome

-- =============================================================================
-- Conflict Equality Tests
-- =============================================================================

||| REQ_CONF_001: Conflict Eq reflexivity - all types equal to themselves
export
test_conflict_eq_reflexive : IO Bool
test_conflict_eq_reflexive =
  pure $ Revert == Revert &&
         Reentrancy == Reentrancy &&
         AuthViolation == AuthViolation &&
         CEIViolation == CEIViolation &&
         EpochMismatch == EpochMismatch &&
         InitAlready == InitAlready &&
         NotInitialized == NotInitialized &&
         StorageAlias == StorageAlias &&
         ExternalCallForbidden == ExternalCallForbidden &&
         UnsafeEntryContext == UnsafeEntryContext

||| REQ_CONF_002: Conflict Eq reflexivity - remaining types
export
test_conflict_eq_reflexive2 : IO Bool
test_conflict_eq_reflexive2 =
  pure $ GasExhausted == GasExhausted &&
         DecodeError == DecodeError &&
         ArithmeticError == ArithmeticError &&
         UpgradeNotAllowed == UpgradeNotAllowed &&
         RollbackNotAllowed == RollbackNotAllowed &&
         AssetAlreadySpent == AssetAlreadySpent &&
         AssetNotOwned == AssetNotOwned &&
         InvariantViolation == InvariantViolation &&
         InvalidTransition == InvalidTransition &&
         ProofMissing == ProofMissing

||| REQ_CONF_003: Conflict Eq different types are not equal
export
test_conflict_eq_different : IO Bool
test_conflict_eq_different =
  pure $ not (Revert == Reentrancy) &&
         not (AuthViolation == CEIViolation) &&
         not (GasExhausted == DecodeError) &&
         not (StorageAlias == EpochMismatch)

-- =============================================================================
-- conflictTag Tests
-- =============================================================================

||| REQ_CONF_004: conflictTag returns correct strings (security)
export
test_conflictTag_security : IO Bool
test_conflictTag_security =
  pure $ conflictTag Reentrancy == "REENTRANCY" &&
         conflictTag AuthViolation == "AUTH_VIOLATION" &&
         conflictTag CEIViolation == "CEI_VIOLATION"

||| REQ_CONF_005: conflictTag returns correct strings (state)
export
test_conflictTag_state : IO Bool
test_conflictTag_state =
  pure $ conflictTag EpochMismatch == "EPOCH_MISMATCH" &&
         conflictTag InitAlready == "INIT_ALREADY" &&
         conflictTag NotInitialized == "NOT_INITIALIZED" &&
         conflictTag InvalidTransition == "INVALID_TRANSITION" &&
         conflictTag UpgradeNotAllowed == "UPGRADE_NOT_ALLOWED" &&
         conflictTag RollbackNotAllowed == "ROLLBACK_NOT_ALLOWED"

||| REQ_CONF_006: conflictTag returns correct strings (resource)
export
test_conflictTag_resource : IO Bool
test_conflictTag_resource =
  pure $ conflictTag StorageAlias == "STORAGE_ALIAS" &&
         conflictTag AssetAlreadySpent == "ASSET_ALREADY_SPENT" &&
         conflictTag AssetNotOwned == "ASSET_NOT_OWNED" &&
         conflictTag InvariantViolation == "INVARIANT_VIOLATION"

||| REQ_CONF_007: conflictTag returns correct strings (execution)
export
test_conflictTag_execution : IO Bool
test_conflictTag_execution =
  pure $ conflictTag Revert == "REVERT" &&
         conflictTag GasExhausted == "GAS_EXHAUSTED" &&
         conflictTag DecodeError == "DECODE_ERROR" &&
         conflictTag ArithmeticError == "ARITHMETIC_ERROR" &&
         conflictTag ProofMissing == "PROOF_MISSING"

||| REQ_CONF_008: conflictTag returns correct strings (policy)
export
test_conflictTag_policy : IO Bool
test_conflictTag_policy =
  pure $ conflictTag ExternalCallForbidden == "EXTERNAL_CALL_FORBIDDEN" &&
         conflictTag UnsafeEntryContext == "UNSAFE_ENTRY_CONTEXT"

-- =============================================================================
-- severity Tests
-- =============================================================================

||| REQ_CONF_009: severity Critical conflicts
export
test_severity_critical : IO Bool
test_severity_critical =
  pure $ severity Reentrancy == Critical &&
         severity AuthViolation == Critical &&
         severity CEIViolation == Critical &&
         severity AssetAlreadySpent == Critical

||| REQ_CONF_010: severity High conflicts
export
test_severity_high : IO Bool
test_severity_high =
  pure $ severity StorageAlias == High &&
         severity EpochMismatch == High &&
         severity UpgradeNotAllowed == High &&
         severity InvariantViolation == High

||| REQ_CONF_011: severity Medium conflicts
export
test_severity_medium : IO Bool
test_severity_medium =
  pure $ severity InvalidTransition == Medium &&
         severity InitAlready == Medium &&
         severity NotInitialized == Medium &&
         severity ExternalCallForbidden == Medium &&
         severity UnsafeEntryContext == Medium &&
         severity RollbackNotAllowed == Medium &&
         severity AssetNotOwned == Medium

||| REQ_CONF_012: severity Low and Info conflicts
export
test_severity_low_info : IO Bool
test_severity_low_info =
  pure $ severity DecodeError == Low &&
         severity ArithmeticError == Low &&
         severity Revert == Low &&
         severity ProofMissing == Low &&
         severity GasExhausted == Info

-- =============================================================================
-- category Tests
-- =============================================================================

||| REQ_CONF_013: category SecurityConflict
export
test_category_security : IO Bool
test_category_security =
  pure $ category Reentrancy == SecurityConflict &&
         category AuthViolation == SecurityConflict &&
         category CEIViolation == SecurityConflict

||| REQ_CONF_014: category StateConflict
export
test_category_state : IO Bool
test_category_state =
  pure $ category InitAlready == StateConflict &&
         category NotInitialized == StateConflict &&
         category EpochMismatch == StateConflict &&
         category InvalidTransition == StateConflict &&
         category UpgradeNotAllowed == StateConflict &&
         category RollbackNotAllowed == StateConflict

||| REQ_CONF_015: category ResourceConflict
export
test_category_resource : IO Bool
test_category_resource =
  pure $ category AssetAlreadySpent == ResourceConflict &&
         category AssetNotOwned == ResourceConflict &&
         category StorageAlias == ResourceConflict &&
         category InvariantViolation == ResourceConflict

||| REQ_CONF_016: category ExecutionConflict
export
test_category_execution : IO Bool
test_category_execution =
  pure $ category GasExhausted == ExecutionConflict &&
         category DecodeError == ExecutionConflict &&
         category ArithmeticError == ExecutionConflict &&
         category Revert == ExecutionConflict &&
         category ProofMissing == ExecutionConflict

||| REQ_CONF_017: category PolicyConflict
export
test_category_policy : IO Bool
test_category_policy =
  pure $ category ExternalCallForbidden == PolicyConflict &&
         category UnsafeEntryContext == PolicyConflict

-- =============================================================================
-- encodeConflict Tests
-- =============================================================================

||| REQ_FR_001: encodeConflict security conflicts (0-3)
export
test_encodeConflict_security : IO Bool
test_encodeConflict_security =
  pure $ encodeConflict Revert == 0 &&
         encodeConflict Reentrancy == 1 &&
         encodeConflict AuthViolation == 2 &&
         encodeConflict CEIViolation == 3

||| REQ_FR_002: encodeConflict state conflicts (4-6)
export
test_encodeConflict_state : IO Bool
test_encodeConflict_state =
  pure $ encodeConflict EpochMismatch == 4 &&
         encodeConflict InitAlready == 5 &&
         encodeConflict NotInitialized == 6

||| REQ_FR_003: encodeConflict resource conflicts (7-9)
export
test_encodeConflict_resource : IO Bool
test_encodeConflict_resource =
  pure $ encodeConflict StorageAlias == 7 &&
         encodeConflict ExternalCallForbidden == 8 &&
         encodeConflict UnsafeEntryContext == 9

||| REQ_FR_004: encodeConflict execution conflicts (10-14)
export
test_encodeConflict_execution : IO Bool
test_encodeConflict_execution =
  pure $ encodeConflict GasExhausted == 10 &&
         encodeConflict DecodeError == 11 &&
         encodeConflict ArithmeticError == 12 &&
         encodeConflict UpgradeNotAllowed == 13 &&
         encodeConflict RollbackNotAllowed == 14

||| REQ_FR_005: encodeConflict remaining conflicts (15-19)
export
test_encodeConflict_remaining : IO Bool
test_encodeConflict_remaining =
  pure $ encodeConflict AssetAlreadySpent == 15 &&
         encodeConflict AssetNotOwned == 16 &&
         encodeConflict InvariantViolation == 17 &&
         encodeConflict InvalidTransition == 18 &&
         encodeConflict ProofMissing == 19

-- =============================================================================
-- decodeConflict Tests
-- =============================================================================

||| REQ_FR_006: decodeConflict security conflicts (0-3)
export
test_decodeConflict_security : IO Bool
test_decodeConflict_security =
  pure $ decodeConflict 0 == Revert &&
         decodeConflict 1 == Reentrancy &&
         decodeConflict 2 == AuthViolation &&
         decodeConflict 3 == CEIViolation

||| REQ_FR_007: decodeConflict state conflicts (4-6)
export
test_decodeConflict_state : IO Bool
test_decodeConflict_state =
  pure $ decodeConflict 4 == EpochMismatch &&
         decodeConflict 5 == InitAlready &&
         decodeConflict 6 == NotInitialized

||| REQ_FR_008: decodeConflict resource conflicts (7-9)
export
test_decodeConflict_resource : IO Bool
test_decodeConflict_resource =
  pure $ decodeConflict 7 == StorageAlias &&
         decodeConflict 8 == ExternalCallForbidden &&
         decodeConflict 9 == UnsafeEntryContext

||| REQ_FR_009: decodeConflict execution conflicts (10-14)
export
test_decodeConflict_execution : IO Bool
test_decodeConflict_execution =
  pure $ decodeConflict 10 == GasExhausted &&
         decodeConflict 11 == DecodeError &&
         decodeConflict 12 == ArithmeticError &&
         decodeConflict 13 == UpgradeNotAllowed &&
         decodeConflict 14 == RollbackNotAllowed

||| REQ_FR_010: decodeConflict remaining conflicts (15-19)
export
test_decodeConflict_remaining : IO Bool
test_decodeConflict_remaining =
  pure $ decodeConflict 15 == AssetAlreadySpent &&
         decodeConflict 16 == AssetNotOwned &&
         decodeConflict 17 == InvariantViolation &&
         decodeConflict 18 == InvalidTransition &&
         decodeConflict 19 == ProofMissing

||| REQ_FR_011: decodeConflict unknown codes default to Revert
export
test_decodeConflict_unknown : IO Bool
test_decodeConflict_unknown =
  pure $ decodeConflict 20 == Revert &&
         decodeConflict 100 == Revert &&
         decodeConflict 255 == Revert

-- =============================================================================
-- encode/decode roundtrip Tests
-- =============================================================================

||| REQ_FR_012: encode/decode roundtrip for all Conflicts
export
test_encode_decode_roundtrip : IO Bool
test_encode_decode_roundtrip =
  pure $ decodeConflict (encodeConflict Revert) == Revert &&
         decodeConflict (encodeConflict Reentrancy) == Reentrancy &&
         decodeConflict (encodeConflict AuthViolation) == AuthViolation &&
         decodeConflict (encodeConflict CEIViolation) == CEIViolation &&
         decodeConflict (encodeConflict EpochMismatch) == EpochMismatch &&
         decodeConflict (encodeConflict InitAlready) == InitAlready &&
         decodeConflict (encodeConflict NotInitialized) == NotInitialized &&
         decodeConflict (encodeConflict StorageAlias) == StorageAlias &&
         decodeConflict (encodeConflict ExternalCallForbidden) == ExternalCallForbidden &&
         decodeConflict (encodeConflict UnsafeEntryContext) == UnsafeEntryContext

||| REQ_FR_013: encode/decode roundtrip for remaining Conflicts
export
test_encode_decode_roundtrip2 : IO Bool
test_encode_decode_roundtrip2 =
  pure $ decodeConflict (encodeConflict GasExhausted) == GasExhausted &&
         decodeConflict (encodeConflict DecodeError) == DecodeError &&
         decodeConflict (encodeConflict ArithmeticError) == ArithmeticError &&
         decodeConflict (encodeConflict UpgradeNotAllowed) == UpgradeNotAllowed &&
         decodeConflict (encodeConflict RollbackNotAllowed) == RollbackNotAllowed &&
         decodeConflict (encodeConflict AssetAlreadySpent) == AssetAlreadySpent &&
         decodeConflict (encodeConflict AssetNotOwned) == AssetNotOwned &&
         decodeConflict (encodeConflict InvariantViolation) == InvariantViolation &&
         decodeConflict (encodeConflict InvalidTransition) == InvalidTransition &&
         decodeConflict (encodeConflict ProofMissing) == ProofMissing

-- =============================================================================
-- FR Helper Tests
-- =============================================================================

||| REQ_FR_014: isRecoverableFR only GasExhausted is recoverable
export
test_isRecoverableFR : IO Bool
test_isRecoverableFR =
  pure $ isRecoverableFR GasExhausted == True &&
         isRecoverableFR Reentrancy == False &&
         isRecoverableFR AuthViolation == False &&
         isRecoverableFR Revert == False

||| REQ_FR_015: shouldEscalateFR identifies security-critical conflicts
export
test_shouldEscalateFR : IO Bool
test_shouldEscalateFR =
  pure $ shouldEscalateFR Reentrancy == True &&
         shouldEscalateFR AuthViolation == True &&
         shouldEscalateFR CEIViolation == True &&
         shouldEscalateFR StorageAlias == True &&
         shouldEscalateFR GasExhausted == False &&
         shouldEscalateFR Revert == False

-- =============================================================================
-- requireFR Tests
-- =============================================================================

||| REQ_FR_016: requireFR returns Ok when condition is True
export
test_requireFR_ok : IO Bool
test_requireFR_ok =
  pure $ case requireFR True AuthViolation "test" of
    Ok () => True
    Fail _ _ => False

||| REQ_FR_017: requireFR returns Fail when condition is False
export
test_requireFR_fail : IO Bool
test_requireFR_fail =
  pure $ case requireFR False AuthViolation "test" of
    Ok () => False
    Fail c _ => c == AuthViolation

||| REQ_FR_018: requireJustFR returns Ok for Just
export
test_requireJustFR_ok : IO Bool
test_requireJustFR_ok =
  pure $ case requireJustFR (Just 42) DecodeError "test" of
    Ok x => x == 42
    Fail _ _ => False

||| REQ_FR_019: requireJustFR returns Fail for Nothing
export
test_requireJustFR_fail : IO Bool
test_requireJustFR_fail =
  pure $ case requireJustFR (the (Maybe Int) Nothing) DecodeError "test" of
    Ok _ => False
    Fail c _ => c == DecodeError

-- =============================================================================
-- Test Runner
-- =============================================================================

allTests : List (String, IO Bool)
allTests =
  [ ("REQ_CONF_001: Conflict Eq reflexive 1", test_conflict_eq_reflexive)
  , ("REQ_CONF_002: Conflict Eq reflexive 2", test_conflict_eq_reflexive2)
  , ("REQ_CONF_003: Conflict Eq different", test_conflict_eq_different)
  , ("REQ_CONF_004: conflictTag security", test_conflictTag_security)
  , ("REQ_CONF_005: conflictTag state", test_conflictTag_state)
  , ("REQ_CONF_006: conflictTag resource", test_conflictTag_resource)
  , ("REQ_CONF_007: conflictTag execution", test_conflictTag_execution)
  , ("REQ_CONF_008: conflictTag policy", test_conflictTag_policy)
  , ("REQ_CONF_009: severity critical", test_severity_critical)
  , ("REQ_CONF_010: severity high", test_severity_high)
  , ("REQ_CONF_011: severity medium", test_severity_medium)
  , ("REQ_CONF_012: severity low/info", test_severity_low_info)
  , ("REQ_CONF_013: category security", test_category_security)
  , ("REQ_CONF_014: category state", test_category_state)
  , ("REQ_CONF_015: category resource", test_category_resource)
  , ("REQ_CONF_016: category execution", test_category_execution)
  , ("REQ_CONF_017: category policy", test_category_policy)
  , ("REQ_FR_001: encodeConflict security", test_encodeConflict_security)
  , ("REQ_FR_002: encodeConflict state", test_encodeConflict_state)
  , ("REQ_FR_003: encodeConflict resource", test_encodeConflict_resource)
  , ("REQ_FR_004: encodeConflict execution", test_encodeConflict_execution)
  , ("REQ_FR_005: encodeConflict remaining", test_encodeConflict_remaining)
  , ("REQ_FR_006: decodeConflict security", test_decodeConflict_security)
  , ("REQ_FR_007: decodeConflict state", test_decodeConflict_state)
  , ("REQ_FR_008: decodeConflict resource", test_decodeConflict_resource)
  , ("REQ_FR_009: decodeConflict execution", test_decodeConflict_execution)
  , ("REQ_FR_010: decodeConflict remaining", test_decodeConflict_remaining)
  , ("REQ_FR_011: decodeConflict unknown", test_decodeConflict_unknown)
  , ("REQ_FR_012: encode/decode roundtrip 1", test_encode_decode_roundtrip)
  , ("REQ_FR_013: encode/decode roundtrip 2", test_encode_decode_roundtrip2)
  , ("REQ_FR_014: isRecoverableFR", test_isRecoverableFR)
  , ("REQ_FR_015: shouldEscalateFR", test_shouldEscalateFR)
  , ("REQ_FR_016: requireFR ok", test_requireFR_ok)
  , ("REQ_FR_017: requireFR fail", test_requireFR_fail)
  , ("REQ_FR_018: requireJustFR ok", test_requireJustFR_ok)
  , ("REQ_FR_019: requireJustFR fail", test_requireJustFR_fail)
  ]

runTest : (String, IO Bool) -> IO (String, Bool)
runTest (name, test) = do
  result <- test
  putStrLn $ (if result then "[PASS] " else "[FAIL] ") ++ name
  pure (name, result)

||| Run all tests - entry point for lazy test runner
export
runAllTests : IO ()
runAllTests = do
  putStrLn "Running Subcontract Core Tests..."
  putStrLn ""
  results <- traverse runTest allTests
  let passed = filter snd results
  let failed = filter (not . snd) results
  putStrLn ""
  putStrLn $ "Results: " ++ show (length passed) ++ "/" ++ show (length results) ++ " passed"
  if length failed == 0
     then putStrLn "ALL TESTS PASSED"
     else putStrLn "SOME TESTS FAILED"

export
main : IO ()
main = runAllTests
