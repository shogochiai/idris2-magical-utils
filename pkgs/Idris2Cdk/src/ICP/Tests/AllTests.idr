||| ICP Test Suite
|||
||| Tests for ICP.Types
module ICP.Tests.AllTests

import ICP.Types
import Data.List

%default total

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

public export
record TestDef where
  constructor MkTest
  testId    : String
  testDesc  : String
  testFunc  : IO Bool

public export
test : String -> String -> IO Bool -> TestDef
test = MkTest

runTest : TestDef -> IO Bool
runTest t = t.testFunc

countPassed : List Bool -> Nat
countPassed = length . filter id

export
runTestSuite : String -> List TestDef -> IO ()
runTestSuite name tests = do
    results <- traverse runTest tests
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Passed: " ++ show (countPassed results) ++ "/" ++ show (length results)
    for_ (zip tests results) $ \(t, r) =>
      putStrLn $ (if r then "[PASS] " else "[FAIL] ") ++ t.testId ++ ": " ++ t.testDesc

-- =============================================================================
-- RejectCode Tests
-- =============================================================================

test_rejectCodeFromInt : IO Bool
test_rejectCodeFromInt = pure $
  rejectCodeFromInt 0 == NoError &&
  rejectCodeFromInt 1 == SysFatal &&
  rejectCodeFromInt 2 == SysTransient &&
  rejectCodeFromInt 3 == DestinationInvalid &&
  rejectCodeFromInt 4 == CanisterReject &&
  rejectCodeFromInt 5 == CanisterError &&
  rejectCodeFromInt 99 == CanisterError

test_rejectCodeToInt : IO Bool
test_rejectCodeToInt = pure $
  rejectCodeToInt NoError == 0 &&
  rejectCodeToInt SysFatal == 1 &&
  rejectCodeToInt SysTransient == 2 &&
  rejectCodeToInt DestinationInvalid == 3 &&
  rejectCodeToInt CanisterReject == 4 &&
  rejectCodeToInt CanisterError == 5

test_roundtrip : IO Bool
test_roundtrip = pure $
  rejectCodeFromInt (cast $ rejectCodeToInt NoError) == NoError &&
  rejectCodeFromInt (cast $ rejectCodeToInt SysFatal) == SysFatal &&
  rejectCodeFromInt (cast $ rejectCodeToInt CanisterReject) == CanisterReject

test_equality : IO Bool
test_equality = pure $
  NoError == NoError &&
  SysFatal == SysFatal &&
  not (NoError == SysFatal) &&
  not (CanisterReject == CanisterError)

test_show : IO Bool
test_show = pure $
  show NoError == "NoError" &&
  show SysFatal == "SysFatal" &&
  show SysTransient == "SysTransient" &&
  show DestinationInvalid == "DestinationInvalid" &&
  show CanisterReject == "CanisterReject" &&
  show CanisterError == "CanisterError"

-- =============================================================================
-- Phase Tests
-- =============================================================================

test_phase_show : IO Bool
test_phase_show = pure $
  show ICP.Types.Init == "Init" &&
  show ICP.Types.Update == "Update" &&
  show ICP.Types.Query == "Query"

test_phase_eq : IO Bool
test_phase_eq = pure $
  ICP.Types.Init == ICP.Types.Init &&
  ICP.Types.Update == ICP.Types.Update &&
  not (ICP.Types.Init == ICP.Types.Update)

-- =============================================================================
-- Test Collection
-- =============================================================================

public export
allTests : List TestDef
allTests =
  [ test "ICP_TYP_001" "rejectCodeFromInt" test_rejectCodeFromInt
  , test "ICP_TYP_002" "rejectCodeToInt" test_rejectCodeToInt
  , test "ICP_TYP_003" "roundtrip" test_roundtrip
  , test "ICP_TYP_004" "equality" test_equality
  , test "ICP_TYP_005" "show" test_show
  , test "ICP_TYP_006" "phase show" test_phase_show
  , test "ICP_TYP_007" "phase eq" test_phase_eq
  ]

export
runAllTests : IO ()
runAllTests = runTestSuite "ICP.Types" allTests

main : IO ()
main = runAllTests
