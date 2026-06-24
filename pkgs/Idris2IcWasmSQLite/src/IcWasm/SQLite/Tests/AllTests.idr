||| IcWasm SQLite Test Suite
|||
||| Pure function tests for IcWasm.SQLite module.
module IcWasm.SQLite.Tests.AllTests

import IcWasm.SQLite

%default covering

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

||| Test definition
public export
record TestDef where
  constructor MkTestDef
  testId      : String
  description : String
  run         : () -> Bool

||| Create test
test : String -> String -> (() -> Bool) -> TestDef
test tid desc fn = MkTestDef tid desc fn

-- =============================================================================
-- toSqlResult Tests
-- =============================================================================

test_REQ_SQL_001 : () -> Bool
test_REQ_SQL_001 () =
  -- toSqlResult 0 => SqlOk
  toSqlResult 0 == SqlOk

test_REQ_SQL_002 : () -> Bool
test_REQ_SQL_002 () =
  -- toSqlResult 1 => SqlError
  toSqlResult 1 == SqlError

test_REQ_SQL_003 : () -> Bool
test_REQ_SQL_003 () =
  -- toSqlResult 5 => SqlBusy
  toSqlResult 5 == SqlBusy

test_REQ_SQL_004 : () -> Bool
test_REQ_SQL_004 () =
  -- toSqlResult 6 => SqlLocked
  toSqlResult 6 == SqlLocked

test_REQ_SQL_005 : () -> Bool
test_REQ_SQL_005 () =
  -- toSqlResult 7 => SqlNoMem
  toSqlResult 7 == SqlNoMem

test_REQ_SQL_006 : () -> Bool
test_REQ_SQL_006 () =
  -- toSqlResult 19 => SqlConstraint
  toSqlResult 19 == SqlConstraint

test_REQ_SQL_007 : () -> Bool
test_REQ_SQL_007 () =
  -- toSqlResult 100 => SqlRow
  toSqlResult 100 == SqlRow

test_REQ_SQL_008 : () -> Bool
test_REQ_SQL_008 () =
  -- toSqlResult 101 => SqlDone
  toSqlResult 101 == SqlDone

test_REQ_SQL_009 : () -> Bool
test_REQ_SQL_009 () =
  -- toSqlResult unknown => SqlOther
  toSqlResult 42 == SqlOther 42

-- =============================================================================
-- toSqlType Tests
-- =============================================================================

test_REQ_SQL_010 : () -> Bool
test_REQ_SQL_010 () =
  -- toSqlType 1 => SqlInteger
  toSqlType 1 == SqlInteger

test_REQ_SQL_011 : () -> Bool
test_REQ_SQL_011 () =
  -- toSqlType 2 => SqlFloat
  toSqlType 2 == SqlFloat

test_REQ_SQL_012 : () -> Bool
test_REQ_SQL_012 () =
  -- toSqlType 3 => SqlText
  toSqlType 3 == SqlText

test_REQ_SQL_013 : () -> Bool
test_REQ_SQL_013 () =
  -- toSqlType 4 => SqlBlob
  toSqlType 4 == SqlBlob

test_REQ_SQL_014 : () -> Bool
test_REQ_SQL_014 () =
  -- toSqlType 0 => SqlNull (unmapped)
  toSqlType 0 == SqlNull

test_REQ_SQL_015 : () -> Bool
test_REQ_SQL_015 () =
  -- toSqlType 99 => SqlNull (unmapped)
  toSqlType 99 == SqlNull

-- =============================================================================
-- Eq SqlResult Tests
-- =============================================================================

test_REQ_SQL_016 : () -> Bool
test_REQ_SQL_016 () =
  -- SqlOk == SqlOk
  SqlOk == SqlOk

test_REQ_SQL_017 : () -> Bool
test_REQ_SQL_017 () =
  -- SqlOk /= SqlError
  SqlOk /= SqlError

test_REQ_SQL_018 : () -> Bool
test_REQ_SQL_018 () =
  -- SqlOther equality (same code)
  SqlOther 42 == SqlOther 42

test_REQ_SQL_019 : () -> Bool
test_REQ_SQL_019 () =
  -- SqlOther inequality (different code)
  SqlOther 42 /= SqlOther 99

-- =============================================================================
-- Eq SqlType Tests
-- =============================================================================

test_REQ_SQL_020 : () -> Bool
test_REQ_SQL_020 () =
  -- SqlInteger == SqlInteger
  SqlInteger == SqlInteger

test_REQ_SQL_021 : () -> Bool
test_REQ_SQL_021 () =
  -- SqlInteger /= SqlText
  SqlInteger /= SqlText

-- =============================================================================
-- Show SqlResult Tests
-- =============================================================================

test_REQ_SQL_022 : () -> Bool
test_REQ_SQL_022 () =
  -- show SqlOk
  show SqlOk == "SQLITE_OK"

test_REQ_SQL_023 : () -> Bool
test_REQ_SQL_023 () =
  -- show SqlError
  show SqlError == "SQLITE_ERROR"

test_REQ_SQL_024 : () -> Bool
test_REQ_SQL_024 () =
  -- show SqlOther
  show (SqlOther 42) == "SQLITE_42"

-- =============================================================================
-- Show SqlType Tests
-- =============================================================================

test_REQ_SQL_025 : () -> Bool
test_REQ_SQL_025 () =
  -- show SqlInteger
  show SqlInteger == "INTEGER"

test_REQ_SQL_026 : () -> Bool
test_REQ_SQL_026 () =
  -- show SqlNull
  show SqlNull == "NULL"

-- =============================================================================
-- QueryRow Tests
-- =============================================================================

test_REQ_SQL_027 : () -> Bool
test_REQ_SQL_027 () =
  -- QueryRow construction
  let row = MkQueryRow [1, 2, 3]
  in row.columns == [1, 2, 3]

test_REQ_SQL_028 : () -> Bool
test_REQ_SQL_028 () =
  -- QueryRow empty columns
  let row = MkQueryRow []
  in row.columns == []

-- =============================================================================
-- SchemaVersion Tests
-- =============================================================================

test_REQ_SQL_029 : () -> Bool
test_REQ_SQL_029 () =
  -- SchemaVersion is Nat
  let v : SchemaVersion = 42
  in v == 42

-- =============================================================================
-- All Tests
-- =============================================================================

||| All tests
export
allTests : List TestDef
allTests =
  -- toSqlResult
  [ test "REQ_SQL_001" "toSqlResult 0 => SqlOk" test_REQ_SQL_001
  , test "REQ_SQL_002" "toSqlResult 1 => SqlError" test_REQ_SQL_002
  , test "REQ_SQL_003" "toSqlResult 5 => SqlBusy" test_REQ_SQL_003
  , test "REQ_SQL_004" "toSqlResult 6 => SqlLocked" test_REQ_SQL_004
  , test "REQ_SQL_005" "toSqlResult 7 => SqlNoMem" test_REQ_SQL_005
  , test "REQ_SQL_006" "toSqlResult 19 => SqlConstraint" test_REQ_SQL_006
  , test "REQ_SQL_007" "toSqlResult 100 => SqlRow" test_REQ_SQL_007
  , test "REQ_SQL_008" "toSqlResult 101 => SqlDone" test_REQ_SQL_008
  , test "REQ_SQL_009" "toSqlResult unknown => SqlOther" test_REQ_SQL_009

  -- toSqlType
  , test "REQ_SQL_010" "toSqlType 1 => SqlInteger" test_REQ_SQL_010
  , test "REQ_SQL_011" "toSqlType 2 => SqlFloat" test_REQ_SQL_011
  , test "REQ_SQL_012" "toSqlType 3 => SqlText" test_REQ_SQL_012
  , test "REQ_SQL_013" "toSqlType 4 => SqlBlob" test_REQ_SQL_013
  , test "REQ_SQL_014" "toSqlType 0 => SqlNull" test_REQ_SQL_014
  , test "REQ_SQL_015" "toSqlType 99 => SqlNull" test_REQ_SQL_015

  -- Eq SqlResult
  , test "REQ_SQL_016" "SqlResult Eq reflexive" test_REQ_SQL_016
  , test "REQ_SQL_017" "SqlResult Eq different" test_REQ_SQL_017
  , test "REQ_SQL_018" "SqlOther Eq same code" test_REQ_SQL_018
  , test "REQ_SQL_019" "SqlOther Eq different code" test_REQ_SQL_019

  -- Eq SqlType
  , test "REQ_SQL_020" "SqlType Eq reflexive" test_REQ_SQL_020
  , test "REQ_SQL_021" "SqlType Eq different" test_REQ_SQL_021

  -- Show SqlResult
  , test "REQ_SQL_022" "Show SqlOk" test_REQ_SQL_022
  , test "REQ_SQL_023" "Show SqlError" test_REQ_SQL_023
  , test "REQ_SQL_024" "Show SqlOther" test_REQ_SQL_024

  -- Show SqlType
  , test "REQ_SQL_025" "Show SqlInteger" test_REQ_SQL_025
  , test "REQ_SQL_026" "Show SqlNull" test_REQ_SQL_026

  -- QueryRow
  , test "REQ_SQL_027" "QueryRow construction" test_REQ_SQL_027
  , test "REQ_SQL_028" "QueryRow empty columns" test_REQ_SQL_028

  -- SchemaVersion
  , test "REQ_SQL_029" "SchemaVersion type alias" test_REQ_SQL_029
  ]

||| Run all tests - pure version
export
runAllTestsPure : (Nat, Nat)
runAllTestsPure =
  let results = map (\t => t.run ()) allTests
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)

||| Run all tests with IO output (for lazy core ask compatibility)
export
runAllTests : IO ()
runAllTests = do
  let (passed, failed) = runAllTestsPure
  putStrLn $ "Tests: " ++ show passed ++ "/" ++ show (passed + failed) ++ " passed"
  if failed == 0
    then putStrLn "All tests PASSED!"
    else putStrLn $ show failed ++ " tests FAILED"
