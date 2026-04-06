||| SourceMap Tests - Tests for Yul comment parsing and source mapping
module EvmCoverage.Tests.AllTests

import Data.List
import Data.SortedMap
import EvmCoverage.SourceMap
import EvmCoverage.StructuredExport
import EvmCoverage.DumpcasesParser
import EvmCoverage.PathRuntime
import EvmCoverage.Types

-- =============================================================================
-- YulComment Parsing Tests
-- =============================================================================

||| REQ_SRCMAP_001: parseYulComments should find comments in Yul source
export
test_parseYulComments_finds_comments : IO Bool
test_parseYulComments_finds_comments = do
  let yul = "/* Main:5:2--5:10 */ let x := 42 /* Main:6:2--6:15 */ let y := 1"
  let comments = parseYulComments yul
  pure $ length comments == 2

||| REQ_SRCMAP_002: parseYulComments should extract module name
export
test_parseYulComments_module_name : IO Bool
test_parseYulComments_module_name = do
  let yul = "/* MyModule:10:1--10:20 */ code"
  let comments = parseYulComments yul
  pure $ case comments of
    [c] => c.idrisLoc.moduleName == "MyModule"
    _ => False

||| REQ_SRCMAP_003: parseYulComments should extract line numbers
export
test_parseYulComments_line_numbers : IO Bool
test_parseYulComments_line_numbers = do
  let yul = "/* Test:42:5--43:10 */ code"
  let comments = parseYulComments yul
  pure $ case comments of
    [c] => c.idrisLoc.startLine == 42 && c.idrisLoc.endLine == 43
    _ => False

||| REQ_SRCMAP_004: parseYulComments should extract column numbers
export
test_parseYulComments_column_numbers : IO Bool
test_parseYulComments_column_numbers = do
  let yul = "/* Test:1:5--1:15 */ code"
  let comments = parseYulComments yul
  pure $ case comments of
    [c] => c.idrisLoc.startCol == 5 && c.idrisLoc.endCol == 15
    _ => False

||| REQ_SRCMAP_005: parseYulComments on empty string should return empty list
export
test_parseYulComments_empty : IO Bool
test_parseYulComments_empty = do
  let comments = parseYulComments ""
  pure $ length comments == 0

||| REQ_SRCMAP_006: parseYulComments with no comments should return empty list
export
test_parseYulComments_no_comments : IO Bool
test_parseYulComments_no_comments = do
  let yul = "let x := 42"
  let comments = parseYulComments yul
  pure $ length comments == 0

||| REQ_SRCMAP_007: parseYulComments should track yul offsets
export
test_parseYulComments_offsets : IO Bool
test_parseYulComments_offsets = do
  let yul = "/* A:1:1--1:2 */ x /* B:2:1--2:2 */ y"
  let comments = parseYulComments yul
  pure $ case comments of
    [c1, c2] => c1.yulOffset < c2.yulOffset
    _ => False

-- =============================================================================
-- IdrisLoc Tests
-- =============================================================================

||| REQ_SRCMAP_008: IdrisLoc Show instance
export
test_idrisLoc_show : IO Bool
test_idrisLoc_show = do
  let loc = MkIdrisLoc "Main" 10 5 10 15
  let s = show loc
  pure $ s == "Main:10:5"

||| REQ_SRCMAP_009: IdrisLoc Eq on same location
export
test_idrisLoc_eq_same : IO Bool
test_idrisLoc_eq_same = do
  let loc1 = MkIdrisLoc "Test" 5 1 5 10
  let loc2 = MkIdrisLoc "Test" 5 2 6 20  -- Different cols but same module/startLine
  pure $ loc1 == loc2

||| REQ_SRCMAP_010: IdrisLoc Eq on different location
export
test_idrisLoc_eq_diff : IO Bool
test_idrisLoc_eq_diff = do
  let loc1 = MkIdrisLoc "Test" 5 1 5 10
  let loc2 = MkIdrisLoc "Test" 6 1 6 10  -- Different line
  pure $ not (loc1 == loc2)

-- =============================================================================
-- findCommentEnd / parseLocComment Edge Case Tests
-- =============================================================================

||| REQ_SRCMAP_011: Unclosed comment (findCommentEnd returns Nothing)
export
test_unclosed_comment : IO Bool
test_unclosed_comment = do
  let yul = "/* Main:1:1--1:5 code without closing"
  let comments = parseYulComments yul
  pure $ length comments == 0

||| REQ_SRCMAP_012: Invalid comment format (parseLocComment returns Nothing)
export
test_invalid_comment_format : IO Bool
test_invalid_comment_format = do
  let yul = "/* not a valid loc format */ code"
  let comments = parseYulComments yul
  pure $ length comments == 0

||| REQ_SRCMAP_013: Missing double-dash separator
export
test_missing_separator : IO Bool
test_missing_separator = do
  let yul = "/* Main:1:1-1:5 */ code"  -- single dash instead of double
  let comments = parseYulComments yul
  pure $ length comments == 0

||| REQ_SRCMAP_014: Invalid line number (non-numeric)
export
test_invalid_line_number : IO Bool
test_invalid_line_number = do
  let yul = "/* Main:abc:1--1:5 */ code"
  let comments = parseYulComments yul
  pure $ length comments == 0

||| REQ_SRCMAP_015: Multiple consecutive comments
export
test_consecutive_comments : IO Bool
test_consecutive_comments = do
  let yul = "/* A:1:1--1:2 *//* B:2:1--2:2 *//* C:3:1--3:2 */"
  let comments = parseYulComments yul
  pure $ length comments == 3

||| REQ_SRCMAP_016: Comment with extra whitespace
export
test_comment_whitespace : IO Bool
test_comment_whitespace = do
  let yul = "/*   Main:10:5--10:15   */ code"
  let comments = parseYulComments yul
  pure $ case comments of
    [c] => c.idrisLoc.moduleName == "Main" && c.idrisLoc.startLine == 10
    _ => False

||| REQ_SRCMAP_017: Long comment body (tests findCommentEnd continuation)
export
test_long_comment : IO Bool
test_long_comment = do
  let yul = "/* VeryLongModuleName.SubModule.Function:1000:999--2000:888 */ x"
  let comments = parseYulComments yul
  pure $ case comments of
    [c] => c.idrisLoc.startLine == 1000 && c.idrisLoc.endLine == 2000
    _ => False

||| REQ_SRCMAP_018: findIdrisLoc with empty comments
export
test_findIdrisLoc_empty : IO Bool
test_findIdrisLoc_empty = do
  let result = findIdrisLoc [] 100
  pure $ case result of
    Nothing => True
    Just _ => False

||| REQ_SRCMAP_019: findIdrisLoc finds nearest preceding
export
test_findIdrisLoc_nearest : IO Bool
test_findIdrisLoc_nearest = do
  let c1 = MkYulComment 10 (MkIdrisLoc "A" 1 1 1 5)
  let c2 = MkYulComment 50 (MkIdrisLoc "B" 2 1 2 5)
  let c3 = MkYulComment 100 (MkIdrisLoc "C" 3 1 3 5)
  let result = findIdrisLoc [c1, c2, c3] 75  -- Should find c2 (offset 50)
  pure $ case result of
    Just loc => loc.moduleName == "B"
    Nothing => False

||| REQ_SRCMAP_020: buildYulToIdrisMap creates correct map
export
test_buildYulToIdrisMap : IO Bool
test_buildYulToIdrisMap = do
  let c1 = MkYulComment 10 (MkIdrisLoc "X" 1 1 1 5)
  let c2 = MkYulComment 20 (MkIdrisLoc "Y" 2 1 2 5)
  let m = buildYulToIdrisMap [c1, c2]
  pure $ case (lookup 10 m, lookup 20 m) of
    (Just l1, Just l2) => l1.moduleName == "X" && l2.moduleName == "Y"
    _ => False

-- =============================================================================
-- Structured Export Adapter Tests
-- =============================================================================

structuredSample : String
structuredSample = """
{
  "compiler_version": "0.8.0",
  "functions": [
    {
      "function_name": "Main.Functions.Vote.vote",
      "nodes": [
        {
          "node_id": "Main.Functions.Vote.vote#0:0",
          "source_span": "src/Main/Functions/Vote.idr:10:1-12:10",
          "branch_index": 0,
          "branch_label": "True",
          "origin": "user_clause"
        },
        {
          "node_id": "Main.Functions.Vote.vote#0:1",
          "source_span": "src/Main/Functions/Vote.idr:13:1-15:10",
          "branch_index": 1,
          "branch_label": "False",
          "origin": "impossible_clause"
        }
      ]
    }
  ]
}
"""

||| REQ_STRUCT_001: structured export parser recognizes compiler node ids
export
test_parseStructuredExport_node_ids : IO Bool
test_parseStructuredExport_node_ids = do
  pure $ case parseStructuredExport structuredSample of
    Right [b1, b2] =>
      b1.branchId.compilerNodeId == Just "Main.Functions.Vote.vote#0:0"
        && b2.branchId.compilerNodeId == Just "Main.Functions.Vote.vote#0:1"
    _ => False

||| REQ_STRUCT_002: structured export origin tags map to branch classes
export
test_parseStructuredExport_origin_mapping : IO Bool
test_parseStructuredExport_origin_mapping = do
  pure $ case parseStructuredExport structuredSample of
    Right [b1, b2] =>
      b1.branchClass == BCCanonical
        && b2.branchClass == BCExcludedNoClauses
    _ => False

||| REQ_STRUCT_003: static export adapter autodetects structured JSON
export
test_parseStaticExport_autodetects_json : IO Bool
test_parseStaticExport_autodetects_json = do
  pure $ case parseStaticExport structuredSample of
    Right [b1, _] => show b1.branchId == "Main.Functions.Vote.vote#0:0"
    _ => False

dumppathsSample : String
dumppathsSample = """
{
  "compiler_version": "0.8.0",
  "export_kind": "canonical_intrafunction_paths",
  "path_schema_version": 1,
  "functions": [
    {
      "function_name": "Main.partialMaybe",
      "paths": [
        {
          "path_id": "Main.partialMaybe#p0",
          "classification": "ReachableObligation",
          "terminal_kind": "reached_clause",
          "steps": [
            {
              "node_id": "Main.partialMaybe#0:0",
              "branch_index": 0,
              "origin": "user_clause",
              "branch_label": "Just"
            }
          ]
        },
        {
          "path_id": "Main.partialMaybe#p1",
          "classification": "UserAdmittedPartialGap",
          "terminal_kind": "partial_gap",
          "steps": [
            {
              "node_id": "Main.partialMaybe#0:1",
              "branch_index": 1,
              "origin": "compiler_partial_completion",
              "branch_label": "default"
            }
          ]
        }
      ]
    }
  ]
}
"""

||| REQ_PATHRT_001: covered terminal branch ids lift to exact path ids
export
test_pathHitsFromCoveredBranchIds_terminal_mapping : IO Bool
test_pathHitsFromCoveredBranchIds_terminal_mapping = do
  pure $ case pathHitsFromCoveredBranchIdsInContent ["Main.partialMaybe#0:0"] dumppathsSample of
    Right [hit] => hit.pathId == "Main.partialMaybe#p0" && hit.hitCount == 1
    _ => False

-- =============================================================================
-- Test Runner
-- =============================================================================

allTests : List (String, IO Bool)
allTests =
  [ ("REQ_SRCMAP_001: finds comments", test_parseYulComments_finds_comments)
  , ("REQ_SRCMAP_002: extracts module name", test_parseYulComments_module_name)
  , ("REQ_SRCMAP_003: extracts line numbers", test_parseYulComments_line_numbers)
  , ("REQ_SRCMAP_004: extracts column numbers", test_parseYulComments_column_numbers)
  , ("REQ_SRCMAP_005: empty string", test_parseYulComments_empty)
  , ("REQ_SRCMAP_006: no comments", test_parseYulComments_no_comments)
  , ("REQ_SRCMAP_007: tracks offsets", test_parseYulComments_offsets)
  , ("REQ_SRCMAP_008: IdrisLoc show", test_idrisLoc_show)
  , ("REQ_SRCMAP_009: IdrisLoc eq same", test_idrisLoc_eq_same)
  , ("REQ_SRCMAP_010: IdrisLoc eq diff", test_idrisLoc_eq_diff)
  , ("REQ_SRCMAP_011: unclosed comment", test_unclosed_comment)
  , ("REQ_SRCMAP_012: invalid format", test_invalid_comment_format)
  , ("REQ_SRCMAP_013: missing separator", test_missing_separator)
  , ("REQ_SRCMAP_014: invalid line number", test_invalid_line_number)
  , ("REQ_SRCMAP_015: consecutive comments", test_consecutive_comments)
  , ("REQ_SRCMAP_016: comment whitespace", test_comment_whitespace)
  , ("REQ_SRCMAP_017: long comment", test_long_comment)
  , ("REQ_SRCMAP_018: findIdrisLoc empty", test_findIdrisLoc_empty)
  , ("REQ_SRCMAP_019: findIdrisLoc nearest", test_findIdrisLoc_nearest)
  , ("REQ_SRCMAP_020: buildYulToIdrisMap", test_buildYulToIdrisMap)
  , ("REQ_STRUCT_001: structured export preserves node ids", test_parseStructuredExport_node_ids)
  , ("REQ_STRUCT_002: structured export maps origin tags", test_parseStructuredExport_origin_mapping)
  , ("REQ_STRUCT_003: static export autodetects structured JSON", test_parseStaticExport_autodetects_json)
  , ("REQ_PATHRT_001: terminal branch ids map to path ids", test_pathHitsFromCoveredBranchIds_terminal_mapping)
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
  putStrLn "Running SourceMap Tests..."
  putStrLn ""
  results <- traverse runTest allTests
  let passed = filter snd results
  let failed = filter (not . snd) results
  traverse_ (\(name, _) => putStrLn $ "  FAIL: " ++ name) failed
  putStrLn ""
  putStrLn $ "Results: " ++ show (length passed) ++ "/" ++ show (length results) ++ " passed"
  if length failed == 0
     then putStrLn "ALL TESTS PASSED"
     else putStrLn "SOME TESTS FAILED"

export
main : IO ()
main = runAllTests
