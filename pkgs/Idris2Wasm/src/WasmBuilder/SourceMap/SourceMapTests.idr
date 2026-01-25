||| SourceMap Tests
module WasmBuilder.SourceMap.SourceMapTests

import Data.List
import Data.String
import WasmBuilder.SourceMap.SourceMap
import WasmBuilder.SourceMap.VLQ

%default covering

-- =============================================================================
-- Test Definitions
-- =============================================================================

public export
record SMTestDef where
  constructor MkSMTestDef
  specId : String
  description : String
  run : () -> Bool

smTest : String -> String -> (() -> Bool) -> SMTestDef
smTest sid desc fn = MkSMTestDef sid desc fn

-- =============================================================================
-- RefC Comment Parsing Tests
-- =============================================================================

test_parse_simple_comment : () -> Bool
test_parse_simple_comment () =
  let mappings = parseRefCComments "test.c" "    return NULL;  // Main:49:8--49:20"
  in length mappings == 1

test_parse_multiline : () -> Bool
test_parse_multiline () =
  let content = unlines
        [ "void foo() {"
        , "    return NULL;  // Main:10:1--10:5"
        , "    return 42;    // Lib:20:3--20:10"
        , "}"
        ]
      mappings = parseRefCComments "test.c" content
  in length mappings == 2

test_parse_no_comment : () -> Bool
test_parse_no_comment () =
  let mappings = parseRefCComments "test.c" "void foo() { return; }"
  in length mappings == 0

test_parse_extracts_module : () -> Bool
test_parse_extracts_module () =
  let mappings = parseRefCComments "test.c" "code // Data.List:5:1--5:10"
  in case mappings of
       [m] => m.idrisLoc.file == "Data.List"
       _ => False

test_parse_extracts_line : () -> Bool
test_parse_extracts_line () =
  let mappings = parseRefCComments "test.c" "code // Main:42:1--42:5"
  in case mappings of
       [m] => m.idrisLoc.startLine == 42
       _ => False

-- =============================================================================
-- JSON Generation Tests
-- =============================================================================

test_json_has_version : () -> Bool
test_json_has_version () =
  let sm = MkSourceMapV3 3 "test.wasm" "" ["Main.idr"] [] "AAAA"
      json = toJson sm
  in isInfixOf "\"version\": 3" json

test_json_has_sources : () -> Bool
test_json_has_sources () =
  let sm = MkSourceMapV3 3 "test.wasm" "" ["Main.idr", "Lib.idr"] [] ""
      json = toJson sm
  in isInfixOf "\"Main.idr\"" json && isInfixOf "\"Lib.idr\"" json

-- =============================================================================
-- Source Map Generation Tests
-- =============================================================================

test_generate_empty : () -> Bool
test_generate_empty () =
  let sm = generateIdrisCSourceMap "test.c" []
  in sm.version == 3 && length sm.sources == 0

test_generate_single_mapping : () -> Bool
test_generate_single_mapping () =
  let loc = MkIdrisLoc "Main" 10 1 10 5
      mapping = MkMapping "test.c" 1 loc
      sm = generateIdrisCSourceMap "test.c" [mapping]
  in sm.version == 3 && length sm.sources == 1

-- =============================================================================
-- Test Runner
-- =============================================================================

||| All SourceMap tests
export
allSourceMapTests : List SMTestDef
allSourceMapTests =
  [ smTest "REQ_SRCMAP_PARSE_001" "Parse simple comment" test_parse_simple_comment
  , smTest "REQ_SRCMAP_PARSE_002" "Parse multiline" test_parse_multiline
  , smTest "REQ_SRCMAP_PARSE_003" "No comment returns empty" test_parse_no_comment
  , smTest "REQ_SRCMAP_PARSE_004" "Extract module name" test_parse_extracts_module
  , smTest "REQ_SRCMAP_PARSE_005" "Extract line number" test_parse_extracts_line
  , smTest "REQ_SRCMAP_JSON_001" "JSON has version" test_json_has_version
  , smTest "REQ_SRCMAP_JSON_002" "JSON has sources" test_json_has_sources
  , smTest "REQ_SRCMAP_GEN_001" "Generate empty map" test_generate_empty
  , smTest "REQ_SRCMAP_GEN_002" "Generate single mapping" test_generate_single_mapping
  ]

||| Run all SourceMap tests
export
runSourceMapTests : (Nat, Nat)
runSourceMapTests =
  let results = map (\t => t.run ()) allSourceMapTests
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)
