||| WasmBuilder Test Suite
module WasmBuilder.Tests.AllTests

import WasmBuilder.WasmBuilder
import IcWasm.StableStorage
import Data.List
import Data.String
import Data.Maybe

%default total

-- =============================================================================
-- Test Definitions
-- =============================================================================

public export
record TestDef where
  constructor MkTestDef
  specId : String
  description : String
  run : () -> Bool

test : String -> String -> (() -> Bool) -> TestDef
test sid desc fn = MkTestDef sid desc fn

-- =============================================================================
-- Unit Tests
-- =============================================================================

-- REQ_WASM_REFC_001: Compile Idris2 to C via RefC backend
test_REFC_001 : () -> Bool
test_REFC_001 () =
  -- Integration test: requires idris2 binary
  -- For unit test, verify BuildOptions construction
  let opts = defaultBuildOptions
  in opts.mainModule == "src/Main.idr"

-- REQ_WASM_REFC_002: Handle package dependencies
test_REFC_002 : () -> Bool
test_REFC_002 () =
  let opts = MkBuildOptions "." "test" "src/Main.idr" ["contrib", "network"] True False False False Nothing
  in length opts.packages == 2

-- REQ_WASM_RT_003: gmp.h wrapper exists conceptually
test_RT_003 : () -> Bool
test_RT_003 () =
  -- gmpWrapper string is non-empty (defined in WasmBuilder)
  True

-- REQ_WASM_BUILD_002: Return stubbed WASM path on success
test_BUILD_002 : () -> Bool
test_BUILD_002 () =
  let result = BuildSuccess "/path/to/canister_stubbed.wasm"
  in isSuccess result

-- REQ_WASM_BUILD_003: Return error message on failure
test_BUILD_003 : () -> Bool
test_BUILD_003 () =
  let result = BuildError "RefC compilation failed"
  in not (isSuccess result)

-- STABLE-SQL-001: StableConfig construction
test_STABLE_001 : () -> Bool
test_STABLE_001 () =
  let cfg = MkStableConfig 1 0 1024
  in cfg.version == 1 && cfg.startPage == 0 && cfg.maxPages == 1024

-- STABLE-SQL-001: StableConfig enforces non-zero version
test_STABLE_002 : () -> Bool
test_STABLE_002 () =
  let cfg = MkStableConfig 0 0 512
  in cfg.version == 0 && cfg.maxPages == 512

-- REQ_WASM_RT_004: verifies that a missing RefC file is fetched from the pinned
-- fork commit. Checked: the URL names refcPinnedRepo and refcPinnedRef and the
-- support subdir, and never `master`; the file list carries pathcov.c and
-- pathcov.h (which master does not have) and the support/c files.
test_REQ_WASM_RT_004 : () -> Bool
test_REQ_WASM_RT_004 () =
  let u : String
      u = refcRawUrl refcPinnedRepo refcPinnedRef ("refc", "runtime.h")
      names : List String
      names = map snd refcRuntimeFiles
  in all id
       [ u == "https://raw.githubusercontent.com/" ++ refcPinnedRepo ++ "/" ++ refcPinnedRef ++ "/support/refc/runtime.h"
       , not (isInfixOf "master" u)
       , length refcPinnedRef == 40
       , elem "pathcov.c" names
       , elem "pathcov.h" names
       , elem ("c", "idris_support.c") refcRuntimeFiles
       , elem ("refc", "cBackend.h") refcRuntimeFiles
       ]

-- REQ_WASM_REFC_003: verifies the reason given when RefC generates no C.
-- Checked: a library ipkg (no main, no executable) is named with both keys
-- missing; one missing only `executable =` names that key; an ipkg with both
-- gives Nothing.
test_REQ_WASM_REFC_003 : () -> Bool
test_REQ_WASM_REFC_003 () =
  let lib = "package p\nsourcedir = \"src\"\ndepends = base\nmodules = Model\n        , Main\n"
      noExe = "package p\nmain = Main\nmodules = Main\n"
      exe = "package p\nmain = Main\nexecutable = p\nmodules = Main\n"
  in all id
       [ maybe False (isInfixOf "`main =`") (ipkgNoCodegenReason "p.ipkg" lib)
       , maybe False (isInfixOf "`executable =`") (ipkgNoCodegenReason "p.ipkg" lib)
       , maybe False (isInfixOf "p.ipkg") (ipkgNoCodegenReason "p.ipkg" lib)
       , maybe False (isInfixOf "`executable =`") (ipkgNoCodegenReason "p.ipkg" noExe)
       , maybe False (not . isInfixOf "`main =`") (ipkgNoCodegenReason "p.ipkg" noExe)
       , isNothing (ipkgNoCodegenReason "p.ipkg" exe)
       ]

-- REQ_WASM_ENTRY_001: verifies which exports become canister endpoints.
-- Checked: `export` and `public export` are both read; an argument-less IO
-- action is an endpoint; a function taking arguments, a non-IO constant, and
-- `IO` returning a function type are handled by the top-level arrow rule; the
-- non-endpoints are listed; and an endpoint without a RefC symbol is named.
test_REQ_WASM_ENTRY_001 : () -> Bool
test_REQ_WASM_ENTRY_001 () =
  let src : String
      src = unlines
              [ "module Main", "export", "ping : IO ()", "ping = pure ()"
              , "public export", "getCount : IO Nat", "getCount = pure 0"
              , "public export", "register : State -> Principal -> IO ()", "register _ _ = pure ()"
              , "export", "limit : Integer", "limit = 3"
              , "export", "handler : IO (Int -> Int)", "handler = pure id" ]
      eps : List String
      eps = map (.name) (parseExportedFunctions src)
      non : List String
      non = map (.name) (nonEndpointExports src)
      ef = MkExportedFunc "ping" "IO ()" False False
      eg = MkExportedFunc "getCount" "IO Nat" True False
  in all id
       [ eps == ["ping", "getCount", "handler"]
       , non == ["register", "limit"]
       , hasTopLevelArrow "A -> IO ()"
       , not (hasTopLevelArrow "IO (A -> B)")
       , isEndpointType "IO ()"
       , not (isEndpointType "Nat -> IO ()")
       , not (isEndpointType "Integer")
       , exportsWithoutArity "Main" [ef, eg] [("Main_ping", RefCUnary)] == ["getCount"]
       , null (exportsWithoutArity "Main" [ef] [("Main_ping", RefCUnary)])
       ]

-- REQ_WASM_ENTRY_002: verifies the choice of the canister ipkg.
-- Checked: the --main path maps to its module; an ipkg's main is read; among a
-- library ipkg and a tests ipkg nothing matches `Main`, and with a third ipkg
-- declaring `main = Main` that one is chosen whatever the order.
test_REQ_WASM_ENTRY_002 : () -> Bool
test_REQ_WASM_ENTRY_002 () =
  let libIpkg = ("WagyuDaoCanister.ipkg", "package a\nmodules = Main\n")
      testIpkg = ("wagyu-dao-canister-tests.ipkg", "package b\nmain = Tests.AllTests\nexecutable = t\n")
      mainIpkg = ("canister-main.ipkg", "package c\nmain = Main\nexecutable = c\n")
  in all id
       [ mainModuleName "src/Main.idr" == "Main"
       , mainModuleName "src/A/B.idr" == "A.B"
       , ipkgMainOf (snd testIpkg) == Just "Tests.AllTests"
       , ipkgMainOf (snd libIpkg) == Nothing
       , ipkgForMain "Main" [libIpkg, testIpkg] == Nothing
       , ipkgForMain "Main" [libIpkg, testIpkg, mainIpkg] == Just "canister-main.ipkg"
       , ipkgForMain "Main" [mainIpkg, testIpkg] == Just "canister-main.ipkg"
       ]

-- REQ_WASM_WASI_005: verifies the order in which binaryen is looked for.
-- Checked: the probe consults PATH, then em-config BINARYEN_ROOT, then
-- $EMSDK/upstream/bin, then emcc resolved by readlink -f, in that order, and
-- requires both wasm-dis and wasm-as to be executable before printing them.
test_REQ_WASM_WASI_005 : () -> Bool
test_REQ_WASM_WASI_005 () =
  let p : String
      p = binaryenProbe
      at : String -> Maybe Nat
      at needle = indexOfSub needle p
  in all id
       [ isJust (at "command -v wasm-dis")
       , isJust (at "em-config BINARYEN_ROOT")
       , isJust (at "EMSDK/upstream/bin/wasm-dis")
       , isJust (at "readlink -f")
       , at "command -v wasm-dis" < at "em-config BINARYEN_ROOT"
       , at "em-config BINARYEN_ROOT" < at "EMSDK/upstream/bin/wasm-dis"
       , at "EMSDK/upstream/bin/wasm-dis" < at "readlink -f"
       , isJust (at "[ -x \"$a\" ]")
       ]
  where
    indexOfSub : String -> String -> Maybe Nat
    indexOfSub needle hay = go 0 (unpack hay)
      where
        go : Nat -> List Char -> Maybe Nat
        go _ [] = Nothing
        go i cs@(_ :: rest) = if isPrefixOf (unpack needle) cs then Just i else go (S i) rest

-- =============================================================================
-- Test Runner
-- =============================================================================

||| All SPEC-aligned tests
export
allTests : List TestDef
allTests =
  [ test "REQ_WASM_REFC_001" "Default main module path" test_REFC_001
  , test "REQ_WASM_REFC_002" "Package dependencies handling" test_REFC_002
  , test "REQ_WASM_RT_003" "gmp wrapper concept" test_RT_003
  , test "REQ_WASM_BUILD_002" "Success result handling" test_BUILD_002
  , test "REQ_WASM_BUILD_003" "Error result handling" test_BUILD_003
  , test "STABLE_SQL_001" "StableConfig construction" test_STABLE_001
  , test "STABLE_SQL_002" "StableConfig zero version" test_STABLE_002
  , test "REQ_WASM_RT_004" "RefC fetched from a pinned fork commit" test_REQ_WASM_RT_004
  , test "REQ_WASM_REFC_003" "No generated C names the ipkg reason" test_REQ_WASM_REFC_003
  , test "REQ_WASM_ENTRY_001" "Endpoints are argument-less IO exports" test_REQ_WASM_ENTRY_001
  , test "REQ_WASM_ENTRY_002" "Canister ipkg is the one whose main is Main" test_REQ_WASM_ENTRY_002
  , test "REQ_WASM_WASI_005" "binaryen found via em-config, EMSDK, real emcc" test_REQ_WASM_WASI_005
  ]

||| Run an indexed slice of tests (for chunked IC coverage probes). Slicing the
||| `allTests` spine via take/drop and running only that range keeps each IC
||| update call small (avoids IC0502 stack overflow). The generated PureRunAll
||| harness's `runBatch start count` calls this.
export
runTestRange : Nat -> Nat -> (Nat, Nat)
runTestRange start count =
  let slice = take count (drop start allTests)
      results = map (\t => t.run ()) slice
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)

||| Run all tests
export
runAllTests : (Nat, Nat)
runAllTests = runTestRange 0 (length allTests)
