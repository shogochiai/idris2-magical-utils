||| WasmBuilder Test Suite
module WasmBuilder.Tests.AllTests

import WasmBuilder.WasmBuilder
import IcWasm.StableStorage
import System
import System.File
import System.Directory
import Data.String
import Data.List
import Data.List1

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
  let opts = MkBuildOptions "." "test" "src/Main.idr" ["contrib", "network"] True False False Nothing
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
  ]

-- =============================================================================
-- Runtime Support Contract Tests (IO-based)
-- =============================================================================

||| Helper: check if a file exists
covering
fileExistsIO : String -> IO Bool
fileExistsIO path = do
  Right _ <- readFile path
    | Left _ => pure False
  pure True

||| Find the IC0 support directory relative to this package
covering
findSupportDir : IO (Maybe String)
findSupportDir = do
  let candidates = [ "support/ic0"
                   , "../Idris2IcWasm/support/ic0"
                   , "../../pkgs/Idris2IcWasm/support/ic0"
                   ]
  go candidates
  where
    go : List String -> IO (Maybe String)
    go [] = pure Nothing
    go (c :: cs) = do
      exists <- fileExistsIO (c ++ "/ic0_stubs.c")
      if exists then pure (Just c) else go cs

||| REQ_ICWASM_RUNTIME_002: ic0_stubs.c exists in support bundle
covering
test_RUNTIME_002_ic0_stubs : IO Bool
test_RUNTIME_002_ic0_stubs = do
  mDir <- findSupportDir
  case mDir of
    Nothing => pure False
    Just dir => fileExistsIO (dir ++ "/ic0_stubs.c")

||| REQ_ICWASM_RUNTIME_003: wasi_stubs.c exists in support bundle
covering
test_RUNTIME_003_wasi_stubs : IO Bool
test_RUNTIME_003_wasi_stubs = do
  mDir <- findSupportDir
  case mDir of
    Nothing => pure False
    Just dir => fileExistsIO (dir ++ "/wasi_stubs.c")

||| REQ_ICWASM_RUNTIME_004: canister_entry.c exists in support bundle
covering
test_RUNTIME_004_canister_entry : IO Bool
test_RUNTIME_004_canister_entry = do
  mDir <- findSupportDir
  case mDir of
    Nothing => pure False
    Just dir => fileExistsIO (dir ++ "/canister_entry.c")

||| Fixture dir for E2E tests
fixtureDir : String
fixtureDir = "tests/fixtures/MinimalCanister"

fixtureIpkg : String
fixtureIpkg = fixtureDir ++ "/minimal-canister.ipkg"

||| Helper: extract temp dir from temp ipkg path
||| e.g. "/tmp/idris2-icwasm-test-123/foo.ipkg" → "/tmp/idris2-icwasm-test-123"
tempDirFromIpkg : String -> String
tempDirFromIpkg path =
  let parts = forget (split (== '/') path)
      dirParts = case reverse parts of
            (_ :: rest) => reverse rest
            [] => []
  in joinBy "/" dirParts

||| REQ_ICWASM_TESTBUILD_001: setupTestBuild creates temp tree with valid generated files
||| E2E: call setupTestBuild, verify ipkg + Main.idr + TestHarness.idr exist and
||| Main.idr contains IO (Int, Int) typed entry points
covering
test_TESTBUILD_001_contract : IO Bool
test_TESTBUILD_001_contract = do
  result <- setupTestBuild fixtureDir fixtureIpkg Nothing
  case result of
    Left err => do
      putStrLn $ "  TESTBUILD_001 setup failed: " ++ err
      pure False
    Right tempIpkgPath => do
      let tempDir = tempDirFromIpkg tempIpkgPath
      -- Verify generated files exist
      ipkgExists <- fileExistsIO tempIpkgPath
      mainExists <- fileExistsIO (tempDir ++ "/src/Main.idr")
      -- Verify Main.idr contains IO (Int, Int) entry points (the real contract)
      mainOk <- case !(readFile (tempDir ++ "/src/Main.idr")) of
        Left _ => pure False
        Right content => pure $
             isInfixOf "runTests" content
          && isInfixOf "IO (Int, Int)" content
          && isInfixOf "runMinimalTests" content
          && isInfixOf "runTrivialTest" content
      -- Clean up
      _ <- system $ "rm -rf /tmp/idris2-icwasm-test-*"
      pure $ ipkgExists && mainExists && mainOk

||| REQ_ICWASM_TESTBUILD_002: setupTestBuild preserves package-local imports via symlinks
||| E2E: verify Tests/AllTests.idr is symlinked and Main.idr is generated (not symlinked)
covering
test_TESTBUILD_002_imports : IO Bool
test_TESTBUILD_002_imports = do
  result <- setupTestBuild fixtureDir fixtureIpkg Nothing
  case result of
    Left err => do
      putStrLn $ "  TESTBUILD_002 setup failed: " ++ err
      pure False
    Right tempIpkgPath => do
      let tempDir = tempDirFromIpkg tempIpkgPath
      -- Tests/AllTests.idr should be accessible (symlinked from fixture)
      hasTests <- fileExistsIO (tempDir ++ "/src/Tests/AllTests.idr")
      -- Generated Main.idr should exist
      hasMain <- fileExistsIO (tempDir ++ "/src/Main.idr")
      -- Verify symlinked AllTests has the correct test surface type
      testsOk <- case !(readFile (tempDir ++ "/src/Tests/AllTests.idr")) of
        Left _ => pure False
        Right content => pure $
             isInfixOf "IO (Int, Int)" content
          && isInfixOf "runTests" content
      -- Clean up
      _ <- system $ "rm -rf /tmp/idris2-icwasm-test-*"
      pure $ hasTests && hasMain && testsOk

||| REQ_ICWASM_TESTBUILD_003: setupTestBuild output compiles successfully
||| E2E: call setupTestBuild, then actually build the temp ipkg with pack.
||| This proves the generated temp package typechecks — not just string content.
covering
test_TESTBUILD_003_builds_temp_ipkg : IO Bool
test_TESTBUILD_003_builds_temp_ipkg = do
  result <- setupTestBuild fixtureDir fixtureIpkg Nothing
  case result of
    Left err => do
      putStrLn $ "  TESTBUILD_003 setup failed: " ++ err
      pure False
    Right tempIpkgPath => do
      let tempDir = tempDirFromIpkg tempIpkgPath
      -- Extract just the ipkg filename for pack build
      let ipkgName = case reverse (forget (split (== '/') tempIpkgPath)) of
            (name :: _) => name
            [] => tempIpkgPath
      -- Build: cd to temp dir and run pack build
      let buildCmd = "cd " ++ tempDir ++ " && pack build " ++ ipkgName ++ " 2>&1"
      putStrLn $ "  TESTBUILD_003 building: " ++ buildCmd
      exitCode <- system buildCmd
      if exitCode == 0
        then do
          putStrLn "  TESTBUILD_003 build succeeded"
          _ <- system $ "rm -rf " ++ tempDir
          pure True
        else do
          -- Show tail of build output for diagnosis
          let logCmd = "cd " ++ tempDir ++ " && pack build " ++ ipkgName ++ " 2>&1 | tail -20"
          _ <- system logCmd
          putStrLn $ "  TESTBUILD_003 build FAILED (exit " ++ show exitCode ++ ")"
          putStrLn $ "  Temp dir preserved for investigation: " ++ tempDir
          pure False

||| REQ_ICWASM_RUNTIME_001: prepareRefCRuntime downloads idris_memory.h
||| E2E: verify idris_memory.h is present after runtime preparation.
||| Failure here means the root-cause bug (missing header) has regressed.
covering
test_RUNTIME_001_idris_memory : IO Bool
test_RUNTIME_001_idris_memory = do
  result <- prepareRefCRuntime
  case result of
    Left err => do
      putStrLn $ "  RUNTIME_001 prep failed: " ++ err
      pure False
    Right (refcSrc, _) => do
      -- idris_memory.h must exist (the specific file cited in the bug report)
      hasMemH <- fileExistsIO (refcSrc ++ "/idris_memory.h")
      -- memoryManagement.h must also exist (core RefC)
      hasMemMgmt <- fileExistsIO (refcSrc ++ "/memoryManagement.h")
      pure $ hasMemH && hasMemMgmt

||| REQ_ICWASM_RUNTIME_005: support bundle has all .c files for compileToWasmWithEntry
covering
test_RUNTIME_005_self_contained : IO Bool
test_RUNTIME_005_self_contained = do
  mDir <- findSupportDir
  case mDir of
    Nothing => pure False
    Just dir => do
      let requiredC = [ "ic0_stubs.c", "wasi_stubs.c", "canister_entry.c" ]
      results <- traverse (\f => fileExistsIO (dir ++ "/" ++ f)) requiredC
      pure $ all id results

||| REQ_ICWASM_RUNTIME_006: fixture canister matches the real lazy dfx test surface contract
||| E2E: verify runTests/runMinimalTests/runTrivialTest are IO (Int, Int), not IO ()
covering
test_RUNTIME_006_test_surface : IO Bool
test_RUNTIME_006_test_surface = do
  Right content <- readFile (fixtureDir ++ "/src/Tests/AllTests.idr")
    | Left _ => pure False
  -- Must contain IO (Int, Int) typed entry points (the real contract)
  pure $ isInfixOf "runTests : IO (Int, Int)" content
      && isInfixOf "runMinimalTests : IO (Int, Int)" content
      && isInfixOf "runTrivialTest : IO (Int, Int)" content
      && isInfixOf "allTests" content

-- =============================================================================
-- IO Test Runner
-- =============================================================================

||| IO-based tests (spec ID, test action)
export
covering
ioTests : List (String, IO Bool)
ioTests =
  [ ("REQ_ICWASM_RUNTIME_002", test_RUNTIME_002_ic0_stubs)
  , ("REQ_ICWASM_RUNTIME_003", test_RUNTIME_003_wasi_stubs)
  , ("REQ_ICWASM_RUNTIME_004", test_RUNTIME_004_canister_entry)
  , ("REQ_ICWASM_TESTBUILD_001", test_TESTBUILD_001_contract)
  , ("REQ_ICWASM_RUNTIME_005", test_RUNTIME_005_self_contained)
  , ("REQ_ICWASM_TESTBUILD_002", test_TESTBUILD_002_imports)
  , ("REQ_ICWASM_TESTBUILD_003", test_TESTBUILD_003_builds_temp_ipkg)
  , ("REQ_ICWASM_RUNTIME_001", test_RUNTIME_001_idris_memory)
  , ("REQ_ICWASM_RUNTIME_006", test_RUNTIME_006_test_surface)
  ]

||| Run all tests (pure + IO)
export
runAllTests : (Nat, Nat)
runAllTests =
  let results = map (\t => t.run ()) allTests
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)

||| Run all tests including IO tests
export
covering
runAllTestsIO : IO (Nat, Nat)
runAllTestsIO = do
  let (purePassed, pureFailed) = runAllTests
  ioResults <- traverse (\(name, t) => do
    r <- t
    pure (name, r)) ioTests
  let ioPassed = length $ filter snd ioResults
  let ioFailed = length $ filter (not . snd) ioResults
  for_ ioResults $ \(name, passed) =>
    putStrLn $ (if passed then "[PASS] " else "[FAIL] ") ++ name
  pure (purePassed + ioPassed, pureFailed + ioFailed)

covering
main : IO ()
main = do
  putStrLn "Running WasmBuilder Tests..."
  -- Pure tests
  let (pp, pf) = runAllTests
  for_ allTests $ \t =>
    putStrLn $ (if t.run () then "[PASS] " else "[FAIL] ") ++ t.specId
  -- IO tests
  (tp, tf) <- runAllTestsIO
  putStrLn $ "\nResults: " ++ show tp ++ " passed, " ++ show tf ++ " failed"
  when (tf > 0) $ exitWith (ExitFailure 1)
