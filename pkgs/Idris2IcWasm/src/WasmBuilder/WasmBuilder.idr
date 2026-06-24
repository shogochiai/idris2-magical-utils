||| WASM Builder for ICP Canisters
|||
||| Idris2 library for building IC-compatible WASM:
||| Idris2 (.idr) → RefC → C → Emscripten → WASM → WASI stub
|||
||| This module provides the build pipeline that was previously
||| implemented in build-canister.sh scripts.
module WasmBuilder.WasmBuilder

import Data.String
import Data.List
import Data.List1
import Data.Maybe
import System
import System.File
import WasmBuilder.BranchProbe
import WasmBuilder.SourceMap.SourceMap
import WasmBuilder.SourceMap.VLQ
import WasmBuilder.CandidStubs

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Build options for WASM compilation
public export
record BuildOptions where
  constructor MkBuildOptions
  projectDir : String      -- Project root directory
  canisterName : String    -- Canister name (for output naming)
  mainModule : String      -- Main module path (default: src/Main.idr)
  packages : List String   -- Additional packages (-p flags)
  generateSourceMap : Bool -- Generate Idris→WASM source map
  instrumentBranchProbes : Bool -- Insert C-level branch probes for runtime coverage
  instrumentPathHits : Bool -- Compiler-injected prim__recordPathHit (canonical path-ids)
  forTestBuild : Bool      -- Generate test Main in /tmp (requires Tests/AllTests.idr)
  testModulePath : Maybe String  -- Custom test module path (default: src/Tests/AllTests.idr)

||| Default build options
public export
defaultBuildOptions : BuildOptions
defaultBuildOptions = MkBuildOptions
  { projectDir = "."
  , canisterName = "canister"
  , mainModule = "src/Main.idr"
  , packages = ["contrib"]
  , generateSourceMap = True
  , instrumentBranchProbes = False
  , instrumentPathHits = False
  , forTestBuild = False
  , testModulePath = Nothing
  }

||| Build result
public export
data BuildResult
  = BuildSuccess String   -- Success with WASM path
  | BuildError String     -- Build failed with error

public export
Show BuildResult where
  show (BuildSuccess path) = "Built: " ++ path
  show (BuildError err) = "Build error: " ++ err

||| Check if build succeeded
public export
isSuccess : BuildResult -> Bool
isSuccess (BuildSuccess _) = True
isSuccess (BuildError _) = False

-- =============================================================================
-- Shell Execution
-- =============================================================================

||| Execute a shell command and capture output
executeCommand : String -> IO (Int, String, String)
executeCommand cmd = do
  let stdoutFile = "/tmp/wasm_build_stdout_" ++ show !time ++ ".txt"
  let stderrFile = "/tmp/wasm_build_stderr_" ++ show !time ++ ".txt"
  let fullCmd = cmd ++ " > " ++ stdoutFile ++ " 2> " ++ stderrFile

  exitCode <- system fullCmd

  Right stdout <- readFile stdoutFile
    | Left _ => pure (exitCode, "", "")
  Right stderr <- readFile stderrFile
    | Left _ => pure (exitCode, stdout, "")

  _ <- system $ "rm -f " ++ stdoutFile ++ " " ++ stderrFile

  pure (exitCode, trim stdout, trim stderr)

resolveIdris2Bin : IO String
resolveIdris2Bin = do
  mBin <- getEnv "IDRIS2_BIN"
  pure $ case mBin of
    Just bin => let trimmed = trim bin in if null trimmed then "idris2" else trimmed
    Nothing => "idris2"

-- =============================================================================
-- Build Pipeline Steps
-- =============================================================================

||| Step 1: Compile Idris2 to C using RefC backend
|||
||| @opts Build options
||| @buildDir Output directory for C files
||| Returns path to generated C file on success

||| Convert file path to Idris module name
||| e.g., "src/Economics/Tests/AllTests.idr" -> "Economics.Tests.AllTests"
pathToModuleName : String -> String
pathToModuleName path =
  let len = cast {to=Int} (length path)
      noSrc = if isPrefixOf "src/" path then strSubstr 4 (len - 4) path else path
      noSrcLen = cast {to=Int} (length noSrc)
      noExt = if isSuffixOf ".idr" noSrc then strSubstr 0 (noSrcLen - 4) noSrc else noSrc
  in pack $ map (\c => if c == '/' then '.' else c) (unpack noExt)

public export
data TestHarnessStyle = ExistingHarness | TupleHarness | TupleHarnessWithRunner | RecordHarness | PureRunAllHarness | PureRunAllHarnessWithRunner

detectTestHarnessStyle : String -> TestHarnessStyle
detectTestHarnessStyle content =
  let hasRunTests = isInfixOf "runTests : IO (Int, Int)" content
      hasIoUnitRunTests = isInfixOf "runTests : IO ()" content
      hasRecordTestDef = isInfixOf "record TestDef where" content && isInfixOf "testFn : IO Bool" content
      hasTupleTests = isInfixOf "allTests : List (String, IO Bool)" content
      hasPureRunAll = isInfixOf "runAllTests : (Nat, Nat)" content
  in if hasRunTests
        then ExistingHarness
     else if hasRecordTestDef
        then RecordHarness
     else if hasTupleTests && hasIoUnitRunTests
        then TupleHarnessWithRunner
     else if hasTupleTests
        then TupleHarness
     else if hasPureRunAll && hasIoUnitRunTests
        then PureRunAllHarnessWithRunner
     else if hasPureRunAll
        then PureRunAllHarness
     else ExistingHarness

takeFirst : Nat -> List a -> List a
takeFirst Z _ = []
takeFirst (S k) [] = []
takeFirst (S k) (x :: xs) = x :: takeFirst k xs

parseIoBoolTestNames : String -> List String
parseIoBoolTestNames content =
  takeFirst 24 $
    nub $
      mapMaybe parseLine (map trim (lines content))
  where
    parseLine : String -> Maybe String
    parseLine line =
      if isSuffixOf ": IO Bool" line
         then let name = trim $ pack $ takeWhile (/= ':') (unpack line)
              in if isPrefixOf "test_" name then Just name else Nothing
         else Nothing

generateDirectTestEntries : String -> List String -> List String
generateDirectTestEntries testModuleName names =
  case names of
    [] => []
    _ =>
      [ "directTests : List (String, IO Bool)"
      , "directTests ="
      ] ++ directRows names
  where
    directRows : List String -> List String
    directRows [] = ["  []"]
    directRows (name :: rest) =
      ("  [ (\"" ++ name ++ "\", " ++ testModuleName ++ "." ++ name ++ ")")
        :: map (\n => "  , (\"" ++ n ++ "\", " ++ testModuleName ++ "." ++ n ++ ")") rest
        ++ ["  ]"]

generateTestHarnessShimContent : TestHarnessStyle -> String -> List String -> String
generateTestHarnessShimContent ExistingHarness testModuleName directTestNames =
  let directEntries = generateDirectTestEntries testModuleName directTestNames
      allTestsLine =
        if null directTestNames
           then "allTestsGeneric = TestModule.allTests"
           else "allTestsGeneric = directTests"
  in
  unlines
    ([ "module TestHarness"
     , "import " ++ testModuleName ++ " as TestModule"
     , "import Data.List"
     , ""
     ]
     ++ directEntries
     ++
     [ ""
     , "export"
     , "allTestsGeneric : List (String, IO Bool)"
     , allTestsLine
     , ""
     , "export"
     , "runBatch : Nat -> Nat -> IO (Int, Int)"
     , "runBatch start count = do"
     , "  let batch = take count (drop start allTestsGeneric)"
     , "  if null batch then pure (0, 0) else do"
     , "    rs <- traverse (\\(_, t) => t) batch"
     , "    let passed = length (filter id rs)"
     , "        failed = length (filter not rs)"
     , "    pure (cast passed, cast failed)"
     , ""
     , "export"
     , "runTests : IO (Int, Int)"
     , "runTests = runBatch 0 (length allTestsGeneric)"
     , ""
     , "export"
     , "runMinimalTests : IO (Int, Int)"
     , "runMinimalTests = runBatch 0 5"
     , ""
     , "export"
     , "runTrivialTest : IO (Int, Int)"
     , "runTrivialTest = runBatch 0 1"
     ])
generateTestHarnessShimContent TupleHarness testModuleName _ =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "import Data.List"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = TestModule.allTests"
    , ""
    , "export"
    , "runBatch : Nat -> Nat -> IO (Int, Int)"
    , "runBatch start count = do"
    , "  let batch = take count (drop start allTestsGeneric)"
    , "  if null batch then pure (0, 0) else do"
    , "    rs <- traverse (\\(_, t) => t) batch"
    , "    let passed = length (filter id rs)"
    , "        failed = length (filter not rs)"
    , "    pure (cast passed, cast failed)"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = runBatch 0 (length allTestsGeneric)"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = runBatch 0 8"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = runBatch 0 1"
    ]
generateTestHarnessShimContent TupleHarnessWithRunner testModuleName _ =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "import Data.List"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = TestModule.allTests"
    , ""
    , "export"
    , "runBatch : Nat -> Nat -> IO (Int, Int)"
    , "runBatch start count = do"
    , "  let batch = take count (drop start allTestsGeneric)"
    , "  if null batch then pure (0, 0) else do"
    , "    rs <- traverse (\\(_, t) => t) batch"
    , "    let passed = length (filter id rs)"
    , "        failed = length (filter not rs)"
    , "    pure (cast passed, cast failed)"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = do"
    , "  TestModule.runTests"
    , "  runBatch 0 (length allTestsGeneric)"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = runBatch 0 8"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = runBatch 0 1"
    ]
generateTestHarnessShimContent RecordHarness testModuleName _ =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "import Data.List"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = map (\\t => (t.testId, t.testFn)) TestModule.allTests"
    , ""
    , "export"
    , "runBatch : Nat -> Nat -> IO (Int, Int)"
    , "runBatch start count = do"
    , "  let batch = take count (drop start allTestsGeneric)"
    , "  if null batch then pure (0, 0) else do"
    , "    rs <- traverse (\\(_, t) => t) batch"
    , "    let passed = length (filter id rs)"
    , "        failed = length (filter not rs)"
    , "    pure (cast passed, cast failed)"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = runBatch 0 (length allTestsGeneric)"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = runBatch 0 8"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = runBatch 0 1"
    ]
generateTestHarnessShimContent PureRunAllHarness testModuleName _ =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = []"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
     , "runTests ="
     , "  let (passed, failed) = TestModule.runAllTests"
     , "  in pure (cast passed, cast failed)"
     , ""
     -- runBatch slices via TestModule.runTestRange so a single IC update call
     -- only evaluates `count` tests (lazy-thunk list), avoiding the IC0502
     -- stack overflow that running all tests in one call triggers.
     , "export"
     , "runBatch : Nat -> Nat -> IO (Int, Int)"
     , "runBatch start count ="
     , "  let (passed, failed) = TestModule.runTestRange start count"
     , "  in pure (cast passed, cast failed)"
     , ""
     , "export"
     , "runMinimalTests : IO (Int, Int)"
     , "runMinimalTests = runBatch 0 8"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = runBatch 0 1"
    ]
generateTestHarnessShimContent PureRunAllHarnessWithRunner testModuleName _ =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = []"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = do"
    , "  TestModule.runTests"
    , "  let (passed, failed) = TestModule.runAllTests"
    , "  pure (cast passed, cast failed)"
    , ""
    -- runBatch slices via TestModule.runTestRange so a single IC update call
    -- only evaluates `count` tests (lazy-thunk list), avoiding the IC0502
    -- stack overflow that running all tests in one call triggers. It does NOT
    -- invoke the IO () runner (TestModule.runTests) so each chunk stays small.
    , "export"
    , "runBatch : Nat -> Nat -> IO (Int, Int)"
    , "runBatch start count ="
    , "  let (passed, failed) = TestModule.runTestRange start count"
    , "  in pure (cast passed, cast failed)"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = runBatch 0 8"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = runBatch 0 1"
    ]

||| Generate test Main.idr content that imports the standardized shim module
||| This is written to /tmp, never touches the original Main.idr
||| One canister export `runTestBatchN` is generated per index; each runs an
||| 8-test slice (start = idx*8). Cover enough indices that the batches span the
||| whole pure-test list (0..31 -> up to 256 tests) so chunked probing records
||| hits for every test without a single all-tests call (which can overflow the
||| IC native stack / crash the replica). Must stay in sync with the dfx-cov
||| probe's batchProbeIndexes. Extended 16->32 batches: GlobalRegistry's
||| pureTestThunks grew past 128 (now ~145 with the path-coverage subsuites), so
||| tests beyond index 15 were never executed on the replica and their paths never
||| recorded (numerator under-counted, run-to-run unstable). Empty batches past the
||| list end are harmless no-ops (runTestRange clamps).
testBatchIndexes : List Int
testBatchIndexes =
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
   16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]

generateTestBatchExport : Int -> List String
generateTestBatchExport idx =
  let name = "runTestBatch" ++ show idx
      start = idx * 8
  in [ ""
     , "export"
     , name ++ " : IO (Int, Int)"
     , name ++ " = TestHarness.runBatch " ++ show start ++ " 8"
     ]

generateTestBatchExports : List String
generateTestBatchExports = concat (map generateTestBatchExport testBatchIndexes)

generateTestMainContent : String
generateTestMainContent =
  unlines
    ([ "||| Auto-generated test entry point for coverage analysis"
    , "module Main"
    , ""
    , "import TestHarness"
    , "import Data.List"
    , "import Data.IORef"
    , ""
    , "%default covering"
    , ""
    , "||| Run all tests - exported for Candid call"
    , "||| Uses the standardized canister test shim"
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = TestHarness.runTests"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = TestHarness.runMinimalTests"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = TestHarness.runTrivialTest"
    ]
    ++ generateTestBatchExports
    ++
    [ ""
    , "||| Force code retention to prevent DCE"
    , "forceRetain : IO ()"
    , "forceRetain = do"
    , "  retain <- newIORef False"
    , "  shouldRun <- readIORef retain"
    , "  if shouldRun"
    , "    then do"
    , "      _ <- Main.runTests"
    , "      _ <- Main.runMinimalTests"
    , "      _ <- Main.runTrivialTest"
    ]
    ++ map (\idx => "      _ <- Main.runTestBatch" ++ show idx) testBatchIndexes
    ++
    [ "      pure ()"
    , "    else pure ()"
    , ""
    , "main : IO ()"
    , "main = forceRetain"
    ])

-- =============================================================================
-- Export Function Parsing (for canister_entry.c generation)
-- =============================================================================

||| Exported function from Main.idr
public export
record ExportedFunc where
  constructor MkExportedFunc
  name : String           -- Function name (e.g., "runTests")
  returnType : String     -- Return type (e.g., "IO (Int, Int)")
  isQuery : Bool          -- True for query, False for update
  fromDid : Bool          -- True if added from .did (no Idris impl)

public export
Show ExportedFunc where
  show ef = ef.name ++ " : " ++ ef.returnType ++ " [" ++ (if ef.isQuery then "query" else "update") ++ "]" ++ (if ef.fromDid then " (stub)" else "")

isTestHarnessExport : String -> Bool
isTestHarnessExport name =
  name == "runTests" ||
  name == "runMinimalTests" ||
  name == "runTrivialTest" ||
  isPrefixOf "runTestBatch" name

||| RefC-level arity for generated top-level symbols.
||| We use this instead of guessing from Idris types because `IO a`
||| may compile either to a unary world-accepting symbol or to a nullary
||| symbol returning a closure that must later be applied to the world.
public export
data RefCArity = RefCNullary | RefCUnary

public export
Show RefCArity where
  show RefCNullary = "nullary"
  show RefCUnary = "unary"

||| Convert DidMethod to ExportedFunc for stub generation
||| Used when building test canisters that need all .did methods as entry points
public export
didMethodToExport : DidMethod -> ExportedFunc
didMethodToExport dm = MkExportedFunc dm.name (show dm.returnType) dm.isQuery True

||| Parse export declarations from Idris source
||| Looks for pattern: export\n funcName : Type
||| Returns list of exported functions
parseExportedFunctions : String -> List ExportedFunc
parseExportedFunctions content =
  let ls = lines content
  in parseLines ls []
  where
    -- Check if a line is "export" keyword alone
    isExportLine : String -> Bool
    isExportLine line = trim line == "export"

    -- Parse type signature line: "funcName : Type"
    parseTypeSig : String -> Maybe (String, String)
    parseTypeSig line =
      let trimmed = trim line
          parts = break (== ':') (unpack trimmed)
      in case parts of
           (namePart, ':' :: typePart) =>
             let funcName = trim (pack namePart)
                 retType = trim (pack typePart)
             in if null funcName then Nothing else Just (funcName, retType)
           _ => Nothing

    -- Determine if function is query (no state mutation) or update
    -- Default to UPDATE for safety. Only mark as query if function name
    -- suggests read-only operation (getter pattern).
    isQueryType : String -> String -> Bool
    isQueryType funcName fullType =
      let name = toLower funcName
          -- Common query prefixes - functions that just read data
          -- Note: "can" removed as it matches "canister*" which are often updates
          queryPrefixes = ["get", "is", "has", "query", "http_request", "read", "fetch", "list", "find", "check"]
      in any (\p => isPrefixOf p name) queryPrefixes

    parseLines : List String -> List ExportedFunc -> List ExportedFunc
    parseLines [] acc = reverse acc
    parseLines [_] acc = reverse acc
    parseLines (line1 :: line2 :: rest) acc =
      if isExportLine line1
        then case parseTypeSig line2 of
               Just (funcName, retType) =>
                 let ef = MkExportedFunc funcName retType (isQueryType funcName retType) False
                 in parseLines rest (ef :: acc)
               Nothing => parseLines (line2 :: rest) acc
        else parseLines (line2 :: rest) acc

-- =============================================================================
-- canister_entry.c Generation
-- =============================================================================

||| Generate C code for a single exported function
||| Creates IC canister_query/canister_update entry point
||| @modulePrefix C function prefix (e.g., "Main" or "Tests_AllTests")
||| @ef Exported function info from Idris
||| @didMethods Parsed .did methods for Candid-aware reply generation
||| @typeDefs Parsed .did type definitions for dynamic Candid encoding
generateFuncEntry : String -> List (String, RefCArity) -> ExportedFunc -> List DidMethod -> List TypeDef -> String
generateFuncEntry modulePrefix cArities ef didMethods typeDefs =
  let queryOrUpdate = if ef.isQuery then "query" else "update"
      cFuncName = modulePrefix ++ "_" ++ ef.name  -- RefC mangling: Module_function
      replyCode = generateReplyCode ef didMethods typeDefs
      -- Generate actual function call for profiling (only for real Idris exports)
      funcCallCode = if ef.fromDid
                       then "    // .did stub - no Idris function to call"
                       else generateFuncCall cFuncName ef.returnType
  in unlines
       [ ""
       , "__attribute__((export_name(\"canister_" ++ queryOrUpdate ++ " " ++ ef.name ++ "\")))"
       , "void canister_" ++ queryOrUpdate ++ "_" ++ ef.name ++ "(void) {"
       , "    debug_log(\"" ++ ef.name ++ " called\");"
       , "    ic_arg_load();  // Load IC message args into buffer for CandidDecoder.readArgs"
       , "    ensure_idris2_init();"
       , funcCallCode
       , "    " ++ replyCode
       , "}"
       ]
  where
    -- Generate code to actually call the Idris function (for profiling)
    generateFuncCall : String -> String -> String
    generateFuncCall funcName retType =
      case lookup funcName cArities of
        Just RefCUnary =>
          unlines
            [ "    // RefC unary symbol: accepts world directly and returns final value"
            , "    Value* _world = idris2_newReference((Value*)0);"
            , "    Value* _result = idris2_trampoline(" ++ funcName ++ "(_world));"
            , "    (void)_result;"
            ]
        Just RefCNullary =>
          unlines
            [ "    // RefC nullary symbol: returns an IO closure, then apply world"
            , "    Value* _world = idris2_newReference((Value*)0);"
            , "    Value* _action = idris2_trampoline(" ++ funcName ++ "());"
            , "    Value* _result = idris2_apply_closure(_action, _world);"
            , "    (void)_result;"
            ]
        Nothing =>
          unlines
            [ "    debug_log(\"Unsupported RefC arity for " ++ funcName ++ "\");"
            , "    ic0_trap((int32_t)\"Unsupported RefC arity\", 22);"
            ]
  where
    -- Generate Candid reply code based on .did return type if available
    generateReplyCode : ExportedFunc -> List DidMethod -> List TypeDef -> String
    generateReplyCode func methods defs =
      -- First try to find matching method in .did file
      case lookupReturnType func.name methods of
        Just candidType =>
          -- Use dynamic Candid-aware stub generation with type definitions
          generateReplyForType func.name candidType defs
        Nothing =>
          -- Fallback to heuristic based on Idris return type
          if isInfixOf "(Int, Int)" func.returnType || isInfixOf "(Nat, Nat)" func.returnType
            then "int32_t _passed = 0; int32_t _failed = 0; extract_int_pair(_result, &_passed, &_failed); reply_int_pair(_passed, _failed);"
            else if isInfixOf "String" func.returnType
              then "reply_text(\"ok\"); // String result"
              else "reply_text(\"done\"); // Generic result (no .did match)"

||| Generate complete canister_entry.c with all exported functions
||| @modulePrefix C function prefix (e.g., "Main" or "Tests_AllTests")
||| @exports List of exported functions from Idris
||| @didMethods Parsed .did methods for Candid-aware reply generation
||| @typeDefs Parsed .did type definitions for dynamic Candid encoding
generateCanisterEntryC : String -> List (String, RefCArity) -> List ExportedFunc -> List DidMethod -> List TypeDef -> Bool -> Bool -> String
generateCanisterEntryC modulePrefix cArities exports didMethods typeDefs instrumentBranchProbes instrumentPathHits =
  let funcEntries = fastConcat $ map (\ef => generateFuncEntry modulePrefix cArities ef didMethods typeDefs) exports
      header = canisterEntryHeader
      branchProbeExterns =
        if instrumentBranchProbes
           then unlines
             [ "extern const char* __dfxcov_format_hits(void);"
             , "extern uint32_t __dfxcov_probe_total(void);"
             , "extern void __dfxcov_reset_hits(void);"
             ]
           else ""
      -- Path-coverage runtime (pathcov.c): canonical-path-id hits recorded by
      -- compiler-injected idris2_recordPathHit. __get_path_hits replies the
      -- comma-separated recorded path-ids (identity join with dumppaths path_id).
      pathHitsExterns =
        if instrumentPathHits
           then unlines
             [ "extern const char* __dfxcov_format_path_hits(void);"
             , "extern uint32_t __dfxcov_path_hit_count(void);"
             , "extern uint32_t __dfxcov_path_hit_saturated(void);"
             , "extern void __dfxcov_reset_path_hits(void);"
             ]
           else ""
      -- Generate extern declarations only for actual Idris functions (not .did stubs)
      idrisExports = filter (\ef => not ef.fromDid) exports
      funcExterns = unlines $ map mkExtern idrisExports
      branchProbeEntry =
        if instrumentBranchProbes
           then unlines
             [ ""
             , "__attribute__((export_name(\"canister_query __get_branch_probes\")))"
             , "void canister_query___get_branch_probes(void) {"
             , "    debug_log(\"__get_branch_probes called\");"
             , "    ensure_idris2_init();"
             , "    reply_text(__dfxcov_format_hits());"
             , "}"
             ]
           else ""
      pathHitsEntry =
        if instrumentPathHits
           then unlines
             [ ""
             , "__attribute__((export_name(\"canister_query __get_path_hits\")))"
             , "void canister_query___get_path_hits(void) {"
             , "    debug_log(\"__get_path_hits called\");"
             , "    ensure_idris2_init();"
             , "    reply_text(__dfxcov_format_path_hits());"
             , "}"
             ]
           else ""
  in header ++ "\n/* Idris Function Externs */\n" ++ branchProbeExterns ++ pathHitsExterns ++ funcExterns ++ "\n" ++ branchProbeEntry ++ pathHitsEntry ++ funcEntries
  where
    mkExtern : ExportedFunc -> String
    mkExtern ef =
      let funcName = modulePrefix ++ "_" ++ ef.name
      in case lookup funcName cArities of
           Just RefCUnary => "extern Value* " ++ funcName ++ "(Value*);"
           Just RefCNullary => "extern Value* " ++ funcName ++ "(void);"
           Nothing => "/* missing RefC arity: " ++ funcName ++ " */"

    canisterEntryHeader : String
    canisterEntryHeader = unlines
      [ "/*"
      , " * Auto-generated Canister Entry Points"
      , " * Generated by idris2-icwasm from Main.idr exports"
      , " */"
      , "#include <stdint.h>"
      , "#include <string.h>"
      , "#include <stdio.h>"
      , ""
      , "/* IC0 Imports */"
      , "extern void ic0_msg_reply(void);"
      , "extern void ic0_msg_reply_data_append(int32_t src, int32_t size);"
      , "extern int32_t ic0_msg_arg_data_size(void);"
      , "extern void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size);"
      , "extern void ic0_debug_print(int32_t src, int32_t size);"
      , "extern void ic0_trap(int32_t src, int32_t size);"
      , "extern int64_t ic0_stable64_grow(int64_t new_pages);"
        , ""
        , "/* IC FFI Bridge (ic_ffi_bridge.c) */"
        , "__attribute__((weak)) void ic_arg_load(void) {}  /* Optional bridge hook for CandidDecoder */"
        , ""
        , "/* Idris2 RefC Runtime - Value types */"
        , "#ifndef IDRIS2_ICWASM_HAS_REFC_HEADERS"
        , "#define CONSTRUCTOR_TAG 17"
        , "#define idris2_vp_is_unboxed(p) ((uintptr_t)(p)&3)"
        , "#define idris2_vp_int_shift 32"
        , "#define idris2_vp_to_Int32(p) ((int32_t)((uintptr_t)(p) >> idris2_vp_int_shift))"
        , ""
        , "typedef struct { uint16_t refCounter; uint8_t tag; uint8_t reserved; } Value_header;"
        , "typedef struct { Value_header header; int32_t total; int32_t tag; char const *name; void* args[]; } Value_Constructor;"
        , "typedef void Value;"
        , "#else"
        , "/* RefC headers ARE present (IDRIS2_ICWASM_HAS_REFC_HEADERS=1). Newer RefC"
        , "   (forked idris2 / 0.8.0) renamed the value types to the Idris2_* family"
        , "   (Idris2_Value / Idris2_header / Idris2_Constructor). Alias the legacy"
        , "   Value / Value_header / Value_Constructor names this generated entry uses"
        , "   onto them so the entry compiles against the real runtime headers"
        , "   (previously these names were undefined here → 'unknown type name Value')."
        , "   The idris2_trampoline / idris2_apply_closure / idris2_newReference funcs"
        , "   keep their names across versions, so only the TYPES need aliasing. */"
        , "#ifndef IDRIS2_ICWASM_VALUE_COMPAT_ALIASES"
        , "#define IDRIS2_ICWASM_VALUE_COMPAT_ALIASES 1"
        , "typedef Idris2_Value Value;"
        , "typedef Idris2_header Value_header;"
        , "typedef Idris2_Constructor Value_Constructor;"
        , "#endif"
        , "#endif"
        , "extern Value* __mainExpression_0(void);"
        , "extern Value* idris2_trampoline(Value*);"
        , "extern Value* idris2_apply_closure(Value*, Value*);"
        , "extern Value* idris2_newReference(Value*);"
        , "extern int idris2_extractInt(Value*);"
        , ""
        , "static int idris2_initialized = 0;"
        , ""
        , "static void ensure_idris2_init(void) {"
        , "    if (!idris2_initialized) {"
        , "        Value* closure = __mainExpression_0();"
        , "        idris2_trampoline(closure);"
      , "        idris2_initialized = 1;"
      , "    }"
      , "}"
      , ""
      , "static void debug_log(const char* msg) {"
      , "    ic0_debug_print((int32_t)msg, strlen(msg));"
      , "}"
      , ""
      , "static void reply_text(const char* text) {"
      , "    size_t len = strlen(text);"
      , "    uint8_t header[16] = { 'D', 'I', 'D', 'L', 0x00, 0x01, 0x71 };"
      , "    int pos = 7;"
      , "    size_t l = len;"
      , "    do {"
      , "        header[pos++] = (l & 0x7f) | (l > 0x7f ? 0x80 : 0);"
      , "        l >>= 7;"
      , "    } while (l > 0);"
      , "    ic0_msg_reply_data_append((int32_t)(uintptr_t)header, pos);"
      , "    ic0_msg_reply_data_append((int32_t)(uintptr_t)text, len);"
      , "    ic0_msg_reply();"
      , "}"
      , ""
      , "/* RefC Value extraction helpers */"
      , "static int32_t extract_int(void* v) {"
      , "    return (int32_t)idris2_extractInt(v);"
      , "}"
      , ""
      , "static void extract_int_pair(void* v, int32_t* a, int32_t* b) {"
      , "    Value_Constructor* con = (Value_Constructor*)v;"
      , "    *a = extract_int(con->args[0]);"
      , "    *b = extract_int(con->args[1]);"
      , "}"
      , ""
      , "/* Reply with text \"passed,failed\" so dfx can always decode it */"
      , "static void reply_int_pair(int32_t passed, int32_t failed) {"
      , "    char buf[64];"
      , "    snprintf(buf, sizeof(buf), \"%d,%d\", passed, failed);"
      , "    reply_text(buf);"
      , "}"
      , ""
      , "/* Canister Lifecycle */"
      , "__attribute__((export_name(\"canister_init\")))"
      , "void canister_init(void) {"
      , "    debug_log(\"Idris2 canister: init\");"
      , "    /* Pre-allocate stable memory for ic-wasm profiling (pages 10-25) */"
      , "    /* Canister data uses pages 0-9, profiling uses 10+ */"
      , "    ic0_stable64_grow(26);"
      , "    ensure_idris2_init();"
      , "}"
      , ""
      , "__attribute__((export_name(\"canister_post_upgrade\")))"
      , "void canister_post_upgrade(void) {"
      , "    debug_log(\"Idris2 canister: post_upgrade\");"
      , "    ensure_idris2_init();"
      , "}"
      , ""
      , "__attribute__((export_name(\"canister_pre_upgrade\")))"
      , "void canister_pre_upgrade(void) {"
      , "    debug_log(\"Idris2 canister: pre_upgrade\");"
      , "}"
      , ""
      , "/* Auto-generated Entry Points */"
      ]

||| Generate a test ipkg that uses symlinked sources with generated Main
||| Returns path to temp ipkg
generateTestIpkg : String -> String -> String -> IO (Either String String)
generateTestIpkg originalIpkg projectDir tempSrcDir = do
  Right content <- readFile originalIpkg
    | Left err => pure (Left $ "Failed to read ipkg: " ++ show err)

  -- Modify ipkg to use temp source directory (with symlinks + generated Main)
  let modified = modifyIpkg content tempSrcDir
  let tempDir = tempSrcDir ++ "/.."
  let tempIpkgPath = tempDir ++ "/test_build.ipkg"

  Right () <- writeFile tempIpkgPath modified
    | Left err => pure (Left $ "Failed to write temp ipkg: " ++ show err)

  pure (Right tempIpkgPath)
  where
    modifyLine : String -> String -> String
    modifyLine tmpSrcDir line =
      let trimmed = trim line
      in if isPrefixOf "main" trimmed
           then "main = Main"
         else if isPrefixOf "sourcedir" trimmed
           then "sourcedir = \"" ++ tmpSrcDir ++ "\""
         else line

    modifyIpkg : String -> String -> String
    modifyIpkg content tmpSrcDir =
      let ls = lines content
          modified = map (modifyLine tmpSrcDir) ls
      in unlines modified

||| Setup test build: check test module exists, create symlinked temp dir
||| Uses symlinks to original src/ files but generates new Main.idr
||| Returns (tempIpkgPath) on success - atomic: never modifies original files
||| @customTestPath - Optional custom test module path (relative to projectDir, e.g., "src/Economics/Tests/AllTests.idr")
setupTestBuild : String -> String -> Maybe String -> IO (Either String String)
setupTestBuild projectDir originalIpkg customTestPath = do
  -- Convert projectDir to absolute path (needed for symlinks to work from temp dir)
  (_, absProjectDir', _) <- executeCommand $ "cd " ++ projectDir ++ " && pwd"
  let absProjectDir = trim absProjectDir'

  -- Check test module exists (fail fast)
  let testModulePath = case customTestPath of
        Just p  => absProjectDir ++ "/" ++ p
        Nothing => absProjectDir ++ "/src/Tests/AllTests.idr"
  let testModuleRelPath = fromMaybe "src/Tests/AllTests.idr" customTestPath
  Right testModuleContent <- readFile testModulePath
    | Left _ => pure (Left $ "Test module not found: " ++ testModuleRelPath)
  let testModuleName = pathToModuleName testModuleRelPath
  let harnessStyle = detectTestHarnessStyle testModuleContent
  let directTestNames = parseIoBoolTestNames testModuleContent

  -- Create temp directory structure
  let tempDir = "/tmp/idris2-icwasm-test-" ++ show !time
  let tempSrcDir = tempDir ++ "/src"
  _ <- system $ "mkdir -p " ++ tempSrcDir

  -- Create symlinks to all src/ items EXCEPT Main.idr
  -- This allows `import Tests.AllTests` to work while using our generated Main
  -- Uses absolute paths so symlinks work from any location
  (_, files, _) <- executeCommand $ "ls " ++ absProjectDir ++ "/src/"
  let srcItems = filter (\s => not (null s) && s /= "Main.idr") (lines files)
  _ <- traverse_ (\item => system $ "ln -sf " ++ absProjectDir ++ "/src/" ++ item ++ " " ++ tempSrcDir ++ "/" ++ item) srcItems

  -- Write generated Main.idr to temp (not a symlink, actual generated file)
  let tempHarnessPath = tempSrcDir ++ "/TestHarness.idr"
  let tempMainPath = tempSrcDir ++ "/Main.idr"
  Right () <- writeFile tempHarnessPath (generateTestHarnessShimContent harnessStyle testModuleName directTestNames)
    | Left err => pure (Left $ "Failed to write temp TestHarness: " ++ show err)
  Right () <- writeFile tempMainPath generateTestMainContent
    | Left err => pure (Left $ "Failed to write temp Main: " ++ show err)

  -- Generate temp ipkg pointing to temp src directory
  Right tempIpkg <- generateTestIpkg originalIpkg absProjectDir tempSrcDir
    | Left err => pure (Left err)

  pure (Right tempIpkg)

||| Find ipkg file in project directory
findIpkg : String -> IO (Maybe String)
findIpkg projectDir = do
  let stableIpkgFilter = " ! -name 'temp*.ipkg' ! -name 'dumpcases-temp-*.ipkg' ! -name 'dfx-dumppaths-temp-*.ipkg' ! -name '*-temp-*.ipkg'"
  canister <- findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*canister*.ipkg' -type f" ++ stableIpkgFilter ++ " | sort | head -1"
  case canister of
    Just path => pure (Just path)
    Nothing => do
      nonMinimal <- findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f ! -name '*minimal*.ipkg'" ++ stableIpkgFilter ++ " | sort | head -1"
      case nonMinimal of
        Just path => pure (Just path)
        Nothing => findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f" ++ stableIpkgFilter ++ " | sort | head -1"
  where
    findPreferred : String -> IO (Maybe String)
    findPreferred cmd = do
      (_, result, _) <- executeCommand cmd
      pure $ if null (trim result) then Nothing else Just (trim result)

resolvePackagesFromIpkg : String -> IO (List String)
resolvePackagesFromIpkg ipkg = do
  Right content <- readFile ipkg
    | Left _ => pure []
  pure $ parseDepends content
  where
    stripComment : String -> String
    stripComment line = pack (takeUntilComment (unpack line))
      where
        takeUntilComment : List Char -> List Char
        takeUntilComment [] = []
        takeUntilComment ('-' :: '-' :: _) = []
        takeUntilComment (c :: cs) = c :: takeUntilComment cs

    tokenChars : String -> List String
    tokenChars s =
      forget $ split (\c => c == ',' || c == ' ' || c == '\t') s

    cleanToken : String -> String
    cleanToken tok =
      let chars0 = unpack (trim tok)
          chars1 = case chars0 of
                     '"' :: rest => rest
                     _ => chars0
          chars2 = case reverse chars1 of
                     '"' :: rest => reverse rest
                     _ => chars1
      in pack chars2

    parseDependsLine : String -> List String
    parseDependsLine raw =
      let line = trim (stripComment raw)
          afterEquals =
            case break (== '=') line of
              (_, "") => line
              (_, rest) => case unpack rest of
                              [] => ""
                              (_ :: chars) => pack chars
      in filter (\tok => not (null tok) && tok /= "depends" && tok /= "=")
                (map cleanToken (tokenChars afterEquals))

    collectDepends : Bool -> List String -> List String
    collectDepends _ [] = []
    collectDepends active (line :: rest) =
      let trimmed = trim (stripComment line)
      in if isPrefixOf "depends" trimmed
            then parseDependsLine trimmed ++ collectDepends True rest
         else if active && isPrefixOf "," trimmed
            then parseDependsLine trimmed ++ collectDepends True rest
         else if active && null trimmed
            then collectDepends active rest
         else if active
            then []
         else collectDepends False rest

    parseDepends : String -> List String
    parseDepends content = collectDepends False (lines content)

resolvePackagesFromProject : String -> IO (List String)
resolvePackagesFromProject projectDir = do
  (_, output, _) <- executeCommand $
    "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f | sort"
  deps <- traverse resolvePackagesFromIpkg (filter (not . null) (lines (trim output)))
  pure (concat deps)

public export
compileToRefC : BuildOptions -> String -> IO (Either String String)
compileToRefC opts buildDir = do
  putStrLn "      Step 1: Idris2 → C (RefC backend)"

  -- Try to find ipkg file for proper dependency resolution
  Just ipkgFile <- findIpkg opts.projectDir
    | Nothing => do
        putStrLn "        No .ipkg file found, using direct compilation"
        compileDirectly opts buildDir

  -- For test builds: generate temp Main.idr in /tmp (atomic, never touches original)
  if opts.forTestBuild
    then do
      let testPath = fromMaybe "src/Tests/AllTests.idr" opts.testModulePath
      putStrLn $ "        Test build mode: generating temp Main from " ++ testPath
      resolvedPackages <- resolvePackagesFromIpkg ipkgFile
      projectPackages <- resolvePackagesFromProject opts.projectDir
      let testPackages = opts.packages ++ resolvedPackages ++ projectPackages
      when (not (null testPackages)) $
        putStrLn $ "        Resolved packages: " ++ show testPackages
      let directOpts =
            if null testPackages then opts else { packages := testPackages } opts
      compileTestDirectly directOpts buildDir testPath
    else compileWithIpkg opts ipkgFile
  where
    -- Extract directory from a file path
    dirname : String -> String
    dirname path =
      let chars = unpack (reverse path)
          (_, rest) = break (== '/') chars
      in pack (reverse (drop 1 rest))

    compileWithIpkg : BuildOptions -> String -> IO (Either String String)
    compileWithIpkg opts' ipkg = do
      -- For test builds, ipkg is in temp dir (e.g., /tmp/idris2-icwasm-test-xxx/test_build.ipkg)
      -- RefC codegen may still emit C under the project build dir even when the
      -- ipkg lives in a temp directory, so search both locations.
      let ipkgDir = dirname ipkg
      let candidateDirs =
            if ipkgDir /= opts'.projectDir && not (null ipkgDir)
               then [opts'.projectDir ++ "/build", ipkgDir ++ "/build"]
               else [opts'.projectDir ++ "/build"]
      let findCmd =
            "sh -c 'find "
            ++ joinBy " " candidateDirs
            ++ " -name \"*.c\" 2>/dev/null | xargs ls -t 2>/dev/null | head -1'"
      let clearCmd =
            "sh -c 'find "
            ++ joinBy " " candidateDirs
            ++ " -name \"*.c\" -delete 2>/dev/null || true'"

      -- Use pack (not bare idris2) so pack.toml dependencies are resolved
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                "pack --cg refc build " ++ ipkg

      _ <- executeCommand clearCmd
      -- RefC generates C file then tries native compile (which fails without GMP)
      -- We ignore the exit code and just check if C file was generated
      _ <- executeCommand cmd

      -- Find generated C file in either the project build dir or temp build dir
      (_, cFile, _) <- executeCommand findCmd
      if null (trim cFile)
        then pure $ Left "No C file generated by RefC"
        else do
          putStrLn $ "        Generated: " ++ trim cFile
          pure $ Right (trim cFile)

    compileTestDirectly : BuildOptions -> String -> String -> IO (Either String String)
    compileTestDirectly opts' buildDir' testModuleRelPath = do
      idris2Bin <- resolveIdris2Bin
      let testModulePath = opts'.projectDir ++ "/" ++ testModuleRelPath
      Right testModuleContent <- readFile testModulePath
        | Left _ => pure $ Left $ "Test module not found: " ++ testModuleRelPath
      let harnessStyle = detectTestHarnessStyle testModuleContent
      let testModuleName = pathToModuleName testModuleRelPath
      let tempDir = "/tmp/idris2-icwasm-test-" ++ show !time
      let tempSrcDir = tempDir ++ "/src"
      let tempMainPath = tempSrcDir ++ "/Main.idr"
      let tempHarnessPath = tempSrcDir ++ "/TestHarness.idr"
      let outputBase = tempDir ++ "/test_runner"
      let tempDumpcasesPath = tempDir ++ "/idris2-test-harness.dumpcases.txt"
      let projectDumpcasesPath = opts'.projectDir ++ "/build/idris2-test-harness.dumpcases.txt"
      let legacyDumpcasesPath = buildDir' ++ "/idris2-test-harness.dumpcases.txt"
      let pkgFlags = unwords $ map (\p => "-p " ++ p) opts'.packages
      let candidateDirs = [buildDir', tempDir, opts'.projectDir ++ "/build"]
      _ <- system $ "mkdir -p " ++ tempSrcDir
      (_, absProjectDir', _) <- executeCommand $ "cd " ++ opts'.projectDir ++ " && pwd"
      let absProjectDir = trim absProjectDir'
      (_, files, _) <- executeCommand $ "ls " ++ absProjectDir ++ "/src/"
      let srcItems = filter (\s => not (null s) && s /= "Main.idr") (lines files)
      _ <- traverse_ (\item => system $ "ln -sf " ++ absProjectDir ++ "/src/" ++ item ++ " " ++ tempSrcDir ++ "/" ++ item) srcItems
      putStrLn $ "        Direct RefC test compile via temp sources: " ++ tempDir
      putStrLn $ "        Direct RefC packages: " ++ show opts'.packages
      Right () <- writeFile tempHarnessPath (generateTestHarnessShimContent harnessStyle testModuleName (parseIoBoolTestNames testModuleContent))
        | Left err => pure $ Left $ "Failed to write temp TestHarness: " ++ show err
      Right () <- writeFile tempMainPath generateTestMainContent
        | Left err => pure $ Left $ "Failed to write temp Main: " ++ show err
      let clearCmd =
            "sh -c 'find "
            ++ joinBy " " candidateDirs
            ++ " -name \"*.c\" -delete 2>/dev/null || true; rm -f "
            ++ outputBase ++ ".c'"
      -- Path-coverage: enable --dumppathshits so the (forked) compiler injects
      -- prim__recordPathHit at every CaseTree leaf → idris2_recordPathHit (pathcov.c).
      -- The file arg content is irrelevant for WASM (recording is in-memory).
      let pathHitsFlag =
            if opts'.instrumentPathHits
               then "--dumppathshits " ++ tempDir ++ "/.pathhits-enable "
               else ""
      -- CRITICAL for path-hits: --dumppathshits only instruments modules that get
      -- RECOMPILED in this invocation. Stale TTCs (e.g. projectDir/build/ttc, or the
      -- temp source's own build/ttc from a prior run) are reused as-is and stay
      -- UN-instrumented, so only Main/TestHarness would record hits. Clear all TTC
      -- caches so every module (Tests.AllTests + logic) rebuilds WITH the flag.
      when opts'.instrumentPathHits $ do
        _ <- system $ "rm -rf " ++ tempDir ++ "/build/ttc " ++ tempSrcDir ++ "/build/ttc "
                    ++ opts'.projectDir ++ "/build/ttc 2>/dev/null || true"
        pure ()
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                idris2Bin ++ " --codegen refc " ++
                pathHitsFlag ++
                "--source-dir " ++ tempSrcDir ++ " " ++
                pkgFlags ++ " " ++
                "-o " ++ outputBase ++ " " ++
                tempMainPath
      let dumpcasesCmd = "cd " ++ opts'.projectDir ++ " && " ++
                         idris2Bin ++ " --dumpcases " ++ tempDumpcasesPath ++ " " ++
                         "--codegen refc " ++
                         "--source-dir " ++ tempSrcDir ++ " " ++
                         pkgFlags ++ " " ++
                         "-o " ++ outputBase ++ " " ++
                         tempMainPath ++ " 2>&1"
      _ <- system $ "mkdir -p " ++ buildDir'
      _ <- system $ "mkdir -p " ++ opts'.projectDir ++ "/build"
      _ <- executeCommand clearCmd
      (_, _, stderr) <- executeCommand cmd
      _ <- executeCommand dumpcasesCmd
      _ <- system $ "sh -c 'test -f " ++ tempDumpcasesPath ++
                    " && cp " ++ tempDumpcasesPath ++ " " ++ projectDumpcasesPath ++
                    " && cp " ++ tempDumpcasesPath ++ " " ++ legacyDumpcasesPath ++
                    " || true'"
      Right _ <- readFile (outputBase ++ ".c")
        | Left _ => do
            let findCmd =
                  "sh -c 'find "
                  ++ joinBy " " candidateDirs
                  ++ " -name \"*.c\" 2>/dev/null | xargs ls -t 2>/dev/null | head -1'"
            (_, cFile, _) <- executeCommand findCmd
            if null (trim cFile)
              then pure $ Left $ "No C file generated by RefC\n" ++ stderr
              else do
                putStrLn $ "        Generated: " ++ trim cFile
                pure $ Right (trim cFile)
      putStrLn $ "        Generated: " ++ outputBase ++ ".c"
      pure $ Right (outputBase ++ ".c")

    compileDirectly : BuildOptions -> String -> IO (Either String String)
    compileDirectly opts' buildDir' = do
      idris2Bin <- resolveIdris2Bin
      let pkgFlags = unwords $ map (\p => "-p " ++ p) opts'.packages
      -- Path-coverage instrumentation: `--dumppathshits <file>` enables the
      -- CompileExpr pass that injects prim__recordPathHit "<fn>#p<n>" at every
      -- canonical CaseTree leaf (lowered by RefC to idris2_recordPathHit). The
      -- file arg is required by the flag but its CONTENT is irrelevant for WASM —
      -- recording happens in-memory via pathcov.c and is read back through the
      -- __get_path_hits canister query, not from this file.
      let pathHitsFlag =
            if opts'.instrumentPathHits
               then "--dumppathshits " ++ buildDir' ++ "/.pathhits-enable "
               else ""
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                "mkdir -p " ++ buildDir' ++ " && " ++
                idris2Bin ++ " --codegen refc " ++
                pathHitsFlag ++
                "--build-dir " ++ buildDir' ++ " " ++
                pkgFlags ++ " " ++
                "--source-dir src " ++
                "-o main " ++
                opts'.mainModule
      let clearCmd = "sh -c 'find " ++ buildDir' ++ " -name \"*.c\" -delete 2>/dev/null || true'"
      _ <- executeCommand clearCmd
      _ <- executeCommand cmd
      let findCmd = "sh -c 'find " ++ buildDir' ++ " -name \"*.c\" 2>/dev/null | xargs ls -t 2>/dev/null | head -1'"
      (_, cFile, _) <- executeCommand findCmd
      if null (trim cFile)
        then pure $ Left "No C file generated by RefC"
        else do
          putStrLn $ "        Generated: " ++ trim cFile
          pure $ Right (trim cFile)

||| Step 2: Download/locate RefC runtime dependencies
|||
||| Returns (refcSrcDir, miniGmpDir)
public export
prepareRefCRuntime : IO (Either String (String, String))
prepareRefCRuntime = do
  putStrLn "      Step 2: Preparing RefC runtime"

  let refcSrc = "/tmp/refc-src"
  let miniGmp = "/tmp/mini-gmp"

  mLocalRefc <- findLocalRefcRuntime
  case mLocalRefc of
    Nothing => pure ()
    Just localRefc => do
      _ <- system $
        "mkdir -p " ++ refcSrc ++ " && " ++
        "rm -f " ++ refcSrc ++ "/datatypes.h && " ++
        "cp " ++ localRefc ++ "/*.c " ++ localRefc ++ "/*.h " ++ refcSrc ++ "/ 2>/dev/null || true"
      pure ()

  -- Check if already available. Prefer the local installed Idris2 support
  -- files, because GitHub master can drift away from the compiler in use.
  Right _ <- readFile (refcSrc ++ "/runtime.c")
    | Left _ => downloadRuntime refcSrc miniGmp

  Right _ <- readFile (refcSrc ++ "/cBackend.h")
    | Left _ => downloadRuntime refcSrc miniGmp

  Right _ <- readFile (refcSrc ++ "/_datatypes.h")
    | Left _ => downloadRuntime refcSrc miniGmp

  Right _ <- readFile (miniGmp ++ "/mini-gmp.c")
    | Left _ => downloadRuntime refcSrc miniGmp

  putStrLn "        Runtime ready"
  pure $ Right (refcSrc, miniGmp)
  where
    findLocalRefcRuntime : IO (Maybe String)
    findLocalRefcRuntime = do
      (_, output, _) <- executeCommand $
        "for d in " ++
        "/Users/bob/code/idrislang-idris2/support/refc " ++
        "\"$HOME/.idris2/idris2-0.8.0/support/refc\" " ++
        "$(ls -td \"$HOME\"/.local/state/pack/install/*/idris2/idris2-0.8.0/support/refc 2>/dev/null); do " ++
        "if [ -f \"$d/runtime.c\" ] && [ -f \"$d/cBackend.h\" ] && [ -f \"$d/_datatypes.h\" ]; then echo \"$d\"; break; fi; " ++
        "done"
      let path = trim output
      if null path then pure Nothing else pure (Just path)

    gmpWrapper : String
    gmpWrapper = "#ifndef GMP_WRAPPER_H\n#define GMP_WRAPPER_H\n#include \"mini-gmp.h\"\n#include <stdarg.h>\nstatic inline void mpz_inits(mpz_t x, ...) {\n    va_list ap; va_start(ap, x); mpz_init(x);\n    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_init(x);\n    va_end(ap);\n}\nstatic inline void mpz_clears(mpz_t x, ...) {\n    va_list ap; va_start(ap, x); mpz_clear(x);\n    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_clear(x);\n    va_end(ap);\n}\n#endif\n"

    downloadRuntime : String -> String -> IO (Either String (String, String))
    downloadRuntime refcSrc miniGmp = do
      putStrLn "        Downloading RefC runtime..."

      -- Download RefC sources
      let refcFiles : List String = ["memoryManagement.c", "runtime.c", "stringOps.c",
                       "mathFunctions.c", "casts.c", "clock.c", "buffer.c",
                       "prim.c", "refc_util.c"]
      let refcHeaders : List String = ["runtime.h", "cBackend.h", "_datatypes.h",
                         "refc_util.h", "mathFunctions.h", "memoryManagement.h",
                         "stringOps.h", "casts.h", "clock.h", "buffer.h",
                         "prim.h", "threads.h"]
      let cFiles : List String = ["idris_support.c", "idris_file.c", "idris_directory.c", "idris_util.c"]
      let cHeaders : List String = ["idris_support.h", "idris_file.h", "idris_directory.h", "idris_util.h"]

      _ <- system $ "mkdir -p " ++ refcSrc ++ " " ++ miniGmp

      -- Download refc files
      _ <- traverse_ (\f => system $
        "curl -sLo " ++ refcSrc ++ "/" ++ f ++
        " https://raw.githubusercontent.com/idris-lang/Idris2/master/support/refc/" ++ f)
        (refcFiles ++ refcHeaders)

      -- Download c support files
      _ <- traverse_ (\f => system $
        "curl -sLo " ++ refcSrc ++ "/" ++ f ++
        " https://raw.githubusercontent.com/idris-lang/Idris2/master/support/c/" ++ f)
        (cFiles ++ cHeaders)

      -- Download mini-gmp
      _ <- system $ "curl -sLo " ++ miniGmp ++ "/mini-gmp.c https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.c"
      _ <- system $ "curl -sLo " ++ miniGmp ++ "/mini-gmp.h https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.h"

      -- Create gmp.h wrapper
      Right _ <- writeFile (miniGmp ++ "/gmp.h") gmpWrapper
        | Left err => pure $ Left $ "Failed to write gmp.h: " ++ show err

      pure $ Right (refcSrc, miniGmp)

||| Step 3: Compile C to WASM using Emscripten
|||
||| @cFile Path to C file from RefC
||| @refcSrc Path to RefC runtime sources
||| @miniGmp Path to mini-gmp
||| @ic0Support Path to IC0 support files (canister_entry.c, etc.)
||| @outputWasm Output WASM path
||| Find FFI header files in a directory.
findFfiHeaders : String -> IO (List String)
findFfiHeaders dir = do
  -- Project canisters often expose package-specific FFI names such as
  -- mmnt_ffi.h, so collect all direct support headers instead of only ic_*.
  (_, output, _) <- executeCommand $
    "{ project_dir=$(cd " ++ dir ++ "/../.. 2>/dev/null && pwd); " ++
    "sqlite_ic0=''; " ++
    "for candidate in \"$project_dir/../Idris2IcWasmSQLite/support/ic0\" \"$project_dir/../../../idris2-magical-utils/pkgs/Idris2IcWasmSQLite/support/ic0\"; do " ++
    "[ -d \"$candidate\" ] && sqlite_ic0=\"$candidate\" && break; " ++
    "done; " ++
    "indexer_ic0=\"$project_dir/../Idris2IcpIndexer/lib/ic0\"; " ++
    "find " ++ dir ++ " -maxdepth 1 -type f -name '*.h' 2>/dev/null; " ++
    "if [ -d \"$sqlite_ic0\" ]; then " ++
    "find \"$sqlite_ic0\" -maxdepth 1 -type f \\( -name 'sqlite_bridge.h' -o -name 'sqlite_vfs_bridge.h' \\) 2>/dev/null; " ++
    "find \"$sqlite_ic0/legacy\" -maxdepth 1 -type f -name 'sqlite_stable.h' 2>/dev/null; " ++
    "elif [ -d \"$indexer_ic0\" ]; then " ++
    "find \"$indexer_ic0\" -maxdepth 1 -type f \\( -name 'sqlite_bridge.h' -o -name 'sqlite_vfs_bridge.h' -o -name 'sqlite_stable.h' -o -name 'ic_http_outcall.h' -o -name 'ic_cycles.h' \\) 2>/dev/null; " ++
    "fi; }"
  pure $ if null (trim output)
         then []
         else nub (lines (trim output))

directSupportHeaderFlags : String -> String
directSupportHeaderFlags ic0Support =
  "$(find " ++ ic0Support ++ " -maxdepth 1 -type f -name '*.h' 2>/dev/null | sed 's/^/-include /') "

findExtraIncludeDirs : String -> IO (List String)
findExtraIncludeDirs ic0Support = do
  (_, output, _) <- executeCommand $
    "{ project_dir=$(cd " ++ ic0Support ++ "/../.. 2>/dev/null && pwd); " ++
    "sqlite_ic0=''; " ++
    "for candidate in \"$project_dir/../Idris2IcWasmSQLite/support/ic0\" \"$project_dir/../../../idris2-magical-utils/pkgs/Idris2IcWasmSQLite/support/ic0\"; do " ++
    "[ -d \"$candidate\" ] && sqlite_ic0=\"$candidate\" && break; " ++
    "done; " ++
    "indexer_ic0=\"$project_dir/../Idris2IcpIndexer/lib/ic0\"; " ++
    "for d in " ++ ic0Support ++ " \"$project_dir/build\" \"$sqlite_ic0\" \"$sqlite_ic0/sqlite\" \"$sqlite_ic0/legacy\"; do " ++
    "[ -d \"$d\" ] && echo \"$d\"; " ++
    "done; " ++
    "if [ ! -d \"$sqlite_ic0\" ]; then " ++
    "for d in \"$indexer_ic0\" \"$indexer_ic0/sqlite\"; do [ -d \"$d\" ] && echo \"$d\"; done; " ++
    "fi; }"
  pure $ if null (trim output)
         then []
         else lines (trim output)

findExtraCSources : String -> IO (List String)
findExtraCSources ic0Support = do
  (_, output, _) <- executeCommand $
    "{ project_dir=$(cd " ++ ic0Support ++ "/../.. 2>/dev/null && pwd); " ++
    "sqlite_ic0=''; " ++
    "for candidate in \"$project_dir/../Idris2IcWasmSQLite/support/ic0\" \"$project_dir/../../../idris2-magical-utils/pkgs/Idris2IcWasmSQLite/support/ic0\"; do " ++
    "[ -d \"$candidate\" ] && sqlite_ic0=\"$candidate\" && break; " ++
    "done; " ++
    "indexer_ic0=\"$project_dir/../Idris2IcpIndexer/lib/ic0\"; " ++
    "support_real=$(cd " ++ ic0Support ++ " 2>/dev/null && pwd); " ++
    "indexer_real=$(cd \"$indexer_ic0\" 2>/dev/null && pwd); " ++
    "sqlite_real=$(cd \"$sqlite_ic0\" 2>/dev/null && pwd); " ++
    "same_indexer_ic0=0; [ -n \"$indexer_real\" ] && [ \"$indexer_real\" = \"$support_real\" ] && same_indexer_ic0=1; " ++
    "same_sqlite_ic0=0; [ -n \"$sqlite_real\" ] && [ \"$sqlite_real\" = \"$support_real\" ] && same_sqlite_ic0=1; " ++
    "for f in " ++ ic0Support ++ "/*.c; do " ++
    "[ -f \"$f\" ] || continue; " ++
    "b=$(basename \"$f\"); " ++
    "case \"$b\" in canister_entry.c|canister_entry_legacy.c|ic0_stubs.c|wasi_stubs.c|ic_ffi_bridge.c|ic_call.c) continue ;; esac; " ++
    "case \"$b\" in sqlite_bridge.c|sqlite_vfs_bridge.c|sqlite_stable.c) [ -d \"$sqlite_ic0\" ] && [ \"$same_sqlite_ic0\" -eq 0 ] && continue ;; esac; " ++
    "case \"$b\" in sqlite_bridge.c|sqlite_vfs_bridge.c|sqlite_stable.c|wasi_polyfill.c) [ -d \"$indexer_ic0\" ] && [ \"$same_indexer_ic0\" -eq 0 ] && continue ;; esac; " ++
    "echo \"$f\"; " ++
    "done; " ++
    -- bulletproof_vfs.c DEFINES sqlite3_os_init (the SQLITE_OS_OTHER app-supplied
    -- VFS init); WITHOUT it the WASM imports sqlite3_os_init from env unresolved →
    -- deploy fails IC0505 "invalid import section". The canonical build
    -- (build-canister-from-config.sh) links it as $VFS_IMPL_C — mirror that here.
    "if [ -d \"$sqlite_ic0\" ] && [ \"$same_sqlite_ic0\" -eq 0 ]; then " ++
    "for rel in sqlite_bridge.c sqlite_vfs_bridge.c bulletproof_vfs.c legacy/sqlite_stable.c sqlite/libsqlite3.a; do " ++
    "[ -f \"$sqlite_ic0/$rel\" ] && echo \"$sqlite_ic0/$rel\"; " ++
    "done; " ++
    "elif [ -d \"$indexer_ic0\" ] && [ \"$same_indexer_ic0\" -eq 0 ]; then " ++
    "for rel in sqlite_bridge.c sqlite_vfs_bridge.c bulletproof_vfs.c sqlite_stable.c wasi_polyfill.c ic_http_outcall.c sqlite/libsqlite3.a; do " ++
    "[ -f \"$indexer_ic0/$rel\" ] && echo \"$indexer_ic0/$rel\"; " ++
    "done; " ++
    "fi; }"
  pure $ if null (trim output)
         then []
         else lines (trim output)

public export
compileToWasm : String -> String -> String -> String -> String -> IO (Either String ())
compileToWasm cFile refcSrc miniGmp ic0Support outputWasm = do
  putStrLn "      Step 3: C → WASM (Emscripten)"

  -- RefC source files (minimal set for canister)
  -- pathcov.c provides idris2_recordPathHit (path-coverage runtime). Linking it
  -- unconditionally is harmless for non-instrumented builds (the symbol is simply
  -- never called); instrumented builds (--dumppathshits) need it to resolve.
  let refcCFiles = unwords $ map (\f => refcSrc ++ "/" ++ f)
        ["runtime.c", "memoryManagement.c", "stringOps.c",
         "mathFunctions.c", "casts.c", "prim.c", "refc_util.c", "pathcov.c"]

  -- Check for ic_ffi_bridge.c (generic FFI bridge)
  Right _ <- readFile (ic0Support ++ "/ic_ffi_bridge.c")
    | Left _ => compileWithoutBridge cFile refcCFiles miniGmp ic0Support outputWasm

  -- Find project-specific FFI headers to include
  ffiHeaders <- findFfiHeaders ic0Support
  let includeFlags = unwords $ map (\h => "-include " ++ h) ffiHeaders
  let directHeaderFlags = directSupportHeaderFlags ic0Support
  let refcForceIncludeFlags = "-DIDRIS2_ICWASM_HAS_REFC_HEADERS=1 " ++
                              "-Didris2_cast_string_to_Integer=idris2_cast_String_to_Integer " ++
                              "-include " ++ refcSrc ++ "/cBackend.h " ++
                              "-include " ++ refcSrc ++ "/pathcov.h " ++
                              "-include " ++ miniGmp ++ "/gmp.h "
  extraIncludeDirs <- findExtraIncludeDirs ic0Support
  let extraIncludeFlags = unwords $ map (\d => "-I" ++ d) extraIncludeDirs
  autoExtraCSources <- findExtraCSources ic0Support
  let autoExtraFiles = if null autoExtraCSources then "" else unwords autoExtraCSources ++ " "

  let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile ++ " " ++
            refcCFiles ++ " " ++
            miniGmp ++ "/mini-gmp.c " ++
            ic0Support ++ "/ic0_stubs.c " ++
            ic0Support ++ "/canister_entry.c " ++
            ic0Support ++ "/wasi_stubs.c " ++
            ic0Support ++ "/ic_ffi_bridge.c " ++
            ic0Support ++ "/ic_call.c " ++
              autoExtraFiles ++
              refcForceIncludeFlags ++
              directHeaderFlags ++
              includeFlags ++ " " ++
            "-I" ++ miniGmp ++ " " ++
            "-I" ++ refcSrc ++ " " ++
            "-I" ++ ic0Support ++ " " ++
            extraIncludeFlags ++ " " ++
            "-o " ++ outputWasm ++ " " ++
            "-s STANDALONE_WASM=1 " ++
            "-s FILESYSTEM=0 " ++
            "-s ERROR_ON_UNDEFINED_SYMBOLS=0 " ++
            "--no-entry " ++
            "-g2 " ++
            "-gsource-map " ++
            "-O2"

  (exitCode, _, stderr) <- executeCommand cmd

  if exitCode /= 0
    then pure $ Left $ "Emscripten compilation failed: " ++ stderr
    else do
      putStrLn $ "        Output: " ++ outputWasm
      pure $ Right ()
  where
    -- Fallback when ic_ffi_bridge.c doesn't exist (legacy projects)
    compileWithoutBridge : String -> String -> String -> String -> String -> IO (Either String ())
    compileWithoutBridge cFile' refcCFiles' miniGmp' ic0Support' outputWasm' = do
      -- Find project-specific FFI headers
      ffiHeaders <- findFfiHeaders ic0Support'
      let includeFlags = unwords $ map (\h => "-include " ++ h) ffiHeaders
      let directHeaderFlags = directSupportHeaderFlags ic0Support'
      let refcForceIncludeFlags = "-DIDRIS2_ICWASM_HAS_REFC_HEADERS=1 " ++
                                  "-Didris2_cast_string_to_Integer=idris2_cast_String_to_Integer " ++
                                  "-include " ++ refcSrc ++ "/cBackend.h " ++
                                  "-include " ++ miniGmp' ++ "/gmp.h "
      extraIncludeDirs <- findExtraIncludeDirs ic0Support'
      let extraIncludeFlags = unwords $ map (\d => "-I" ++ d) extraIncludeDirs
      autoExtraCSources <- findExtraCSources ic0Support'
      let autoExtraFiles = if null autoExtraCSources then "" else unwords autoExtraCSources ++ " "

      let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile' ++ " " ++
                refcCFiles' ++ " " ++
                miniGmp' ++ "/mini-gmp.c " ++
                ic0Support' ++ "/ic0_stubs.c " ++
                ic0Support' ++ "/canister_entry.c " ++
                ic0Support' ++ "/wasi_stubs.c " ++
                ic0Support' ++ "/ic_call.c " ++
                  autoExtraFiles ++
                  refcForceIncludeFlags ++
                  directHeaderFlags ++
                  includeFlags ++ " " ++
                "-I" ++ miniGmp' ++ " " ++
                "-I" ++ refcSrc ++ " " ++
                "-I" ++ ic0Support' ++ " " ++
                extraIncludeFlags ++ " " ++
                "-o " ++ outputWasm' ++ " " ++
                "-s STANDALONE_WASM=1 " ++
                "-s FILESYSTEM=0 " ++
                "-s ERROR_ON_UNDEFINED_SYMBOLS=0 " ++
                "--no-entry " ++
                "-g2 " ++
                "-gsource-map " ++
                "-O2"

      (exitCode, _, stderr) <- executeCommand cmd

      if exitCode /= 0
        then pure $ Left $ "Emscripten compilation failed: " ++ stderr
        else do
          putStrLn $ "        Output: " ++ outputWasm'
          pure $ Right ()

||| Compile C to WASM with custom canister_entry.c path
||| Used when canister_entry.c is generated from Main.idr exports
public export
compileToWasmWithEntry : String -> String -> String -> String -> String -> String -> List String -> IO (Either String ())
compileToWasmWithEntry cFile refcSrc miniGmp ic0Support canisterEntryPath outputWasm extraCSources = do
  putStrLn "      Step 3: C → WASM (Emscripten)"

  -- pathcov.c provides idris2_recordPathHit (path-coverage runtime). Linking it
  -- unconditionally is harmless for non-instrumented builds (the symbol is simply
  -- never called); instrumented builds (--dumppathshits) need it to resolve.
  let refcCFiles = unwords $ map (\f => refcSrc ++ "/" ++ f)
        ["runtime.c", "memoryManagement.c", "stringOps.c",
         "mathFunctions.c", "casts.c", "prim.c", "refc_util.c", "pathcov.c"]

  -- Find project-specific FFI headers
  ffiHeaders <- findFfiHeaders ic0Support
  let includeFlags = unwords $ map (\h => "-include " ++ h) ffiHeaders
  let directHeaderFlags = directSupportHeaderFlags ic0Support
  let refcForceIncludeFlags = "-DIDRIS2_ICWASM_HAS_REFC_HEADERS=1 " ++
                              "-Didris2_cast_string_to_Integer=idris2_cast_String_to_Integer " ++
                              "-include " ++ refcSrc ++ "/cBackend.h " ++
                              "-include " ++ refcSrc ++ "/pathcov.h " ++
                              "-include " ++ miniGmp ++ "/gmp.h "
  extraIncludeDirs <- findExtraIncludeDirs ic0Support
  let extraIncludeFlags = unwords $ map (\d => "-I" ++ d) extraIncludeDirs
  autoExtraCSources <- findExtraCSources ic0Support
  let autoExtraFiles = if null autoExtraCSources then "" else unwords autoExtraCSources ++ " "

  -- Check for ic_ffi_bridge.c
  hasBridge <- do
    Right _ <- readFile (ic0Support ++ "/ic_ffi_bridge.c")
      | Left _ => pure False
    pure True

  -- Check for ic_call.c
  hasCall <- do
    Right _ <- readFile (ic0Support ++ "/ic_call.c")
      | Left _ => pure False
    pure True

  let bridgeFile = if hasBridge then ic0Support ++ "/ic_ffi_bridge.c " else ""
  let callFile = if hasCall then ic0Support ++ "/ic_call.c " else ""
  let extraFiles = if null extraCSources then autoExtraFiles else unwords extraCSources ++ " " ++ autoExtraFiles

  let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile ++ " " ++
            refcCFiles ++ " " ++
            miniGmp ++ "/mini-gmp.c " ++
            ic0Support ++ "/ic0_stubs.c " ++
            canisterEntryPath ++ " " ++  -- Use provided canister_entry.c
            ic0Support ++ "/wasi_stubs.c " ++
            bridgeFile ++
            callFile ++
              extraFiles ++
              refcForceIncludeFlags ++
              directHeaderFlags ++
              includeFlags ++ " " ++
            "-I" ++ miniGmp ++ " " ++
            "-I" ++ refcSrc ++ " " ++
            "-I" ++ ic0Support ++ " " ++
            extraIncludeFlags ++ " " ++
            "-o " ++ outputWasm ++ " " ++
            "-s STANDALONE_WASM=1 " ++
            "-s FILESYSTEM=0 " ++
            "-s ERROR_ON_UNDEFINED_SYMBOLS=0 " ++
            "--no-entry " ++
            "-g2 " ++
            "-gsource-map " ++
            "-O2"

  (exitCode, _, stderr) <- executeCommand cmd

  if exitCode /= 0
    then pure $ Left $ "Emscripten compilation failed: " ++ stderr
    else do
      putStrLn $ "        Output: " ++ outputWasm
      pure $ Right ()

||| Step 4: Stub WASI imports using wabt tools
|||
||| IC doesn't support WASI, so we replace WASI imports with stubs.
||| @inputWasm Input WASM with WASI imports
||| @outputWasm Output WASM with stubs
public export
stubWasi : String -> String -> IO (Either String ())
stubWasi inputWasm outputWasm = do
  putStrLn "      Step 4: WASI stubbing"

  -- Check if wabt tools available
  (code, _, _) <- executeCommand "which wasm2wat wat2wasm python3"
  if code /= 0
    then do
      -- Missing tools, just copy
      putStrLn "        wabt/python3 not found, skipping WASI stub"
      _ <- system $ "cp " ++ inputWasm ++ " " ++ outputWasm
      pure $ Right ()
    else do
      let watFile = inputWasm ++ ".wat"
      let stubbedWat = inputWasm ++ "_stubbed.wat"

      -- Convert to WAT
      (c1, _, e1) <- executeCommand $ "wasm2wat " ++ inputWasm ++ " -o " ++ watFile
      if c1 /= 0
        then pure $ Left $ "wasm2wat failed: " ++ e1
        else do
          -- Use stub_wasi.py script from idris2-icwasm/support/tools
          let scriptFile = "/Users/bob/code/idris2-magical-utils/pkgs/Idris2IcWasm/support/tools/stub_wasi.py"

          (c2, _, e2) <- executeCommand $ "python3 " ++ scriptFile ++ " " ++ watFile ++ " " ++ stubbedWat

          if c2 /= 0
            then do
              putStrLn $ "        Python stubbing failed: " ++ e2 ++ ", using original"
              _ <- system $ "cp " ++ inputWasm ++ " " ++ outputWasm
              pure $ Right ()
            else do
              -- Convert back to WASM (--debug-names preserves function names)
              (c3, _, e3) <- executeCommand $ "wat2wasm --debug-names " ++ stubbedWat ++ " -o " ++ outputWasm
              _ <- system $ "rm -f " ++ watFile ++ " " ++ stubbedWat

              if c3 /= 0
                then do
                  putStrLn $ "        wat2wasm failed: " ++ e3 ++ ", using original"
                  _ <- system $ "cp " ++ inputWasm ++ " " ++ outputWasm
                  pure $ Right ()
                else do
                  -- Verify no WASI imports remain
                  (_, wasiCheck, _) <- executeCommand $ "wasm2wat " ++ outputWasm ++ " 2>/dev/null | grep -c wasi_snapshot_preview1 || echo 0"
                  putStrLn $ "        WASI imports stubbed (remaining: " ++ trim wasiCheck ++ ")"
                  pure $ Right ()

-- =============================================================================
-- Main Build Function
-- =============================================================================

||| Find .did file in project directory
||| Looks for src/*.did or *.did in project root
findDidFile : String -> IO (Maybe String)
findDidFile projectDir = do
  (_, result, _) <- executeCommand $ "find " ++ projectDir ++ "/src -maxdepth 1 -name '*.did' 2>/dev/null | head -1"
  if not (null (trim result))
    then pure $ Just (trim result)
    else do
      (_, result2, _) <- executeCommand $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.did' 2>/dev/null | head -1"
      pure $ if null (trim result2) then Nothing else Just (trim result2)

||| Infer the RefC-level arity of generated top-level symbols from the RefC C output.
||| This is required because top-level `IO a` exports may compile either as:
||| - unary symbols accepting the world directly, or
||| - nullary symbols returning an IO closure
||| Normalize RefC value-type naming so the arity patterns match across compiler
||| versions. Newer RefC (forked idris2 / 0.8.0) emits `Idris2_Value *Foo(...)`
||| signatures; the patterns below were written for the older bare `Value *Foo`.
||| Without this, EVERY export's arity inference silently fails (the C never
||| contains a bare `Value *` signature) → "Failed to infer RefC arity for one or
||| more exports", which blocked the dfx coverage test-build entirely.
normalizeRefCValueType : String -> String
normalizeRefCValueType s =
  pack (go (unpack s))
  where
    needle : List Char
    needle = unpack "Idris2_Value"
    go : List Char -> List Char
    go [] = []
    go cs@(c :: rest) =
      if isPrefixOf needle cs
         then unpack "Value" ++ go (drop (length needle) cs)
         else c :: go rest

parseRefCExportArities : String -> String -> List ExportedFunc -> List (String, RefCArity)
parseRefCExportArities modulePrefix cContent exports =
  let ls = map normalizeRefCValueType (lines cContent)
  in mapMaybe (parseOne ls) exports
  where
    targetName : ExportedFunc -> String
    targetName ef = modulePrefix ++ "_" ++ ef.name

    classifyFollowing : List String -> Maybe RefCArity
    classifyFollowing [] = Nothing
    classifyFollowing [line] =
      let t = trim line
      in if t == "(void)" then Just RefCNullary
         else if t == "(" then Nothing
         else if isPrefixOf "Value *" t then Just RefCUnary
         else Nothing
    classifyFollowing (line1 :: line2 :: _) =
      let t1 = trim line1
          t2 = trim line2
      in if t1 == "(void)" then Just RefCNullary
         else if t1 == "(" && isPrefixOf "Value *" t2 then Just RefCUnary
         else if isPrefixOf "(Value *" t1 then Just RefCUnary
         else Nothing

    classifySignature : String -> String -> List String -> Maybe RefCArity
    classifySignature target line rest =
      let sameLineVoid = "Value *" ++ target ++ "(void)"
          sameLineUnary = "Value *" ++ target ++ "("
          plainHeader = "Value *" ++ target
      in if line == sameLineVoid
            then Just RefCNullary
         else if line == plainHeader
            then classifyFollowing rest
         else if line == sameLineUnary
            then classifyFollowing rest
         else Nothing

    findSignature : String -> List String -> Maybe (String, RefCArity)
    findSignature target [] = Nothing
    findSignature target [line] =
      case classifySignature target (trim line) [] of
        Just arity => Just (target, arity)
        Nothing => Nothing
    findSignature target (line :: rest) =
      case classifySignature target (trim line) rest of
        Just arity => Just (target, arity)
        Nothing => findSignature target rest

    parseOne : List String -> ExportedFunc -> Maybe (String, RefCArity)
    parseOne ls ef = findSignature (targetName ef) ls

allRealExportsHaveArities : String -> List ExportedFunc -> List (String, RefCArity) -> Bool
allRealExportsHaveArities modulePrefix exports arities =
  let names = map fst arities
      required = map (\ef => modulePrefix ++ "_" ++ ef.name) (filter (\ef => not ef.fromDid) exports)
  in all (\name => name `elem` names) required

modulePathToPrefix : String -> String
modulePathToPrefix path =
  let withoutExt =
        if isSuffixOf ".idr" path
           then pack (reverse (drop 4 (reverse (unpack path))))
           else path
      parts = filter (\part => not (null part)) (forget (split (== '/') withoutExt))
      relParts =
        case parts of
          "src" :: rest => rest
          _ => parts
  in fastConcat $ intersperse "_" relParts

||| Generate canister_entry.c from Main.idr exports
||| Writes to temp file and returns path
generateCanisterEntry : BuildOptions -> String -> String -> IO (Either String String)
generateCanisterEntry opts cFile ic0Support = do
  -- Determine Main.idr content
  mainContent <- if opts.forTestBuild
    then do
      -- In test mode: ONLY generate runTests export
      -- DO NOT read original Main.idr - those functions won't be in test WASM
      -- .did methods will be added later via didMethodToExport (with fromDid = True)
      let testModulePath' = fromMaybe "src/Tests/AllTests.idr" opts.testModulePath
      pure generateTestMainContent
    else do
      let mainPath = opts.projectDir ++ "/" ++ opts.mainModule
      Right content <- readFile mainPath
        | Left _ => pure ""
      pure content

  -- Parse exported functions from Idris
  let rawExports = parseExportedFunctions mainContent
  -- Deduplicate by function name (keep first occurrence)
  let exports = nubBy (\a, b => a.name == b.name) rawExports
  putStrLn $ "        Parsed exports: " ++ show (length exports) ++ " functions"

  -- Try to find and parse .did file for Candid-aware stub generation
  (didMethods, typeDefs) <- do
    Just didPath <- findDidFile opts.projectDir
      | Nothing => do
          putStrLn "        No .did file found, using heuristic reply types"
          pure ([], [])
    Right didContent <- readFile didPath
      | Left _ => do
          putStrLn $ "        Warning: Could not read .did file: " ++ didPath
          pure ([], [])
    let methods = parseDidFile didContent
    let types = parseTypeDefinitions didContent
    putStrLn $ "        Parsed .did file: " ++ show (length methods) ++ " methods, " ++ show (length types) ++ " types"
    pure (methods, types)

  -- For test builds: use .did methods if available (for coverage testing)
  -- This ensures all canister methods are exported even if Idris doesn't define them
  let effectiveExports = if opts.forTestBuild && not (null didMethods)
        then let didExports = map didMethodToExport didMethods
                 -- Merge: keep Idris exports, add .did methods not already exported
                 existingNames = map (\e => e.name) exports
                 newFromDid = filter (\e => not (e.name `elem` existingNames)) didExports
             in exports ++ newFromDid
        else exports
  let normalizedExports =
        if opts.forTestBuild
           then map (\ef =>
                       if isTestHarnessExport ef.name
                          then { isQuery := False } ef
                          else ef)
                    effectiveExports
           else effectiveExports

  -- Test builds generate a temporary Main.idr. Production builds need the real module prefix.
  let modulePrefix =
        if opts.forTestBuild
           then "Main"
           else modulePathToPrefix opts.mainModule

  cArities <- do
    Right cContent <- readFile cFile
      | Left err => do
          let emptyArities : List (String, RefCArity)
              emptyArities = []
          pure emptyArities
    pure $ parseRefCExportArities modulePrefix cContent normalizedExports

  if null normalizedExports
    then do
      -- No exports found, use static canister_entry.c
      pure $ Right (ic0Support ++ "/canister_entry.c")
    else if not (allRealExportsHaveArities modulePrefix normalizedExports cArities)
      then pure $ Left $ "Failed to infer RefC arity for one or more exports"
    else do
      -- Generate dynamic canister_entry.c with Candid-aware stubs
      let entryC = generateCanisterEntryC modulePrefix cArities normalizedExports didMethods typeDefs opts.instrumentBranchProbes opts.instrumentPathHits
      let tempEntryPath = "/tmp/canister_entry_generated.c"
      Right () <- writeFile tempEntryPath entryC
        | Left err => pure $ Left $ "Failed to write canister_entry.c: " ++ show err
      when opts.forTestBuild $
        putStrLn $ "        Test build: merged " ++ show (length exports) ++ " Idris exports + " ++ show (length didMethods) ++ " .did methods"
      putStrLn $ "        Generated canister_entry.c with " ++ show (length normalizedExports) ++ " entry points"
      pure $ Right tempEntryPath

||| Build complete canister WASM from Idris2 source
|||
||| @opts Build options
||| @ic0Support Path to IC0 support files directory
||| Returns path to final stubbed WASM on success
public export
buildCanister : BuildOptions -> String -> IO BuildResult
buildCanister opts ic0Support = do
  putStrLn "    Building WASM (Idris2 → RefC → Emscripten)..."

  let buildDir = opts.projectDir ++ "/build/idris"
  let wasmDir = opts.projectDir ++ "/build"
  let rawWasm = wasmDir ++ "/" ++ opts.canisterName ++ ".wasm"
  let stubbedWasm = wasmDir ++ "/" ++ opts.canisterName ++ "_stubbed.wasm"
  let exportedProbeCsv = wasmDir ++ "/idris2-branch-probes.csv"

  -- Step 1: Idris2 → C
  Right cFile <- compileToRefC opts buildDir
    | Left err => pure $ BuildError err

  let cDir = if null (buildDirName cFile) then buildDir else buildDirName cFile
  probePrep <- if opts.instrumentBranchProbes
    then do
      result <- instrumentRefCCFile cFile cDir opts.projectDir
      pure $ map (\artifacts => (artifacts.instrumentedCPath, [artifacts.probeCPath], Just artifacts.csvPath)) result
    else pure $ Right (cFile, [], Nothing)
  Right (preparedCFile, extraCSources, mProbeCsv) <- pure probePrep
    | Left err => pure $ BuildError err
  when opts.instrumentBranchProbes $
    putStrLn $ "      Step 1.5: Generated branch probes"
  case mProbeCsv of
    Just csvPath => do
      _ <- system $ "mkdir -p " ++ wasmDir ++ " && cp " ++ csvPath ++ " " ++ exportedProbeCsv
      putStrLn $ "        Exported branch probe map: " ++ exportedProbeCsv
    Nothing => pure ()

  -- Step 2: Prepare runtime
  Right (refcSrc, miniGmp) <- prepareRefCRuntime
    | Left err => pure $ BuildError err

  -- Step 2.5: Generate canister_entry.c from Main.idr exports
  Right canisterEntryPath <- generateCanisterEntry opts preparedCFile ic0Support
    | Left err => pure $ BuildError err

  -- Step 3: C → WASM (use generated canister_entry.c)
  Right () <- compileToWasmWithEntry preparedCFile refcSrc miniGmp ic0Support canisterEntryPath rawWasm extraCSources
    | Left err => pure $ BuildError err

  -- Step 4: Stub WASI
  Right () <- stubWasi rawWasm stubbedWasm
    | Left err => pure $ BuildError err

  -- Step 5: Generate Source Maps (if enabled)
  when opts.generateSourceMap $ do
    putStrLn "      Step 5: Generating Source Maps"
    Right cContent <- readFile preparedCFile
      | Left _ => putStrLn "        Warning: Could not read C file for source map"
    let idrisCMap = generateIdrisCSourceMapWithFunctions preparedCFile cContent
    let idrisCMapPath = wasmDir ++ "/idris2-c.map"
    Right () <- writeSourceMap idrisCMapPath idrisCMap
      | Left _ => putStrLn "        Warning: Could not write idris2-c.map"
    putStrLn $ "        Generated: " ++ idrisCMapPath
    putStrLn $ "        Sources: " ++ show (length idrisCMap.sources) ++ " Idris files"
    putStrLn $ "        Functions: " ++ show (length idrisCMap.names) ++ " Idris functions"

  putStrLn $ "    Build complete: " ++ stubbedWasm
  pure $ BuildSuccess stubbedWasm
  where
    buildDirName : String -> String
    buildDirName path =
      let chars = unpack (reverse path)
          (_, rest) = break (== '/') chars
      in pack (reverse (drop 1 rest))

||| Build canister using project's lib/ic0 for support files
|||
||| @opts Build options
||| Returns path to final stubbed WASM on success
public export
buildCanisterAuto : BuildOptions -> IO BuildResult
buildCanisterAuto opts = do
  let candidateIc0Dirs =
        [ opts.projectDir ++ "/lib/ic0"
        , opts.projectDir ++ "/../idris2-icwasm/support/ic0"
        , opts.projectDir ++ "/../Idris2IcWasm/support/ic0"
        , opts.projectDir ++ "/../../Idris2IcWasm/support/ic0"
        , opts.projectDir ++ "/../../../Idris2IcWasm/support/ic0"
        ]
  supportDir <- firstExistingIc0Support candidateIc0Dirs
  case supportDir of
    Just dir => buildCanister opts dir
    Nothing => pure $ BuildError "IC0 support files not found in lib/ic0, ../idris2-icwasm/support/ic0, ../Idris2IcWasm/support/ic0, ../../Idris2IcWasm/support/ic0, or ../../../Idris2IcWasm/support/ic0"
  where
    firstExistingIc0Support : List String -> IO (Maybe String)
    firstExistingIc0Support [] = pure Nothing
    firstExistingIc0Support (dir :: rest) = do
      Right _ <- readFile (dir ++ "/canister_entry.c")
        | Left _ => firstExistingIc0Support rest
      pure (Just dir)
