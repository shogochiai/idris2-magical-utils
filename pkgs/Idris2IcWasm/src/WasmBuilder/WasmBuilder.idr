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
data TestHarnessStyle = ExistingHarness | TupleHarness | RecordHarness

detectTestHarnessStyle : String -> TestHarnessStyle
detectTestHarnessStyle content =
  let hasRunTests = isInfixOf "runTests : IO (Int, Int)" content
      hasRecordTestDef = isInfixOf "record TestDef where" content && isInfixOf "testFn : IO Bool" content
      hasTupleTests = isInfixOf "allTests : List (String, IO Bool)" content
  in if hasRunTests
        then ExistingHarness
     else if hasRecordTestDef
        then RecordHarness
     else if hasTupleTests
        then TupleHarness
     else ExistingHarness

generateTestHarnessShimContent : TestHarnessStyle -> String -> String
generateTestHarnessShimContent ExistingHarness testModuleName =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "%hide TestModule.main"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = TestModule.allTests"
    , ""
    , "export"
    , "runTests : IO (Int, Int)"
    , "runTests = TestModule.runTests"
    , ""
    , "export"
    , "runMinimalTests : IO (Int, Int)"
    , "runMinimalTests = TestModule.runMinimalTests"
    , ""
    , "export"
    , "runTrivialTest : IO (Int, Int)"
    , "runTrivialTest = TestModule.runTrivialTest"
    ]
generateTestHarnessShimContent TupleHarness testModuleName =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "import Data.List"
    , "%hide TestModule.main"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = TestModule.allTests"
    , ""
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
generateTestHarnessShimContent RecordHarness testModuleName =
  unlines
    [ "module TestHarness"
    , "import " ++ testModuleName ++ " as TestModule"
    , "import Data.List"
    , "%hide TestModule.main"
    , ""
    , "%default covering"
    , ""
    , "export"
    , "allTestsGeneric : List (String, IO Bool)"
    , "allTestsGeneric = map (\\t => (t.testId, t.testFn)) TestModule.allTests"
    , ""
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

||| Generate test Main.idr content that imports the standardized shim module
||| This is written to /tmp, never touches the original Main.idr
generateTestMainContent : String
generateTestMainContent =
  unlines
    [ "||| Auto-generated test entry point for coverage analysis"
    , "module Main"
    , ""
    , "import TestHarness"
    , "import Data.List"
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
    , ""
    , "||| Force code retention to prevent DCE"
    , "forceRetain : IO ()"
    , "forceRetain ="
    , "  let retained = [Main.runTests, Main.runMinimalTests, Main.runTrivialTest] in"
    , "    case retained of"
    , "      [] => pure ()"
    , "      _ => pure ()"
    , ""
    , "main : IO ()"
    , "main = forceRetain"
    ]

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
            , "    void* _world = idris2_newReference((void*)0);"
            , "    void* _result = idris2_trampoline(" ++ funcName ++ "(_world));"
            , "    (void)_result;"
            ]
        Just RefCNullary =>
          unlines
            [ "    // RefC nullary symbol: returns an IO closure, then apply world"
            , "    void* _world = idris2_newReference((void*)0);"
            , "    void* _action = idris2_trampoline(" ++ funcName ++ "());"
            , "    void* _result = idris2_apply_closure(_action, _world);"
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
generateCanisterEntryC : String -> List (String, RefCArity) -> List ExportedFunc -> List DidMethod -> List TypeDef -> Bool -> String
generateCanisterEntryC modulePrefix cArities exports didMethods typeDefs instrumentBranchProbes =
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
  in header ++ "\n/* Idris Function Externs */\n" ++ branchProbeExterns ++ funcExterns ++ "\n" ++ branchProbeEntry ++ funcEntries
  where
    mkExtern : ExportedFunc -> String
    mkExtern ef =
      let funcName = modulePrefix ++ "_" ++ ef.name
      in case lookup funcName cArities of
           Just RefCUnary => "extern void* " ++ funcName ++ "(void*);"
           Just RefCNullary => "extern void* " ++ funcName ++ "(void);"
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
      , "extern void ic_arg_load(void);  /* Load IC message args into buffer for CandidDecoder */"
      , ""
      , "/* Idris2 RefC Runtime - Value types */"
      , "#define CONSTRUCTOR_TAG 17"
      , "#define idris2_vp_is_unboxed(p) ((uintptr_t)(p)&3)"
      , "#define idris2_vp_int_shift 32"
      , "#define idris2_vp_to_Int32(p) ((int32_t)((uintptr_t)(p) >> idris2_vp_int_shift))"
      , ""
      , "typedef struct { uint16_t refCounter; uint8_t tag; uint8_t reserved; } Value_header;"
      , "typedef struct { Value_header header; int32_t total; int32_t tag; char const *name; void* args[]; } Value_Constructor;"
      , "typedef void* Value;"
      , "extern void* __mainExpression_0(void);"
      , "extern void* idris2_trampoline(void*);"
      , "extern void* idris2_apply_closure(void*, void*);"
      , "extern void* idris2_newReference(void*);"
      , "extern int idris2_extractInt(void*);"
      , ""
      , "static int idris2_initialized = 0;"
      , ""
      , "static void ensure_idris2_init(void) {"
      , "    if (!idris2_initialized) {"
      , "        void* closure = __mainExpression_0();"
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
  Right () <- writeFile tempHarnessPath (generateTestHarnessShimContent harnessStyle testModuleName)
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
  canister <- findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*canister*.ipkg' -type f | sort | head -1"
  case canister of
    Just path => pure (Just path)
    Nothing => do
      nonMinimal <- findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f ! -name '*minimal*.ipkg' | sort | head -1"
      case nonMinimal of
        Just path => pure (Just path)
        Nothing => findPreferred $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f | sort | head -1"
  where
    findPreferred : String -> IO (Maybe String)
    findPreferred cmd = do
      (_, result, _) <- executeCommand cmd
      pure $ if null (trim result) then Nothing else Just (trim result)

resolvePackagesFromIpkg : String -> IO (List String)
resolvePackagesFromIpkg ipkg = do
  idris2Bin <- resolveIdris2Bin
  let tmpJson = "/tmp/ipkg_dump_" ++ show !time ++ ".json"
  let dumpCmd = idris2Bin ++ " --dump-ipkg-json " ++ ipkg ++ " > " ++ tmpJson
  _ <- system dumpCmd
  let parseCmd =
        "python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); [print(next(iter(dep))) for dep in d.get(\"depends\", [])]' "
        ++ tmpJson
  (_, output, _) <- executeCommand parseCmd
  _ <- system $ "rm -f " ++ tmpJson
  pure $ filter (\s => not (null s)) (lines (trim output))

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
      when (not (null resolvedPackages)) $
        putStrLn $ "        Resolved packages: " ++ show resolvedPackages
      let directOpts =
            if null resolvedPackages then opts else { packages := resolvedPackages } opts
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
      Right () <- writeFile tempHarnessPath (generateTestHarnessShimContent harnessStyle testModuleName)
        | Left err => pure $ Left $ "Failed to write temp TestHarness: " ++ show err
      Right () <- writeFile tempMainPath generateTestMainContent
        | Left err => pure $ Left $ "Failed to write temp Main: " ++ show err
      let clearCmd =
            "sh -c 'find "
            ++ joinBy " " candidateDirs
            ++ " -name \"*.c\" -delete 2>/dev/null || true; rm -f "
            ++ outputBase ++ ".c'"
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                idris2Bin ++ " --codegen refc " ++
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
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                "mkdir -p " ++ buildDir' ++ " && " ++
                idris2Bin ++ " --codegen refc " ++
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

  -- Check if already downloaded
  Right _ <- readFile (refcSrc ++ "/runtime.c")
    | Left _ => downloadRuntime refcSrc miniGmp

  Right _ <- readFile (miniGmp ++ "/mini-gmp.c")
    | Left _ => downloadRuntime refcSrc miniGmp

  putStrLn "        Runtime ready"
  pure $ Right (refcSrc, miniGmp)
  where
    gmpWrapper : String
    gmpWrapper = "#ifndef GMP_WRAPPER_H\n#define GMP_WRAPPER_H\n#include \"mini-gmp.h\"\n#include <stdarg.h>\nstatic inline void mpz_inits(mpz_t x, ...) {\n    va_list ap; va_start(ap, x); mpz_init(x);\n    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_init(x);\n    va_end(ap);\n}\nstatic inline void mpz_clears(mpz_t x, ...) {\n    va_list ap; va_start(ap, x); mpz_clear(x);\n    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_clear(x);\n    va_end(ap);\n}\n#endif\n"

    downloadRuntime : String -> String -> IO (Either String (String, String))
    downloadRuntime refcSrc miniGmp = do
      putStrLn "        Downloading RefC runtime..."

      -- Download RefC sources
      let refcFiles : List String = ["memoryManagement.c", "runtime.c", "stringOps.c",
                       "mathFunctions.c", "casts.c", "clock.c", "buffer.c",
                       "prim.c", "refc_util.c"]
      let refcHeaders : List String = ["runtime.h", "cBackend.h", "datatypes.h", "_datatypes.h",
                         "refc_util.h", "mathFunctions.h", "memoryManagement.h",
                         "stringOps.h", "casts.h", "clock.h", "buffer.h",
                         "prim.h", "threads.h"]
      let cFiles : List String = ["idris_support.c", "idris_file.c", "idris_directory.c", "idris_util.c"]
      let cHeaders : List String = ["idris_support.h", "idris_file.h", "idris_directory.h", "idris_util.h"]

      _ <- system $ "mkdir -p " ++ refcSrc ++ " " ++ miniGmp

      -- Download refc files
      _ <- traverse_ (\f => system $
        "curl -sLo " ++ refcSrc ++ "/" ++ f ++
        " https://raw.githubusercontent.com/idris-lang/Idris2/main/support/refc/" ++ f)
        (refcFiles ++ refcHeaders)

      -- Download c support files
      _ <- traverse_ (\f => system $
        "curl -sLo " ++ refcSrc ++ "/" ++ f ++
        " https://raw.githubusercontent.com/idris-lang/Idris2/main/support/c/" ++ f)
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
||| Find FFI header files in a directory (*.h files starting with ic0_ or ic_)
findFfiHeaders : String -> IO (List String)
findFfiHeaders dir = do
  -- Look for project-specific FFI headers using find (more portable than glob)
  (_, output, _) <- executeCommand $ "find " ++ dir ++ " -maxdepth 1 -name 'ic0_*.h' -o -name 'ic_*.h' 2>/dev/null"
  pure $ if null (trim output)
         then []
         else lines (trim output)

public export
compileToWasm : String -> String -> String -> String -> String -> IO (Either String ())
compileToWasm cFile refcSrc miniGmp ic0Support outputWasm = do
  putStrLn "      Step 3: C → WASM (Emscripten)"

  -- RefC source files (minimal set for canister)
  let refcCFiles = unwords $ map (\f => refcSrc ++ "/" ++ f)
        ["runtime.c", "memoryManagement.c", "stringOps.c",
         "mathFunctions.c", "casts.c", "prim.c", "refc_util.c"]

  -- Check for ic_ffi_bridge.c (generic FFI bridge)
  Right _ <- readFile (ic0Support ++ "/ic_ffi_bridge.c")
    | Left _ => compileWithoutBridge cFile refcCFiles miniGmp ic0Support outputWasm

  -- Find project-specific FFI headers to include
  ffiHeaders <- findFfiHeaders ic0Support
  let includeFlags = unwords $ map (\h => "-include " ++ h) ffiHeaders

  let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile ++ " " ++
            refcCFiles ++ " " ++
            miniGmp ++ "/mini-gmp.c " ++
            ic0Support ++ "/ic0_stubs.c " ++
            ic0Support ++ "/canister_entry.c " ++
            ic0Support ++ "/wasi_stubs.c " ++
            ic0Support ++ "/ic_ffi_bridge.c " ++
            ic0Support ++ "/ic_call.c " ++
            includeFlags ++ " " ++
            "-I" ++ miniGmp ++ " " ++
            "-I" ++ refcSrc ++ " " ++
            "-I" ++ ic0Support ++ " " ++
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

      let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile' ++ " " ++
                refcCFiles' ++ " " ++
                miniGmp' ++ "/mini-gmp.c " ++
                ic0Support' ++ "/ic0_stubs.c " ++
                ic0Support' ++ "/canister_entry.c " ++
                ic0Support' ++ "/wasi_stubs.c " ++
                ic0Support' ++ "/ic_call.c " ++
                includeFlags ++ " " ++
                "-I" ++ miniGmp' ++ " " ++
                "-I" ++ refcSrc ++ " " ++
                "-I" ++ ic0Support' ++ " " ++
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

  let refcCFiles = unwords $ map (\f => refcSrc ++ "/" ++ f)
        ["runtime.c", "memoryManagement.c", "stringOps.c",
         "mathFunctions.c", "casts.c", "prim.c", "refc_util.c"]

  -- Find project-specific FFI headers
  ffiHeaders <- findFfiHeaders ic0Support
  let includeFlags = unwords $ map (\h => "-include " ++ h) ffiHeaders

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
  let extraFiles = if null extraCSources then "" else unwords extraCSources ++ " "

  let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile ++ " " ++
            refcCFiles ++ " " ++
            miniGmp ++ "/mini-gmp.c " ++
            ic0Support ++ "/ic0_stubs.c " ++
            canisterEntryPath ++ " " ++  -- Use provided canister_entry.c
            ic0Support ++ "/wasi_stubs.c " ++
            bridgeFile ++
            callFile ++
            extraFiles ++
            includeFlags ++ " " ++
            "-I" ++ miniGmp ++ " " ++
            "-I" ++ refcSrc ++ " " ++
            "-I" ++ ic0Support ++ " " ++
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
parseRefCExportArities : String -> String -> List ExportedFunc -> List (String, RefCArity)
parseRefCExportArities modulePrefix cContent exports =
  let ls = lines cContent
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

  Right cContent <- readFile cFile
    | Left err => pure $ Left $ "Failed to read generated RefC C file: " ++ show err

  let cArities = parseRefCExportArities modulePrefix cContent normalizedExports

  if null normalizedExports
    then do
      -- No exports found, use static canister_entry.c
      pure $ Right (ic0Support ++ "/canister_entry.c")
    else if not (allRealExportsHaveArities modulePrefix normalizedExports cArities)
      then pure $ Left $ "Failed to infer RefC arity for one or more exports"
    else do
      -- Generate dynamic canister_entry.c with Candid-aware stubs
      let entryC = generateCanisterEntryC modulePrefix cArities normalizedExports didMethods typeDefs opts.instrumentBranchProbes
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
