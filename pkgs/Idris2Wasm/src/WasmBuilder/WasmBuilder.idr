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

||| Generate test Main.idr content that imports the specified test module
||| This is written to /tmp, never touches the original Main.idr
generateTestMainContent : String -> String
generateTestMainContent testModuleName = unlines
  [ "||| Auto-generated test entry point for coverage analysis"
  , "module Main"
  , ""
  , "import " ++ testModuleName  -- Import test module (not public to avoid name clashes)
  , ""
  , "%default covering"
  , ""
  , "-- FFI bridge for code retention"
  , "%foreign \"C:ic_ffi_get_arg,libic0\""
  , "prim__ic_ffi_get_arg : Int -> PrimIO Int"
  , ""
  , "||| Run all tests - exported for Candid call"
  , "||| Wraps " ++ testModuleName ++ ".runAllTests with IO for canister compatibility"
  , "export"
  , "runTests : IO (Int, Int)"
  , "runTests = do"
  , "  let (passed, failed) = " ++ testModuleName ++ ".runAllTests"
  , "  pure (cast passed, cast failed)"
  , ""
  , "||| Force code retention to prevent DCE"
  , "forceRetain : IO ()"
  , "forceRetain = do"
  , "  v <- primIO $ prim__ic_ffi_get_arg 7"
  , "  when (v == (-999999)) $ do"
  , "    _ <- Main.runTests"  -- Explicit to avoid clash with Tests.AllTests.runTests
  , "    pure ()"
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
generateFuncEntry : String -> ExportedFunc -> List DidMethod -> List TypeDef -> String
generateFuncEntry modulePrefix ef didMethods typeDefs =
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
       , "    ensure_idris2_init();"
       , funcCallCode
       , "    " ++ replyCode
       , "}"
       ]
  where
    -- Generate code to actually call the Idris function (for profiling)
    generateFuncCall : String -> String -> String
    generateFuncCall funcName retType =
      -- Call the Idris function via trampoline to trigger internal function traces
      unlines
        [ "    // Actually call Idris function for profiling"
        , "    void* _world = idris2_newReference((void*)0);"
        , "    void* _result = idris2_trampoline(" ++ funcName ++ "(_world));"
        , "    (void)_result; // TODO: Extract actual return value"
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
            then "reply_text(\"42,0\"); // DEBUG: Simple string reply"
            else if isInfixOf "String" func.returnType
              then "reply_text(\"ok\"); // String result"
              else "reply_text(\"done\"); // Generic result (no .did match)"

||| Generate complete canister_entry.c with all exported functions
||| @modulePrefix C function prefix (e.g., "Main" or "Tests_AllTests")
||| @exports List of exported functions from Idris
||| @didMethods Parsed .did methods for Candid-aware reply generation
||| @typeDefs Parsed .did type definitions for dynamic Candid encoding
generateCanisterEntryC : String -> List ExportedFunc -> List DidMethod -> List TypeDef -> String
generateCanisterEntryC modulePrefix exports didMethods typeDefs =
  let funcEntries = fastConcat $ map (\ef => generateFuncEntry modulePrefix ef didMethods typeDefs) exports
      header = canisterEntryHeader
      -- Generate extern declarations only for actual Idris functions (not .did stubs)
      idrisExports = filter (\ef => not ef.fromDid) exports
      funcExterns = unlines $ map (\ef => "extern void* " ++ modulePrefix ++ "_" ++ ef.name ++ "(void*);") idrisExports
  in header ++ "\n/* Idris Function Externs */\n" ++ funcExterns ++ "\n" ++ funcEntries
  where
    canisterEntryHeader : String
    canisterEntryHeader = unlines
      [ "/*"
      , " * Auto-generated Canister Entry Points"
      , " * Generated by idris2-wasm from Main.idr exports"
      , " */"
      , "#include <stdint.h>"
      , "#include <string.h>"
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
      , "extern void* idris2_newReference(void*);"
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
      , "    if (idris2_vp_is_unboxed(v)) {"
      , "        return idris2_vp_to_Int32(v);"
      , "    }"
      , "    // Boxed Int32 - skip 4-byte header"
      , "    return *((int32_t*)((char*)v + 4));"
      , "}"
      , ""
      , "static void extract_int_pair(void* v, int32_t* a, int32_t* b) {"
      , "    Value_Constructor* con = (Value_Constructor*)v;"
      , "    *a = extract_int(con->args[0]);"
      , "    *b = extract_int(con->args[1]);"
      , "}"
      , ""
      , "/* Reply with Candid record { passed : int; failed : int } */"
      , "static void reply_int_pair(int32_t passed, int32_t failed) {"
      , "    uint8_t buf[64];"
      , "    int pos = 0;"
      , "    // DIDL magic"
      , "    buf[pos++] = 'D'; buf[pos++] = 'I'; buf[pos++] = 'D'; buf[pos++] = 'L';"
      , "    // Type table: 1 type (record)"
      , "    buf[pos++] = 0x01;"
      , "    buf[pos++] = 0x6c; // record type"
      , "    buf[pos++] = 0x02; // 2 fields"
      , "    // Field 'failed' hash = 0xa2a7d6c4 (must be sorted by hash)"
      , "    buf[pos++] = 0xc4; buf[pos++] = 0xd6; buf[pos++] = 0xa7; buf[pos++] = 0xa2;"
      , "    buf[pos++] = 0x75; // int type"
      , "    // Field 'passed' hash = 0xc7c6e1c6"
      , "    buf[pos++] = 0xc6; buf[pos++] = 0xe1; buf[pos++] = 0xc6; buf[pos++] = 0xc7;"
      , "    buf[pos++] = 0x75; // int type"
      , "    // Args: 1 arg of type 0"
      , "    buf[pos++] = 0x01; buf[pos++] = 0x00;"
      , "    // Values in field hash order: failed, passed"
      , "    int32_t v = failed;"
      , "    do { buf[pos++] = (v & 0x7f) | ((v >> 7) ? 0x80 : 0); v >>= 7; } while (v > 0);"
      , "    if (pos == 21) buf[pos++] = 0; // ensure at least 1 byte for 0"
      , "    v = passed;"
      , "    do { buf[pos++] = (v & 0x7f) | ((v >> 7) ? 0x80 : 0); v >>= 7; } while (v > 0);"
      , "    if (failed == 0 && passed == 0) { buf[pos-1] = 0; } // fix zero case"
      , "    ic0_msg_reply_data_append((int32_t)buf, pos);"
      , "    ic0_msg_reply();"
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
  Right _ <- readFile testModulePath
    | Left _ => pure (Left $ "Test module not found: " ++ testModuleRelPath ++ "\nCreate this file with: export runAllTests : (Int, Int)")

  -- Create temp directory structure
  let tempDir = "/tmp/idris2-wasm-test-" ++ show !time
  let tempSrcDir = tempDir ++ "/src"
  _ <- system $ "mkdir -p " ++ tempSrcDir

  -- Create symlinks to all src/ items EXCEPT Main.idr
  -- This allows `import Tests.AllTests` to work while using our generated Main
  -- Uses absolute paths so symlinks work from any location
  (_, files, _) <- executeCommand $ "ls " ++ absProjectDir ++ "/src/"
  let srcItems = filter (\s => not (null s) && s /= "Main.idr") (lines files)
  _ <- traverse_ (\item => system $ "ln -sf " ++ absProjectDir ++ "/src/" ++ item ++ " " ++ tempSrcDir ++ "/" ++ item) srcItems

  -- Write generated Main.idr to temp (not a symlink, actual generated file)
  let tempMainPath = tempSrcDir ++ "/Main.idr"
  let testModuleName = pathToModuleName testModuleRelPath
  Right () <- writeFile tempMainPath (generateTestMainContent testModuleName)
    | Left err => pure (Left $ "Failed to write temp Main: " ++ show err)

  -- Generate temp ipkg pointing to temp src directory
  Right tempIpkg <- generateTestIpkg originalIpkg absProjectDir tempSrcDir
    | Left err => pure (Left err)

  pure (Right tempIpkg)

||| Find ipkg file in project directory
findIpkg : String -> IO (Maybe String)
findIpkg projectDir = do
  (_, result, _) <- executeCommand $ "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' | head -1"
  pure $ if null (trim result) then Nothing else Just (trim result)

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
      Right tempIpkg <- setupTestBuild opts.projectDir ipkgFile opts.testModulePath
        | Left err => pure (Left err)
      compileWithIpkg opts tempIpkg
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
      -- For test builds, ipkg is in temp dir (e.g., /tmp/idris2-wasm-test-xxx/test_build.ipkg)
      -- Build output goes to that directory's build/ folder
      let ipkgDir = dirname ipkg
      let buildSearchDir = if ipkgDir /= opts'.projectDir && not (null ipkgDir)
            then ipkgDir ++ "/build"
            else opts'.projectDir ++ "/build"

      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                "idris2 --codegen refc --build " ++ ipkg

      -- RefC generates C file then tries native compile (which fails without GMP)
      -- We ignore the exit code and just check if C file was generated
      _ <- executeCommand cmd

      -- Find generated C file in build directory
      let findCmd = "sh -c 'find " ++ buildSearchDir ++ " -name \"*.c\" 2>/dev/null | head -1'"
      (_, cFile, _) <- executeCommand findCmd
      if null (trim cFile)
        then pure $ Left "No C file generated by RefC"
        else do
          putStrLn $ "        Generated: " ++ trim cFile
          pure $ Right (trim cFile)

    compileDirectly : BuildOptions -> String -> IO (Either String String)
    compileDirectly opts' buildDir' = do
      let pkgFlags = unwords $ map (\p => "-p " ++ p) opts'.packages
      let cmd = "cd " ++ opts'.projectDir ++ " && " ++
                "mkdir -p " ++ buildDir' ++ " && " ++
                "idris2 --codegen refc " ++
                "--build-dir " ++ buildDir' ++ " " ++
                pkgFlags ++ " " ++
                "--source-dir src " ++
                "-o main " ++
                opts'.mainModule
      _ <- executeCommand cmd
      let findCmd = "sh -c 'find " ++ buildDir' ++ " -name \"*.c\" 2>/dev/null | head -1'"
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
compileToWasmWithEntry : String -> String -> String -> String -> String -> String -> IO (Either String ())
compileToWasmWithEntry cFile refcSrc miniGmp ic0Support canisterEntryPath outputWasm = do
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

  let cmd = "CPATH= CPLUS_INCLUDE_PATH= emcc " ++ cFile ++ " " ++
            refcCFiles ++ " " ++
            miniGmp ++ "/mini-gmp.c " ++
            ic0Support ++ "/ic0_stubs.c " ++
            canisterEntryPath ++ " " ++  -- Use provided canister_entry.c
            ic0Support ++ "/wasi_stubs.c " ++
            bridgeFile ++
            callFile ++
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
          -- Use stub_wasi.py script from idris2-wasm/support/tools
          let scriptFile = "/Users/bob/code/idris2-wasm/support/tools/stub_wasi.py"

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

||| Generate canister_entry.c from Main.idr exports
||| Writes to temp file and returns path
generateCanisterEntry : BuildOptions -> String -> IO (Either String String)
generateCanisterEntry opts ic0Support = do
  -- Determine Main.idr content
  mainContent <- if opts.forTestBuild
    then do
      -- In test mode: ONLY generate runTests export
      -- DO NOT read original Main.idr - those functions won't be in test WASM
      -- .did methods will be added later via didMethodToExport (with fromDid = True)
      let testModulePath' = fromMaybe "src/Tests/AllTests.idr" opts.testModulePath
      let testModuleName = pathToModuleName testModulePath'
      pure (generateTestMainContent testModuleName)
    else do
      let mainPath = opts.projectDir ++ "/src/Main.idr"
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

  -- Determine module prefix: "Main" for both prod and test builds
  -- (test builds generate a temp Main.idr that re-exports from Tests.AllTests)
  let modulePrefix = "Main"

  if null effectiveExports
    then do
      -- No exports found, use static canister_entry.c
      pure $ Right (ic0Support ++ "/canister_entry.c")
    else do
      -- Generate dynamic canister_entry.c with Candid-aware stubs
      let entryC = generateCanisterEntryC modulePrefix effectiveExports didMethods typeDefs
      let tempEntryPath = "/tmp/canister_entry_generated.c"
      Right () <- writeFile tempEntryPath entryC
        | Left err => pure $ Left $ "Failed to write canister_entry.c: " ++ show err
      when opts.forTestBuild $
        putStrLn $ "        Test build: merged " ++ show (length exports) ++ " Idris exports + " ++ show (length didMethods) ++ " .did methods"
      putStrLn $ "        Generated canister_entry.c with " ++ show (length effectiveExports) ++ " entry points"
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

  -- Step 1: Idris2 → C
  Right cFile <- compileToRefC opts buildDir
    | Left err => pure $ BuildError err

  -- Step 2: Prepare runtime
  Right (refcSrc, miniGmp) <- prepareRefCRuntime
    | Left err => pure $ BuildError err

  -- Step 2.5: Generate canister_entry.c from Main.idr exports
  Right canisterEntryPath <- generateCanisterEntry opts ic0Support
    | Left err => pure $ BuildError err

  -- Step 3: C → WASM (use generated canister_entry.c)
  Right () <- compileToWasmWithEntry cFile refcSrc miniGmp ic0Support canisterEntryPath rawWasm
    | Left err => pure $ BuildError err

  -- Step 4: Stub WASI
  Right () <- stubWasi rawWasm stubbedWasm
    | Left err => pure $ BuildError err

  -- Step 5: Generate Source Maps (if enabled)
  when opts.generateSourceMap $ do
    putStrLn "      Step 5: Generating Source Maps"
    Right cContent <- readFile cFile
      | Left _ => putStrLn "        Warning: Could not read C file for source map"
    let idrisCMap = generateIdrisCSourceMapWithFunctions cFile cContent
    let idrisCMapPath = wasmDir ++ "/idris2-c.map"
    Right () <- writeSourceMap idrisCMapPath idrisCMap
      | Left _ => putStrLn "        Warning: Could not write idris2-c.map"
    putStrLn $ "        Generated: " ++ idrisCMapPath
    putStrLn $ "        Sources: " ++ show (length idrisCMap.sources) ++ " Idris files"
    putStrLn $ "        Functions: " ++ show (length idrisCMap.names) ++ " Idris functions"

  putStrLn $ "    Build complete: " ++ stubbedWasm
  pure $ BuildSuccess stubbedWasm

||| Build canister using project's lib/ic0 for support files
|||
||| @opts Build options
||| Returns path to final stubbed WASM on success
public export
buildCanisterAuto : BuildOptions -> IO BuildResult
buildCanisterAuto opts = do
  -- Try to find IC0 support in project or sibling
  let projectIc0 = opts.projectDir ++ "/lib/ic0"
  let siblingIc0 = opts.projectDir ++ "/../idris2-wasm/support/ic0"

  Right _ <- readFile (projectIc0 ++ "/canister_entry.c")
    | Left _ => do
        Right _ <- readFile (siblingIc0 ++ "/canister_entry.c")
          | Left _ => pure $ BuildError "IC0 support files not found in lib/ic0 or ../idris2-wasm/support/ic0"
        buildCanister opts siblingIc0

  buildCanister opts projectIc0
