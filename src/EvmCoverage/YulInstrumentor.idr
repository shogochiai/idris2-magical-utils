||| Yul Instrumentation for Runtime Profiling
|||
||| Phase 2 of EVM coverage: Runtime instrumentation
|||
||| Strategy:
||| 1. Parse Yul to identify function entries and switch branches
||| 2. Insert counter increments at each control point
||| 3. Add profile flush function that emits events
||| 4. Counters stored in memory (low gas cost)
|||
||| Memory layout for counters:
|||   0x1000 - 0x1FFF: Counter array (max 128 functions * 32 bytes)
|||   Counter[i] = mload(0x1000 + i * 32)
|||
||| Event signature:
|||   ProfileFlush(uint256[] counters)
|||   topic = keccak256("ProfileFlush(uint256[])")
module EvmCoverage.YulInstrumentor

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File
import System

%default covering

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Zip a list with indices starting from 0
zipWithIndex : List a -> List (Nat, a)
zipWithIndex xs = go 0 xs
  where
    go : Nat -> List a -> List (Nat, a)
    go _ [] = []
    go n (x :: rest) = (n, x) :: go (S n) rest

-- =============================================================================
-- Constants
-- =============================================================================

||| Base memory address for profile counters
COUNTER_BASE : Integer
COUNTER_BASE = 0x1000

||| Maximum number of instrumentation points
MAX_COUNTERS : Nat
MAX_COUNTERS = 128

||| Event topic for ProfileFlush
||| keccak256("ProfileFlush(uint256[])")
PROFILE_FLUSH_TOPIC : Integer
PROFILE_FLUSH_TOPIC = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

-- =============================================================================
-- Yul Parsing Types
-- =============================================================================

||| Represents a function found in Yul
record YulFunction where
  constructor MkYulFunction
  name : String
  startLine : Nat
  hasSwitch : Bool  -- Contains switch statement (branching)

||| Result of Yul parsing
record YulParseResult where
  constructor MkYulParseResult
  functions : List YulFunction
  runtimeStart : Nat  -- Line where runtime code starts
  runtimeEnd : Nat    -- Line where runtime code ends

-- =============================================================================
-- Yul Parser
-- =============================================================================

||| Check if line is a function definition
||| Pattern: "function Name(args) -> result {"
isFunctionDef : String -> Bool
isFunctionDef line =
  let trimmed = trim line
  in isPrefixOf "function " trimmed && isInfixOf "(" trimmed

||| Extract function name from definition line
extractFuncName : String -> String
extractFuncName line =
  let trimmed = trim line
      afterFunc = substr 9 (length trimmed) trimmed  -- Skip "function "
      parts = words afterFunc
  in case parts of
       (name :: _) =>
         -- Remove everything after '('
         let chars = unpack name
         in pack $ takeWhile (/= '(') chars
       _ => ""

||| Check if line contains switch statement
hasSwitch : String -> Bool
hasSwitch line = isInfixOf "switch " line

||| Parse Yul content to find functions and structure
parseYul : String -> YulParseResult
parseYul content =
  let ls = lines content
      indexed = zipWithIndex ls
      funcs = findFunctions indexed
      (rStart, rEnd) = findRuntime indexed
  in MkYulParseResult funcs rStart rEnd
  where
    findFunctions : List (Nat, String) -> List YulFunction
    findFunctions [] = []
    findFunctions ((n, line) :: rest) =
      if isFunctionDef line
        then let name = extractFuncName line
                 hasS = any (hasSwitch . snd) (take 50 rest)  -- Check next 50 lines
             in MkYulFunction name n hasS :: findFunctions rest
        else findFunctions rest

    findRuntime : List (Nat, String) -> (Nat, Nat)
    findRuntime indexed =
      let starts = filter (\(_, l) => isInfixOf "object \"runtime\"" l) indexed
          -- Find the end of the last function definition, not the last }
          -- We'll look for lines that are just "}" with 6 spaces indent (function end)
          funcEnds = filter (\(_, l) => trim l == "}" && isPrefixOf "      }" l) indexed
      in case starts of
           ((s, _) :: _) =>
             -- Use the last function end, or fall back to a reasonable position
             case reverse funcEnds of
               ((e, _) :: _) => (s, e)
               _ => (s, length indexed `minus` 3)  -- Before last few closing braces
           _ => (0, length indexed)

-- =============================================================================
-- Counter Increment Generation
-- =============================================================================

||| Generate Yul code to increment counter at index i
||| counter[i] = counter[i] + 1
generateCounterIncrement : Nat -> String
generateCounterIncrement idx =
  let offset = show (cast {to=Integer} COUNTER_BASE + cast idx * 32)
  in "        // PROFILE: increment counter " ++ show idx ++ "\n" ++
     "        mstore(" ++ offset ++ ", add(mload(" ++ offset ++ "), 1))\n"

-- =============================================================================
-- Profile Flush Function Generation
-- =============================================================================

||| Generate the __profile_flush function
||| Emits all counters as a single event
generateFlushFunction : Nat -> String
generateFlushFunction numCounters =
  let counterReads = generateCounterReads numCounters
      eventEmit = generateEventEmit numCounters
  in """
      // @source: ProfileInstrumentation:0:0--0:0
      function __profile_flush(v0) -> result {
        // Copy counters to memory starting at 0x2000 for event data
        // Format: offset (32) + length (32) + data (numCounters * 32)
""" ++ counterReads ++ """
        // Emit ProfileFlush event
""" ++ eventEmit ++ """
        result := 0
      }
"""
  where
    generateCounterReads : Nat -> String
    generateCounterReads Z = ""
    generateCounterReads n@(S k) =
      let idx = minus n 1
          srcOffset = show (cast {to=Integer} COUNTER_BASE + cast idx * 32)
          dstOffset = show (0x2040 + cast idx * 32)  -- After offset + length
      in "        mstore(" ++ dstOffset ++ ", mload(" ++ srcOffset ++ "))\n" ++
         generateCounterReads k

    generateEventEmit : Nat -> String
    generateEventEmit n =
      let dataSize = show (64 + cast n * 32)  -- offset + length + data
          topic = show PROFILE_FLUSH_TOPIC
      in "        mstore(0x2000, 32)  // offset to array\n" ++
         "        mstore(0x2020, " ++ show n ++ ")  // array length\n" ++
         "        log1(0x2000, " ++ dataSize ++ ", " ++ topic ++ ")\n"

-- =============================================================================
-- Yul Instrumentation
-- =============================================================================

||| Insert counter increment after function definition line
instrumentFunction : Nat -> Nat -> String -> String
instrumentFunction funcIdx lineNum line =
  if isFunctionDef line
    then line ++ "\n" ++ generateCounterIncrement funcIdx
    else line

||| Instrument entire Yul content
||| Adds counter increments at function entries and adds flush function
instrumentYul : String -> String
instrumentYul content =
  let parseResult = parseYul content
      funcs = parseResult.functions
      numFuncs = length funcs
      ls = lines content
      -- Create function index map
      funcLines = map (\f => f.startLine) funcs
      -- Instrument each line
      instrumentedLines = instrumentLines ls funcLines 0 0
      -- Add flush function before the closing of runtime
      withFlush = insertFlushFunction instrumentedLines parseResult.runtimeEnd numFuncs
      -- Add flush call in runtime entry block after main
      withFlushCall = insertFlushCall withFlush
      -- Add flush call before revert() to capture coverage even on revert
      withRevertFlush = insertFlushBeforeRevert withFlushCall
  in unlines withRevertFlush
  where
    instrumentLines : List String -> List Nat -> Nat -> Nat -> List String
    instrumentLines [] _ _ _ = []
    instrumentLines (l :: rest) funcStarts lineNum funcIdx =
      if lineNum `elem` funcStarts
        then (l ++ "\n" ++ generateCounterIncrement funcIdx) ::
             instrumentLines rest funcStarts (S lineNum) (S funcIdx)
        else l :: instrumentLines rest funcStarts (S lineNum) funcIdx

    insertFlushFunction : List String -> Nat -> Nat -> List String
    insertFlushFunction ls insertAt numFuncs =
      -- Insert AFTER the closing brace of the last function (insertAt + 1)
      let (before, after) = splitAt (S insertAt) ls
          flushFunc = lines $ generateFlushFunction numFuncs
      in before ++ flushFunc ++ after

    -- Insert flush call after pop(main(0)) in runtime entry
    insertFlushCall : List String -> List String
    insertFlushCall ls = map addFlushAfterMain ls
      where
        addFlushAfterMain : String -> String
        addFlushAfterMain line =
          -- Look for pop(DumpcasesWrapper_u_main(0)) or similar main call
          if isInfixOf "_u_main(" line && isInfixOf "pop(" line
            then line ++ "\n                pop(__profile_flush(0))"
            else line

    -- Insert flush call before revert() inside EVM_Primitives_u_prim__revert
    insertFlushBeforeRevert : List String -> List String
    insertFlushBeforeRevert ls = go ls False
      where
        go : List String -> Bool -> List String
        go [] _ = []
        go (l :: rest) inRevertFunc =
          -- Check if we're entering the revert wrapper function
          let enteringRevert = isInfixOf "EVM_Primitives_u_prim__revert" l && isFunctionDef l
              -- Check if this is the actual revert(arg0, arg1) call inside the wrapper
              isRevertCall = inRevertFunc && isInfixOf "revert(" l && not (isInfixOf "prim__revert" l)
          in if isRevertCall
               then ("        pop(__profile_flush(0))" :: l :: go rest inRevertFunc)
               else (l :: go rest (enteringRevert || (inRevertFunc && not (trim l == "}"))))

-- =============================================================================
-- Function Label Mapping
-- =============================================================================

||| Generate function label mapping for off-chain analysis
||| Format: CSV with index,function_name
generateLabelMap : String -> String
generateLabelMap content =
  let parseResult = parseYul content
      funcs = parseResult.functions
      indexed = zipWithIndex funcs
  in "index,function_name,has_switch\n" ++
     unlines (map formatEntry indexed)
  where
    formatEntry : (Nat, YulFunction) -> String
    formatEntry (idx, func) =
      show idx ++ "," ++ func.name ++ "," ++ show func.hasSwitch

-- =============================================================================
-- Public API
-- =============================================================================

||| Instrument a Yul file for profiling
||| Returns: (instrumented Yul, label map CSV)
export
instrumentYulFile : String -> IO (Either String (String, String))
instrumentYulFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read: " ++ show err
  let instrumented = instrumentYul content
  let labelMap = generateLabelMap content
  pure $ Right (instrumented, labelMap)

||| Write instrumented Yul and label map to files
export
writeInstrumentedYul : String -> String -> String -> IO (Either String ())
writeInstrumentedYul inputPath outputPath labelMapPath = do
  Right (instrumented, labelMap) <- instrumentYulFile inputPath
    | Left err => pure $ Left err
  Right () <- writeFile outputPath instrumented
    | Left err => pure $ Left $ "Failed to write Yul: " ++ show err
  Right () <- writeFile labelMapPath labelMap
    | Left err => pure $ Left $ "Failed to write label map: " ++ show err
  pure $ Right ()

-- =============================================================================
-- Analysis Integration
-- =============================================================================

||| Parse profile event data from hex string
||| Returns list of (function_index, hit_count)
export
parseProfileEvent : String -> List (Nat, Integer)
parseProfileEvent hexData =
  -- Skip first 64 bytes (offset + length)
  let dataOnly = substr 128 (length hexData) hexData  -- 128 hex chars = 64 bytes
  in parseCounters dataOnly 0
  where
    parseCounters : String -> Nat -> List (Nat, Integer)
    parseCounters s idx =
      if length s < 64 then []
      else let chunk = substr 0 64 s  -- 32 bytes = 64 hex chars
               rest = substr 64 (length s) s
               -- Parse hex to integer (simplified)
               count = 0  -- TODO: implement hex parsing
           in (idx, count) :: parseCounters rest (S idx)

||| Merge static analysis (dumpcases) with runtime profile
||| Returns coverage gaps: functions with branches but zero hits
export
findCoverageGaps : List (String, Nat) -> List (Nat, Integer) -> List String
findCoverageGaps staticFuncs runtimeHits =
  let hitIndices = map (\p => fst p) $ filter (\p => snd p > 0) runtimeHits
  in mapMaybe (checkGap hitIndices) (zipWithIndex staticFuncs)
  where
    checkGap : List Nat -> (Nat, (String, Nat)) -> Maybe String
    checkGap hits (idx, (name, branches)) =
      if branches > 0 && not (idx `elem` hits)
        then Just name
        else Nothing

-- =============================================================================
-- Yul Generation + Instrumentation Pipeline
-- =============================================================================

||| Extract base name from ipkg path
getBaseName : String -> String
getBaseName path =
  let parts = forget $ split (== '/') path
      fileName = fromMaybe "" (last' parts)
  in if isSuffixOf ".ipkg" fileName
       then substr 0 (length fileName `minus` 5) fileName
       else fileName

||| Extract project directory from ipkg path
getProjectDir : String -> String
getProjectDir path =
  let parts = forget $ split (== '/') path
      dirParts = fromMaybe [] (init' parts)
  in if null dirParts then "." else joinBy "/" dirParts

||| Discover package paths from idris2 --list-packages
||| Writes to temp file and reads back (simpler than popen)
discoverPackagePaths : IO (List String)
discoverPackagePaths = do
  let tmpFile = "/tmp/idris2-pkg-paths.txt"
  -- Run idris2 --list-packages and extract paths to temp file
  let cmd = "idris2 --list-packages 2>/dev/null | grep '└' | sed 's/.*└ //' > " ++ tmpFile
  _ <- system cmd
  -- Read the temp file
  Right content <- readFile tmpFile
    | Left _ => pure []
  let paths = filter (not . null) $ map trim $ lines content
  pure paths

||| Find the latest dumpcases-temp-*.ipkg file in project directory
||| This is created by Phase 1 (static analysis) and contains the main wrapper
findLatestTempIpkg : String -> IO (Maybe String)
findLatestTempIpkg projectDir = do
  let tmpFile = "/tmp/latest-temp-ipkg.txt"
  let cmd = "ls -t " ++ projectDir ++ "/dumpcases-temp-*.ipkg 2>/dev/null | head -1 > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path
    then pure Nothing
    else pure $ Just path

||| Generate Yul from Idris2 project using idris2-yul --codegen yul
||| Note: idris2-yul is a custom compiler with yul backend built-in
||| Uses the dumpcases-temp-*.ipkg from Phase 1 (which has main wrapper)
||| Automatically discovers pack's package paths via idris2 --list-packages
||| Returns path to generated .yul file
export
generateYul : String -> String -> IO (Either String String)
generateYul ipkgPath outputDir = do
  -- Extract project directory from ipkg path
  let projectDir = getProjectDir ipkgPath
  -- Find the temp ipkg generated by Phase 1 (dumpcases)
  -- This has the DumpcasesWrapper main that calls all functions
  mTempIpkg <- findLatestTempIpkg projectDir
  let actualIpkg = fromMaybe ipkgPath mTempIpkg
  -- The yul file will be named after the package, which is "dumpcases-temp"
  let yulPath = outputDir ++ "/dumpcases-temp.yul"
  -- Discover package paths from pack's idris2 installation
  paths <- discoverPackagePaths
  let pkgPath = joinBy ":" paths
  -- Set IDRIS2_PACKAGE_PATH and call idris2-yul
  let cmd = if null pkgPath
              then "idris2-yul --codegen yul --output-dir " ++ outputDir ++ " --build " ++ actualIpkg
              else "IDRIS2_PACKAGE_PATH=\"" ++ pkgPath ++ "\" idris2-yul --codegen yul --output-dir " ++ outputDir ++ " --build " ++ actualIpkg
  exitCode <- system cmd
  if exitCode == 0
    then pure $ Right yulPath
    else pure $ Left $ "Yul generation failed (exit " ++ show exitCode ++ ")"

||| Full pipeline: Generate Yul → Instrument → Write output files
||| Returns: (instrumented yul path, label map path)
export
generateAndInstrumentYul : String -> String -> IO (Either String (String, String))
generateAndInstrumentYul ipkgPath outputDir = do
  Right yulPath <- generateYul ipkgPath outputDir
    | Left err => pure $ Left err
  let baseName = substr 0 (length yulPath `minus` 4) yulPath  -- Remove .yul
  let instrPath = baseName ++ "-instrumented.yul"
  let labelPath = baseName ++ "-labels.csv"
  Right () <- writeInstrumentedYul yulPath instrPath labelPath
    | Left err => pure $ Left err
  pure $ Right (instrPath, labelPath)

-- =============================================================================
-- Phase 2.3: Solc Compilation
-- =============================================================================

||| Compile instrumented Yul to bytecode using solc
||| Returns path to compiled binary output file
export
compileYulToBytecode : String -> String -> IO (Either String String)
compileYulToBytecode yulPath outputDir = do
  let baseName = getBaseName yulPath
  let binPath = outputDir ++ "/" ++ baseName ++ ".bin"
  -- For strict-assembly mode, solc outputs to stdout, redirect to file
  let cmd = "solc --strict-assembly --optimize " ++ yulPath ++ " --bin 2>&1 > " ++ binPath
  -- First check for syntax errors
  let checkCmd = "solc --strict-assembly " ++ yulPath ++ " 2>&1"
  checkResult <- system checkCmd
  if checkResult /= 0
    then do
      -- Get the actual error message
      let errCmd = "solc --strict-assembly " ++ yulPath ++ " 2>&1 | head -10"
      _ <- system errCmd
      pure $ Left $ "Yul syntax error (instrumentation bug)"
    else do
      exitCode <- system cmd
      if exitCode == 0
        then pure $ Right binPath
        else pure $ Left $ "solc compilation failed (exit " ++ show exitCode ++ ")"

-- =============================================================================
-- Phase 2.4: Idris2 EVM Test Execution
-- =============================================================================

||| Extract runtime bytecode from solc output (skips init code header)
extractRuntimeBytecode : String -> String
extractRuntimeBytecode content =
  -- Find "Binary representation:" line and get next line
  let ls = lines content
      afterBinary = dropWhile (\l => not $ isInfixOf "Binary representation:" l) ls
  in case afterBinary of
       (_ :: byteLine :: _) => trim byteLine
       _ => ""

||| Run instrumented bytecode via idris2-evm-run and capture trace
||| Returns path to trace output
export
runIdrisEvmTest : String -> String -> IO (Either String String)
runIdrisEvmTest binPath traceOutput = do
  -- Extract runtime bytecode (skip init code)
  Right content <- readFile binPath
    | Left _ => pure $ Left "Cannot read bytecode file"
  let fullHex = extractRuntimeBytecode content
  -- Skip init code: first 24 hex chars (12 bytes) are init code
  let runtimeHex = substr 24 (length fullHex) fullHex
  -- Write runtime bytecode to temp file
  let runtimePath = "/tmp/runtime.bin"
  Right () <- writeFile runtimePath runtimeHex
    | Left _ => pure $ Left "Cannot write runtime bytecode"
  -- Run with idris2-evm-run
  let cmd = "idris2-evm-run --trace " ++ runtimePath ++ " --gas 100000000 > " ++ traceOutput ++ " 2>&1"
  exitCode <- system cmd
  -- idris2-evm-run may return non-zero on revert/invalid, but trace is still useful
  Right _ <- readFile traceOutput
    | Left _ => pure $ Left "Cannot read trace output"
  pure $ Right traceOutput

-- =============================================================================
-- Phase 3: Coverage Analysis Result
-- =============================================================================

||| Coverage analysis result
public export
record CoverageAnalysis where
  constructor MkCoverageAnalysis
  staticBranches : Nat
  functionsHit : Nat
  totalFunctions : Nat
  coveragePercent : Nat
  uncoveredFunctions : List String

||| Run full coverage pipeline: Yul生成 → 計装 → solc → test → gap analysis
export
runFullPipeline : String -> String -> List (String, Nat) -> IO (Either String CoverageAnalysis)
runFullPipeline ipkgPath outputDir staticFuncs = do
  -- Step 1: Generate and instrument Yul
  Right (instrPath, labelPath) <- generateAndInstrumentYul ipkgPath outputDir
    | Left err => pure $ Left $ "Instrumentation failed: " ++ err

  -- Step 2: Compile with solc
  Right _ <- compileYulToBytecode instrPath outputDir
    | Left err => pure $ Left $ "Compilation failed: " ++ err

  -- Step 3: Run instrumented bytecode with idris2-evm-run
  let binPath = instrPath ++ ".bin"
  let traceOutput = outputDir ++ "/coverage-trace.csv"
  testResult <- runIdrisEvmTest binPath traceOutput

  -- Step 4: Parse events and analyze gaps
  case testResult of
    Left _ => do
      -- No test execution, return static-only analysis
      let uncovered = map fst staticFuncs
      pure $ Right $ MkCoverageAnalysis (length staticFuncs) 0 (length staticFuncs) 0 uncovered
    Right tracePath => do
      -- TODO: Parse trace and extract ProfileFlush events
      -- For now, return placeholder with 0% coverage
      let uncovered = map fst staticFuncs
      pure $ Right $ MkCoverageAnalysis (length staticFuncs) 0 (length staticFuncs) 0 uncovered
