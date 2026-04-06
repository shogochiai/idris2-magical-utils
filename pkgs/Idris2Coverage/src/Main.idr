||| idris2-coverage CLI
||| Code coverage tool for Idris2 using Chez Scheme profiler
module Main

import Coverage.Types
import Coverage.Collector
import Coverage.SourceAnalyzer
import Coverage.TestRunner
import Coverage.Aggregator
import Coverage.Report
import Coverage.DumpcasesParser
import Coverage.PathCoverage
import Coverage.TestCoverage
import Coverage.UnifiedRunner
import Coverage.Config
import Coverage.Exclusions
import Coverage.Standardization.Types

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import System
import System.File
import System.Directory
import System.Clock

%default covering

-- =============================================================================
-- CLI Options
-- =============================================================================

record Options where
  constructor MkOptions
  format       : OutputFormat
  outputPath   : Maybe String
  runTests     : Maybe String    -- glob pattern for tests
  targetPath   : Maybe String    -- directory or ipkg path
  sourceFiles  : List String
  showHelp     : Bool
  showVersion  : Bool
  subcommand   : Maybe String    -- "branches" etc.
  showUncovered : Bool           -- --uncovered flag for branches
  jsonOutput   : Bool            -- --json flag for machine-readable output
  topK         : Nat             -- --top N for high impact targets (default 10)
  reportLeak   : Bool            -- --report-leak flag to contribute
  dumppathsJson : Maybe String   -- explicit --dumppaths-json input path
  pathHitsPath : Maybe String    -- optional runtime path-hit file

defaultOptions : Options
defaultOptions = MkOptions JSON Nothing Nothing (Just ".") [] False False Nothing False False 10 False Nothing Nothing

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("branches" :: rest) opts =
  parseArgs rest ({ subcommand := Just "branches" } opts)
parseArgs ("paths" :: rest) opts =
  parseArgs rest ({ subcommand := Just "paths" } opts)
parseArgs ("--uncovered" :: rest) opts =
  parseArgs rest ({ showUncovered := True } opts)
parseArgs ("--json" :: rest) opts =
  parseArgs rest ({ jsonOutput := True } opts)
parseArgs ("--dumppaths-json" :: path :: rest) opts =
  parseArgs rest ({ dumppathsJson := Just path } opts)
parseArgs ("--path-hits" :: path :: rest) opts =
  parseArgs rest ({ pathHitsPath := Just path } opts)
parseArgs ("--top" :: n :: rest) opts =
  let k : Nat = fromMaybe 10 (parsePositive n)
  in parseArgs rest ({ topK := k } opts)
parseArgs ("--help" :: rest) opts =
  parseArgs rest ({ showHelp := True } opts)
parseArgs ("-h" :: rest) opts =
  parseArgs rest ({ showHelp := True } opts)
parseArgs ("--version" :: rest) opts =
  parseArgs rest ({ showVersion := True } opts)
parseArgs ("-v" :: rest) opts =
  parseArgs rest ({ showVersion := True } opts)
parseArgs ("--format" :: fmt :: rest) opts =
  let format = if fmt == "text" then Text else JSON
  in parseArgs rest ({ format := format } opts)
parseArgs ("-f" :: fmt :: rest) opts =
  let format = if fmt == "text" then Text else JSON
  in parseArgs rest ({ format := format } opts)
parseArgs ("--output" :: path :: rest) opts =
  parseArgs rest ({ outputPath := Just path } opts)
parseArgs ("-o" :: path :: rest) opts =
  parseArgs rest ({ outputPath := Just path } opts)
parseArgs ("--run-tests" :: pattern :: rest) opts =
  parseArgs rest ({ runTests := Just pattern } opts)
parseArgs ("--report-leak" :: rest) opts =
  parseArgs rest ({ reportLeak := True } opts)
parseArgs ("contribute" :: rest) opts =
  parseArgs rest ({ reportLeak := True } opts)
parseArgs (arg :: rest) opts =
  -- Accept directory or ipkg as target
  if isSuffixOf ".idr" arg
     then parseArgs rest ({ sourceFiles $= (arg ::) } opts)
     else parseArgs rest ({ targetPath := Just arg } opts)

-- =============================================================================
-- Help Text
-- =============================================================================

helpText : String
helpText = """
idris2-coverage - Classification-aware semantic coverage for Idris2

USAGE:
  idris2-cov [options] [<dir-or-ipkg>]

  Target defaults to current directory if not specified.

EXAMPLES:
  idris2-cov                           # analyze current directory
  idris2-cov .                         # same as above
  idris2-cov pkgs/LazyCore/            # analyze specific directory
  idris2-cov myproject.ipkg            # analyze specific ipkg
  idris2-cov paths --dumppaths-json out.json
  idris2-cov paths myproject.ipkg      # requires forked IDRIS2_BIN with --dumppaths-json
  idris2-cov --uncovered .             # only show coverage gaps
  idris2-cov --json .                  # JSON output with high_impact_targets
  idris2-cov --json --top 5 .          # JSON with top 5 targets

OPTIONS:
  -h, --help        Show this help message
  -v, --version     Show version
  --uncovered       Only show functions with bugs/unknown CRASHes
  --json            Output JSON with high_impact_targets and reading_guide
  --dumppaths-json  Read path obligations from an existing dumppaths JSON file
  --path-hits       Optional newline or csv file of covered path ids
  --top N           Number of high impact targets to include (default: 10)
  --report-leak     Found stdlib/compiler funcs in targets? Report them!
                    Creates a PR automatically. Your help keeps this fresh.

BRANCH CLASSIFICATION (per dunham):
  canonical:    Reachable branches (test denominator)
  excluded:     NoClauses - void/uninhabited (safe to exclude)
  bugs:         UnhandledInput - genuine coverage gaps (FIX THESE)
  optimizer:    Nat case - non-semantic artifact (ignore)
  unknown:      Other CRASHes - conservative bucket (investigate)
"""

versionText : String
versionText = "idris2-coverage 0.1.0"

-- =============================================================================
-- Path Coverage Command
-- =============================================================================

resolveIdris2Command : IO String
resolveIdris2Command = do
  mcmd <- getEnv "IDRIS2_BIN"
  pure $ fromMaybe "idris2" mcmd

supportsDumppathsJson : String -> IO Bool
supportsDumppathsJson cmd = do
  t <- time
  let probe = "/tmp/idris2-dumppaths-json-probe-" ++ show t ++ ".json"
  exitCode <- system $ cmd ++ " --dumppaths-json " ++ probe ++ " --version >/dev/null 2>&1"
  _ <- system $ "rm -f " ++ probe
  pure $ exitCode == 0

splitPath : String -> (String, String)
splitPath path =
  let parts = forget $ split (== '/') path in
  case reverse parts of
    [] => (".", path)
    [name] => (".", name)
    (name :: revDirs) => (joinBy "/" (reverse revDirs), name)

getIdris2VersionForPaths : IO String
getIdris2VersionForPaths = do
  let tmpFile = "/tmp/idris2-version-paths.txt"
  _ <- system $ "idris2 --version > " ++ tmpFile ++ " 2>&1"
  Right content <- readFile tmpFile
    | Left _ => pure "unknown"
  pure $ trim content

loadPathExclusionsForCLI : IO LoadedExclusions
loadPathExclusionsForCLI = do
  idris2Ver <- getIdris2VersionForPaths
  let tryPaths = ["./exclusions", "../exclusions", "exclusions"]
  findAndLoad tryPaths idris2Ver
  where
    findAndLoad : List String -> String -> IO LoadedExclusions
    findAndLoad [] ver = pure emptyExclusions
    findAndLoad (p :: ps) ver = do
      Right _ <- readFile (p ++ "/base.txt")
        | Left _ => findAndLoad ps ver
      loadExclusions p ver

findIpkgInDirForPaths : String -> IO (Maybe String)
findIpkgInDirForPaths dir = do
  Right entries <- listDir dir
    | Left _ => pure Nothing
  let ipkgs = filter (isSuffixOf ".ipkg") entries
  let nonTemp = filter (not . isPrefixOf "temp-") ipkgs
  case nonTemp of
    (x :: _) => pure $ Just (dir ++ "/" ++ x)
    [] => case ipkgs of
            (x :: _) => pure $ Just (dir ++ "/" ++ x)
            [] => pure Nothing

resolveIpkgForPaths : String -> IO (Either String String)
resolveIpkgForPaths target = do
  let cleanTarget = if isSuffixOf "/" target
                       then pack $ reverse $ drop 1 $ reverse $ unpack target
                       else target
  if isSuffixOf ".ipkg" cleanTarget
     then pure $ Right cleanTarget
     else do
       result <- findIpkgInDirForPaths cleanTarget
       case result of
         Nothing => pure $ Left $ "No .ipkg file found in " ++ cleanTarget
         Just ipkg => pure $ Right ipkg

runDumppathsJson : String -> IO (Either String String)
runDumppathsJson ipkgPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  idris2Cmd <- resolveIdris2Command
  supported <- supportsDumppathsJson idris2Cmd
  if not supported
     then pure $ Left "Current Idris2 does not support --dumppaths-json. Set IDRIS2_BIN to your forked compiler."
     else do
       t <- time
       let outPath = "/tmp/idris2_cov_paths_" ++ show t ++ ".json"
       let logPath = "/tmp/idris2_cov_paths_" ++ show t ++ ".log"
       let cmd = "cd " ++ projectDir ++ " && "
              ++ idris2Cmd ++ " --dumppaths-json " ++ outPath ++ " --build " ++ ipkgName
              ++ " > " ++ logPath ++ " 2>&1"
       _ <- system cmd
       Right content <- readFile outPath
         | Left err => do
             Right logContent <- readFile logPath
               | Left _ => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
             pure $ Left $ "Failed to read dumppaths JSON: " ++ show err ++ "\nBuild log tail:\n" ++ unlines (reverse (take 20 (reverse (lines logContent))))
       _ <- system $ "rm -f " ++ outPath ++ " " ++ logPath
       if null (trim content)
          then pure $ Left "dumppaths JSON was empty"
          else pure $ Right content

parsePathHitLine : String -> Maybe PathRuntimeHit
parsePathHitLine line =
  let trimmed = trim line in
  if null trimmed || isPrefixOf "#" trimmed
     then Nothing
     else case forget (split (== ',') trimmed) of
            [pathId] => Just (MkPathRuntimeHit (trim pathId) 1)
            [pathId, countStr] =>
              let parsed = fromMaybe 1 (parsePositive (trim countStr))
              in Just (MkPathRuntimeHit (trim pathId) parsed)
            _ => Just (MkPathRuntimeHit trimmed 1)

loadPathHits : Maybe String -> IO (Either String (List PathRuntimeHit))
loadPathHits Nothing = pure $ Right []
loadPathHits (Just path) = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read path hits: " ++ show err
  pure $ Right $ mapMaybe parsePathHitLine (lines content)

pathMeasurementSummary : CoverageMeasurement -> String
pathMeasurementSummary m = unlines
  [ "Path-level semantic measurement:"
  , "  denominator_ids: " ++ show (length m.denominatorIds)
  , "  covered_ids:     " ++ show (length m.coveredIds)
  , "  excluded_ids:    " ++ show (length m.excludedIds)
  , "  unknown_ids:     " ++ show (length m.unknownIds)
  ]

escapeJson : String -> String
escapeJson s = fastConcat $ map escapeChar (unpack s)
  where
    escapeChar : Char -> String
    escapeChar '\n' = "\\n"
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = singleton c

pathToJson : PathObligation -> String
pathToJson p = unlines
  [ "    {"
  , "      \"path_id\": \"" ++ escapeJson p.pathId ++ "\","
  , "      \"function_name\": \"" ++ escapeJson p.functionName ++ "\","
  , "      \"classification\": \"" ++ escapeJson (show p.classification) ++ "\","
  , "      \"terminal_kind\": \"" ++ escapeJson p.terminalKind ++ "\","
  , "      \"summary\": \"" ++ escapeJson (pathSummary p) ++ "\""
  , "    }"
  ]

pathsToJsonArray : List PathObligation -> String
pathsToJsonArray [] = "[]"
pathsToJsonArray paths =
  "[\n" ++ fastConcat (intersperse ",\n" (map pathToJson paths)) ++ "\n  ]"

pathCoverageReportToJson : PathCoverageResult -> String
pathCoverageReportToJson result = unlines
  [ "{"
  , "  \"coverage_model\": \"" ++ escapeJson result.coverageModel ++ "\","
  , "  \"claim_admissible\": " ++ boolToJson result.claimAdmissible ++ ","
  , "  \"coverage_percent\": " ++ show (fromMaybe 100.0 result.coveragePercent) ++ ","
  , "  \"measurement\": " ++ indentJson (coverageMeasurementToJson result.measurement) ++ ","
  , "  \"missing_paths\": " ++ pathsToJsonArray result.missingPaths
  , "}"
  ]
  where
    boolToJson : Bool -> String
    boolToJson True = "true"
    boolToJson False = "false"

    indentJson : String -> String
    indentJson s =
      let ls = lines s
      in fastConcat $ intersperse "\n" $ map ("  " ++) ls

parseIpkgModulesForPaths : String -> List String
parseIpkgModulesForPaths content =
  let ls = lines content
      moduleLines = collectModuleLines ls False
      joined = fastConcat $ intersperse " " moduleLines
      afterEq = case break (== '=') (unpack joined) of
                  (_, rest) => pack $ drop 1 rest
      parts = forget $ split (== ',') afterEq
  in map (trim . pack . filter isModuleChar . unpack . trim) parts
  where
    isModuleChar : Char -> Bool
    isModuleChar c = isAlphaNum c || c == '.' || c == '_'

    collectModuleLines : List String -> Bool -> List String
    collectModuleLines [] _ = []
    collectModuleLines (l :: ls) False =
      if isInfixOf "modules" l && isInfixOf "=" l
         then l :: collectModuleLines ls True
         else collectModuleLines ls False
    collectModuleLines (l :: ls) True =
      let trimmed = trim l in
      if null trimmed
         then collectModuleLines ls True
         else if isPrefixOf "," trimmed || isPrefixOf " " l || isPrefixOf "\t" l
                 then l :: collectModuleLines ls True
                 else []

getProjectDirForPaths : String -> String
getProjectDirForPaths ipkg =
  let parts = forget $ split (== '/') ipkg
      allButLast = reverse $ drop 1 $ reverse parts
  in case allButLast of
       [] => "."
       dirs => joinBy "/" dirs

findTestModulesForPaths : String -> IO (List String)
findTestModulesForPaths ipkg = do
  Right content <- readFile ipkg
    | Left _ => discoverTestModules (getProjectDirForPaths ipkg)
  let allModules = parseIpkgModulesForPaths content
  let testMods = filter (isSuffixOf "AllTests") allModules
  case testMods of
    [] => discoverTestModules (getProjectDirForPaths ipkg)
    mods => pure mods

runPaths : Options -> IO ()
runPaths opts = do
  loadedExcl <- loadPathExclusionsForCLI
  pathHitsResult <- loadPathHits opts.pathHitsPath
  case pathHitsResult of
    Left err => putStrLn $ "Error: " ++ err
    Right explicitHits => do
      artifactsResult <- case opts.dumppathsJson of
        Just path => do
          Right content <- readFile path
            | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
          pure $ Right (content, explicitHits)
        Nothing =>
          case opts.targetPath of
            Nothing => pure $ Left "paths command requires either --dumppaths-json or a target .ipkg/directory"
            Just target => do
              ipkgResult <- resolveIpkgForPaths target
              case ipkgResult of
                Left err => pure $ Left err
                Right ipkg =>
                  case opts.pathHitsPath of
                    Just _ => do
                      contentResult <- runDumppathsJson ipkg
                      pure $ map (\content => (content, explicitHits)) contentResult
                    Nothing => do
                      testModules <- findTestModulesForPaths ipkg
                      ipkgContentResult <- readFile ipkg
                      let projectModules = either (const []) parseIpkgModulesForPaths ipkgContentResult
                      case testModules of
                        [] => do
                          contentResult <- runDumppathsJson ipkg
                          pure $ map (\content => (content, explicitHits)) contentResult
                        mods => do
                          artifacts <- runTestsWithPathCoverageArtifacts (getProjectDirForPaths ipkg) projectModules mods 120
                          case artifacts of
                            Right pair => pure $ Right pair
                            Left err => do
                              putStrLn $ "Exact path runner failed; falling back to static analysis: " ++ err
                              contentResult <- runDumppathsJson ipkg
                              pure $ map (\content => (content, explicitHits)) contentResult
      case artifactsResult of
        Left err => putStrLn $ "Error: " ++ err
        Right (content, hits) =>
          case analyzePathCoverageFromContent loadedExcl emptyExclusionConfig content hits of
            Left err => putStrLn $ "Error: " ++ err
            Right result =>
              if opts.jsonOutput
                 then putStrLn $ pathCoverageReportToJson result
                 else do
                   putStrLn "# Path Coverage Report"
                   putStrLn $ "coverage_model:   " ++ result.coverageModel
                   putStrLn $ "claim_admissible: " ++ show result.claimAdmissible
                   putStrLn $ "coverage_percent: " ++ show (fromMaybe 100.0 result.coveragePercent)
                   putStrLn ""
                   putStrLn $ pathMeasurementSummary result.measurement
                   putStrLn $ "Missing paths: " ++ show (length result.missingPaths)
                   traverse_ (\p => putStrLn $ "- " ++ p.pathId ++ " :: " ++ pathSummary p) result.missingPaths

-- =============================================================================
-- Branches Command
-- =============================================================================

||| Check if function has uncovered branches (bugs or unknown)
hasUncoveredBranches : CompiledFunction -> Bool
hasUncoveredBranches f =
  countBugCases f > 0 || countUnknownCases f > 0

||| Find .ipkg file in directory (prefer non-temp files)
findIpkgInDir : String -> IO (Maybe String)
findIpkgInDir dir = do
  Right entries <- listDir dir
    | Left _ => pure Nothing
  let ipkgs = filter (isSuffixOf ".ipkg") entries
  -- Prefer non-temp ipkg files
  let nonTemp = filter (not . isPrefixOf "temp-") ipkgs
  case nonTemp of
    (x :: _) => pure $ Just (dir ++ "/" ++ x)
    [] => case ipkgs of
            (x :: _) => pure $ Just (dir ++ "/" ++ x)
            [] => pure Nothing

||| Resolve target path to ipkg path
resolveIpkg : String -> IO (Either String String)
resolveIpkg target = do
  -- Remove trailing slash if present
  let cleanTarget = if isSuffixOf "/" target
                       then pack $ reverse $ drop 1 $ reverse $ unpack target
                       else target
  if isSuffixOf ".ipkg" cleanTarget
     then pure $ Right cleanTarget
     else do
       -- Assume it's a directory, look for .ipkg
       result <- findIpkgInDir cleanTarget
       case result of
         Nothing => pure $ Left $ "No .ipkg file found in " ++ cleanTarget
         Just ipkg => pure $ Right ipkg

||| Get current timestamp as ISO-ish string
getTimestamp : IO String
getTimestamp = do
  t <- clockTime UTC
  let secs = seconds t
  pure $ "timestamp:" ++ show secs

||| Get Idris2 version string by running `idris2 --version`
getIdris2Version : IO String
getIdris2Version = do
  let tmpFile = "/tmp/idris2-version.txt"
  _ <- system $ "idris2 --version > " ++ tmpFile ++ " 2>&1"
  Right content <- readFile tmpFile
    | Left _ => pure "unknown"
  pure $ trim content

||| Load exclusion patterns from idris2-coverage's exclusions/ directory
||| Falls back to empty exclusions if not found
loadExclusionsForCLI : IO LoadedExclusions
loadExclusionsForCLI = do
  idris2Ver <- getIdris2Version
  -- Try to find exclusions/ directory relative to executable or current dir
  -- For installed packages, this would be in the package data dir
  -- For development, it's in the repo root
  let tryPaths = ["./exclusions", "../exclusions", "exclusions"]
  findAndLoad tryPaths idris2Ver
  where
    findAndLoad : List String -> String -> IO LoadedExclusions
    findAndLoad [] ver = pure emptyExclusions
    findAndLoad (p :: ps) ver = do
      Right _ <- readFile (p ++ "/base.txt")
        | Left _ => findAndLoad ps ver
      loadExclusions p ver

||| Format bug function line for report
formatBugLine : CompiledFunction -> String
formatBugLine f = "- " ++ f.fullName ++ ": UnhandledInput"

||| Format unknown function line for report
formatUnknownLine : CompiledFunction -> String
formatUnknownLine f = "- " ++ f.fullName ++ ": Unknown CRASH"

||| Parse ipkg modules field (handles multi-line format)
||| Format: modules = Foo, Bar
|||                 , Baz.Qux
|||                 , Tests.AllTests
parseIpkgModules : String -> List String
parseIpkgModules content =
  let ls = lines content
      -- Find "modules = ..." and collect continuation lines
      moduleLines = collectModuleLines ls False
      -- Join and split on comma
      joined = fastConcat $ intersperse " " moduleLines
      -- Remove "modules" and "="
      afterEq = case break (== '=') (unpack joined) of
                  (_, rest) => pack $ drop 1 rest
      -- Split on comma and clean up
      parts = forget $ split (== ',') afterEq
  in map (trim . pack . filter isModuleChar . unpack . trim) parts
  where
    isModuleChar : Char -> Bool
    isModuleChar c = isAlphaNum c || c == '.' || c == '_'

    -- Collect lines that are part of modules declaration
    collectModuleLines : List String -> Bool -> List String
    collectModuleLines [] _ = []
    collectModuleLines (l :: ls) False =
      if isInfixOf "modules" l && isInfixOf "=" l
         then l :: collectModuleLines ls True
         else collectModuleLines ls False
    collectModuleLines (l :: ls) True =
      let trimmed = trim l
      in if null trimmed
            then collectModuleLines ls True  -- skip empty
            else if isPrefixOf "," trimmed || isPrefixOf " " l || isPrefixOf "\t" l
                    then l :: collectModuleLines ls True
                    else []  -- hit next field, stop

||| Extract project directory from ipkg path
getProjectDir : String -> String
getProjectDir ipkg =
  let parts = forget $ split (== '/') ipkg
      allButLast = reverse $ drop 1 $ reverse parts
  in case allButLast of
       [] => "."
       dirs => joinBy "/" dirs

||| Find test modules - ipkg first, then filesystem discovery
findTestModules : String -> IO (List String)
findTestModules ipkg = do
  -- Try ipkg-based discovery first
  Right content <- readFile ipkg
    | Left _ => discoverFromFs
  let allModules = parseIpkgModules content
  let testMods = filter (isSuffixOf "AllTests") allModules
  case testMods of
    [] => discoverFromFs  -- Fallback to filesystem
    mods => pure mods
  where
    discoverFromFs : IO (List String)
    discoverFromFs = discoverTestModules (getProjectDir ipkg)

||| Convert CompiledFunction to FunctionTestCoverage for target extraction
||| Uses 0 as executed count for static-only analysis
funcToTestCoverage : CompiledFunction -> FunctionTestCoverage
funcToTestCoverage f = functionToTestCoverage f 0

||| Convert CompiledFunction to FunctionTestCoverage with runtime proportion
||| DEPRECATED: Use runTestsWithFunctionHits for accurate per-function data
||| This is the legacy approximation; kept for fallback when profiler unavailable
funcToTestCoverageWithRuntime : Nat -> Nat -> CompiledFunction -> FunctionTestCoverage
funcToTestCoverageWithRuntime totalExecuted totalCanonical f =
  let funcCanonical = countCanonicalCases f
      -- Proportional estimate: if project has 8% coverage, each function ~8% covered
      proportion : Double
      proportion = if totalCanonical == 0 then 0.0
                   else cast totalExecuted / cast totalCanonical
      estimatedExecuted : Nat
      estimatedExecuted = cast (proportion * cast funcCanonical)
  in functionToTestCoverage f estimatedExecuted

||| Get high impact targets, preferring per-function runtime data when available
getHighImpactTargets : Maybe (List FunctionRuntimeHit)
                     -> Maybe TestCoverage
                     -> LoadedExclusions
                     -> ExclusionConfig
                     -> Nat
                     -> List CompiledFunction
                     -> Nat
                     -> List HighImpactTarget
getHighImpactTargets (Just hits) _ loadedExcl exclusionConfig k funcs _ =
  -- NEW: Use accurate per-function data
  topKTargetsFromRuntimeHitsWithStatic loadedExcl exclusionConfig k hits funcs
getHighImpactTargets Nothing runtimeCov loadedExcl exclusionConfig k funcs totalCanonical =
  -- FALLBACK: Use legacy proportional approximation
  let runtimeExecuted = fromMaybe 0 (map (.executedCanonical) runtimeCov)
      funcsCov = map (funcToTestCoverageWithRuntime runtimeExecuted totalCanonical) funcs
  in topKTargetsWithExclusions loadedExcl exclusionConfig k funcsCov

||| Run coverage analysis using lib API
runBranches : Options -> IO ()
runBranches opts = do
  case opts.targetPath of
    Nothing => putStrLn "Error: No target specified\n\nUsage: idris2-cov <dir-or-ipkg>"
    Just target => do
      ipkgResult <- resolveIpkg target
      case ipkgResult of
        Left err => putStrLn $ "Error: " ++ err
        Right ipkg => do
          -- Get timestamp
          ts <- getTimestamp

          -- Load exclusion config from .idris2-cov.toml (if exists)
          let projectDir = getProjectDir ipkg
          ipkgDepends <- readProjectDepends projectDir
          exclusionConfig <- loadConfigWithDepends projectDir ipkgDepends

          -- Load exclusion patterns from exclusions/ directory
          loadedExcl <- loadExclusionsForCLI

          -- Step 1: Static analysis (always)
          staticResult <- analyzeProjectFunctions ipkg
          case staticResult of
            Left err => putStrLn $ "Error: " ++ err
            Right funcs => do
              let analysis = aggregateAnalysisWithConfig exclusionConfig funcs
              let bugFuncs = filter (\f => countBugCases f > 0) funcs
              let unknownFuncs = filter (\f => countUnknownCases f > 0) funcs

              -- Step 2: Find and run tests (using lib API)
              testModules <- findTestModules ipkg

              -- Step 3: Get per-function runtime coverage (NEW: accurate data)
              -- Using runTestsWithFunctionHits for correct severity calculation
              functionHitsResult <- case testModules of
                [] => pure (Left "No test modules")
                mods => runTestsWithFunctionHits projectDir mods 120

              functionHits <- case functionHitsResult of
                Left _ => pure Nothing
                Right hits => pure (Just hits)

              let measurement = case functionHits of
                                  Nothing => functionCoverageMeasurementFromRuntimeHits funcs []
                                  Just hits => functionCoverageMeasurementFromRuntimeHits funcs hits

              -- Also get aggregate coverage for display (uses separate test run)
              runtimeCov <- case testModules of
                [] => pure Nothing
                mods => do
                  result <- runTestsWithTestCoverage projectDir mods 120
                  case result of
                    Left _ => pure Nothing
                    Right cov => pure $ Just cov

              -- JSON output mode
              if opts.jsonOutput
                 then do
                   -- Use per-function runtime hits if available (accurate severity)
                   -- Otherwise fall back to proportional approximation (legacy)
                   let targets = getHighImpactTargets functionHits runtimeCov loadedExcl exclusionConfig opts.topK funcs analysis.totalCanonical
                   putStrLn $ coverageReportToJsonWithMeasurement analysis measurement targets
                 else do
                   -- Text output mode (original behavior)
                   putStrLn $ "# Coverage Report"
                   putStrLn $ ts
                   putStrLn $ "target: " ++ target
                   putStrLn ""

                   -- Show runtime coverage if available (from test binary's --dumpcases)
                   case runtimeCov of
                     Nothing => putStrLn "## Runtime Coverage: (no tests found/run)"
                     Just cov => do
                       let pct = testCoveragePercent cov
                       putStrLn $ "## Runtime Coverage (test binary)"
                       putStrLn $ "executed:           " ++ show cov.executedCanonical
                                ++ "/" ++ show cov.totalCanonical
                                ++ " (" ++ show (cast {to=Int} pct) ++ "%)"
                       putStrLn $ "note: denominator is test binary's branches, not main binary"
                   putStrLn ""

                   putStrLn "## Branch Classification (main binary - static)"
                   putStrLn $ "canonical:          " ++ show analysis.totalCanonical
                            ++ "   # reachable branches in main binary"
                   putStrLn $ "excluded_void:      " ++ show analysis.totalExcluded
                            ++ "   # NoClauses - safe to exclude"
                   putStrLn $ "bugs:               " ++ show analysis.totalBugs
                            ++ "   # UnhandledInput - genuine gaps (FIX THESE)"
                   putStrLn $ "optimizer_artifacts:" ++ show analysis.totalOptimizerArtifacts
                            ++ "   # Nat case - ignore (non-semantic)"
                   putStrLn $ "unknown:            " ++ show analysis.totalUnknown
                            ++ "   # conservative bucket"
                   putStrLn $ "coverage_model:     " ++ analysis.coverageModel
                            ++ "   # standard vocabulary used for branch classification"
                   putStrLn $ "unknown_policy:     " ++ analysis.unknownPolicy
                            ++ "   # handling rule for unclassified obligations"
                   putStrLn $ "claim_admissible:   " ++ show analysis.claimAdmissible
                            ++ "   # False means strong semantic-coverage claim is blocked"
                   putStrLn ""
                   putStrLn $ formatCoverageMeasurementSummary measurement
                   putStrLn "## Excluded from Denominator:"
                   putStrLn $ "  compiler_generated: " ++ show analysis.exclusionBreakdown.compilerGenerated
                            ++ "   # {csegen:*}, _builtin.*, prim__*"
                   putStrLn $ "  standard_library:   " ++ show analysis.exclusionBreakdown.standardLibrary
                            ++ "   # Prelude.*, System.*, Data.*"
                   putStrLn $ "  type_constructors:  " ++ show analysis.exclusionBreakdown.typeConstructors
                            ++ "   # names ending with '.'"
                   putStrLn $ "  dependencies:       " ++ show analysis.exclusionBreakdown.dependencies
                            ++ "   # user-specified packages"
                   putStrLn ""

                   -- Show bugs (UnhandledInput) - the main test targets
                   case bugFuncs of
                     [] => putStrLn "## Bugs (UnhandledInput): none"
                     _ => do
                       putStrLn $ "## Bugs (UnhandledInput) - Test Targets: " ++ show (length bugFuncs)
                       traverse_ (putStrLn . formatBugLine) bugFuncs
                   putStrLn ""

                   -- Show unknown CRASHes
                   case unknownFuncs of
                     [] => putStrLn "## Unknown CRASHes: none"
                     _ => do
                       putStrLn $ "## Unknown CRASHes (investigate): " ++ show (length unknownFuncs)
                       traverse_ (putStrLn . formatUnknownLine) unknownFuncs

-- =============================================================================
-- Report Leak Command
-- =============================================================================

reportLeakUrl : String
reportLeakUrl = "https://raw.githubusercontent.com/shogochiai/idris2-coverage/main/scripts/report-leak.sh"

||| Run the report-leak flow
runReportLeak : Options -> IO ()
runReportLeak opts = do
  putStrLn "=== idris2-coverage Leak Reporter ==="
  putStrLn ""
  putStrLn "This will help you report exclusion pattern leaks."
  putStrLn "The script will:"
  putStrLn "  1. Fork & clone idris2-coverage (if needed)"
  putStrLn "  2. Detect leaks in your project"
  putStrLn "  3. Create a PR automatically"
  putStrLn ""
  putStrLn "Prerequisites: gh CLI (https://cli.github.com/) and jq"
  putStrLn ""
  let target = fromMaybe "." opts.targetPath
  let topN = show opts.topK
  putStrLn $ "Target project: " ++ target
  putStrLn $ "Top N targets: " ++ topN
  putStrLn ""
  putStrLn "Downloading report-leak.sh..."
  putStrLn ""
  -- Download to temp file first, then execute (so stdin works for read prompts)
  let tmpScript = "/tmp/idris2-cov-report-leak.sh"
  let downloadCmd = "curl -sL " ++ reportLeakUrl ++ " -o " ++ tmpScript ++ " && chmod +x " ++ tmpScript
  _ <- system downloadCmd
  -- Execute the script directly (not via pipe) so stdin works
  let runCmd = tmpScript ++ " \"" ++ target ++ "\" " ++ topN
  _ <- system runCmd
  -- Cleanup
  _ <- system $ "rm -f " ++ tmpScript
  pure ()

-- =============================================================================
-- Main
-- =============================================================================

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs (drop 1 args) defaultOptions  -- drop program name

  if opts.showHelp
     then putStrLn helpText
     else if opts.showVersion
             then putStrLn versionText
             else if opts.reportLeak
                     then runReportLeak opts
                     else case opts.subcommand of
                            Just "paths" => runPaths opts
                            _ => runBranches opts  -- branches is the default command
