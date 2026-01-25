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
import Coverage.TestCoverage
import Coverage.UnifiedRunner
import Coverage.Config
import Coverage.Exclusions

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

defaultOptions : Options
defaultOptions = MkOptions JSON Nothing Nothing (Just ".") [] False False Nothing False False 10 False

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("branches" :: rest) opts =
  parseArgs rest ({ subcommand := Just "branches" } opts)
parseArgs ("--uncovered" :: rest) opts =
  parseArgs rest ({ showUncovered := True } opts)
parseArgs ("--json" :: rest) opts =
  parseArgs rest ({ jsonOutput := True } opts)
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
  idris2-cov --uncovered .             # only show coverage gaps
  idris2-cov --json .                  # JSON output with high_impact_targets
  idris2-cov --json --top 5 .          # JSON with top 5 targets

OPTIONS:
  -h, --help        Show this help message
  -v, --version     Show version
  --uncovered       Only show functions with bugs/unknown CRASHes
  --json            Output JSON with high_impact_targets and reading_guide
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
                   putStrLn $ coverageReportToJson analysis targets
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
                     else runBranches opts  -- branches is the default command
