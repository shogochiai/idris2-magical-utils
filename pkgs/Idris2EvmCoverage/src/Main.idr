||| idris2-evm-cov CLI
||| EVM semantic coverage analysis using Chez Scheme profiler
module Main

import EvmCoverage.Types
import EvmCoverage.DumpcasesParser
import EvmCoverage.ProfileParser
import EvmCoverage.SchemeMapper
import EvmCoverage.Aggregator
import EvmCoverage.Report
import EvmCoverage.EvmCoverage
import EvmCoverage.Runtime as Runtime
import EvmCoverage.ProfileParserLinear
import EvmCoverage.ProfileParserFSM
import EvmCoverage.YulInstrumentor as YulInstr

import Data.List
import Data.Maybe
import Data.String
import System
import System.Clock
import System.File
import System.Directory

%default covering

-- =============================================================================
-- CLI Options
-- =============================================================================

record Options where
  constructor MkOptions
  targetPath   : Maybe String
  ipkgPath     : Maybe String
  outputDir    : Maybe String
  format       : OutputFormat
  threshold    : Double
  showHelp     : Bool
  showVersion  : Bool
  verbose      : Bool
  dumpcasesOnly : Bool
  evmInterpreter : Bool  -- New: analyze idris2-evm interpreter coverage
  benchMode    : Bool    -- Benchmark extractSpans (V0)
  benchV1Mode  : Bool    -- Benchmark extractSpansLinear (V1)
  benchV2Mode  : Bool    -- Benchmark extractHitsFSM (V2)
  runtimeTrace : Maybe String  -- Path to trace.csv for runtime analysis
  runtimeLabels : Maybe String -- Path to labels.csv for runtime analysis
  fullPipeline : Bool

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing Nothing Text 80.0 False False False False False False False False Nothing Nothing False

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("--help" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("--version" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("-v" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("--verbose" :: rest) opts = parseArgs rest ({ verbose := True } opts)
parseArgs ("--json" :: rest) opts = parseArgs rest ({ format := JSON } opts)
parseArgs ("--text" :: rest) opts = parseArgs rest ({ format := Text } opts)
parseArgs ("--markdown" :: rest) opts = parseArgs rest ({ format := Markdown } opts)
parseArgs ("--oneline" :: rest) opts = parseArgs rest ({ format := OneLine } opts)
parseArgs ("--dumpcases" :: rest) opts = parseArgs rest ({ dumpcasesOnly := True } opts)
parseArgs ("--evm-interpreter" :: rest) opts = parseArgs rest ({ evmInterpreter := True } opts)
parseArgs ("--bench" :: rest) opts = parseArgs rest ({ benchMode := True } opts)
parseArgs ("--bench-v1" :: rest) opts = parseArgs rest ({ benchV1Mode := True } opts)
parseArgs ("--bench-v2" :: rest) opts = parseArgs rest ({ benchV2Mode := True } opts)
parseArgs ("--runtime-trace" :: t :: rest) opts = parseArgs rest ({ runtimeTrace := Just t } opts)
parseArgs ("--runtime-labels" :: l :: rest) opts = parseArgs rest ({ runtimeLabels := Just l } opts)
parseArgs ("--full-pipeline" :: rest) opts = parseArgs rest ({ fullPipeline := True } opts)
parseArgs ("--threshold" :: t :: rest) opts =
  let th = fromMaybe 80.0 (parseDouble t)
  in parseArgs rest ({ threshold := th } opts)
parseArgs ("--output" :: o :: rest) opts =
  parseArgs rest ({ outputDir := Just o } opts)
parseArgs ("-o" :: o :: rest) opts =
  parseArgs rest ({ outputDir := Just o } opts)
parseArgs ("--ipkg" :: p :: rest) opts =
  parseArgs rest ({ ipkgPath := Just p } opts)
parseArgs (arg :: rest) opts =
  if isPrefixOf "-" arg
    then parseArgs rest opts  -- Skip unknown flags
    else parseArgs rest ({ targetPath := Just arg } opts)

-- =============================================================================
-- Help Text
-- =============================================================================

helpText : String
helpText = """
idris2-evm-cov - EVM Semantic Coverage Analysis

USAGE:
  idris2-evm-cov [options] <project-dir>
  idris2-evm-cov --ipkg <package.ipkg> [options]

DESCRIPTION:
  Analyzes EVM code coverage using Idris2's --dumpcases for static branch
  analysis (denominator) and Chez Scheme profiler for runtime hits (numerator).

OPTIONS:
  -h, --help          Show this help message
  -v, --version       Show version
  --verbose           Verbose output
  --json              Output in JSON format
  --text              Output in text format (default)
  --markdown          Output in Markdown format
  --oneline           One-line summary output
  --dumpcases         Static analysis only (no profiler)
  --threshold <N>     Coverage threshold percentage (default: 80)
  -o, --output <dir>  Output directory for reports
  --ipkg <path>       Path to .ipkg file
  --runtime-trace <file>   Path to trace.csv for runtime analysis
  --runtime-labels <file>  Path to labels.csv for runtime analysis
  --full-pipeline          Run static dumpcases + runtime Yul instrumentation pipeline

EXAMPLES:
  # Analyze a project
  idris2-evm-cov ~/code/idris2-evm

  # JSON output for CI
  idris2-evm-cov --json ~/code/idris2-evm

  # Static analysis only
  idris2-evm-cov --dumpcases ~/code/idris2-evm

  # Full runtime branch pipeline
  idris2-evm-cov --full-pipeline --ipkg pkg.ipkg ~/code/project

COVERAGE MODEL:
  Denominator: Canonical branches from --dumpcases (excludes void/uninhabited)
  Numerator:   Branches hit during test execution (Chez profiler)
  Standard:    semantic test obligation coverage (branch-level profile)
  Unknowns:    block strong claim unless classified

  Branch Classifications:
    - Canonical:        Reachable branches (included in coverage %)
    - ExcludedNoClauses: Void/uninhabited patterns (excluded)
    - BugUnhandledInput: Partial code bugs (flagged)
    - OptimizerNat:     Nat→Integer artifacts (non-semantic)
    - Unknown:          Investigate manually
"""

versionText : String
versionText = "idris2-evm-cov 0.1.0 - EVM Semantic Coverage Analysis"

-- =============================================================================
-- Benchmark Mode
-- =============================================================================

getTimeNs : IO Integer
getTimeNs = do
  t <- clockTime Monotonic
  pure $ seconds t * 1000000000 + nanoseconds t

runBench : String -> IO ()
runBench inputPath = do
  Right content <- readFile inputPath
    | Left err => putStrLn $ "Error reading file: " ++ show err
  putStrLn $ "[V0] Input: " ++ inputPath
  putStrLn $ "Size: " ++ show (length content) ++ " chars"
  putStrLn ""
  -- Run 3 iterations
  let go : Nat -> IO ()
      go 0 = pure ()
      go (S n) = do
        start <- getTimeNs
        let spans = extractSpans content
        let count = length spans
        end <- getTimeNs
        let elapsed = (end - start) `div` 1000000
        putStrLn $ "Run " ++ show (3 `minus` n) ++ ": " ++ show count ++ " spans, " ++ show elapsed ++ " ms"
        go n
  go 3

runBenchV1 : String -> IO ()
runBenchV1 inputPath = do
  Right content <- readFile inputPath
    | Left err => putStrLn $ "Error reading file: " ++ show err
  putStrLn $ "[V1] Input: " ++ inputPath
  putStrLn $ "Size: " ++ show (length content) ++ " chars"
  putStrLn ""
  -- Run 3 iterations
  let go : Nat -> IO ()
      go 0 = pure ()
      go (S n) = do
        start <- getTimeNs
        let spans = extractSpans content
        let count = length spans
        end <- getTimeNs
        let elapsed = (end - start) `div` 1000000
        putStrLn $ "Run " ++ show (3 `minus` n) ++ ": " ++ show count ++ " spans, " ++ show elapsed ++ " ms"
        go n
  go 3

runBenchV2 : String -> IO ()
runBenchV2 inputPath = do
  Right content <- readFile inputPath
    | Left err => putStrLn $ "Error reading file: " ++ show err
  putStrLn $ "[V2 FSM] Input: " ++ inputPath
  putStrLn $ "Size: " ++ show (length content) ++ " chars"
  putStrLn ""
  -- Run 3 iterations
  let go : Nat -> IO ()
      go 0 = pure ()
      go (S n) = do
        start <- getTimeNs
        let hits = extractHitsFSM content
        let count = length hits
        end <- getTimeNs
        let elapsed = (end - start) `div` 1000000
        putStrLn $ "Run " ++ show (3 `minus` n) ++ ": " ++ show count ++ " hits, " ++ show elapsed ++ " ms"
        go n
  go 3

-- =============================================================================
-- Runtime Analysis Mode (ProfileFlush)
-- =============================================================================

runRuntimeAnalysis : String -> String -> Options -> IO ()
runRuntimeAnalysis tracePath labelPath opts = do
    when opts.verbose $ putStrLn $ "Runtime analysis mode"
    when opts.verbose $ putStrLn $ "  Trace: " ++ tracePath
    when opts.verbose $ putStrLn $ "  Labels: " ++ labelPath
    res <- analyzeRuntimeCoverage tracePath labelPath
    case res of
        Left err => do putStrLn $ "Error: " ++ err
                       exitWith (ExitFailure 1)
        Right cov => do printRuntimeCoverage cov
                        putStrLn $ "Done"

joinField : List String -> String
joinField [] = ""
joinField [x] = x
joinField (x :: xs) = x ++ "|" ++ joinField xs

findIpkgInDir : String -> IO (Maybe String)
findIpkgInDir dir = do
  Right files <- listDir dir | Left _ => pure Nothing
  let ipkgs = filter (\f => isSuffixOf ".ipkg" f) files
  case ipkgs of
    f :: _ => pure (Just (dir ++ "/" ++ f))
    [] => pure Nothing

runFullPipelineMode : Options -> IO ()
runFullPipelineMode opts =
  case opts.targetPath of
    Nothing => do
      putStrLn "FULL_PIPELINE_ERROR"
      putStrLn "message=No target path specified"
      exitWith (ExitFailure 1)
    Just target => do
      mIpkg <- case opts.ipkgPath of
                 Just p => pure (Just p)
                 Nothing => findIpkgInDir target
      case mIpkg of
        Nothing => do
          putStrLn "FULL_PIPELINE_ERROR"
          putStrLn "message=No .ipkg file found"
          exitWith (ExitFailure 1)
        Just ipkgPath => do
          let outputPath = "/tmp/dumpcases.txt"
          dres <- runDumpcasesAndParse ipkgPath outputPath
          case dres of
            Left err => do
              putStrLn "FULL_PIPELINE_ERROR"
              putStrLn ("message=" ++ err)
              exitWith (ExitFailure 1)
            Right branches => do
              let outputDir = fromMaybe (target ++ "/build/exec") opts.outputDir
              pres <- YulInstr.runFullPipeline ipkgPath outputDir branches
              case pres of
                Left err => do
                  putStrLn "FULL_PIPELINE_ERROR"
                  putStrLn ("message=" ++ err)
                  exitWith (ExitFailure 1)
                Right analysis => do
                  putStrLn "FULL_PIPELINE_OK"
                  putStrLn ("static_branches=" ++ show analysis.staticBranches)
                  putStrLn ("materialized_branches=" ++ show analysis.materializedBranches)
                  putStrLn ("branch_hits=" ++ show analysis.branchHits)
                  putStrLn ("functions_hit=" ++ show analysis.functionsHit)
                  putStrLn ("total_functions=" ++ show analysis.totalFunctions)
                  putStrLn ("coverage_percent=" ++ show analysis.coveragePercent)
                  putStrLn ("covered_branch_ids=" ++ joinField analysis.coveredBranchIds)
                  putStrLn ("unobservable_branch_ids=" ++ joinField analysis.unobservableBranchIds)
                  putStrLn ("materialized_branch_ids=" ++ joinField analysis.materializedBranchIds)
                  putStrLn ("uncovered_functions=" ++ joinField analysis.uncoveredFunctions)

-- =============================================================================
-- Main Execution
-- =============================================================================

runMain : Options -> IO ()
runMain opts =
  if opts.fullPipeline then runFullPipelineMode opts else
  -- Check if runtime analysis mode
  case (opts.runtimeTrace, opts.runtimeLabels) of
    (Just trace, Just labels) => runRuntimeAnalysis trace labels opts
    (Just _, Nothing) => do
      putStrLn "Error: --runtime-trace requires --runtime-labels"
      exitWith (ExitFailure 1)
    (Nothing, Just _) => do
      putStrLn "Error: --runtime-labels requires --runtime-trace"
      exitWith (ExitFailure 1)
    (Nothing, Nothing) =>
      case opts.targetPath of
        Nothing => do
          putStrLn "Error: No target path specified"
          putStrLn "Use --help for usage information"
          exitWith (ExitFailure 1)
        Just target => do
          when opts.verbose $ putStrLn $ "Analyzing: " ++ target

          -- EVM interpreter coverage mode (not yet implemented)
          when opts.evmInterpreter $ do
            putStrLn "Error: --evm-interpreter mode not yet implemented"
            exitWith (ExitFailure 1)

          let outputDir = fromMaybe (target ++ "/coverage") opts.outputDir
          let config = MkEvmCoverageConfig
                         target
                         opts.ipkgPath
                         outputDir
                         opts.threshold
                         opts.verbose

          result <- analyzeCoverage config

          case result of
            Left err => do
              putStrLn $ "Error: " ++ err
              exitWith (ExitFailure 1)
            Right cov => do
              -- Output report
              case opts.format of
                OneLine => putStrLn $ toOneLine cov
                JSON => putStrLn $ toJson "now" target cov
                _ => do
                  writeReport opts.format outputDir cov
                  putStrLn $ toOneLine cov

              -- Check threshold
              if meetsThreshold opts.threshold cov
                then when opts.verbose $ putStrLn "✓ Coverage threshold met"
                else do
                  putStrLn $ "✗ Coverage below threshold (" ++ show opts.threshold ++ "%)"
                  exitWith (ExitFailure 1)

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs (drop 1 args) defaultOptions

  if opts.showHelp
    then putStrLn helpText
    else if opts.showVersion
      then putStrLn versionText
      else if opts.benchMode
        then case opts.targetPath of
          Nothing => putStrLn "Usage: idris2-evm-cov --bench <html_file>"
          Just path => runBench path
        else if opts.benchV1Mode
          then case opts.targetPath of
            Nothing => putStrLn "Usage: idris2-evm-cov --bench-v1 <html_file>"
            Just path => runBenchV1 path
          else if opts.benchV2Mode
            then case opts.targetPath of
              Nothing => putStrLn "Usage: idris2-evm-cov --bench-v2 <html_file>"
              Just path => runBenchV2 path
            else runMain opts
