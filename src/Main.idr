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

import Data.List
import Data.Maybe
import Data.String
import System
import System.File

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

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing Nothing Text 80.0 False False False False

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

EXAMPLES:
  # Analyze a project
  idris2-evm-cov ~/code/idris2-evm

  # JSON output for CI
  idris2-evm-cov --json ~/code/idris2-evm

  # Static analysis only
  idris2-evm-cov --dumpcases ~/code/idris2-evm

COVERAGE MODEL:
  Denominator: Canonical branches from --dumpcases (excludes void/uninhabited)
  Numerator:   Branches hit during test execution (Chez profiler)

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
-- Main Execution
-- =============================================================================

runMain : Options -> IO ()
runMain opts =
  case opts.targetPath of
    Nothing => do
      putStrLn "Error: No target path specified"
      putStrLn "Use --help for usage information"
      exitWith (ExitFailure 1)
    Just target => do
      when opts.verbose $ putStrLn $ "Analyzing: " ++ target

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
      else runMain opts
