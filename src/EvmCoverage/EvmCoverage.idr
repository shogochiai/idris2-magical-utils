||| High-level EVM coverage API
||| Combines dumpcases analysis with Chez profiler
module EvmCoverage.EvmCoverage

import EvmCoverage.Types
import EvmCoverage.DumpcasesParser
import EvmCoverage.ProfileParser
import EvmCoverage.SchemeMapper
import EvmCoverage.Aggregator
import EvmCoverage.Report

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Directory

%default covering

-- =============================================================================
-- Project Discovery
-- =============================================================================

||| Find ipkg file in directory
export
findIpkg : String -> IO (Maybe String)
findIpkg dir = do
  Right files <- listDir dir
    | Left _ => pure Nothing
  let ipkgs = filter (isSuffixOf ".ipkg") files
  case ipkgs of
    [] => pure Nothing
    (f :: _) => pure $ Just (dir ++ "/" ++ f)

||| Find build directory
export
findBuildDir : String -> IO (Maybe String)
findBuildDir projectDir = do
  let buildDir = projectDir ++ "/build"
  True <- exists buildDir
    | False => pure Nothing
  pure $ Just buildDir

-- =============================================================================
-- Static Analysis (Dumpcases)
-- =============================================================================

||| Run static analysis on a project
export
runStaticAnalysis : String -> String -> IO (Either String StaticBranchAnalysis)
runStaticAnalysis ipkgPath outputPath = do
  result <- runDumpcasesAndParse ipkgPath outputPath
  case result of
    Left err => pure $ Left err
    Right branches => pure $ Right $ analyzeStatic branches

-- =============================================================================
-- Dynamic Analysis (Profiler)
-- =============================================================================

||| Generate profiler wrapper script
generateProfilerScript : String -> String -> String
generateProfilerScript ssPath outputDir = """
(import (chezscheme))
(compile-profile 'source)
(profile-clear)
(load "\{ssPath}")
(profile-dump-html "\{outputDir}/profile.html")
(profile-dump-data "\{outputDir}/profile.data")
"""

||| Run with Chez profiler
export
runWithProfiler : String -> String -> List String -> IO (Either String String)
runWithProfiler execPath profileDir args = do
  -- Create profile script
  let scriptPath = profileDir ++ "/run_profile.ss"
  let ssPath = execPath ++ "_app/" ++
               (fromMaybe "program" $ last' $ forget $ split (== '/') execPath) ++ ".ss"
  Right () <- writeFile scriptPath (generateProfilerScript ssPath profileDir)
    | Left err => pure $ Left $ "Failed to write script: " ++ show err
  -- Run with chez
  let cmd = "/opt/homebrew/bin/chez --script " ++ scriptPath ++ " " ++ unwords args
  exitCode <- system cmd
  if exitCode == 0
    then pure $ Right $ profileDir ++ "/profile.html"
    else pure $ Left $ "Profiler failed with exit code: " ++ show exitCode

||| Parse profiler output
export
parseProfilerOutput : String -> IO (Either String (List ProfileHit))
parseProfilerOutput profileDir = do
  let htmlPath = profileDir ++ "/profile.htmlprofile.html"
  Right content <- readFile htmlPath
    | Left _ => do
        -- Try alternate path
        let altPath = profileDir ++ "/profile.html"
        Right altContent <- readFile altPath
          | Left err => pure $ Left $ "Failed to read profile: " ++ show err
        pure $ Right $ extractProfileHits altPath altContent
  pure $ Right $ extractProfileHits htmlPath content

-- =============================================================================
-- Full Analysis Pipeline
-- =============================================================================

||| Configuration for coverage analysis
public export
record EvmCoverageConfig where
  constructor MkEvmCoverageConfig
  projectDir    : String
  ipkgPath      : Maybe String
  outputDir     : String
  threshold     : Double
  verbose       : Bool

||| Default configuration
export
defaultConfig : String -> EvmCoverageConfig
defaultConfig projectDir =
  MkEvmCoverageConfig
    projectDir
    Nothing
    (projectDir ++ "/coverage")
    80.0
    False

||| Run full coverage analysis
export
analyzeCoverage : EvmCoverageConfig -> IO (Either String AggregatedCoverage)
analyzeCoverage config = do
  -- Ensure output directory exists
  _ <- system $ "mkdir -p " ++ config.outputDir

  -- Find or use ipkg
  ipkg <- case config.ipkgPath of
            Just p => pure $ Just p
            Nothing => findIpkg config.projectDir

  case ipkg of
    Nothing => pure $ Left "No .ipkg file found"
    Just ipkgPath => do
      when config.verbose $ putStrLn $ "Using ipkg: " ++ ipkgPath

      -- Step 1: Static analysis (dumpcases)
      let dumpcasesPath = config.outputDir ++ "/dumpcases.txt"
      when config.verbose $ putStrLn "Running static analysis..."
      staticResult <- runStaticAnalysis ipkgPath dumpcasesPath

      case staticResult of
        Left err => pure $ Left $ "Static analysis failed: " ++ err
        Right static => do
          when config.verbose $ do
            putStrLn $ "  Canonical branches: " ++ show static.canonicalCount
            putStrLn $ "  Bugs: " ++ show static.bugsCount

          -- For now, return static-only analysis
          -- TODO: Add profiler integration
          let cov = MkAggregatedCoverage
                      static.canonicalCount
                      0  -- No hits yet
                      static.bugsCount
                      static.unknownCount
                      0.0
                      (filter (\b => isCanonical b.branchClass) static.allBranches)

          pure $ Right cov

-- =============================================================================
-- Quick Analysis (static only)
-- =============================================================================

||| Quick static-only analysis
export
quickAnalyze : String -> IO (Either String AggregatedCoverage)
quickAnalyze projectDir = do
  let config = defaultConfig projectDir
  analyzeCoverage config

||| Analyze with existing dumpcases file
export
analyzeFromDumpcases : String -> IO (Either String AggregatedCoverage)
analyzeFromDumpcases dumpcasesPath = do
  Right content <- readFile dumpcasesPath
    | Left err => pure $ Left $ "Failed to read dumpcases: " ++ show err
  let branches = parseDumpcases content
  let static = analyzeStatic branches
  let cov = MkAggregatedCoverage
              static.canonicalCount
              0
              static.bugsCount
              static.unknownCount
              0.0
              (filter (\b => isCanonical b.branchClass) static.allBranches)
  pure $ Right cov

-- =============================================================================
-- Top K Targets (for CLI display)
-- =============================================================================

||| Get top K high-impact targets from coverage results
||| Sorted by severity (branch count, descending)
export
getTopKTargets : Nat -> AggregatedCoverage -> List HighImpactTarget
getTopKTargets k cov = topKTargetsFromBranches k cov.uncoveredBranches

-- =============================================================================
-- Report Generation
-- =============================================================================

||| Generate and write report
export
writeReport : OutputFormat -> String -> AggregatedCoverage -> IO ()
writeReport fmt outputPath cov = do
  let timestamp = "2025-01-01T00:00:00Z"  -- TODO: Get real timestamp
  let content = generateReport fmt timestamp outputPath cov
  case fmt of
    OneLine => putStrLn content
    _ => do
      let ext = show fmt
      let filePath = outputPath ++ "/coverage." ++ ext
      Right () <- writeFile filePath content
        | Left err => putStrLn $ "Failed to write report: " ++ show err
      putStrLn $ "Report written to: " ++ filePath
