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
import EvmCoverage.PathCoverage
import EvmCoverage.PathRuntime
import Coverage.Core.DumppathsJson
import EvmCoverage.Runtime as Runtime
import EvmCoverage.ProfileParserLinear
import EvmCoverage.ProfileParserFSM
import EvmCoverage.YulInstrumentor as YulInstr

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import Coverage.Standardization.Types
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
  subcommand   : Maybe String
  dumppathsJson : Maybe String
  pathHitsPath : Maybe String

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing Nothing Text 80.0 False False False False False False False False Nothing Nothing False Nothing Nothing Nothing

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("paths" :: rest) opts = parseArgs rest ({ subcommand := Just "paths" } opts)
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
parseArgs ("--dumppaths-json" :: path :: rest) opts = parseArgs rest ({ dumppathsJson := Just path } opts)
parseArgs ("--path-hits" :: path :: rest) opts = parseArgs rest ({ pathHitsPath := Just path } opts)
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
  --dumppaths-json <file>  Read path obligations from an existing dumppaths JSON file
  --path-hits <file>       Optional newline or csv file of covered path ids

EXAMPLES:
  # Analyze a project
  idris2-evm-cov ~/code/idris2-evm

  # JSON output for CI
  idris2-evm-cov --json ~/code/idris2-evm

  # Static analysis only
  idris2-evm-cov --dumpcases ~/code/idris2-evm

  # Full runtime branch pipeline
  idris2-evm-cov --full-pipeline --ipkg pkg.ipkg ~/code/project

  # Path obligations and missing paths
  idris2-evm-cov paths --dumppaths-json dumppaths.json --path-hits hits.txt
  idris2-evm-cov paths --dumppaths-json dumppaths.json --runtime-trace trace.txt --runtime-labels labels.csv
  idris2-evm-cov paths --ipkg pkg.ipkg ~/code/project

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
-- Path Coverage Mode
-- =============================================================================

resolveIdris2CommandForPaths : IO String
resolveIdris2CommandForPaths = do
  mcmd <- getEnv "IDRIS2_BIN"
  pure $ fromMaybe "idris2" mcmd

supportsDumppathsJson : String -> IO Bool
supportsDumppathsJson cmd = do
  t <- time
  let probe = "/tmp/idris2-evm-dumppaths-probe-" ++ show t ++ ".json"
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

findPackInstallBase : IO (Maybe String)
findPackInstallBase = do
  let tmpFile = "/tmp/pack-install-base-paths.txt"
  _ <- system $ "ls -td ~/.local/state/pack/install/*/ 2>/dev/null | head -1 > " ++ tmpFile
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path then pure Nothing else pure (Just path)

discoverPackagePath : IO String
discoverPackagePath = do
  mBase <- findPackInstallBase
  case mBase of
    Nothing => pure ""
    Just basePath => do
      let tmpFile = "/tmp/idris2-evm-cov-package-paths.txt"
      let cmd = "find " ++ basePath ++ " -type d -name 'idris2-*' 2>/dev/null > " ++ tmpFile
      _ <- system cmd
      Right content <- readFile tmpFile
        | Left _ => pure ""
      pure $ joinBy ":" (filter (not . null) (map trim (lines content)))

findLatestTempIpkgForPaths : String -> IO (Maybe String)
findLatestTempIpkgForPaths projectDir = do
  let tmpFile = "/tmp/idris2-evm-cov-latest-temp-ipkg.txt"
  let cmd = "ls -t " ++ projectDir ++ "/dumpcases-temp-*.ipkg 2>/dev/null"
         ++ " | grep -v -- '-yul-' | head -1 > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path then pure Nothing else pure (Just path)

resolveIpkgForPaths : Options -> IO (Either String String)
resolveIpkgForPaths opts =
  case opts.ipkgPath of
    Just ipkg => pure $ Right ipkg
    Nothing =>
      case opts.targetPath of
        Nothing => pure $ Left "paths command requires either --dumppaths-json or a target .ipkg/directory"
        Just target => do
          let cleanTarget =
                if isSuffixOf "/" target
                   then pack $ reverse $ drop 1 $ reverse $ unpack target
                   else target
          if isSuffixOf ".ipkg" cleanTarget
             then pure $ Right cleanTarget
             else do
               Right files <- listDir cleanTarget
                 | Left _ => pure $ Left $ "No .ipkg file found in " ++ cleanTarget
               let ipkgs = filter (\f => isSuffixOf ".ipkg" f) files
               let nonTemp = filter (not . isPrefixOf "temp-") ipkgs
               case nonTemp of
                 f :: _ => pure $ Right (cleanTarget ++ "/" ++ f)
                 [] => case ipkgs of
                         f :: _ => pure $ Right (cleanTarget ++ "/" ++ f)
                         [] => pure $ Left $ "No .ipkg file found in " ++ cleanTarget

runDumppathsJson : String -> IO (Either String String)
runDumppathsJson ipkgPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  idris2Cmd <- resolveIdris2CommandForPaths
  supported <- supportsDumppathsJson idris2Cmd
  if not supported
     then pure $ Left "Current Idris2 does not support --dumppaths-json. Set IDRIS2_BIN to your forked compiler."
     else do
       t <- time
       let outPath = "/tmp/idris2_evm_cov_paths_" ++ show t ++ ".json"
       let logPath = "/tmp/idris2_evm_cov_paths_" ++ show t ++ ".log"
       let partsPath = outPath ++ ".parts"
       -- When measuring with the fork-yul stack, the denominator build must resolve
       -- the project's custom local deps (e.g. idris2-subcontract) from the fork
       -- build-prefix, exactly like the YulInstrumentor codegen path. EVM_PATHCOV_PREFIX
       -- (the fork build-prefix) is exported as IDRIS2_PREFIX so the fork compiler finds
       -- its fork-built libs. IDRIS2_PREFIX and IDRIS2_PACKAGE_PATH must NOT both be set
       -- (they conflict), so the prefix mode wins when present.
       mPathcovPrefix <- getEnv "EVM_PATHCOV_PREFIX"
       pkgPath <- discoverPackagePath
       let envPrefix = case mPathcovPrefix of
                         Just p => "IDRIS2_PREFIX=\"" ++ p ++ "\" "
                         Nothing => if null pkgPath then "" else "IDRIS2_PACKAGE_PATH=\"" ++ pkgPath ++ "\" "
       let cmd = "cd " ++ projectDir ++ " && "
              ++ envPrefix ++ idris2Cmd ++ " --dumppaths-json " ++ outPath ++ " --build " ++ ipkgName
              ++ " > " ++ logPath ++ " 2>&1"
       _ <- system cmd
       fileResult <- readFile outPath
       content <- case fileResult of
         Right c => pure c
         Left _ => do
           partsResult <- readFile partsPath
           case partsResult of
             -- .parts is a per-module FRAGMENT stream, not the merged
             -- document. Only accept it when it happens to be a complete
             -- dumppaths JSON (single-module builds emit one whole object);
             -- otherwise returning it upgrades a build failure into a
             -- downstream "Failed to parse dumppaths JSON" with no build log.
             Right c => if looksLikeDumppathsJson c then pure c else pure ""
             Left _ => pure $ ""
       logResult <- readFile logPath
       let logContent = either (const "") id logResult
       let cleanupCmd = "rm -f " ++ outPath ++ " " ++ partsPath ++ " " ++ logPath
       _ <- system cleanupCmd
       if null (trim content)
          then do
            pure $ Left $ "Failed to read dumppaths JSON: File Not Found\nBuild log tail:\n" ++ unlines (reverse (take 20 (reverse (lines logContent))))
          else pure $ Right content

runDumppathsJsonInIsolatedCopy : String -> IO (Either String String)
runDumppathsJsonInIsolatedCopy ipkgPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  t <- time
  let tempDir = "/tmp/idris2-evm-pathdump-" ++ ipkgName ++ "-" ++ show t
  let copyCmd =
        "rm -rf " ++ tempDir
        ++ " && mkdir -p " ++ tempDir
        ++ " && cp -R " ++ projectDir ++ "/. " ++ tempDir
        ++ " && rm -rf " ++ tempDir ++ "/build"
        -- A dumpcases temp ipkg bakes `opts = --dumpcases-json ./build/exec/…`;
        -- with the build tree wiped (and the dedicated dumpcases builddir no
        -- longer recreating build/), that write target must exist or the fork
        -- dies mid-dump — leaving only .parts fragments for the caller to
        -- mis-parse as the dumppaths document.
        ++ " && mkdir -p " ++ tempDir ++ "/build/exec"
  copyExit <- system copyCmd
  if copyExit /= 0
     then pure $ Left $ "Failed to prepare isolated path dump copy for " ++ projectDir
     else do
       result <- runDumppathsJson (tempDir ++ "/" ++ ipkgName)
       _ <- system $ "rm -rf " ++ tempDir
       pure result

loadDumppathsForPipeline : String -> IO (Either String String)
loadDumppathsForPipeline ipkgPath = runDumppathsJson ipkgPath

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
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = singleton c

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

maybeNatJson : Maybe Nat -> String
maybeNatJson Nothing = "null"
maybeNatJson (Just n) = show n

maybeStringJson : Maybe String -> String
maybeStringJson Nothing = "null"
maybeStringJson (Just s) = "\"" ++ escapeJson s ++ "\""

pathStepToFullJson : PathStep -> String
pathStepToFullJson step =
  "{"
    ++ "\"node_id\":\"" ++ escapeJson step.nodeId ++ "\","
    ++ "\"branch_index\":" ++ show step.branchIndex ++ ","
    ++ "\"origin\":\"" ++ escapeJson step.origin ++ "\","
    ++ "\"case_index\":" ++ maybeNatJson step.caseIndex ++ ","
    ++ "\"branch_label\":" ++ maybeStringJson step.branchLabel ++ ","
    ++ "\"source_span\":" ++ maybeStringJson step.sourceSpan
    ++ "}"

obligationClassName : ObligationClass -> String
obligationClassName ReachableObligation = "ReachableObligation"
obligationClassName LogicallyUnreachable = "LogicallyUnreachable"
obligationClassName UserAdmittedPartialGap = "UserAdmittedPartialGap"
obligationClassName CompilerInsertedArtifact = "CompilerInsertedArtifact"
obligationClassName ExternalEffectBoundary = "ExternalEffectBoundary"
obligationClassName UnknownClassification = "UnknownClassification"
obligationClassName StubbedReach = "StubbedReach"

pathToFullJson : PathObligation -> String
pathToFullJson path =
  "{"
    ++ "\"path_id\":\"" ++ escapeJson path.pathId ++ "\","
    ++ "\"classification\":\"" ++ obligationClassName path.classification ++ "\","
    ++ "\"terminal_kind\":\"" ++ escapeJson path.terminalKind ++ "\","
    ++ "\"terminal_clause_id\":" ++ maybeNatJson path.terminalClauseId ++ ","
    ++ "\"steps\":[" ++ fastConcat (intersperse "," (map pathStepToFullJson path.steps)) ++ "],"
    ++ "\"source_span_union\":" ++ maybeStringJson path.sourceSpanUnion ++ ","
    ++ "\"path_length\":" ++ show path.pathLength
    ++ "}"

pathObligationFunctionToFullJson : PathObligation -> String
pathObligationFunctionToFullJson path =
  "{"
    ++ "\"function_name\":\"" ++ escapeJson path.functionName ++ "\","
    ++ "\"paths\":[" ++ pathToFullJson path ++ "]"
    ++ "}"

pathObligationsToDumppathsJson : List PathObligation -> String
pathObligationsToDumppathsJson paths =
  "{"
    ++ "\"compiler_version\":\"evm-observable-path-filter\","
    ++ "\"export_kind\":\"canonical_intrafunction_paths\","
    ++ "\"path_schema_version\":1,"
    ++ "\"functions\":["
    ++ fastConcat (intersperse "," (map pathObligationFunctionToFullJson paths))
    ++ "]}"

observableBranchIdsFromLabels : List Runtime.LabelEntry -> List String
observableBranchIdsFromLabels labels =
  mapMaybe toBranchId labels
  where
    toBranchId : Runtime.LabelEntry -> Maybe String
    toBranchId label =
      case label.labelKind of
        Runtime.LKBranch => label.branchId
        _ => Nothing

||| Classify each source path by EVM observability WITHOUT shrinking the
||| denominator (the previous filterDumppathsToObservableBranches silently
||| dropped non-observable paths → false 100%). Every source path is kept; a
||| product (ReachableObligation) path is reclassified three ways:
|||   - terminal branch materialized in the instrumented bytecode (∈ labels)
|||       → ReachableObligation (a genuine obligation; covered iff hit at runtime)
|||   - terminal branch resolvable but NOT materialized (Yul/solc optimized it away)
|||       → CompilerInsertedArtifact (provably not an EVM-observable obligation)
|||   - terminal branch unresolvable (cause undetermined)
|||       → UnknownClassification (honesty backstop: flips claim_admissible False
|||         under BlockCoverageClaim, surfacing the mapping gap instead of hiding it)
||| `observableBranchIdsFromLabels` reads ALL LKBranch labels the instrumentor
||| materialized (hit or not), so "∈ labels" means "materialized", distinct from
||| "hit at runtime" (the numerator). The returned JSON carries the full path set
||| with honest per-path classification for the shared coverage measurement.
classifyDumppathsByObservability : String -> String -> IO (Either String String)
classifyDumppathsByObservability dumppathsContent labelPath = do
  Right labels <- Runtime.loadLabels labelPath
    | Left err => pure $ Left err
  case parseProjectDumppathsJson defaultPathExclusions dumppathsContent of
    Left err => pure $ Left err
    Right paths =>
      let observableIds = observableBranchIdsFromLabels labels
          classified = map (classifyByObservability observableIds) paths
      in pure $ Right $ pathObligationsToDumppathsJson classified
  where
    classifyByObservability : List String -> PathObligation -> PathObligation
    classifyByObservability observableIds path =
      case path.classification of
        -- Only product obligations are subject to observability reclassification.
        -- Exclusion-reclassified paths (CompilerInsertedArtifact / etc.) are kept.
        ReachableObligation =>
          if pathCoveredByBranchIds observableIds path
            then path  -- materialized as a distinct EVM branch: genuine obligation
            else
              -- A product source path that the EVM/Yul/solc pipeline did NOT
              -- materialize as a distinct branch (inlined/merged/optimized). We
              -- CANNOT prove from the trace whether it executed, so honesty
              -- requires UnknownClassification (NOT CompilerInsertedArtifact):
              -- it is excluded from the denominator but flips claim_admissible
              -- False under BlockCoverageClaim, truthfully signalling that EVM
              -- bytecode-level coverage cannot make a complete source-path claim.
              -- (Genuinely-dead/stdlib/test paths are already CompilerInsertedArtifact
              -- via classifyEvmExclusion before this point.)
              { classification := UnknownClassification } path
        _ => path

coverageMeasurementToJson : CoverageMeasurement -> String
coverageMeasurementToJson m = unlines
  [ "{"
  , "  \"denominator_ids\": " ++ show (length m.denominatorIds) ++ ","
  , "  \"covered_ids\": " ++ show (length m.coveredIds) ++ ","
  , "  \"excluded_ids\": " ++ show (length m.excludedIds) ++ ","
  , "  \"unknown_ids\": " ++ show (length m.unknownIds)
  , "}"
  ]

pathCoverageReportToJson : PathCoverageResult -> String
pathCoverageReportToJson result =
  -- v2 contract: RAW COUNTS only — no coverage_percent field exists to fake.
  let c = evidenceCounts result in unlines
  [ "{"
  , "  \"coverage_model\": \"" ++ escapeJson result.coverageModel ++ "\","
  , "  \"claim_admissible\": " ++ boolToJson result.claimAdmissible ++ ","
  , "  \"paths_total\": " ++ show c.pathsTotal ++ ","
  , "  \"paths_denominator\": " ++ show c.pathsDenominator ++ ","
  , "  \"paths_hit\": " ++ show c.pathsHit ++ ","
  , "  \"paths_excluded\": " ++ show c.pathsExcluded ++ ","
  , "  \"paths_unknown\": " ++ show c.pathsUnknown ++ ","
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

runPathFullPipelineArtifacts : Options -> IO (Either String (String, List PathRuntimeHit))
runPathFullPipelineArtifacts opts = do
  ipkgResult <- resolveIpkgForPaths opts
  case ipkgResult of
    Left err => pure $ Left err
    Right ipkgPath => do
      let targetDir = fromMaybe (fst (splitPath ipkgPath)) opts.targetPath
      let outputDir = fromMaybe (targetDir ++ "/build/exec") opts.outputDir
      let dumpcasesOut = outputDir ++ "/dumpcases-paths.txt"
      dres <- runDumpcasesAndParse ipkgPath dumpcasesOut
      case dres of
        Left err => pure $ Left err
        Right branches => do
          let wrapperIpkgResult = !(findLatestTempIpkgForPaths targetDir)
          let pathIpkg = fromMaybe ipkgPath wrapperIpkgResult
          Right dumppathsContent <- runDumppathsJsonInIsolatedCopy pathIpkg
            | Left err => pure $ Left err
          let canonicalBranches = filter (\b => isCanonical b.branchClass) branches
          mPathcov <- getEnv "EVM_PATHCOV_YUL"
          case mPathcov of
            Just _ => do
              -- SOURCE-LEVEL path coverage, driven by REAL contract dispatch.
              --
              -- A deployed EVM contract is pure selector dispatch, not a test-IO
              -- program; idris2-yul compiles `main : IO ()` to an unapplied
              -- closure that never runs. So when the project ships a production
              -- dispatch ipkg (`*-dispatch.ipkg`, whose `main` is `dispatch
              -- [entries]`), we measure coverage against THAT universe:
              --   * denominator = dumppaths of the dispatch ipkg (production
              --     reachable paths), test modules excluded by construction.
              --   * numerator   = path markers fired by sending each production
              --     selector as calldata (runDispatchSelectors), captured by the
              --     revm inspector even across handler reverts.
              -- The fork-yul (--dumppathshits) emits log1(0,0,FNV(path-id)) and
              -- the entry now APPLIES the IO closure, so the handlers actually
              -- execute. If no dispatch ipkg exists we fall back to the previous
              -- single-run-of-the-executable behaviour (zero regression).
              mDispatchIpkg <- YulInstr.findDispatchIpkg targetDir
              let buildIpkg = fromMaybe ipkgPath mDispatchIpkg
              -- Denominator universe: dispatch ipkg if present, else test ipkg.
              denomContent <- case mDispatchIpkg of
                                Just di => do
                                  Right c <- runDumppathsJsonInIsolatedCopy di
                                    | Left _ => pure dumppathsContent
                                  pure c
                                Nothing => pure dumppathsContent
              Right baseYul <- YulInstr.generateYul buildIpkg outputDir
                | Left err => pure $ Left $ "Pathcov Yul generation failed: " ++ err
              Right _ <- YulInstr.compileYulToBytecode baseYul outputDir
                | Left err => pure $ Left $ "Pathcov compilation failed: " ++ err
              let binPath = baseYul ++ ".bin"
              let traceOutput = outputDir ++ "/coverage-trace.csv"
              tracePathResult <- the (IO (Either String String)) $ case mDispatchIpkg of
                Just _ => do
                  selectors <- YulInstr.discoverSelectors targetDir
                  YulInstr.runDispatchSelectors binPath traceOutput selectors
                Nothing => YulInstr.runIdrisEvmTest binPath traceOutput
              Right tracePath <- pure tracePathResult
                | Left err => pure $ Left $ "Pathcov execution failed: " ++ err
              hitsResult <- analyzePathHitsFromTraceAndLabelsContent denomContent tracePath ""
              pure $ map (\hits => (denomContent, hits)) hitsResult
            Nothing => do
              Right (instrPath, labelPath) <- YulInstr.generateAndInstrumentYul ipkgPath outputDir canonicalBranches
                | Left err => pure $ Left $ "Instrumentation failed: " ++ err
              Right _ <- YulInstr.compileYulToBytecode instrPath outputDir
                | Left err => pure $ Left $ "Compilation failed: " ++ err
              let binPath = instrPath ++ ".bin"
              let traceOutput = outputDir ++ "/coverage-trace.csv"
              Right tracePath <- YulInstr.runIdrisEvmTest binPath traceOutput
                | Left err => pure $ Left $ "Execution failed: " ++ err
              -- Denominator = FULL source path set with honest observability
              -- classification (no silent shrink). Numerator = observable trace hits
              -- (the EVM trace can only see materialized branches; that is correct).
              Right classifiedContent <- classifyDumppathsByObservability dumppathsContent labelPath
                | Left err => pure $ Left $ "Observability classification failed: " ++ err
              hitsResult <- analyzePathHitsFromTraceAndLabelsContent classifiedContent tracePath labelPath
              pure $ map (\hits => (classifiedContent, hits)) hitsResult

runPaths : Options -> IO ()
runPaths opts = do
  artifactsResult <-
    if opts.fullPipeline
       then runPathFullPipelineArtifacts opts
    else do
      contentResult <- case opts.dumppathsJson of
        Just path => do
          Right content <- readFile path
            | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
          pure $ Right content
        Nothing => do
          ipkgResult <- resolveIpkgForPaths opts
          case ipkgResult of
            Left err => pure $ Left err
            Right ipkg => runDumppathsJson ipkg
      case contentResult of
        Left err => pure $ Left err
        Right content => do
          hitsResult <- case opts.pathHitsPath of
            Just _ => loadPathHits opts.pathHitsPath
            Nothing =>
              case (opts.runtimeTrace, opts.runtimeLabels) of
                (Just trace, Just labels) => analyzePathHitsFromTraceAndLabelsContent content trace labels
                (Just _, Nothing) => pure $ Left "--runtime-trace requires --runtime-labels for path mode"
                (Nothing, Just _) => pure $ Left "--runtime-labels requires --runtime-trace for path mode"
                (Nothing, Nothing) => pure $ Right []
          pure $ map (\hits => (content, hits)) hitsResult
  -- HONEST DENOMINATOR: when the source was instrumented with the FORK-yul
  -- (EVM_PATHCOV_YUL), every source CaseTree leaf carries a log1 marker and is
  -- directly observable, so the released-yul "branch-label collapse" /
  -- "constant-false" exclusions that previously hid real product branches
  -- (Tally/Tokenomics/Members) MUST be suppressed — otherwise the denominator is
  -- the narrow released-yul artifact, not the full source-branch set.
  mHonest <- getEnv "EVM_PATHCOV_YUL"
  let honestObservable = isJust mHonest
  case artifactsResult of
    Left err => do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right (content, hits) =>
      case analyzePathCoverageFromContentMode honestObservable defaultPathExclusions content hits of
        Left err => do
          putStrLn $ "Error: " ++ err
          exitWith (ExitFailure 1)
        Right result =>
          case opts.format of
            JSON => putStrLn $ pathCoverageReportToJson result
            _ => do
              -- v2: canonical evidence renderer — raw counts, unknown paths in
              -- the denominator-adjacent limbo bucket, NO percent to fake. The
              -- 4-of-158 admissible-subset over-report ("50%" for an honest
              -- 1.2%) is unrepresentable: the consumer divides hit by
              -- (denominator + unknown) itself.
              putStrLn $ renderPathEvidence "EVM " result
              putStrLn $ pathMeasurementSummary result.measurement
              -- EVM-specific diagnostic: terminal nodeIds of untriaged paths,
              -- cross-referencable against the *-labels.csv observable set.
              let unknownPaths = filter (\p => p.classification == UnknownClassification) result.allPaths
              traverse_ (\p => putStrLn $ "? " ++ p.pathId
                            ++ " [term=" ++ fromMaybe "-" (terminalBranchId p) ++ "]"
                            ++ " :: " ++ pathSummary p) unknownPaths
              -- NOTE: term= is the dumppaths terminal nodeId; cross-reference against
              -- the *-labels.csv observable set. A term with NO matching label is an
              -- inlined/non-materialised branch (e.g. Maybe-returning guards like
              -- requireMemberProof that get inlined into callers) — genuinely
              -- unobservable at EVM bytecode level. Closing those honestly needs
              -- source-level path-id instrumentation (the dfx prim__recordPathHit
              -- mechanism, applied to idris2-yul), not classification.

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

isTransientIpkgName : String -> Bool
isTransientIpkgName name =
  isPrefixOf "dumpcases-temp-" name || isInfixOf "-yul-" name

isWrapperCoverageIpkgName : String -> Bool
isWrapperCoverageIpkgName name =
  isSuffixOf ".ipkg" name
    && isPrefixOf "dumpcases-temp-" name
    && not (isInfixOf "-yul-" name)

isAuxCoverageIpkgName : String -> Bool
isAuxCoverageIpkgName name =
  isSuffixOf ".ipkg" name
    && (isInfixOf "debug" name || isInfixOf "coverage" name)
    && not (isTransientIpkgName name)

isLikelyTestIpkgName : String -> Bool
isLikelyTestIpkgName name =
  name == "tests.ipkg"
    || isPrefixOf "test-" name
    || isPrefixOf "test_" name

ipkgBaseName : String -> String
ipkgBaseName ipkgPath =
  case reverse (forget (split (== '/') ipkgPath)) of
    (name :: _) =>
      if isSuffixOf ".ipkg" name
         then substr 0 (length name `minus` 5) name
         else name
    [] => ipkgPath

findAuxCoverageIpkgs : String -> String -> IO (List String)
findAuxCoverageIpkgs projectDir primaryIpkgPath = do
  Right files <- listDir projectDir | Left _ => pure []
  let primaryName =
        case reverse (forget (split (== '/') primaryIpkgPath)) of
          (x :: _) => x
          [] => primaryIpkgPath
  pure $
    map (\f => projectDir ++ "/" ++ f) $
      sort $
      filter (\f => f /= primaryName && isAuxCoverageIpkgName f && not (isLikelyTestIpkgName f)) files

findWrapperCoverageIpkg : String -> String -> IO (Maybe String)
findWrapperCoverageIpkg projectDir primaryIpkgPath = do
  Right files <- listDir projectDir | Left _ => pure Nothing
  let primaryName =
        case reverse (forget (split (== '/') primaryIpkgPath)) of
          (x :: _) => x
          [] => primaryIpkgPath
  case reverse (sort (filter (\f => f /= primaryName && isWrapperCoverageIpkgName f) files)) of
    [] => pure Nothing
    (f :: _) => pure (Just (projectDir ++ "/" ++ f))

branchFuncName : String -> String
branchFuncName branchId =
  case reverse (forget (split (== '#') branchId)) of
    (_ :: prefixRev) => joinBy "#" (reverse prefixRev)
    [] => branchId

countCoveredFunctions : List String -> Nat
countCoveredFunctions branchIds = cast {to=Nat} (length (nub (map branchFuncName branchIds)))

combineCoverageAnalyses : YulInstr.CoverageAnalysis -> List YulInstr.CoverageAnalysis -> YulInstr.CoverageAnalysis
combineCoverageAnalyses primary auxs =
  let primaryMaterialized = nub primary.materializedBranchIds
      auxCovered = concatMap (.coveredBranchIds) auxs
      coveredUnion = filter (\bid => elem bid primaryMaterialized) (nub (primary.coveredBranchIds ++ auxCovered))
      uncoveredBranchIds = filter (\bid => not (elem bid coveredUnion)) primaryMaterialized
      uncoveredFns = nub (map branchFuncName uncoveredBranchIds)
      materializedTotal = length primaryMaterialized
      hitTotal = length coveredUnion
      pct =
        if materializedTotal == 0
           then 100
           else cast {to=Nat} (floor (100.0 * cast hitTotal / cast materializedTotal))
   in { branchHits := hitTotal
      , functionsHit := countCoveredFunctions coveredUnion
      , coveragePercent := pct
      , coveredBranchIds := coveredUnion
      , uncoveredFunctions := uncoveredFns
      } primary

findIpkgInDir : String -> IO (Maybe String)
findIpkgInDir dir = do
  Right files <- listDir dir | Left _ => pure Nothing
  let ipkgs = filter (\f => isSuffixOf ".ipkg" f) files
  let nonTemp = filter (not . isPrefixOf "temp-") ipkgs
  case nonTemp of
    f :: _ => pure (Just (dir ++ "/" ++ f))
    [] => case ipkgs of
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
                Right primaryAnalysis => do
                  mWrapperIpkg <- findWrapperCoverageIpkg target ipkgPath
                  auxIpkgs <- findAuxCoverageIpkgs target ipkgPath
                  wrapperResults <- traverse
                    (\wrapperIpkg => do
                       let auxOutputDir = outputDir ++ "/aux-wrapper"
                       wrapperRun <- YulInstr.runFullPipeline wrapperIpkg auxOutputDir branches
                       case wrapperRun of
                         Left _ => pure Nothing
                         Right wrapperAnalysis => pure (Just wrapperAnalysis))
                    mWrapperIpkg
                  auxResults <- traverse
                    (\auxIpkg => do
                       auxDump <- runDumpcasesAndParse auxIpkg "/tmp/dumpcases.txt"
                       case auxDump of
                         Left _ => pure Nothing
                         Right auxBranches => do
                           let auxOutputDir = outputDir ++ "/aux-" ++ ipkgBaseName auxIpkg
                           auxRun <- YulInstr.runFullPipeline auxIpkg auxOutputDir auxBranches
                           case auxRun of
                             Left _ => pure Nothing
                             Right auxAnalysis => pure (Just auxAnalysis))
                    auxIpkgs
                  let wrapperAux =
                        case wrapperResults of
                          Just (Just x) => [x]
                          _ => []
                  let allAux = wrapperAux ++ mapMaybe id auxResults
                  let analysis = combineCoverageAnalyses primaryAnalysis allAux
                  putStrLn "FULL_PIPELINE_OK"
                  putStrLn ("static_branches=" ++ show analysis.staticBranches)
                  putStrLn ("materialized_branches=" ++ show analysis.materializedBranches)
                  putStrLn ("branch_hits=" ++ show analysis.branchHits)
                  putStrLn ("functions_hit=" ++ show analysis.functionsHit)
                  putStrLn ("total_functions=" ++ show analysis.totalFunctions)
                  -- (no percent line: raw counts above are the contract; any
                  -- percentage is the consumer's division, not the producer's)
                  putStrLn ("covered_branch_ids=" ++ joinField analysis.coveredBranchIds)
                  putStrLn ("unobservable_branch_ids=" ++ joinField analysis.unobservableBranchIds)
                  putStrLn ("materialized_branch_ids=" ++ joinField analysis.materializedBranchIds)
                  putStrLn ("branch_diagnostics_csv=" ++ outputDir ++ "/branch-counter-diagnostics.csv")
                  putStrLn ("uncovered_functions=" ++ joinField analysis.uncoveredFunctions)

-- =============================================================================
-- Main Execution
-- =============================================================================

runMain : Options -> IO ()
runMain opts =
  if opts.subcommand == Just "paths" then runPaths opts else
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
