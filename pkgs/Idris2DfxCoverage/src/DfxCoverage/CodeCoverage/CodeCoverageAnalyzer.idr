||| Code Coverage Analyzer
|||
||| Analyzes code-level coverage by combining:
||| - WASM function mapping (name section)
||| - Execution traces (from instrumented runtime)
||| - Branch coverage data
|||
||| Produces detailed coverage reports at function and branch level.
module DfxCoverage.CodeCoverage.CodeCoverageAnalyzer

import Data.List
import Data.Maybe
import Data.String
import System.File

import DfxCoverage.WasmMapper.WasmFunc
import DfxCoverage.WasmMapper.NameSection
import DfxCoverage.WasmTrace.TraceEntry
import DfxCoverage.WasmTrace.TraceParser
import DfxCoverage.CodeCoverage.CodeCoverageResult
import DfxCoverage.Exclusions

%default covering

-- =============================================================================
-- Analysis Configuration
-- =============================================================================

||| Configuration for coverage analysis
public export
record CoverageConfig where
  constructor MkCoverageConfig
  wasmPath : String                 -- Path to .wasm file
  tracePath : Maybe String          -- Path to trace file (if pre-collected)
  exclusions : List ExclPattern     -- Exclusion patterns
  includeImports : Bool             -- Include imported functions in report
  granularity : CoverageGranularity -- Level of detail

||| Default configuration
export
defaultConfig : String -> CoverageConfig
defaultConfig wasmPath = MkCoverageConfig
  { wasmPath = wasmPath
  , tracePath = Nothing
  , exclusions = []
  , includeImports = False
  , granularity = FunctionLevel
  }

-- =============================================================================
-- Trace to Coverage Mapping
-- =============================================================================

||| Map trace entries to function coverage
traceToCoverage : FuncMappingTable -> List WasmTraceEntry -> List FuncCoverageInfo
traceToCoverage mappings traces =
  let hits = aggregateHits traces
  in map (toFuncCoverage hits) mappings.mappings
  where
    findHits : Nat -> List FuncHitCount -> Maybe FuncHitCount
    findHits idx = find (\h => h.funcIdx == idx)

    toFuncCoverage : List FuncHitCount -> FuncMapping -> FuncCoverageInfo
    toFuncCoverage hits fm =
      let status = case fm.wasmFunc.kind of
            ImportedFunc _ => FuncImport
            _ => case findHits fm.wasmFunc.funcIdx hits of
              Nothing => FuncNotCovered
              Just h => FuncCovered h.hitCount
          uniquePCs = case findHits fm.wasmFunc.funcIdx hits of
            Nothing => 0
            Just h => h.uniquePCs
      in MkFuncCoverageInfo
           fm.wasmFunc.funcIdx
           fm.wasmFunc.wasmName
           fm.idris2Name
           status
           uniquePCs
           Nothing  -- Branch coverage computed separately

-- =============================================================================
-- Branch Coverage Analysis
-- =============================================================================

||| Analyze branch coverage from traces
analyzeBranches : List WasmTraceEntry -> List BranchCoverageInfo
analyzeBranches traces =
  let branchPoints = extractBranchPoints traces
  in map toBranchCoverage (groupByPC branchPoints)
  where
    groupByPC : List WasmBranchPoint -> List (Nat, Nat, List WasmBranchPoint)
    groupByPC [] = []
    groupByPC (bp :: rest) =
      let (matching, others) = partition (\b => b.funcIdx == bp.funcIdx && b.pc == bp.pc) rest
      in (bp.funcIdx, bp.pc, bp :: matching) :: groupByPC others

    toBranchCoverage : (Nat, Nat, List WasmBranchPoint) -> BranchCoverageInfo
    toBranchCoverage (funcIdx, pc, bps) =
      let branchType = fromMaybe "unknown" (map (.branchType) (head' bps))
          takenCount = length $ filter (.taken) bps
          notTakenCount = length $ filter (not . (.taken)) bps
      in MkBranchCoverageInfo funcIdx pc branchType takenCount notTakenCount

-- =============================================================================
-- Module Breakdown
-- =============================================================================

||| Group function coverage by module
groupByModule : List FuncCoverageInfo -> List ModuleCoverageInfo
groupByModule funcInfos =
  let grouped = groupByModulePath funcInfos
  in map toModuleCoverage grouped
  where
    getModulePath : FuncCoverageInfo -> String
    getModulePath fc = case fc.idris2Name of
      Nothing => "<unknown>"
      Just qn => moduleName qn

    groupByModulePath : List FuncCoverageInfo -> List (String, List FuncCoverageInfo)
    groupByModulePath [] = []
    groupByModulePath (fc :: rest) =
      let modPath = getModulePath fc
          (matching, others) = partition (\f => getModulePath f == modPath) rest
      in (modPath, fc :: matching) :: groupByModulePath others

    toModuleCoverage : (String, List FuncCoverageInfo) -> ModuleCoverageInfo
    toModuleCoverage (modPath, funcs) =
      let funcCount = length funcs
          coveredCount = length $ filter isFuncCovered funcs
          percent = if funcCount == 0
                      then 100.0
                      else (cast {to=Double} coveredCount / cast {to=Double} funcCount) * 100.0
      in MkModuleCoverageInfo modPath funcCount coveredCount percent funcs

-- =============================================================================
-- Main Analysis
-- =============================================================================

||| Analyze coverage from WASM and trace files
|||
||| @config  Analysis configuration
||| @traces  Pre-collected traces (or empty to use tracePath from config)
export
analyzeCodeCoverage : CoverageConfig -> List WasmTraceEntry -> IO (Either String CodeCoverageResult)
analyzeCodeCoverage config traces = do
  -- Build function mapping from WASM
  Right mappings <- buildMappingTableFromWasm config.wasmPath
    | Left err => pure $ Left $ "Failed to parse WASM: " ++ err

  -- Get traces (from parameter or file)
  allTraces <- case traces of
    (_ :: _) => pure traces  -- Use provided traces
    [] => case config.tracePath of
      Nothing => pure []
      Just path => do
        Right fileTraces <- readTraceFile path
          | Left err => pure []
        pure fileTraces

  -- Compute function coverage
  let funcCoverage = traceToCoverage mappings allTraces

  -- Apply exclusions
  let funcWithExclusions = map (applyExclusions config.exclusions) funcCoverage

  -- Filter out imports if requested
  let filteredFuncs = if config.includeImports
                        then funcWithExclusions
                        else filter (not . isImport) funcWithExclusions

  -- Compute branch coverage
  let branchCoverage = analyzeBranches allTraces

  -- Build module breakdown
  let moduleBreakdown = groupByModule filteredFuncs

  -- Compute statistics
  let totalFuncs = length filteredFuncs
  let coveredFuncs = length $ filter isFuncCovered filteredFuncs
  let excludedFuncs = length $ filter isExcluded filteredFuncs
  let importedFuncs = length $ filter isImport funcWithExclusions
  let funcPercent = if totalFuncs == 0
                      then 100.0
                      else (cast {to=Double} coveredFuncs / cast {to=Double} totalFuncs) * 100.0

  let totalBranches = length branchCoverage
  let fullBranches = length $ filter isBranchFullyCovered branchCoverage
  let partialBranches = length $ filter isBranchPartiallyCovered branchCoverage
  let branchPercent = if totalBranches == 0
                        then 100.0
                        else (cast {to=Double} fullBranches / cast {to=Double} totalBranches) * 100.0

  -- Find gaps
  let uncoveredFuncs = filter (not . isFuncCovered) filteredFuncs
  let uncoveredBranches = filter (not . isBranchFullyCovered) branchCoverage

  pure $ Right $ MkCodeCoverageResult
    totalFuncs coveredFuncs excludedFuncs importedFuncs funcPercent
    totalBranches fullBranches partialBranches branchPercent
    filteredFuncs branchCoverage moduleBreakdown
    uncoveredFuncs uncoveredBranches

  where
    applyExclusions : List ExclPattern -> FuncCoverageInfo -> FuncCoverageInfo
    applyExclusions excls fc =
      case isMethodExcluded excls fc.wasmName of
        Just reason => { status := FuncExcluded reason } fc
        Nothing => fc

    isImport : FuncCoverageInfo -> Bool
    isImport fc = case fc.status of
      FuncImport => True
      _ => False

    isExcluded : FuncCoverageInfo -> Bool
    isExcluded fc = case fc.status of
      FuncExcluded _ => True
      _ => False

-- =============================================================================
-- Simplified Analysis (for quick checks)
-- =============================================================================

||| Quick function coverage check (no branch analysis)
isImportCov : FuncCoverageInfo -> Bool
isImportCov fc = case fc.status of
  FuncImport => True
  _ => False

||| Quick function coverage check (no branch analysis)
export
quickFunctionCoverage : String -> List WasmTraceEntry -> IO (Either String (Nat, Nat, Double))
quickFunctionCoverage wasmPath traces = do
  Right mappings <- buildMappingTableFromWasm wasmPath
    | Left err => pure $ Left err

  let funcCoverage = traceToCoverage mappings traces
  let defined = filter (not . isImportCov) funcCoverage
  let totalCount = length defined
  let coveredCount = length $ filter isFuncCovered defined
  let percent = if totalCount == 0
                  then 100.0
                  else (cast {to=Double} coveredCount / cast {to=Double} totalCount) * 100.0

  pure $ Right (coveredCount, totalCount, percent)

-- =============================================================================
-- Report Generation
-- =============================================================================

showModuleInfo : ModuleCoverageInfo -> String
showModuleInfo mc = "  " ++ mc.modulePath ++ ": " ++
                show mc.coveredCount ++ "/" ++ show mc.funcCount ++
                " (" ++ show mc.coveragePercent ++ "%)"

showUncoveredFunc : FuncCoverageInfo -> String
showUncoveredFunc fc = "  - " ++ fc.wasmName ++
                   maybe "" (\qn => " (" ++ show qn ++ ")") fc.idris2Name

showBranchInfo : BranchCoverageInfo -> String
showBranchInfo bc = "  - func[" ++ show bc.funcIdx ++ "] PC=" ++ show bc.pc ++
                " " ++ bc.branchType

||| Generate text report
export
generateReport : CodeCoverageResult -> String
generateReport cr =
  let sep = pack (replicate 60 '=')
      headerLines = [ sep
                    , "CODE COVERAGE REPORT"
                    , sep
                    , ""
                    , "SUMMARY"
                    , "-------"
                    , "Functions: " ++ show cr.coveredFunctions ++ "/" ++ show cr.totalFunctions ++
                      " (" ++ show cr.functionCoveragePercent ++ "%)"
                    , "Branches:  " ++ show cr.fullyCoveredBranches ++ "/" ++ show cr.totalBranches ++
                      " (" ++ show cr.branchCoveragePercent ++ "%)"
                    , "Excluded:  " ++ show cr.excludedFunctions
                    , "Imports:   " ++ show cr.importedFunctions
                    , ""
                    , "MODULE BREAKDOWN"
                    , "----------------"
                    ]
      moduleLines = map showModuleInfo cr.moduleBreakdown
      uncovFuncHeader = [ ""
                        , "UNCOVERED FUNCTIONS (" ++ show (length cr.uncoveredFunctions) ++ ")"
                        , "--------------------"
                        ]
      uncovFuncLines = map showUncoveredFunc cr.uncoveredFunctions
      uncovBranchHeader = [ ""
                          , "UNCOVERED BRANCHES (" ++ show (length cr.uncoveredBranches) ++ ")"
                          , "-------------------"
                          ]
      branchLines = map showBranchInfo (take 20 cr.uncoveredBranches)
      moreLines = if length cr.uncoveredBranches > 20
                    then ["... and " ++ show (minus (length cr.uncoveredBranches) 20) ++ " more"]
                    else []
  in unlines (headerLines ++ moduleLines ++ uncovFuncHeader ++ uncovFuncLines ++
              uncovBranchHeader ++ branchLines ++ moreLines)

||| Write report to file
export
writeReport : String -> CodeCoverageResult -> IO (Either String ())
writeReport path cr = do
  let content = generateReport cr
  Right () <- writeFile path content
    | Left err => pure $ Left $ "Failed to write report: " ++ show err
  pure $ Right ()
