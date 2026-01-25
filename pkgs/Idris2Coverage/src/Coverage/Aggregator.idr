||| Coverage data aggregation and called_by_tests computation
||| REQ_COV_AGG_001 - REQ_COV_AGG_004
module Coverage.Aggregator

import Coverage.Types
import Coverage.Collector
import Coverage.SourceAnalyzer
import Coverage.TestRunner
import Data.List
import Data.String
import Data.SortedMap

%default total

-- =============================================================================
-- Function Hit Aggregation
-- =============================================================================

||| Aggregate hits for a function across all tests
||| REQ_COV_AGG_001: Map each function to list of tests that called it
public export
record FunctionHitAggregate where
  constructor MkFunctionHitAggregate
  schemeFunc   : String
  totalHits    : Nat
  calledByTests : List String

||| Aggregate profile results by function
export
aggregateByFunction : List TestProfileResult -> List FunctionHitAggregate
aggregateByFunction results =
  let -- Collect all (funcName, testId, hits) tuples
      allHits : List (String, String, Nat)
      allHits = concatMap extractHits results

      -- Group by function name
      grouped = groupBy schemeFunc allHits
  in map aggregate grouped
  where
    extractHits : TestProfileResult -> List (String, String, Nat)
    extractHits r = map (\h => (h.schemeFunc, r.testId, h.hitCount)) r.profileHits

    schemeFunc : (String, String, Nat) -> String
    schemeFunc (f, _, _) = f

    groupBy : (a -> String) -> List a -> List (String, List a)
    groupBy _ [] = []
    groupBy key xs =
      let keys = nub $ map key xs
      in map (\k => (k, filter (\x => key x == k) xs)) keys

    aggregate : (String, List (String, String, Nat)) -> FunctionHitAggregate
    aggregate (funcName, hits) =
      let tests = nub $ map (\(_, t, _) => t) hits
          totalHits = sum $ map (\(_, _, h) => h) hits
      in MkFunctionHitAggregate funcName totalHits tests

-- =============================================================================
-- Coverage Percent Calculation
-- =============================================================================

||| REQ_COV_AGG_002: Compute coverage_percent per function
export
computeFunctionCoverage : (exported : List FunctionDef)
                       -> (aggregated : List FunctionHitAggregate)
                       -> (moduleName : String)
                       -> List FunctionCoverage
computeFunctionCoverage exported aggregated moduleName =
  map (toCoverage aggregated moduleName) exported
  where
    findAggregate : String -> String -> List FunctionHitAggregate -> Maybe FunctionHitAggregate
    findAggregate modName funcName aggs =
      let schemePrefix = idrisToSchemeModule modName
          targetName = schemePrefix ++ "-" ++ funcName
      in find (\a => a.schemeFunc == targetName) aggs

    toCoverage : List FunctionHitAggregate -> String -> FunctionDef -> FunctionCoverage
    toCoverage aggs modName def =
      case findAggregate modName def.name aggs of
        Nothing =>
          -- Not found in profile - either dead code or not called
          MkFunctionCoverage
            modName
            def.name
            def.signature
            (Just def.lineStart)
            (Just def.lineEnd)
            0
            (def.lineEnd `minus` def.lineStart + 1)
            0.0
            []
        Just agg =>
          let totalLines = def.lineEnd `minus` def.lineStart + 1
              -- Simplification: if hit > 0, assume all lines covered
              coveredLines = if agg.totalHits > 0 then totalLines else 0
              percent = if totalLines > 0
                           then cast coveredLines / cast totalLines * 100.0
                           else 0.0
          in MkFunctionCoverage
               modName
               def.name
               def.signature
               (Just def.lineStart)
               (Just def.lineEnd)
               coveredLines
               totalLines
               percent
               agg.calledByTests

-- =============================================================================
-- Dead Code Detection
-- =============================================================================

||| REQ_COV_AGG_003: Detect dead code (exported but not in .ss)
export
detectDeadCode : (exported : List FunctionDef)
              -> (schemeDefs : List (String, Nat))
              -> (moduleName : String)
              -> List String
detectDeadCode exported schemeDefs moduleName =
  let schemePrefix = idrisToSchemeModule moduleName
      schemeNameList = map fst schemeDefs
      exportedNames = map (.name) exported
  in filter (notInScheme schemePrefix schemeNameList) exportedNames
  where
    notInScheme : String -> List String -> String -> Bool
    notInScheme pref names funcName =
      let targetName = pref ++ "-" ++ funcName
      in not $ elem targetName names

-- =============================================================================
-- Module Level Aggregation
-- =============================================================================

||| REQ_COV_AGG_004: Aggregate to module level
export
aggregateModule : (path : String)
               -> (functions : List FunctionCoverage)
               -> ModuleCoverage
aggregateModule path functions =
  let funcCount = length functions
      coveredCount = length $ filter (\f => f.coveragePercent > 0.0) functions
      avgPercent = if funcCount > 0
                      then sum (map (.coveragePercent) functions) / cast funcCount
                      else 0.0
  in MkModuleCoverage path funcCount coveredCount avgPercent

-- =============================================================================
-- Project Level Aggregation
-- =============================================================================

||| Aggregate to project level (without branch coverage)
export
aggregateProject : (modules : List ModuleCoverage) -> ProjectCoverage
aggregateProject modules =
  let totalFuncs = sum $ map (.functionsTotal) modules
      coveredFuncs = sum $ map (.functionsCovered) modules
      avgPercent = if totalFuncs > 0
                      then cast coveredFuncs / cast totalFuncs * 100.0
                      else 0.0
  in MkProjectCoverage totalFuncs coveredFuncs avgPercent Nothing

||| Aggregate to project level with branch coverage
export
aggregateProjectWithBranches : (modules : List ModuleCoverage)
                            -> (branchSummary : BranchCoverageSummary)
                            -> ProjectCoverage
aggregateProjectWithBranches modules branchSummary =
  let totalFuncs = sum $ map (.functionsTotal) modules
      coveredFuncs = sum $ map (.functionsCovered) modules
      avgPercent = if totalFuncs > 0
                      then cast coveredFuncs / cast totalFuncs * 100.0
                      else 0.0
  in MkProjectCoverage totalFuncs coveredFuncs avgPercent (Just branchSummary.branchPercent)

-- =============================================================================
-- Full Aggregation Pipeline
-- =============================================================================

||| Aggregate all data into a CoverageReport
export
aggregateAll : (testResults : List TestProfileResult)
            -> (sourceAnalysis : List (String, String, List FunctionDef))  -- (path, moduleName, funcs)
            -> (schemeDefs : List (String, Nat))
            -> CoverageReport
aggregateAll testResults sourceAnalysis schemeDefs =
  let aggregated = aggregateByFunction testResults
      functions = concatMap (processModule aggregated schemeDefs) sourceAnalysis
      modules = map (processModuleSummary functions) sourceAnalysis
      project = aggregateProject modules
  in MkCoverageReport functions modules project
  where
    processModule : List FunctionHitAggregate
                 -> List (String, Nat)
                 -> (String, String, List FunctionDef)
                 -> List FunctionCoverage
    processModule aggs _ (_, modName, funcs) =
      computeFunctionCoverage funcs aggs modName

    processModuleSummary : List FunctionCoverage
                        -> (String, String, List FunctionDef)
                        -> ModuleCoverage
    processModuleSummary allFuncs (path, modName, _) =
      let moduleFuncs = filter (\f => f.moduleName == modName) allFuncs
      in aggregateModule path moduleFuncs
