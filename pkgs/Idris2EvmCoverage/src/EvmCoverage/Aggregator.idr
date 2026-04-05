||| Coverage aggregation
||| Combines static analysis (dumpcases) with dynamic hits (profiler)
module EvmCoverage.Aggregator

import EvmCoverage.Types
import EvmCoverage.SchemeMapper
import EvmCoverage.DumpcasesParser
import Data.List
import Data.Maybe
import Data.SortedMap
import Coverage.Standardization.Types as CovStd

%default covering

-- =============================================================================
-- Hit Aggregation (OR semantics across test runs)
-- =============================================================================

||| Group consecutive elements by first component
groupConsecutive : List (BranchId, Nat) -> List (BranchId, List Nat)
groupConsecutive [] = []
groupConsecutive ((bid, cnt) :: rest) =
  let (same, different) = span (\(b, _) => b == bid) rest
      counts = cnt :: map snd same
  in (bid, counts) :: groupConsecutive different

||| Find maximum in a list
maxHits : List Nat -> Nat
maxHits [] = 0
maxHits (x :: xs) = foldl max x xs

||| Group hits by branch ID
groupByBranchId : List (BranchId, Nat) -> List (BranchId, List Nat)
groupByBranchId hits =
  let sorted = sortBy (\(a, _), (b, _) => compare a b) hits
  in groupConsecutive sorted

||| Merge multiple test runs with OR semantics
||| A branch is "hit" if it was hit in ANY test run
export
mergeTestRuns : List TestRunHits -> List (BranchId, Nat)
mergeTestRuns runs =
  let allHits = concatMap (.branchHits) runs
      grouped = groupByBranchId allHits
  in map (\(bid, counts) => (bid, maxHits counts)) grouped

-- =============================================================================
-- ProfileHit to BranchId Mapping
-- =============================================================================

||| Convert a single ProfileHit to branch hit
tobranchHit : ProfileHit -> Maybe (BranchId, Nat)
tobranchHit hit =
  if hit.hitCount == 0 || shouldExcludeDecoded hit.schemeFunc
    then Nothing
    else Just (schemeFuncToBranchId hit.schemeFunc hit.line, hit.hitCount)

||| Convert ProfileHit list to TestRunHits
export
profileHitsToTestRun : String -> List ProfileHit -> TestRunHits
profileHitsToTestRun testName hits =
  let branchHits = mapMaybe tobranchHit hits
  in MkTestRunHits testName branchHits

-- =============================================================================
-- Coverage Calculation
-- =============================================================================

||| Check if branch ID is in hit set
isHit : BranchId -> List BranchId -> Bool
isHit bid hitSet = any (== bid) hitSet

||| Calculate which canonical branches were hit
export
calculateHits : StaticBranchAnalysis -> List (BranchId, Nat) -> (Nat, List ClassifiedBranch)
calculateHits static hits =
  let hitSet = map fst hits
      canonical = filter (\b => isCanonical b.branchClass) static.allBranches
      (hitBranches, missedBranches) = partition (\b => isHit b.branchId hitSet) canonical
  in (length hitBranches, missedBranches)

isHitCounted : BranchId -> List (BranchId, Nat) -> Bool
isHitCounted bid hits =
  any (\(other, cnt) => other == bid && cnt > 0) hits

summarizeFunction : List (BranchId, Nat) -> (String, List ClassifiedBranch) -> BranchFunctionSummary
summarizeFunction mergedHits (fn, branches) =
  let canonical = filter (\b => isCanonical b.branchClass) branches
      hitCanonical = filter (\b => isHitCounted b.branchId mergedHits) canonical
      uncoveredIds =
        map (show . (.branchId)) $
          filter (\b => not (isHitCounted b.branchId mergedHits)) canonical
      bugCount = length $ filter (\b => isBug b.branchClass) branches
      unknownCount = length $ filter isUnknown branches
  in MkBranchFunctionSummary
       fn
       (length canonical)
       (length hitCanonical)
       uncoveredIds
       bugCount
       unknownCount
  where
    isUnknown : ClassifiedBranch -> Bool
    isUnknown b = case b.branchClass of
      BCUnknownCrash _ => True
      _ => False

sortSummariesByPriority : List BranchFunctionSummary -> List BranchFunctionSummary
sortSummariesByPriority [] = []
sortSummariesByPriority (x :: xs) = insert x (sortSummariesByPriority xs)
  where
    priority : BranchFunctionSummary -> Nat
    priority s =
      let uncovered = length s.uncoveredBranchIds
      in uncovered * 100 + s.bugBranches * 10 + s.unknownBranches

    insert : BranchFunctionSummary -> List BranchFunctionSummary -> List BranchFunctionSummary
    insert e [] = [e]
    insert e (y :: ys) =
      if priority e >= priority y then e :: y :: ys else y :: insert e ys

groupByFunctionName : List ClassifiedBranch -> List (String, List ClassifiedBranch)
groupByFunctionName branches = go branches []
  where
    insertOrUpdate : String -> ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    insertOrUpdate fn b [] = [(fn, [b])]
    insertOrUpdate fn b ((k, vs) :: rest) =
      if k == fn then (k, b :: vs) :: rest
      else (k, vs) :: insertOrUpdate fn b rest

    go : List ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    go [] acc = acc
    go (b :: bs) acc = go bs (insertOrUpdate b.branchId.funcName b acc)

export
buildFunctionSummaries : StaticBranchAnalysis -> List (BranchId, Nat) -> List BranchFunctionSummary
buildFunctionSummaries static mergedHits =
  let grouped = groupByFunctionName static.allBranches
      summaries = map (summarizeFunction mergedHits) grouped
      interesting =
        filter (\s => s.canonicalBranches > 0 &&
                      (length s.uncoveredBranchIds > 0 || s.bugBranches > 0 || s.unknownBranches > 0))
               summaries
  in sortSummariesByPriority interesting

||| Create aggregated coverage from static analysis and hits
export
aggregateCoverage : StaticBranchAnalysis -> List TestRunHits -> AggregatedCoverage
aggregateCoverage static runs =
  let mergedHits = mergeTestRuns runs
  in case calculateHits static mergedHits of
       (hitCount, uncovered) =>
         let tot = static.canonicalCount
             percent = calcCoveragePercent tot hitCount
             measurement = coverageMeasurementForBranches static.allBranches (map fst mergedHits)
             summaries = buildFunctionSummaries static mergedHits
             targets = topKTargetsFromBranches 10 uncovered
         in MkAggregatedCoverage
              tot
              hitCount
              static.bugsCount
              static.unknownCount
              percent
              defaultEvmExecutionProfileName
              measurement
              evmCoverageModelName
              evmUnknownPolicyName
              (coverageClaimAdmissibleForBranches static.allBranches)
              targets
              summaries
              uncovered

-- =============================================================================
-- Gap Generation
-- =============================================================================

||| Convert branch to uncovered gap
toUncoveredGap : ClassifiedBranch -> CoverageGap
toUncoveredGap cb =
  MkCoverageGap cb.branchId "Uncovered canonical branch" 3

||| Generate coverage gaps from uncovered branches
export
generateGaps : AggregatedCoverage -> List CoverageGap
generateGaps cov =
  let uncoveredGaps = map toUncoveredGap cov.uncoveredBranches
      bugGaps = mapMaybe branchToGap cov.uncoveredBranches
  in uncoveredGaps ++ bugGaps

-- =============================================================================
-- Summary Statistics
-- =============================================================================

||| Coverage summary for reporting
public export
record CoverageSummary where
  constructor MkCoverageSummary
  totalCanonical : Nat
  hitCanonical   : Nat
  coveragePercent : Double
  bugs           : Nat
  unknown        : Nat
  gapsCount      : Nat

public export
Show CoverageSummary where
  show s = """
Coverage Summary:
  Canonical: \{show s.hitCanonical}/\{show s.totalCanonical} (\{show s.coveragePercent}%)
  Bugs (UnhandledInput): \{show s.bugs}
  Unknown: \{show s.unknown}
  Gaps: \{show s.gapsCount}
"""

||| Create summary from aggregated coverage
export
toSummary : AggregatedCoverage -> CoverageSummary
toSummary cov =
  let gaps = generateGaps cov
  in MkCoverageSummary
       cov.canonicalTotal
       cov.canonicalHit
       cov.coveragePercent
       cov.bugsTotal
       cov.unknownTotal
       (length gaps)

-- =============================================================================
-- Threshold Checking
-- =============================================================================

||| Check if coverage meets threshold
export
meetsThreshold : Double -> AggregatedCoverage -> Bool
meetsThreshold threshold cov = cov.coveragePercent >= threshold

||| Calculate coverage delta from previous run
export
coverageDelta : AggregatedCoverage -> AggregatedCoverage -> Double
coverageDelta prev curr = curr.coveragePercent - prev.coveragePercent
