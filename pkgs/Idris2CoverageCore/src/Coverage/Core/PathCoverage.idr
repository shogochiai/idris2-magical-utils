||| Shared path-obligation types and coverage aggregation.
module Coverage.Core.PathCoverage

import Data.List
import Data.Maybe
import Data.String

import Coverage.Core.Types
import Coverage.Core.RuntimeHit
import Coverage.Standardization.Types
import Coverage.Standardization.Model

%default total

public export
record PathStep where
  constructor MkPathStep
  nodeId      : String
  branchIndex : Nat
  origin      : String
  caseIndex   : Maybe Nat
  branchLabel : Maybe String
  sourceSpan  : Maybe String

public export
Show PathStep where
  show step =
    let label = fromMaybe "branch" step.branchLabel in
    step.nodeId ++ ":" ++ show step.branchIndex ++ ":" ++ label

public export
Eq PathStep where
  a == b =
       a.nodeId == b.nodeId
    && a.branchIndex == b.branchIndex
    && a.origin == b.origin
    && a.caseIndex == b.caseIndex
    && a.branchLabel == b.branchLabel
    && a.sourceSpan == b.sourceSpan

public export
record PathObligation where
  constructor MkPathObligation
  pathId           : String
  functionName     : String
  moduleName       : String
  classification   : ObligationClass
  terminalKind     : String
  terminalClauseId : Maybe Nat
  steps            : List PathStep
  sourceSpanUnion  : Maybe String
  pathLength       : Nat

showPathClassification : ObligationClass -> String
showPathClassification ReachableObligation = "ReachableObligation"
showPathClassification LogicallyUnreachable = "LogicallyUnreachable"
showPathClassification UserAdmittedPartialGap = "UserAdmittedPartialGap"
showPathClassification CompilerInsertedArtifact = "CompilerInsertedArtifact"
showPathClassification ExternalEffectBoundary = "ExternalEffectBoundary"
showPathClassification UnknownClassification = "UnknownClassification"
showPathClassification StubbedReach = "StubbedReach"

public export
Show PathObligation where
  show path =
    path.pathId ++ " [" ++ showPathClassification path.classification ++ "] "
      ++ path.functionName ++ " -> " ++ path.terminalKind

public export
Eq PathObligation where
  a == b = a.pathId == b.pathId

public export
pathSummary : PathObligation -> String
pathSummary path =
  let stepLabels = map (\step => fromMaybe ("branch#" ++ show step.branchIndex) step.branchLabel) path.steps
  in path.functionName ++ " :: " ++ joinBy " -> " stepLabels ++ " => " ++ path.terminalKind

public export
pathObligationToCoverageObligation : PathObligation -> CoverageObligation
pathObligationToCoverageObligation path =
  MkCoverageObligation
    path.pathId
    ElaboratedCaseTree
    PathLevel
    (pathSummary path)
    (Just path.functionName)
    path.classification

||| The path-coverage result a producer may report. DELIBERATELY carries NO
||| percentage field: a producer that could write its own percent could also lie
||| with it (live incidents: dfx smoke echoed `coverage_percent: 100.0` with no
||| path denominator at all; evm shrank the denominator to a 4-of-158 admissible
||| subset and reported 50% for what is honestly 1.2%; several renderers turned
||| an unmeasurable `Nothing` into 100.0 via `fromMaybe`). Producers emit RAW
||| EVIDENCE (path lists + bucket counts, below); the CONSUMER computes any
||| percentage in one place from those counts. Deception by percent is now
||| unrepresentable in this record rather than merely discouraged.
public export
record PathCoverageResult where
  constructor MkPathCoverageResult
  allPaths        : List PathObligation
  coveredPaths    : List PathObligation
  missingPaths    : List PathObligation
  measurement     : CoverageMeasurement
  coverageModel   : String
  claimAdmissible : Bool

renderPathCoverageResult : PathCoverageResult -> String
renderPathCoverageResult result =
  "PathCoverage: " ++ show (length (coveredIds result.measurement))
    ++ "/" ++ show (length (denominatorIds result.measurement))
    ++ " of " ++ show (length result.allPaths) ++ " paths"

public export
Show PathCoverageResult where
  show = renderPathCoverageResult

public export
Eq PathCoverageResult where
  a == b =
       a.allPaths == b.allPaths
    && a.coveredPaths == b.coveredPaths
    && a.missingPaths == b.missingPaths
    && a.measurement.denominatorIds == b.measurement.denominatorIds
    && a.measurement.coveredIds == b.measurement.coveredIds

public export
filterPaths : (PathObligation -> Bool) -> List PathObligation -> List PathObligation
filterPaths predicate = filter predicate

public export
dedupePathsById : List PathObligation -> List PathObligation
dedupePathsById paths = reverse (go [] [] paths)
  where
    go : List String -> List PathObligation -> List PathObligation -> List PathObligation
    go _ acc [] = acc
    go seen acc (path :: rest) =
      if elem path.pathId seen
         then go seen acc rest
         else go (path.pathId :: seen) (path :: acc) rest

public export
pathCoverageMeasurement : List PathObligation -> List String -> CoverageMeasurement
pathCoverageMeasurement paths hitPathIds =
  let uniquePaths = dedupePathsById paths
      obligations = map pathObligationToCoverageObligation uniquePaths
      denominatorIds =
        nub $ map (.obligationId) $
          filter (\ob => countsAsDenominator ob.classification) obligations
      excludedIds =
        nub $ map (.obligationId) $
          filter (\ob => mustBeExcluded ob.classification) obligations
      -- claim-blocking ids: any class whose `blocksClaim` is True — currently
      -- UnknownClassification (untriaged foreign prim) and StubbedReach (stub/spy
      -- hit). Using `blocksClaim` not `== UnknownClassification` keeps this gate
      -- totality-anchored: a new blocking class cannot slip past silently.
      unknownIds =
        nub $ map (.obligationId) $
          filter (\ob => blocksClaim ob.classification) obligations
      coveredIds =
        filter (\oid => elem oid denominatorIds) (nub hitPathIds)
  in MkCoverageMeasurement denominatorIds coveredIds excludedIds unknownIds

public export
buildPathCoverageResult : List PathObligation -> List String -> PathCoverageResult
buildPathCoverageResult paths hitPathIds =
  let uniquePaths = dedupePathsById paths
      measurement = pathCoverageMeasurement uniquePaths hitPathIds
      covered =
        filter (\path => elem path.pathId measurement.coveredIds) uniquePaths
      missing =
        filter (\path => elem path.pathId measurement.denominatorIds
                      && not (elem path.pathId measurement.coveredIds)) uniquePaths
      obligations = map pathObligationToCoverageObligation uniquePaths
  in MkPathCoverageResult
       uniquePaths
       covered
       missing
       measurement
       (standardName semanticPathObligationStandard)
       (isCoverageClaimAdmissible semanticPathObligationStandard obligations)

public export
buildPathCoverageResultFromHits : List PathObligation -> List PathRuntimeHit -> PathCoverageResult
buildPathCoverageResultFromHits paths hits =
  buildPathCoverageResult paths (coveredPathIds hits)

public export
missingPathIds : PathCoverageResult -> List String
missingPathIds result = map (.pathId) result.missingPaths

public export
missingPathSummaries : PathCoverageResult -> List String
missingPathSummaries result = map pathSummary result.missingPaths

public export
parseQualifiedFunction : String -> (String, String)
parseQualifiedFunction = parseFullName

-- =============================================================================
-- Canonical raw-evidence report (the ONE step4 path contract emitter)
-- =============================================================================

||| Bucket counts derived from a result. The buckets PARTITION the enumerated
||| paths by ObligationClass — denominator {Reachable, UserAdmittedPartialGap,
||| StubbedReach}, excluded {LogicallyUnreachable, CompilerInsertedArtifact,
||| ExternalEffectBoundary}, unknown-limbo {UnknownClassification} — so the
||| conservation law `total = denominator + excluded + unknown` holds BY
||| CONSTRUCTION and a consumer can verify it at parse time. `unknown` here is
||| the limbo bucket (untriaged), NOT the claim-blocking set: StubbedReach
||| blocks the claim but sits in the denominator, so using `unknownIds`
||| (= blocksClaim set) directly would double-count it.
public export
record EvidenceCounts where
  constructor MkEvidenceCounts
  pathsTotal       : Nat
  pathsDenominator : Nat
  pathsHit         : Nat
  pathsExcluded    : Nat
  pathsUnknown     : Nat

public export
evidenceCounts : PathCoverageResult -> EvidenceCounts
evidenceCounts result =
  let denomIds = denominatorIds result.measurement
      exclIds  = excludedIds result.measurement
      limbo    = filter
                   (\p => not (elem p.pathId denomIds) && not (elem p.pathId exclIds))
                   result.allPaths
  in MkEvidenceCounts
       (length result.allPaths)
       (length denomIds)
       (length (coveredIds result.measurement))
       (length exclIds)
       (length limbo)

||| The conservation law a consumer re-checks after parsing: buckets partition
||| the enumeration and the hits fit inside the denominator. Exported so tests
||| (and the soundness preflight) can assert it against any produced result.
public export
evidenceConserved : EvidenceCounts -> Bool
evidenceConserved c =
     c.pathsTotal == c.pathsDenominator + c.pathsExcluded + c.pathsUnknown
  && c.pathsHit <= c.pathsDenominator

||| THE canonical textual step4 path-evidence report. Every path-coverage
||| producer (core/evm/web/dfx cov Mains) renders through here — no hand-rolled
||| putStrLn blocks — so no producer code path can emit a `coverage_percent`
||| line (there is none). The consumer computes any percentage from the counts.
||| `headerLabel` names the family surface ("", "EVM ", "Web ", "DFX ").
public export
renderPathEvidence : (headerLabel : String) -> PathCoverageResult -> String
renderPathEvidence headerLabel result =
  let c = evidenceCounts result
      unknownPaths = filter
                       (\p => not (elem p.pathId (denominatorIds result.measurement))
                           && not (elem p.pathId (excludedIds result.measurement)))
                       result.allPaths
  in joinBy "\n" $
       [ "# " ++ headerLabel ++ "Path Coverage Report"
       , "coverage_model:   " ++ result.coverageModel
       , "claim_admissible: " ++ show result.claimAdmissible
       , "paths_total: " ++ show c.pathsTotal
       , "paths_denominator: " ++ show c.pathsDenominator
       , "paths_hit: " ++ show c.pathsHit
       , "paths_excluded: " ++ show c.pathsExcluded
       , "paths_unknown: " ++ show c.pathsUnknown
       , ""
       , "Missing paths: " ++ show (length result.missingPaths)
       ]
       ++ map (\p => "- " ++ p.pathId ++ " :: " ++ pathSummary p) result.missingPaths
       ++ (if null unknownPaths
             then []
             else ("Unknown paths: " ++ show (length unknownPaths))
                  :: map (\p => "- " ++ p.pathId ++ " :: " ++ pathSummary p) unknownPaths)

||| Canonical JSON form of the same evidence (again: counts and lists only, no
||| percent field to fake).
public export
pathEvidenceToJson : PathCoverageResult -> String
pathEvidenceToJson result =
  let c = evidenceCounts result
  in joinBy "\n"
       [ "{"
       , "  \"coverage_model\": " ++ show result.coverageModel ++ ","
       , "  \"claim_admissible\": " ++ (if result.claimAdmissible then "true" else "false") ++ ","
       , "  \"paths_total\": " ++ show c.pathsTotal ++ ","
       , "  \"paths_denominator\": " ++ show c.pathsDenominator ++ ","
       , "  \"paths_hit\": " ++ show c.pathsHit ++ ","
       , "  \"paths_excluded\": " ++ show c.pathsExcluded ++ ","
       , "  \"paths_unknown\": " ++ show c.pathsUnknown ++ ","
       , "  \"missing_path_ids\": [" ++ joinBy ", " (map (show . (.pathId)) result.missingPaths) ++ "]"
       , "}"
       ]
