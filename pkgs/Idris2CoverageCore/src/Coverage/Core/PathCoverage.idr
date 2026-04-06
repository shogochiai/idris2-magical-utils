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
showPathClassification UnknownClassification = "UnknownClassification"

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

public export
record PathCoverageResult where
  constructor MkPathCoverageResult
  allPaths        : List PathObligation
  coveredPaths    : List PathObligation
  missingPaths    : List PathObligation
  measurement     : CoverageMeasurement
  coveragePercent : Maybe Double
  coverageModel   : String
  claimAdmissible : Bool

renderPathCoverageResult : PathCoverageResult -> String
renderPathCoverageResult result =
  "PathCoverage: " ++ show (length (coveredIds result.measurement))
    ++ "/" ++ show (length (denominatorIds result.measurement))
    ++ " (" ++ show (fromMaybe 100.0 result.coveragePercent) ++ "%)"

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
pathCoverageMeasurement : List PathObligation -> List String -> CoverageMeasurement
pathCoverageMeasurement paths hitPathIds =
  let obligations = map pathObligationToCoverageObligation paths
      denominatorIds =
        map (.obligationId) $
          filter (\ob => countsAsDenominator ob.classification) obligations
      excludedIds =
        map (.obligationId) $
          filter (\ob => mustBeExcluded ob.classification) obligations
      unknownIds =
        map (.obligationId) $
          filter (\ob => ob.classification == UnknownClassification) obligations
      coveredIds =
        filter (\oid => elem oid denominatorIds) (nub hitPathIds)
  in MkCoverageMeasurement denominatorIds coveredIds excludedIds unknownIds

public export
buildPathCoverageResult : List PathObligation -> List String -> PathCoverageResult
buildPathCoverageResult paths hitPathIds =
  let measurement = pathCoverageMeasurement paths hitPathIds
      covered =
        filter (\path => elem path.pathId measurement.coveredIds) paths
      missing =
        filter (\path => elem path.pathId measurement.denominatorIds
                      && not (elem path.pathId measurement.coveredIds)) paths
      obligations = map pathObligationToCoverageObligation paths
  in MkPathCoverageResult
       paths
       covered
       missing
       measurement
       (coveragePct measurement)
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
