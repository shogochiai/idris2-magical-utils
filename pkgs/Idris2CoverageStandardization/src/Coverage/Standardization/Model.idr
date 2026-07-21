module Coverage.Standardization.Model

import Coverage.Standardization.Types

%default total

public export
record CoverageStandard where
  constructor MkCoverageStandard
  standardName : String
  granularity : ObligationGranularity
  numeratorDefinition : String
  denominatorDefinition : String
  admissibleLayers : List ObservationLayer
  soundnessEnvelope : SoundnessEnvelope

public export
semanticBranchObligationStandard : CoverageStandard
semanticBranchObligationStandard =
  MkCoverageStandard
    "Semantic Test Obligation Coverage (Branch-Level)"
    BranchLevel
    "Covered executable obligations observed at runtime and mapped back to stable source or elaborated obligation identifiers."
    "All source- or elaboration-level executable obligations, excluding logically unreachable obligations and non-semantic compiler artifacts."
    [SourceSurface, ElaboratedCaseTree, RuntimeExecution]
    defaultSoundnessEnvelope

public export
semanticFunctionObligationStandard : CoverageStandard
semanticFunctionObligationStandard =
  MkCoverageStandard
    "Semantic Test Obligation Coverage (Function-Level)"
    FunctionLevel
    "Covered executable function obligations observed at runtime and mapped back to stable source or elaborated function identifiers."
    "All source- or elaboration-level executable function obligations, excluding functions whose branches are entirely logically unreachable and non-semantic compiler artifacts."
    [SourceSurface, ElaboratedCaseTree, RuntimeExecution]
    defaultSoundnessEnvelope

public export
semanticPathObligationStandard : CoverageStandard
semanticPathObligationStandard =
  MkCoverageStandard
    "Semantic Test Obligation Coverage (Path-Level)"
    PathLevel
    "Covered executable paths observed at runtime and mapped back to stable elaborated path obligation identifiers."
    "All source- or elaboration-level executable path obligations, excluding logically unreachable paths and non-semantic compiler artifacts."
    [SourceSurface, ElaboratedCaseTree, RuntimeExecution]
    defaultSoundnessEnvelope

public export
semanticTestObligationStandard : CoverageStandard
semanticTestObligationStandard = semanticBranchObligationStandard

public export
isCoverageClaimAdmissible : CoverageStandard -> List CoverageObligation -> Bool
isCoverageClaimAdmissible standard obligations =
  let env = soundnessEnvelope standard in
    allExpectedGranularity obligations &&
      case unknownPolicy env of
        BlockCoverageClaim => allKnown obligations
        CountAsGap => True
        ReportSeparately => True
  where
    allExpectedGranularity : List CoverageObligation -> Bool
    allExpectedGranularity [] = True
    allExpectedGranularity (obligation :: rest) =
      obligation.granularity == standard.granularity
        && allExpectedGranularity rest

    -- An obligation makes the claim inadmissible iff its class `blocksClaim`
    -- (UnknownClassification or StubbedReach). Using the totality-anchored
    -- `blocksClaim` rather than a `case ... of X => False; _ => True` means a new
    -- ObligationClass ctor cannot slip past this gate as silently "known".
    allKnown : List CoverageObligation -> Bool
    allKnown [] = True
    allKnown (obligation :: rest) =
      if blocksClaim (classification obligation)
        then False
        else allKnown rest

||| The one honest divide over raw evidence counts, for consumers that parsed
||| `paths_hit` / `paths_denominator` / `paths_unknown` out of a v2 report:
||| percent = hit / (denominator + unknown). Nothing when there is nothing to
||| measure — an empty measurement is UNMEASURED, not 100%.
public export
evidencePercent : (hit : Nat) -> (denominator : Nat) -> (unknown : Nat) -> Maybe Double
evidencePercent hit denominator unknown =
  case denominator + unknown of
    Z          => Nothing
    honest@(S _) => Just (100.0 * cast hit / cast honest)

||| THE canonical percentage — computed by CONSUMERS, never emitted by producers.
||| Honest denominator: admissible obligations PLUS untriaged (unknown-limbo)
||| ones. An untriaged path counts AGAINST coverage until it is either covered
||| or justified into an excluded class — so "classify as Unknown to shrink the
||| denominator" (the live evm incident: 4-of-158 admissible, reported 50% for
||| an honest 1.2%) is self-defeating rather than an over-report. `unknownIds`
||| is the claim-blocking set and CONTAINS StubbedReach, which already sits in
||| the denominator — subtract the overlap so nothing is double-counted.
||| Nothing = nothing to measure (0 obligations), which is NOT 100%: renderers
||| must surface it as unmeasured, never default it.
public export
coveragePct : CoverageMeasurement -> Maybe Double
coveragePct measurement =
  evidencePercent coveredSize denominatorSize limboSize
  where
    denominatorSize : Nat
    denominatorSize = length (denominatorIds measurement)

    coveredSize : Nat
    coveredSize = length (coveredIds measurement)

    limboSize : Nat
    limboSize = length (filter (\i => not (elem i (denominatorIds measurement)))
                               (unknownIds measurement))
