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

    allKnown : List CoverageObligation -> Bool
    allKnown [] = True
    allKnown (obligation :: rest) =
      case classification obligation of
        UnknownClassification => False
        _ => allKnown rest

public export
coveragePct : CoverageMeasurement -> Maybe Double
coveragePct measurement =
  case denominatorSize of
    Z => Nothing
    S _ => Just (100.0 * cast coveredSize / cast denominatorSize)
  where
    denominatorSize : Nat
    denominatorSize = length (denominatorIds measurement)

    coveredSize : Nat
    coveredSize = length (coveredIds measurement)
