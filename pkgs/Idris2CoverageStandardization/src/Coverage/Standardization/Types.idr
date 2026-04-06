module Coverage.Standardization.Types

%default total

public export
data ObservationLayer
  = SourceSurface
  | ElaboratedCaseTree
  | RuntimeExecution
  | BackendArtifact

public export
Eq ObservationLayer where
  SourceSurface == SourceSurface = True
  ElaboratedCaseTree == ElaboratedCaseTree = True
  RuntimeExecution == RuntimeExecution = True
  BackendArtifact == BackendArtifact = True
  _ == _ = False

public export
data ObligationClass
  = ReachableObligation
  | LogicallyUnreachable
  | UserAdmittedPartialGap
  | CompilerInsertedArtifact
  | UnknownClassification

public export
Show ObligationClass where
  show ReachableObligation = "ReachableObligation"
  show LogicallyUnreachable = "LogicallyUnreachable"
  show UserAdmittedPartialGap = "UserAdmittedPartialGap"
  show CompilerInsertedArtifact = "CompilerInsertedArtifact"
  show UnknownClassification = "UnknownClassification"

public export
Eq ObligationClass where
  ReachableObligation == ReachableObligation = True
  LogicallyUnreachable == LogicallyUnreachable = True
  UserAdmittedPartialGap == UserAdmittedPartialGap = True
  CompilerInsertedArtifact == CompilerInsertedArtifact = True
  UnknownClassification == UnknownClassification = True
  _ == _ = False

public export
data ObligationGranularity
  = FunctionLevel
  | BranchLevel
  | PathLevel

public export
Show ObligationGranularity where
  show FunctionLevel = "FunctionLevel"
  show BranchLevel = "BranchLevel"
  show PathLevel = "PathLevel"

public export
Eq ObligationGranularity where
  FunctionLevel == FunctionLevel = True
  BranchLevel == BranchLevel = True
  PathLevel == PathLevel = True
  _ == _ = False

public export
data UnknownPolicy
  = BlockCoverageClaim
  | CountAsGap
  | ReportSeparately

public export
Eq UnknownPolicy where
  BlockCoverageClaim == BlockCoverageClaim = True
  CountAsGap == CountAsGap = True
  ReportSeparately == ReportSeparately = True
  _ == _ = False

public export
record CoverageObligation where
  constructor MkCoverageObligation
  obligationId : String
  layer : ObservationLayer
  granularity : ObligationGranularity
  summary : String
  provenanceTag : Maybe String
  classification : ObligationClass

public export
record CoverageMeasurement where
  constructor MkCoverageMeasurement
  denominatorIds : List String
  coveredIds : List String
  excludedIds : List String
  unknownIds : List String

public export
record SoundnessEnvelope where
  constructor MkSoundnessEnvelope
  exclusionRequiresSemanticUnreachability : Bool
  hitMappingRequiresStableObligationIds : Bool
  backendArtifactsAreOutsideCoverageModel : Bool
  unknownPolicy : UnknownPolicy

public export
countsAsDenominator : ObligationClass -> Bool
countsAsDenominator ReachableObligation = True
countsAsDenominator UserAdmittedPartialGap = True
countsAsDenominator LogicallyUnreachable = False
countsAsDenominator CompilerInsertedArtifact = False
countsAsDenominator UnknownClassification = False

public export
mustBeCovered : ObligationClass -> Bool
mustBeCovered ReachableObligation = True
mustBeCovered UserAdmittedPartialGap = True
mustBeCovered LogicallyUnreachable = False
mustBeCovered CompilerInsertedArtifact = False
mustBeCovered UnknownClassification = False

public export
mustBeExcluded : ObligationClass -> Bool
mustBeExcluded LogicallyUnreachable = True
mustBeExcluded CompilerInsertedArtifact = True
mustBeExcluded ReachableObligation = False
mustBeExcluded UserAdmittedPartialGap = False
mustBeExcluded UnknownClassification = False

public export
defaultSoundnessEnvelope : SoundnessEnvelope
defaultSoundnessEnvelope =
  MkSoundnessEnvelope
    True
    True
    True
    BlockCoverageClaim
