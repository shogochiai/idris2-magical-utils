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

-- =============================================================================
-- Canonical, Totality-anchored exclusion reasons (the single source of truth for
-- WHY a path leaves the coverage denominator). Each family classifier returns a
-- `Maybe ExclusionReason` rather than an arbitrary (ObligationClass, String): the
-- legitimate set is enumerated ONCE here, so no family can invent an ad-hoc
-- exclusion category (e.g. evm's former "Yul branch-label collapse"). There is NO
-- observability/medium-collapse ctor on purpose: under source-level path markers
-- every product branch is observable, so a family that wants to drop an observable
-- product branch simply CANNOT express it — the type has no constructor for it.
-- =============================================================================
public export
data ExclusionReason
  = NonProductModule       -- .Tests./.Storages./.Schema/wrapper       -> CompilerInsertedArtifact
  | StandardLibrary        -- prelude/stdlib                           -> CompilerInsertedArtifact
  | GeneratedProjection    -- record projection                       -> CompilerInsertedArtifact
  | SingleCtorDestructure  -- irrefutable destructure (not a branch)   -> CompilerInsertedArtifact
  | StraightLineClause     -- no branch obligation                    -> CompilerInsertedArtifact
  | ConstantFalseGuard     -- literal-false guard, logically dead     -> LogicallyUnreachable

public export
Show ExclusionReason where
  show NonProductModule      = "NonProductModule"
  show StandardLibrary       = "StandardLibrary"
  show GeneratedProjection   = "GeneratedProjection"
  show SingleCtorDestructure = "SingleCtorDestructure"
  show StraightLineClause    = "StraightLineClause"
  show ConstantFalseGuard    = "ConstantFalseGuard"

public export
Eq ExclusionReason where
  NonProductModule      == NonProductModule      = True
  StandardLibrary       == StandardLibrary       = True
  GeneratedProjection   == GeneratedProjection   = True
  SingleCtorDestructure == SingleCtorDestructure = True
  StraightLineClause    == StraightLineClause    = True
  ConstantFalseGuard    == ConstantFalseGuard    = True
  _ == _ = False

||| Canonical, single-source-of-truth mapping from exclusion reason to the
||| obligation class it lands in. Totality-checked: adding an ExclusionReason ctor
||| forces this match (and every family classifier) to handle it — no catch-all, so
||| a new category cannot silently shrink any denominator.
public export
reasonClass : ExclusionReason -> ObligationClass
reasonClass NonProductModule      = CompilerInsertedArtifact
reasonClass StandardLibrary       = CompilerInsertedArtifact
reasonClass GeneratedProjection   = CompilerInsertedArtifact
reasonClass SingleCtorDestructure = CompilerInsertedArtifact
reasonClass StraightLineClause    = CompilerInsertedArtifact
reasonClass ConstantFalseGuard    = LogicallyUnreachable

||| Stable, ctor-derived human reason (no free-form per-call drift). Whatever
||| detailed proof a family records elsewhere (e.g. WebCoverage.Exclusions.reason),
||| the classification reason itself comes from this single table.
public export
reasonString : ExclusionReason -> String
reasonString NonProductModule      = "non-product module (test/storage/schema/wrapper)"
reasonString StandardLibrary       = "standard library / prelude"
reasonString GeneratedProjection   = "generated record projection"
reasonString SingleCtorDestructure = "single-constructor destructure (irrefutable, not a branch)"
reasonString StraightLineClause    = "straight-line clause, no branch obligation"
reasonString ConstantFalseGuard    = "constant-false guard (logically unreachable)"

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
