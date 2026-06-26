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
  | ExternalEffectBoundary
    -- A path whose function transitively reaches a RECOGNISED external effect hole
    -- (ProcessSpawn / NetworkOutcall / CanisterCall / FileSystemIO) — a COMPILER
    -- FACT (a ForeignDef with a matching cc string on a refersTo path). The harness
    -- genuinely cannot execute it (it cannot spawn git, open a socket, call a
    -- canister, or touch the filesystem in a pure test), so it is a KNOWN, deliberate
    -- exclusion — distinct from UnknownClassification, which is reserved for an
    -- UNRECOGNISED foreign prim that must be triaged. Excluded, non-blocking, not in
    -- the denominator. Reaching a recognised hole is neither a compiler artifact nor
    -- logically dead, so it gets its own honest class rather than being folded into
    -- CompilerInsertedArtifact or wrongly blocking the claim as "unknown".
  | UnknownClassification

public export
Show ObligationClass where
  show ReachableObligation = "ReachableObligation"
  show LogicallyUnreachable = "LogicallyUnreachable"
  show UserAdmittedPartialGap = "UserAdmittedPartialGap"
  show CompilerInsertedArtifact = "CompilerInsertedArtifact"
  show ExternalEffectBoundary = "ExternalEffectBoundary"
  show UnknownClassification = "UnknownClassification"

public export
Eq ObligationClass where
  ReachableObligation == ReachableObligation = True
  LogicallyUnreachable == LogicallyUnreachable = True
  UserAdmittedPartialGap == UserAdmittedPartialGap = True
  CompilerInsertedArtifact == CompilerInsertedArtifact = True
  ExternalEffectBoundary == ExternalEffectBoundary = True
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

-- ===========================================================================
-- EffectBoundary: FACT-grounded harness-unexecutability (NOT value/judgment)
-- ===========================================================================
-- A path is honestly excludable from the test-coverage denominator only when it
-- is UNEXECUTABLE in the test harness for a reason the OBSERVER CANNOT MOVE.
-- "value is low" (a human weight) and "it's IO" (a human declaration) both fail
-- this: they are observer judgments and degenerate into a denominator trick.
--
-- The trick-proof basis is the EFFECT BOUNDARY a path reaches: which hole in the
-- program's FFI/IO surface it transitively opens. That reachability is a COMPILER
-- FACT (call-graph; ideally a TYPE fact once the boundary resource is linear —
-- `runProc : Argv -> IO (Res Output)` makes "consumes Res" type-checkable). A
-- path that reaches ProcessSpawn / NetworkOutcall / CanisterCall cannot run in
-- the Chez/node harness (no process / no network / no replica); a PureComputation
-- path can. The witness (reachVia: the call chain to the %foreign primitive) is
-- emitted by instrumentation, never asserted by a human.
--
-- Totality anchor: adding a boundary kind forces every match below to answer
-- "is this harness-excludable?" — a new hole cannot silently widen the excluded
-- set. This is the ExclusionReason discipline applied to EXECUTABILITY.
public export
data EffectBoundary
  = PureComputation        -- opens no hole: stays in the denominator, must be tested
  | ProcessSpawn           -- reaches popen2/runProc: no process in harness
  | NetworkOutcall         -- reaches HTTP/RPC outcall: no network in harness
  | CanisterCall           -- reaches IC inter-canister call: no replica in harness
  | FileSystemIO           -- reaches real fs read/write outside a fixture
  -- SOUNDNESS ctor: reaches a %foreign whose calling-convention is NOT one of the
  -- known boundary primitives above. Without this, an unrecognised FFI hole would
  -- silently fall through to PureComputation (an UN-SOUND miss — a harness-
  -- unexecutable path mistaken for a testable one). Carrying the cc string keeps
  -- it VISIBLE: a new external hole appears as UnclassifiedForeign "<cc>" and must
  -- be triaged (promoted to a precise ctor or confirmed harness-safe), never lost.
  | UnclassifiedForeign String

public export
Show EffectBoundary where
  show PureComputation         = "PureComputation"
  show ProcessSpawn            = "ProcessSpawn"
  show NetworkOutcall          = "NetworkOutcall"
  show CanisterCall            = "CanisterCall"
  show FileSystemIO            = "FileSystemIO"
  show (UnclassifiedForeign c) = "UnclassifiedForeign(" ++ c ++ ")"

public export
Eq EffectBoundary where
  PureComputation == PureComputation = True
  ProcessSpawn    == ProcessSpawn    = True
  NetworkOutcall  == NetworkOutcall  = True
  CanisterCall    == CanisterCall    = True
  FileSystemIO    == FileSystemIO    = True
  (UnclassifiedForeign a) == (UnclassifiedForeign b) = a == b
  _               == _               = False

||| Is a path reaching this boundary honestly excludable from the test-coverage
||| denominator? True ONLY for genuinely harness-unexecutable holes. Totality-
||| checked: a new EffectBoundary ctor breaks this until classified, so a hole
||| can never be silently added to the excluded set.
public export
boundaryExcludable : EffectBoundary -> Bool
boundaryExcludable PureComputation        = False  -- no hole → testable → stays in denom
boundaryExcludable ProcessSpawn           = True
boundaryExcludable NetworkOutcall         = True
boundaryExcludable CanisterCall           = True
boundaryExcludable FileSystemIO           = True
boundaryExcludable (UnclassifiedForeign _) = True  -- unknown hole → SAFE side: excludable,
                                                   -- but classified Unknown (visible) below

||| The ObligationClass a harness-unexecutable boundary path reclassifies to.
||| Reaching an external hole is not a compiler artifact and not logically dead —
||| it is reachable product logic the HARNESS cannot drive. UnknownClassification
||| keeps it honest (visible, claim-affecting) rather than vanishing it.
public export
boundaryClass : EffectBoundary -> ObligationClass
boundaryClass PureComputation         = ReachableObligation
-- RECOGNISED external effect holes: a known, deliberate, non-blocking exclusion
-- (the harness cannot spawn a process, open a socket, call a canister, or touch
-- the filesystem in a pure test). These are NOT "unknown" — the compiler named the
-- hole — so they get the dedicated ExternalEffectBoundary class instead of wrongly
-- blocking the coverage claim as UnknownClassification.
boundaryClass ProcessSpawn            = ExternalEffectBoundary
boundaryClass NetworkOutcall          = ExternalEffectBoundary
boundaryClass CanisterCall            = ExternalEffectBoundary
boundaryClass FileSystemIO            = ExternalEffectBoundary
-- UNRECOGNISED foreign prim: genuinely unknown — stays UnknownClassification so it
-- surfaces (blocks the claim) and gets triaged onto the benign or boundary lists.
boundaryClass (UnclassifiedForeign _) = UnknownClassification

||| The %foreign primitive a boundary funnels through — the documented hole. Used
||| to validate that a path's claimed boundary really does reach this primitive.
public export
boundaryPrimitive : EffectBoundary -> String
boundaryPrimitive PureComputation         = ""
boundaryPrimitive ProcessSpawn            = "popen2"
boundaryPrimitive NetworkOutcall          = "http_request"
boundaryPrimitive CanisterCall            = "ic0.call_new"
boundaryPrimitive FileSystemIO            = "openFile"
boundaryPrimitive (UnclassifiedForeign c) = c

-- ===========================================================================
-- EffectBoundarySpec: the PLUGGABLE boundary policy (canonical + fork-extensible)
-- ===========================================================================
-- EffectBoundary's `boundaryExcludable`/`boundaryPrimitive` above are GLOBAL
-- defaults, but the SAME hole differs per delivery backend: "what cc opens a hole"
-- (popen2 on Chez vs child_process on node vs a Yul opcode on EVM) and "is that
-- hole harness-unexecutable" (a precompile is real-unrunnable on Chez but RUNNABLE
-- under revm) are BOTH backend-specific. So the policy must be parameterised by the
-- coverage family — and, to allow free development (etherclaw forks shipping their
-- own backend/runner), it must be PLUGGABLE, not a closed match in core.
--
-- EffectBoundarySpec is plain DATA so an external plugin can construct its own set
-- for a fork's family without editing core. The canonical families' specs are
-- provided by a Totality-anchored table (in a coverage package that can see
-- CoverageFamily); forks add a value with their own familyTag.
--
-- TRICK-PROOF CAVEAT (the open question this surfaces): `excludable` here is still
-- a Bool a plugin asserts. A plugin could declare a real product hole "excludable"
-- to shrink its denominator — the value-weighting trick, returned. The honest
-- endpoint keeps `excludable` a fact: a hole is excludable iff that family's runtime
-- cannot construct its boundary-resource (a linear `Res` whose ctor is absent from
-- that backend's primitive set). Until that linear grounding lands, a plugin's
-- `excludable` MUST be auditable: the spec carries the cc it matches and the family
-- tag, so a reviewer sees exactly what a fork excluded and why.
public export
record EffectBoundarySpec where
  constructor MkEffectBoundarySpec
  ||| Which coverage family (canonical tag like "evm"/"dfx"/"web"/"core", or a
  ||| fork's own tag) this spec applies to. Open string → forks are not gated.
  familyTag    : String
  ||| The boundary kind a matching hole reaches (drives boundaryClass).
  boundary     : EffectBoundary
  ||| Substrings of a %foreign calling-convention that identify this hole on THIS
  ||| backend (e.g. ["popen2"] on Chez, ["child_process"] on node). A ForeignDef
  ||| whose cc contains one of these → `boundary`.
  ccSubstrings : List String
  ||| Is a path reaching this hole harness-unexecutable for THIS family's runner?
  ||| (Chez: a precompile is N/A; revm: a precompile is RUNNABLE → not excludable.)
  ||| AUDIT, do not trust blindly: see the trick-proof caveat above.
  excludable   : Bool

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
countsAsDenominator ExternalEffectBoundary = False
countsAsDenominator UnknownClassification = False

public export
mustBeCovered : ObligationClass -> Bool
mustBeCovered ReachableObligation = True
mustBeCovered UserAdmittedPartialGap = True
mustBeCovered LogicallyUnreachable = False
mustBeCovered CompilerInsertedArtifact = False
mustBeCovered ExternalEffectBoundary = False
mustBeCovered UnknownClassification = False

public export
mustBeExcluded : ObligationClass -> Bool
mustBeExcluded LogicallyUnreachable = True
mustBeExcluded CompilerInsertedArtifact = True
mustBeExcluded ExternalEffectBoundary = True
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
