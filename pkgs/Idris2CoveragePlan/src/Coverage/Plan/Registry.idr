||| The COVERAGE FAMILY REGISTRY — the totality anchor.
|||
||| `coverageStatusFor : CoverageFamily -> CoverageStatus` is TOTAL with no catch-all
||| and a NON-Maybe codomain. Two compounding guarantees make "add a family, forget
||| coverage" impossible:
|||   1. ENUM-side: adding a `CoverageFamily` constructor makes this function
|||      non-covering → a COMPILE ERROR ("coverageStatusFor is not covering. Missing
|||      cases: <X>"). There is no wildcard to hide behind.
|||   2. CODOMAIN-side: the result is `CoverageStatus` (Implemented | DeclaredUnimplemented),
|||      NOT `Maybe CoverageImpl`, so the historical silent escape
|||      `lazyCoverageFamily (Humanoid _) = Nothing` is UNTYPEABLE. A family with no
|||      runnable backend MUST be `DeclaredUnimplemented` with a non-empty rationale
|||      (enforced by `test_no_silent_gap`), surfacing as a known, non-passing gap.
|||
||| The per-family `CoverageImpl` values are self-contained here (Core-only) so the
||| anchor doesn't fan in every heavy family package (evm/icwasm/react-native). Each
||| captures the family's join key, denominator backend, exclusion mechanism, and
||| production-safety witness — the four PathCoverage concepts. The family runner
||| packages (web/dfx/evm/android) keep their own richer machinery; this registry is
||| the single place that proves COMPLETENESS across families.
module Coverage.Plan.Registry

import Data.List

import Idris2.CoverageFamily
import Coverage.Core.PathCoverage   -- PathObligation (.pathId)
import Coverage.Core.Backend
import Coverage.Core.Exclusions
import Coverage.Standardization.InstrumentationSafety

%default total

-- ============================================================================
-- Per-family coverage backends (the four concepts, Core-only)
-- ============================================================================

||| Test-harness exclusion patterns shared by the string-identity MVU/library
||| families. (Family-specific supersets, e.g. evm's Yul rules, live in the family
||| package; this is the registry-level baseline used for the anchor's impl values.)
harnessPatterns : List ExclPattern
harnessPatterns =
  [ containsPattern ".Tests." "Test harness/spec module"
  , prefixPattern "Tests." "Test harness/spec module"
  , containsPattern "TestHarness" "Test harness shim"
  , prefixPattern "test_" "Test helper function"
  , exactPattern "Main.main" "Test runner entry point"
  , exactPattern "Main_runTests" "Test runner itself"
  ]

||| evm: forked dumppaths denominator (RefC, no cg flag), FNV-hash numerator join.
||| k = String at the registry level (the anchor only needs the family's denominator
||| + exclusion shape; evm's Integer-keyed hash join lives in EvmCoverage.PathRuntime).
||| Bare-only projection (historical). Production-safe by build separation (opt-in
||| EVM_PATHCOV_YUL fork; prod Yul has no LOG probe).
evmCoverageImpl : CoverageImpl String
evmCoverageImpl = MkCoverageImpl
  Nothing (ForkedIpkg "") (.pathId) (PureKeys [])
  (reclassifyArtifactsBare harnessPatterns)
  (AbsentByBuildSeparation "path-hit log1 emitted only under opt-in instrumentation (EVM_PATHCOV_YUL); prod Yul has no LOG probe")

||| dfx: forked dumppaths denominator (RefC, no cg flag), string-identity join,
||| bare-only projection (historical). Production-safe by build separation
||| (recordPathHit compiled only in the coverage fork; prod canister WASM omits it).
dfxCoverageImpl : CoverageImpl String
dfxCoverageImpl = MkCoverageImpl
  Nothing (ForkedIpkg "") (.pathId) (PureKeys [])
  (reclassifyArtifactsBare harnessPatterns)
  (AbsentByBuildSeparation "recordPathHit compiled only in the coverage fork build; prod canister WASM omits it")

||| web/android-MVU/iOS: forked dumppaths denominator with --cg node, string-identity
||| join, the WEB superset projection (bare + dotted). Production-safe by no-op hook.
webCoverageImpl : CoverageImpl String
webCoverageImpl = MkCoverageImpl
  (Just "node") (ForkedIpkg "") (.pathId) (PureKeys [])
  (reclassifyArtifacts harnessPatterns)
  (NoOpWithoutHook "globalThis.__idris2_recordPathHit is undefined in prod; the emitted call is a no-op unless the harness installs the hook")

||| android-device: pre-collected id-file denominator, string-identity join, PURE
||| (no forked build). Web-superset projection (the device exclusion also drops
||| dotted accessors). Production-safe by no-op (logcat lines inert without harness).
androidDeviceImpl : CoverageImpl String
androidDeviceImpl = MkCoverageImpl
  Nothing (PrebuiltIdFiles []) (.pathId) (PureKeys [])
  (reclassifyArtifacts harnessPatterns)
  (NoOpWithoutHook "logcat IDRIS_PATHHIT lines are inert unless the device harness reads them; the prod APK is unaffected")

||| core/CLI: forked dumppaths denominator (Chez/default), string-identity join,
||| bare-only projection. Production-safe by build separation (the --dumppathshits
||| build is separate from the shipped binary).
coreCoverageImpl : CoverageImpl String
coreCoverageImpl = MkCoverageImpl
  Nothing (ForkedIpkg "") (.pathId) (PureKeys [])
  (reclassifyArtifactsBare harnessPatterns)
  (AbsentByBuildSeparation "the --dumppathshits/--profile build is separate from the shipped Chez binary")

-- ============================================================================
-- The totality anchor
-- ============================================================================

||| The coverage story for every family. TOTAL, no catch-all, non-Maybe codomain —
||| adding a CoverageFamily constructor breaks this build until the new family
||| supplies a real CoverageImpl (Implemented) or a written rationale
||| (DeclaredUnimplemented). `= Nothing` is untypeable.
public export
coverageStatusFor : CoverageFamily -> CoverageStatus
coverageStatusFor EvmHash       = Implemented evmCoverageImpl
coverageStatusFor DfxWasm       = Implemented dfxCoverageImpl
coverageStatusFor WebMVU        = Implemented webCoverageImpl
coverageStatusFor AndroidDevice = Implemented androidDeviceImpl
coverageStatusFor CoreLib       = Implemented coreCoverageImpl
coverageStatusFor Humanoid      =
  DeclaredUnimplemented (MkUnimplementedReason
    "embodiment hit-stream (actuator/sensor path evidence) not yet specified"
    (Just "ETHERCLAW-humanoid-coverage"))

||| Is a family's coverage story a runnable backend (vs declared-unimplemented)?
public export
isImplemented : CoverageStatus -> Bool
isImplemented (Implemented _)           = True
isImplemented (DeclaredUnimplemented _) = False

||| A DeclaredUnimplemented family's rationale (for the report). Empty for the
||| Implemented arm — `test_no_silent_gap` requires a non-empty rationale for the
||| unimplemented arm, so an empty string here means "implemented", never a gap.
public export
statusRationale : CoverageStatus -> String
statusRationale (Implemented _)           = ""
statusRationale (DeclaredUnimplemented r) = r.rationale
