||| Android real-device coverage, expressed as a Coverage.Core.Backend CoverageImpl
||| so the on-device family plugs into the SAME totality anchor as web/dfx/evm.
|||
||| Android device is the PURE family: the harness has already collected the
||| denominator (dumppaths path_ids from a file) and the numerator (on-device hit
||| path_ids from logcat) as flat `List String`, so there is no forked build and no
||| IO evidence step. It uses `DenominatorSource.PrebuiltIdFiles` and
||| `EvidenceSource.PureKeys`, and a string-identity join on `.pathId` — the same
||| `coveredByKey` Core uses for web/dfx (k = String).
|||
||| The pure `PathCoverage.pathCoverage`/`PathCov`/`report` stay as-is (the device
||| report bytes are preserved); this module is the additive adapter that lifts the
||| flat id lists into the shared `PathObligation` model so the ONE reclassifier +
||| `coveredByKey` + `buildPathCoverageResultFromHits` apply unchanged.
module Android.Coverage.Backend

import Data.List
import Data.String

import Android.Coverage.PathCoverage   -- isExcludedFn (the device exclusion policy)
import Coverage.Core.PathCoverage
import Coverage.Core.RuntimeHit
import Coverage.Core.Backend
import Coverage.Standardization.Types
import Coverage.Standardization.InstrumentationSafety

%default total

||| The function-name portion of a path_id ("Mod.fn#pN" -> "Mod.fn"). (Mirrors the
||| private PathCoverage.funcOf; the device exclusion keys on the function name.)
funcOf : String -> String
funcOf pid = case span (/= '#') (unpack pid) of (f, _) => pack f

||| Lift a flat device path_id into the shared obligation model. Each id becomes a
||| minimal ReachableObligation; the device's name-based `isExcludedFn` is then run
||| by the reclassifier (below) to demote excluded ids to CompilerInsertedArtifact,
||| so `countsAsDenominator` drops them — the SAME denominator the pure
||| `pathCoverage` computes by filtering, now expressed via the shared pipeline.
export
androidIdToObligation : String -> PathObligation
androidIdToObligation pid =
  MkPathObligation pid (funcOf pid) "" ReachableObligation
                   "reached_clause" Nothing [] Nothing 0

||| Reclassify obligations whose function name matches the device exclusion policy
||| (`isExcludedFn`: synthetic helpers, record accessors, Eq-== arms, test fns,
||| library prefixes). Built on the shared `reclassifyByClassifier`, so device and
||| host families share the exclusion MECHANISM even though the device's predicate
||| is name-based (it has no PathObligation structure — flat ids only).
export
reclassifyDeviceExclusions : List PathObligation -> List PathObligation
reclassifyDeviceExclusions =
  reclassifyByClassifier classify
  where
    classify : PathObligation -> Maybe ExclusionReason
    classify p =
      if isExcludedFn p.functionName
        then Just NonProductModule  -- lib/synthetic/accessor/test -> CompilerInsertedArtifact
        else Nothing

||| The android REAL-DEVICE coverage backend. PURE: PrebuiltIdFiles + PureKeys,
||| string-identity join, the device-name reclassifier, and a no-op-without-hook
||| safety witness (logcat IDRIS_PATHHIT lines are inert unless the device harness
||| reads them). `buildDenominator`/`gatherEvidence` are filled per run with the
||| harness's id lists (already lifted via androidIdToObligation / used as-is).
export
androidDeviceCoverageImpl : CoverageImpl String
androidDeviceCoverageImpl = MkCoverageImpl
  Nothing                           -- cgFlag: device family does not build
  (PrebuiltIdFiles [])              -- buildDenominator: filled per run (dumppaths id file)
  (.pathId)                         -- joinKey: string identity
  (PureKeys [])                     -- gatherEvidence: filled per run (logcat hit ids), PURE
  reclassifyDeviceExclusions        -- EXCLUSIONS: device name policy via shared mechanism
  (NoOpWithoutHook "logcat IDRIS_PATHHIT lines are inert unless the device harness reads them; the prod APK is unaffected")

||| Convenience: compute the shared PathCoverageResult from the harness's raw id
||| lists (denominator path_ids + on-device hit path_ids), going through the SAME
||| reclassify → coveredByKey → buildPathCoverageResultFromHits pipeline as every
||| other family. Pure (no IO) — android device is the pure family.
export
deviceCoverageResult : (denomIds : List String) -> (hitIds : List String)
                    -> PathCoverageResult
deviceCoverageResult denomIds hitIds =
  let obligations  = map androidIdToObligation (nub denomIds)
      reclassified = androidDeviceCoverageImpl.reclassify obligations
      hits         = hitsByKey androidDeviceCoverageImpl.joinKey (nub hitIds) reclassified
  in buildPathCoverageResultFromHits reclassified hits
