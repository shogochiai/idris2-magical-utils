||| Canonical path-id coverage collector for web/android/ios (ES/JS engine).
|||
||| The forked compiler's `--dumppathshits` pass injects, at every CaseTree leaf,
||| a call to `globalThis.__idris2_recordPathHit("<fn>#p<n>")` (the ES backend's
||| lowering of `prim__recordPathHit`; see idrislang-idris2 Compiler/ES/Codegen.idr).
||| A JS harness installs that hook, runs the instrumented bundle in a real JS
||| engine (node / Hermes / JSC), and writes the recorded canonical path-ids.
|||
||| Those ids are the SAME strings as the `--dumppaths-json` `path_id` (the
||| denominator), so coverage is an exact set intersection — no source map,
||| byte-ratio, ordinal, or span heuristic. This is the ES/JS twin of
||| DfxCoverage.PathRuntime.analyzePathHitsFromPathIds (RefC/canister) and the
||| same shared `buildPathCoverageResult` produces coverage_percent /
||| claim_admissible / Missing paths.
module WebCoverage.PathHitCollector

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import Coverage.Core.DumppathsJson
import Coverage.Core.PathCoverage
import Coverage.Core.Types
import Coverage.Core.Exclusions
import Coverage.Core.Backend
import Coverage.Standardization.InstrumentationSafety
import public Coverage.Core.RuntimeHit

%default covering

-- =============================================================================
-- Product denominator filtering (mirrors DfxCoverage.PathCoverage)
-- =============================================================================
-- The web denominator is the forTestBuild test universe (the test ipkg), so it
-- contains (a) the test-harness driver functions themselves and (b) the
-- compiler's auto-generated record projections. Neither is product MVU logic
-- under test, so — exactly as dfx/evm do — they are removed from the
-- denominator (NOT reclassified, NOT hidden behind a byte ratio). What remains
-- is genuine Model/Msg/update/view obligations. Honest exclusion, with reasons.

||| Test-harness / non-product exclusion patterns for web/android/ios. Same
||| policy as DfxCoverage icpDefaultExclusions' Tests.* rule and evm's
||| isNonProductEvmFunction, restricted to the family-neutral subset (no ICP/WASM
||| /libc patterns). The MVU app's own modules (Model/Msg/Update/View) are kept.
export
webPathExclusions : List ExclPattern
webPathExclusions =
  [ containsPattern ".Tests." "Test harness/spec module (not product MVU logic)"
  , prefixPattern "Tests." "Test harness/spec module (not product MVU logic)"
  , containsPattern "TestHarness" "Test harness shim"
  , prefixPattern "test_" "Test helper function"
  , exactPattern "Main.main" "Test runner entry point"
  , exactPattern "Main_runTests" "Test runner itself"
  ]

-- The generated-record-projection predicates (bare `Record.field` + dotted
-- `Record.(.field)`) now live ONCE in Coverage.Core.Backend
-- (isGeneratedRecordProjectionPath) and are shared by every family. The web
-- denominator policy is "reclassify test-harness + generated projections to
-- CompilerInsertedArtifact", which is exactly Backend.reclassifyArtifacts bound to
-- webPathExclusions — no local delete-filter, no copied projection predicate.

||| The web/android/ios MVU coverage backend: forked `--cg node --dumppaths-json`
||| denominator, string-identity join on `.pathId`, the shared artifact reclassifier
||| bound to the web test-harness patterns, and a no-op-without-hook production-safety
||| witness (the ES `globalThis.__idris2_recordPathHit` call is inert in prod). The
||| `buildDenominator`/`gatherEvidence` are filled per run by the runner.
export
webCoverageImpl : CoverageImpl String
webCoverageImpl = MkCoverageImpl
  (Just "node")                         -- cgFlag: ES/Node backend
  (ForkedIpkg "")                       -- buildDenominator: filled per run
  (.pathId)                             -- joinKey: string identity
  (PureKeys [])                         -- gatherEvidence: filled per run (recorded ids)
  (reclassifyArtifacts webPathExclusions)   -- EXCLUSIONS: shared reclassifier
  (NoOpWithoutHook "globalThis.__idris2_recordPathHit is undefined in prod; the emitted call is a no-op unless the harness installs the hook")

||| Split a harness output file into trimmed, non-empty path-id tokens.
||| The harness writes one canonical path-id per line (newline-joined Set).
export
parseRecordedPathIds : String -> List String
parseRecordedPathIds content =
  nub $ filter (/= "") $ map trim (lines content)

||| IDENTITY JOIN: a path obligation is covered iff its canonical path_id was
||| recorded by the device/engine harness. Mirrors DfxCoverage.PathRuntime
||| (RefC) but for the ES/JS hook; the recorded ids are byte-identical to the
||| dumppaths `path_id`, so this is a pure string intersection. The harness may
||| record ids outside the app's path universe (e.g. the test runner's own
||| `Tests.AllTests.*` paths); intersecting against the dumppaths obligations
||| drops them, so only real obligations count toward the numerator.
export
analyzeWebPathHits : (dumppathsContent : String) -> (recordedIds : List String)
                  -> Either String (List PathRuntimeHit)
analyzeWebPathHits dumppathsContent recordedIds = do
  paths <- parseDumppathsJson dumppathsContent
  -- reclassify (not delete) test-harness + generated projections, then identity-join
  let reclassified = webCoverageImpl.reclassify paths
  pure (hitsByKey webCoverageImpl.joinKey recordedIds reclassified)

||| File-driven variant: read the dumppaths JSON and the harness recorded-ids
||| file, then identity-join. Numerator = recorded ∩ dumppaths; the full
||| coverage result (denominator, exclusions, percent, admissibility) is built
||| by the shared `buildPathCoverageResultFromHits` in Coverage.Core.
export
analyzeWebPathHitsFromFiles : (dumppathsPath : String) -> (recordedIdsPath : String)
                           -> IO (Either String (List PathRuntimeHit))
analyzeWebPathHitsFromFiles dumppathsPath recordedIdsPath = do
  Right dumppaths <- readFile dumppathsPath
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  Right recorded <- readFile recordedIdsPath
    | Left err => pure $ Left $ "Failed to read recorded path ids: " ++ show err
  pure $ analyzeWebPathHits dumppaths (parseRecordedPathIds recorded)

||| Build the full path coverage result from dumppaths content + recorded ids.
||| This is the single entry point the LazyWeb Step 4 runner uses: it produces
||| the same PathCoverageResult shape as core/evm/dfx, so the renderStep4Contract
||| surface (coverage_percent / claim_admissible / Missing paths) is unchanged.
export
webPathCoverageResult : (dumppathsContent : String) -> (recordedIds : List String)
                     -> Either String PathCoverageResult
webPathCoverageResult dumppathsContent recordedIds =
  -- The ONE shared pipeline: reclassify (exclusions) → coveredByKey (numerator) →
  -- buildPathCoverageResultFromHits. Denominator = obligations whose class still
  -- countsAsDenominator after reclassifying test-harness + generated projections to
  -- CompilerInsertedArtifact (same numbers as the old delete-filter, but honest:
  -- excluded paths survive reclassified, not vanished). Same Step4 surface.
  coverageResultFor webCoverageImpl dumppathsContent recordedIds
