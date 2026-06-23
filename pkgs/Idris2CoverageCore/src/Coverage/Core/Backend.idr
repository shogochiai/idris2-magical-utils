||| The unified path-coverage backend abstraction: the family-variable pieces
||| (Denominator source, Numerator join key + evidence, Exclusions predicate,
||| production-safety) gathered into ONE record, plus the polymorphic join and the
||| shared artifact reclassifier that every family uses.
|||
||| Chunking is deliberately NOT a backend field — it lives entirely in the shared
||| denominator builder (`runStaticDumppathsJson`, parameterized only by module
||| count and the `cgFlag`), so the coverage axis stays at the right altitude.
|||
||| The four PathCoverage concepts as they appear here:
|||   - DENOMINATOR : `cgFlag` + `buildDenominator` (a `DenominatorSource`).
|||   - NUMERATOR   : `joinKey` (the per-family join function `PathObligation -> k`,
|||                   identity `(.pathId)` or evm's FNV `pathIdTopic`) + `evidence`
|||                   (an `EvidenceSource k`, IO live-run or pure id list).
|||   - CHUNK       : not here — shared in the denominator builder.
|||   - EXCLUSIONS  : `reclassify` (the shared artifact reclassifier, pre-bound with
|||                   the family's test-harness predicate) demotes generated
|||                   projections + test-harness paths to `CompilerInsertedArtifact`
|||                   so the existing `countsAsDenominator` shrinks the denominator —
|||                   no per-family delete-filter, no silent denominator shrink.
|||
||| `coveredByKey` subsumes BOTH the string-identity join (web/dfx/core/android, k =
||| String) and evm's non-invertible FNV-hash join (k = Integer) without a tagged
||| union: the family fixes `k` at construction.
module Coverage.Core.Backend

import Data.List
import Data.List1
import Data.Maybe
import Data.String

import Coverage.Core.PathCoverage
import Coverage.Core.DumppathsJson
import Coverage.Core.RuntimeHit
import Coverage.Core.Exclusions
import Coverage.Standardization.Types
import Coverage.Standardization.InstrumentationSafety

%default total

-- ============================================================================
-- Generated-record-projection detection (PROMOTED — single canonical copy)
-- ============================================================================
-- These were copied in dfx (DfxCoverage.PathCoverage), web (PathHitCollector) and
-- partly evm (EvmCoverage.PathCoverage). The web copy is the superset (bare AND
-- dotted forms); it is canonicalised here and the families now reference it.

isAsciiUpper : Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower : Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

startsWith : (Char -> Bool) -> String -> Bool
startsWith predicate s =
  case unpack s of
    [] => False
    c :: _ => predicate c

||| Bare getter `Record.field`: second-to-last dot segment is the Uppercase record,
||| the last is the lowercase field. Weak on its own (almost any `Module.function`
||| matches) — ONLY ever used with the structural path constraints below.
public export
isGeneratedRecordProjectionName : String -> Bool
isGeneratedRecordProjectionName name =
  case reverse (forget $ split (== '.') name) of
    field :: recordName :: _ =>
         startsWith isAsciiUpper recordName
      && startsWith isAsciiLower field
      && not (isInfixOf ":" field)
      && not (isInfixOf "case block" name)
    _ => False

||| Dotted getter `Record.(.field)` — the ES/RN projection form. `split` on '.'
||| shatters "(.field)", so detect it on the whole name: a ".(." ending in ")".
public export
isDottedProjectionName : String -> Bool
isDottedProjectionName name =
  isInfixOf ".(." name && isSuffixOf ")" name && not (isInfixOf "case block" name)

||| A generated record projection (bare OR dotted). NAME shape AND the structural
||| shape (a 0-branch accessor: no path-level sourceSpanUnion). Real MVU functions
||| own a sourceSpanUnion, so they are never matched even if a name shape coincides.
public export
isGeneratedRecordProjectionPath : PathObligation -> Bool
isGeneratedRecordProjectionPath path =
  path.terminalKind == "reached_clause"
  && isNothing path.sourceSpanUnion
  && ( (path.steps == [] && isGeneratedRecordProjectionName path.functionName)
       || isDottedProjectionName path.functionName )

-- ============================================================================
-- The shared artifact reclassifier (replaces per-family delete-filters)
-- ============================================================================

||| Reclassify (NOT delete) obligations to CompilerInsertedArtifact / etc. Each
||| path stays in the set with an honest class so `denominator + excluded + unknown
||| == total` (the anti-"hallucinated perfection" rule). IDEMPOTENCY GUARD: only a
||| ReachableObligation is ever overridden, so re-parsing already-classified content
||| never resets a non-Reachable class. The `classifier` returns `Just (cls, reason)`
||| to reclassify, or `Nothing` to leave a path as a normal product obligation.
||| (This is exactly evm's reclassifyPathObligations generalised over the classifier;
||| evm composes its rich `classifyEvmExclusion`, dfx/web compose `artifactClassifier`.)
public export
reclassifyByClassifier : (PathObligation -> Maybe (ObligationClass, String))
                      -> List PathObligation -> List PathObligation
reclassifyByClassifier classifier = map reclassify
  where
    reclassify : PathObligation -> PathObligation
    reclassify path =
      case path.classification of
        ReachableObligation =>
          case classifier path of
            Just (cls, _) => { classification := cls } path
            Nothing       => path
        _ => path

||| The family-neutral artifact classifier: a path is a CompilerInsertedArtifact if
||| it is a generated record projection OR matches the family's test-harness exclusion
||| patterns. This is the shared subset dfx/web/core/android use; evm composes its
||| richer classifier on top (Yul collapse, constant-false → LogicallyUnreachable).
public export
artifactClassifier : (testHarness : List ExclPattern)
                  -> PathObligation -> Maybe (ObligationClass, String)
artifactClassifier testHarness path =
  if isGeneratedRecordProjectionPath path
    then Just (CompilerInsertedArtifact, "generated record projection")
  else case isMethodExcluded testHarness path.functionName of
         Just reason => Just (CompilerInsertedArtifact, reason)
         Nothing     => Nothing

||| Convenience: reclassify with the shared artifactClassifier bound to a family's
||| test-harness pattern list. This is the `reclassify` a string-identity family
||| (web/dfx/core/android) puts in its CoverageImpl.
public export
reclassifyArtifacts : (testHarness : List ExclPattern)
                   -> List PathObligation -> List PathObligation
reclassifyArtifacts testHarness =
  reclassifyByClassifier (artifactClassifier testHarness)

-- ============================================================================
-- The polymorphic numerator JOIN (subsumes identity AND hash)
-- ============================================================================

||| A path obligation is covered iff its join key is in the runtime-evidence set.
||| `key = (.pathId)` gives the string-identity join (web/dfx/core/android); `key =
||| pathIdTopic` gives evm's FNV-hash join (k = Integer). One polymorphic filter, no
||| tagged union — the family fixes `k`.
public export
coveredByKey : Eq k => (PathObligation -> k) -> List k -> List PathObligation
            -> List PathObligation
coveredByKey key evidence paths = filter (\p => elem (key p) evidence) paths

||| The covered obligations as PathRuntimeHit (hitCount 1), ready for
||| buildPathCoverageResultFromHits.
public export
hitsByKey : Eq k => (PathObligation -> k) -> List k -> List PathObligation
         -> List PathRuntimeHit
hitsByKey key evidence paths =
  map (\p => MkPathRuntimeHit p.pathId 1) (coveredByKey key evidence paths)

-- ============================================================================
-- The family-variable backend bundle
-- ============================================================================

||| WHERE the denominator path-ids come from. A forked `--dumppaths-json` build
||| (IO, chunked by the shared builder) vs path-ids already collected by a harness
||| (pure — android device reads logcat/id files, no build).
public export
data DenominatorSource
  = ForkedIpkg      String         -- ipkg path → runStaticDumppathsJson(cgFlag)
  | PrebuiltIdFiles (List String)  -- flat path_ids already collected (PURE)

||| WHERE the numerator evidence comes from. The IO arm is a live run (deploy/run/
||| scrape); the Pure arm never forces IO (android device already has its hit ids).
public export
data EvidenceSource : Type -> Type where
  LiveRun  : IO (Either String (List k)) -> EvidenceSource k
  PureKeys : List k -> EvidenceSource k

||| The coverage backend for ONE family. The four PathCoverage concepts are NAMED
||| fields — the record cannot be constructed with one missing, so each family's
||| coverage story is totality-checked at the value level. `k` is the join-key type
||| (String for identity families, Integer for evm's FNV topic).
public export
record CoverageImpl k where
  constructor MkCoverageImpl
  cgFlag           : Maybe String                       -- DENOMINATOR: web=Just "node"
  buildDenominator : DenominatorSource                  -- DENOMINATOR: source
  joinKey          : PathObligation -> k                -- NUMERATOR: (.pathId) | pathIdTopic
  gatherEvidence   : EvidenceSource k                   -- NUMERATOR: IO run | pure ids
  reclassify       : List PathObligation -> List PathObligation  -- EXCLUSIONS (shared, pre-bound)
  safety           : InstrumentationSafety              -- production-safety witness

||| A mandatory, human-written reason a family has no runnable backend yet (the
||| contract test rejects an empty rationale). Surfaces in the report so the gap is
||| KNOWN and non-passing, never the silent `Nothing` of the old String dispatch.
public export
record UnimplementedReason where
  constructor MkUnimplementedReason
  rationale   : String
  trackingRef : Maybe String

||| The total-dispatch result. There is NO `Maybe` here — THIS is what makes
||| "= Nothing" untypeable. A family is EITHER implemented (a full CoverageImpl, its
||| `k` and `Eq k` captured existentially) OR explicitly, reportably declared
||| unimplemented WITH a rationale.
public export
data CoverageStatus : Type where
  Implemented           : {0 k : Type} -> Eq k => CoverageImpl k -> CoverageStatus
  DeclaredUnimplemented : UnimplementedReason -> CoverageStatus

-- ============================================================================
-- The common pipeline (the ONE place families converge)
-- ============================================================================

||| Build the coverage result for an Implemented backend from its denominator JSON +
||| gathered evidence keys: reclassify (exclusions) → join via key (numerator) →
||| buildPathCoverageResultFromHits. The denominator JSON is produced by the caller
||| (via the shared chunked builder + `cgFlag`); evidence keys are the result of
||| running `gatherEvidence`. Pure — IO (the live-run evidence + the forked build)
||| is interpreted by the runner, keeping this core step total and testable.
public export
coverageResultFor : Eq k => CoverageImpl k -> (dumppathsContent : String)
                 -> (evidenceKeys : List k) -> Either String PathCoverageResult
coverageResultFor impl content keys =
  case parseDumppathsJson content of
    Left err => Left err
    Right paths =>
      let reclassified = impl.reclassify paths
          hits = hitsByKey impl.joinKey keys reclassified
      in Right (buildPathCoverageResultFromHits reclassified hits)
