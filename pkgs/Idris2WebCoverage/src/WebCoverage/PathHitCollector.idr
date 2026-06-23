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
import Data.String
import System.File

import Coverage.Core.DumppathsJson
import Coverage.Core.PathCoverage
import Coverage.Core.Types
import public Coverage.Core.RuntimeHit

%default covering

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
  let covered = filter (\p => elem p.pathId recordedIds) paths
  pure (map (\p => MkPathRuntimeHit p.pathId 1) covered)

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
webPathCoverageResult dumppathsContent recordedIds = do
  paths <- parseDumppathsJson dumppathsContent
  let hits = map (\p => MkPathRuntimeHit p.pathId 1)
                 (filter (\p => elem p.pathId recordedIds) paths)
  pure (buildPathCoverageResultFromHits paths hits)
