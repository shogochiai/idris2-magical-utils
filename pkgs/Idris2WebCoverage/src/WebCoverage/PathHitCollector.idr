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

isAsciiUpper : Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower : Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

startsWith : (Char -> Bool) -> String -> Bool
startsWith predicate s =
  case unpack s of
    [] => False
    c :: _ => predicate c

||| The compiler emits record getters in TWO syntactic forms, both auto-generated
||| total accessors (not MVU branches):
|||   - bare  : `Record.field`           — terminal clause, no steps, no span
|||   - paren : `Record.(.field)`         — the dotted-projection form, 1 record
|||                                          pattern-match step, no span
||| The bare form is the dfx isGeneratedRecordProjectionPath case; the paren form
||| is web/RN-specific (the ES backend keeps the `.field` projection syntax).
||| Both have an Uppercase record segment and a lowercase field, no source span,
||| and are NOT `case block` lambdas — so neither is product logic under test.
||| Name shape of a generated record getter `Record.field`: the second-to-last
||| dot segment is the Uppercase record, the last is the lowercase field (no ':'
||| span tokens, not a `case block` lambda). NOTE this NAME test alone is weak —
||| almost any `Module.function` satisfies it — so it is ONLY ever applied
||| together with the structural path constraints below (no steps, no source
||| span), which is what actually distinguishes a 0-branch generated projection
||| from a real function. The compiler renders the dotted-projection form
||| `Record.(.field)` but the dumppaths functionName is the bare `Record.field`.
isGeneratedRecordProjectionName : String -> Bool
isGeneratedRecordProjectionName name =
  case reverse (forget $ split (== '.') name) of
    field :: recordName :: _ =>
         startsWith isAsciiUpper recordName
      && startsWith isAsciiLower field
      && not (isInfixOf ":" field)
      && not (isInfixOf "case block" name)
    _ => False

||| The dotted-projection getter form `Record.(.field)`. The compiler emits this
||| as a 1-step obligation whose step carries the RECORD's decl span (the whole
||| record block) but whose path has NO sourceSpanUnion — distinct from a real
||| function whose path owns a union span. The `.(.…)` name marks it
||| unambiguously as the auto-generated dotted accessor (a real function name
||| never contains `.(.`).
isDottedProjectionName : String -> Bool
isDottedProjectionName name =
  isInfixOf ".(." name && isSuffixOf ")" name && not (isInfixOf "case block" name)

||| A generated record projection. TWO compiler-emitted forms, each requiring its
||| NAME shape AND the structural shape that separates a 0-branch accessor from a
||| real function (which always carries a path-level sourceSpanUnion):
|||   - bare   `Record.field`     : no steps, no union span (dfx form)
|||   - dotted  `Record.(.field)` : single step, no union span (ES/RN form)
||| Real MVU functions (update/view/parsers) own a sourceSpanUnion, so they are
||| never excluded even when a name shape coincides.
isGeneratedRecordProjectionPath : PathObligation -> Bool
isGeneratedRecordProjectionPath path =
  path.terminalKind == "reached_clause"
  && isNothing path.sourceSpanUnion
  && ( (path.steps == [] && isGeneratedRecordProjectionName path.functionName)
       || isDottedProjectionName path.functionName )

shouldExcludePath : List ExclPattern -> PathObligation -> Bool
shouldExcludePath patterns path =
  isJust (isMethodExcluded patterns path.functionName)
  || isGeneratedRecordProjectionPath path

||| Filter the parsed obligations down to genuine product MVU paths.
export
filterProductPaths : List ExclPattern -> List PathObligation -> List PathObligation
filterProductPaths patterns =
  filter (\path => not (shouldExcludePath patterns path))

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
  let product = filterProductPaths webPathExclusions paths
  let covered = filter (\p => elem p.pathId recordedIds) product
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
  -- Denominator = product MVU paths only (test-harness + generated record
  -- projections removed with reasons; same policy as dfx/evm). Numerator =
  -- recorded ∩ product denominator (identity join).
  let product = filterProductPaths webPathExclusions paths
  let hits = map (\p => MkPathRuntimeHit p.pathId 1)
                 (filter (\p => elem p.pathId recordedIds) product)
  pure (buildPathCoverageResultFromHits product hits)
