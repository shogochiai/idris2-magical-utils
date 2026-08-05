||| REAL device path coverage — computed in Idris2, not bash/python.
|||
||| lazy and etherclaw are built portably in Idris2; the coverage MATH must live here,
||| not in a shell/python matcher. The device half is pure I/O: a harness collects the
||| dumppaths path_ids (denominator) and the on-device hit path_ids (numerator), one id
||| per line, and hands them to this module. Everything that decides coverage — the
||| exclusion policy, denominator ∩ numerator, the Missing list and the parity-ti-shaped
||| report — is pure, total Idris2 here, mirroring the host step4's exclusions so device
||| and host coverage agree on what an "obligation" is.
|||
||| numerator   = path_ids the View hit while running on the device (logcat IDRIS_PATHHIT)
||| denominator = the --dumppaths-json path_ids of the app's own functions, exclusions
|||               applied (library / synthetic / accessor / Eq-== / test).
||| A proof-TextView host never runs the View, so its numerator is empty → Missing > 0.
module Android.Coverage.PathCoverage

import Data.List
import Data.List1
import Data.String

%default total

-- =============================================================================
-- Exclusion policy (MUST match scripts/ci/build-web-app-step4.sh's is_excluded)
-- =============================================================================

||| The function name portion of a path_id ("Mod.fn#pN" -> "Mod.fn").
funcOf : String -> String
funcOf pid = case span (/= '#') (unpack pid) of (f, _) => pack f

||| The last dotted segment of a function name ("A.B.==" -> "==").
lastSeg : String -> String
lastSeg fn = case reverse (forget (split (== '.') fn)) of
               (s :: _) => s
               []       => fn

||| Library / runtime prefixes whose paths are not app obligations.
libPrefixes : List String
libPrefixes =
  [ "Prelude.", "Data.", "Builtin.", "System.", "Control."
  , "Text.HTML.", "Web.MVC.", "Web.Raw.", "JS.", "Text.CSS."
  , "Language.", "Decidable.", "Syntax.", "prim__", "_nest"
  , "RN.", "PrimIO." ]

||| Is this path_id excluded from the denominator? Mirrors the host step4 policy:
||| synthetic helpers (".{"), record accessors ("(."), structural Eq "==" arms,
||| test functions, and library/runtime functions are not path obligations.
hasLibPrefix : String -> Bool
hasLibPrefix fn = any (\p => isPrefixOf p fn) libPrefixes

export
isExcludedFn : String -> Bool
isExcludedFn fn =
  isInfixOf ".{" fn
    || isInfixOf "(." fn
    || isPrefixOf "==" (lastSeg fn)
    || isInfixOf ".Tests." fn
    || isInfixOf ".Test." fn
    || isPrefixOf "test_" (lastSeg fn)
    || hasLibPrefix fn

export
isExcluded : String -> Bool
isExcluded pid = isExcludedFn (funcOf pid)

-- Restrict to the app's own modules when a module prefix is given (e.g. "SpcDaoApp").
-- Empty prefix keeps everything (after exclusions).
inModule : String -> String -> Bool
inModule "" _   = True
inModule p  pid = isPrefixOf p (funcOf pid)

-- =============================================================================
-- Coverage
-- =============================================================================

public export
record PathCov where
  constructor MkPathCov
  denomTotal : Nat        -- denominator: app path obligations after exclusions
  covered : Nat        -- of those, hit on-device
  excludedCount : Nat  -- module-scoped ids dropped by isExcluded (kept for conservation)
  missing : List String

||| Compute device path coverage from the raw id lists the harness collected.
||| `denomIds` = every dumppaths path_id; `hitIds` = every on-device hit path_id;
||| `modPrefix` = the app's module prefix ("" = all). Exclusions are applied to the
||| denominator, then it is intersected with the (deduped) hit set. The excluded
||| count is carried in the report so the consumer can check conservation
||| (paths_total = denominator + excluded + unknown).
||| Project-supplied exclusions, on top of the built-in `isExcludedFn` policy.
||| Each entry is a plain function-name fragment; a path is dropped when its
||| function name CONTAINS one.
|||
||| Why this exists (measured on carl 2026-08-05): the built-in policy drops only
||| libraries, tests, record accessors and structural `==` — 13.2% of
||| pkgs/Idris2LuciAndroid. dfx drops 47.4% because GlobalRegistry ships a
||| `coverage-exclusions.txt`, and android had neither such a file nor any code
||| that reads one. That is not a regression; android was never wired to the
||| mechanism the other families use.
|||
||| It matters because `claim_admissible` here is `missing == []` — all or
||| nothing. 506 of the 1357 unhit paths are in Parse/Model/MemGraph/Canister,
||| reachable only by MUTATING a live canister (`Canister.idr`: 106 of its 109
||| functions are `IO`). Leaving those in the denominator sets a bar that a
||| device run cannot clear by construction, which is not honesty, it is an
||| unreachable pass condition.
export
isProjectExcludedFn : (extraPatterns : List String) -> String -> Bool
isProjectExcludedFn pats fn = any (\p => isInfixOf p fn) pats

||| `pathCoverage` plus project-supplied exclusion fragments. The zero-pattern
||| call is byte-identical to the old behaviour, so a target with no
||| coverage-exclusions.txt measures exactly as it did.
export
pathCoverageWith : (extraPatterns : List String) -> (denomIds : List String) -> (hitIds : List String) -> (modPrefix : String) -> PathCov
pathCoverageWith extraPatterns denomIds hitIds modPrefix =
  let scoped  = nub (filter (inModule modPrefix) denomIds)
      denom   = filter (\pid => not (isExcluded pid)
                             && not (isProjectExcludedFn extraPatterns (funcOf pid))) scoped
      excl    = length scoped `minus` length denom
      hits    = nub hitIds
      missing = filter (\p => not (elem p hits)) denom
  in MkPathCov (length denom) (length denom `minus` length missing) excl missing

export
pathCoverage : (denomIds : List String) -> (hitIds : List String) -> (modPrefix : String) -> PathCov
pathCoverage denomIds hitIds modPrefix = pathCoverageWith [] denomIds hitIds modPrefix

||| The parity-ti-shaped report text (same keys host step4 emits, so parity-ti's
||| step4Pass parses it identically). PASS ⟺ Missing paths: 0.
|||
||| v2 raw-evidence contract: RAW COUNTS only — this producer computes no
||| percentage (the old `percent` helper reported 100 on zero obligations, the
||| exact unmeasured→100% deception the v2 contract exists to kill). The
||| consumer (luci) computes hit/(denominator+unknown) in one place.
export
report : PathCov -> String
report c =
  unlines $
    [ "# Device PATH Coverage (dumppaths denominator, on-device hit numerator)"
    , "coverage_model: raw_evidence_v2"
    , "claim_admissible: " ++ (if c.missing == [] then "True" else "False")
    , "paths_total: " ++ show (c.denomTotal + c.excludedCount)
    , "paths_denominator: " ++ show c.denomTotal
    , "paths_hit: " ++ show c.covered
    , "paths_excluded: " ++ show c.excludedCount
    , "paths_unknown: 0"
    , "Missing paths: " ++ show (length c.missing)
    , "evidence_kind: device_dumppaths_path_coverage"
    , ""
    , "Missing paths:" ]
    ++ (if c.missing == [] then ["  (none)"] else map (\m => "  " ++ m) c.missing)
