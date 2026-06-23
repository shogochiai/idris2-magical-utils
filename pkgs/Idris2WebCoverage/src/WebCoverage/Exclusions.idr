||| Path-id-level unreachability exclusions for the web/android/ios MVU coverage.
|||
||| The function-name reclassifier (`Coverage.Core.Backend.artifactClassifier`,
||| bound to `webPathExclusions`) removes WHOLE non-product functions (the test
||| harness, generated record projections). It cannot express "this ONE leaf of
||| an otherwise-product function can never be reached by any input". A handful of
||| total-function defensive arms — the `Nothing`/`Nil` terminus of a `last'`/match
||| over a value that is provably non-empty, and the compiler-synthesised `default`
||| arm of an exhaustive `List` cons match — are exactly that: they sit inside a
||| reachable product function, so the function-name reclassifier (correctly) keeps
||| the function, but the specific leaf has NO reachable input.
|||
||| This module reclassifies THOSE SPECIFIC path-ids to `LogicallyUnreachable`
||| (type-level proof) or `CompilerInsertedArtifact` (compiler-synthesised arm).
||| Each carries a reason that STATES THE PROOF. The paths are NOT deleted: they
||| stay in `allPaths` with an honest class (so `denominator + excluded + unknown
||| == total`), exactly the anti-"hallucinated perfection" invariant the shared
||| `reclassifyByClassifier` enforces. This is the web twin of evm's
||| `classifyEvmExclusion` constant-false → LogicallyUnreachable rule, keyed by the
||| canonical `--dumppaths-json` path_id (string identity) instead of a heuristic.
|||
||| HONESTY CONTRACT (do not weaken):
|||   * Only paths with a written, checkable unreachability proof appear here.
|||   * Reachable product logic is NEVER listed — those get a test, not an exclusion.
|||   * The list is keyed by EXACT path_id, so it can only ever match the proven leaf,
|||     never the sibling reachable obligations of the same function.
module WebCoverage.Exclusions

import Data.List
import Data.String

import Coverage.Core.PathCoverage
import Coverage.Core.Backend
import Coverage.Standardization.Types

%default covering

||| One proven-unreachable path obligation: its exact canonical path_id, the
||| canonical `ExclusionReason` it is reclassified by (which fixes the resulting
||| ObligationClass via `reasonClass`), and the PROOF of why no input reaches it.
||| Using `ExclusionReason` (not a free `ObligationClass`) keeps web on the SAME
||| Totality anchor as every other family: a 7th category cannot be smuggled in.
|||   * a type-level-dead arm (provably no reachable input)  -> ConstantFalseGuard
|||     (the only reason mapping to LogicallyUnreachable, preserving the class).
|||   * a compiler-synthesised `default` arm (no user clause) -> StraightLineClause
|||     (maps to CompilerInsertedArtifact, preserving the class).
public export
record UnreachablePath where
  constructor MkUnreachablePath
  pathId : String
  reclassifiedTo : ExclusionReason
  reason : String

||| The proven-unreachable leaves of the EtherClawAndroid MVU app. Each `proof`
||| is a checkable argument; the `pathId` is the exact `--dumppaths-json` id so
||| the match is surgical (one leaf, never a reachable sibling).
public export
unreachablePaths : List UnreachablePath
unreachablePaths =
  [ -- artifactBase (in renderDraftSpec): `last' (forget (split (== '/') p))`.
    -- `Data.String.split` returns `List1` (ALWAYS non-empty); `forget` of a
    -- List1 is a non-empty List; `last'` of a non-empty list is ALWAYS `Just`.
    -- The `Nothing` arm therefore has no input (even refPath == "" yields the
    -- single segment [""], so last' = Just ""). Type-level unreachable.
    MkUnreachablePath
      "EtherClawAndroid.Parse.case block in renderDraftSpec,artifactBase#p0"
      ConstantFalseGuard
      "last' (forget (split (=='/') p)) is Just for all p: split : List1 (never []), forget keeps it non-empty, so last' = Just; the Nothing arm is unreachable (refPath \"\" -> [\"\"] -> Just \"\")."

    -- artifactBase inner: `noext = forget (split (== '.') s)` then
    -- `case noext of (h :: _) => h; [] => s`. `split` returns `List1`, so `noext`
    -- is ALWAYS a non-empty list; the `[]` (Nil) arm has no input. Unreachable.
  , MkUnreachablePath
      "EtherClawAndroid.Parse.case block in case block in renderDraftSpec,artifactBase#p1"
      ConstantFalseGuard
      "noext = forget (split (=='.') s) is non-empty for all s (split : List1), so the [] (Nil) arm is unreachable; even s == \"\" yields [\"\"]."

    -- afterDelim: rest = snd (break (\\c => c==':' || c=='=') raw); break = span (not.p),
    -- so `rest` either STARTS with ':'/'=' (the delimiter that stopped span) or is "".
    -- After trim, cs = unpack (trim rest) is either [] or starts with ':'/'='. Hence
    -- the inner `case cs of (':' :: t) | ('=' :: t) | _` :
    --   * #p2 = head is some OTHER char (cs = x::t, x not in {':','='}) — IMPOSSIBLE:
    --     a non-empty cs always has ':'/'=' as head.
    --   * #p3 = cs == [] (the default/empty arm). Reachable ONLY if some caller passes
    --     a raw with no ':' and no '='. afterDelim is NOT exported; its three call
    --     sites (classify Accept/Target, parseTaskTreePlan title) each fire ONLY after
    --     an isPrefixOf "...:"/"...=" guard, so every caller's raw contains a delimiter
    --     => rest is non-empty => cs is non-empty => the [] arm is unreachable too.
  , MkUnreachablePath
      "EtherClawAndroid.Parse.case block in afterDelim#p2"
      ConstantFalseGuard
      "cs = unpack (trim (snd (break delim raw))); break=span(not.delim) leaves rest empty or delimiter-headed, so a non-empty cs always heads ':' or '='; the 'other char' arm is unreachable."
  , MkUnreachablePath
      "EtherClawAndroid.Parse.case block in afterDelim#p3"
      ConstantFalseGuard
      "afterDelim is un-exported; all three callers (classify Accept/Target, title line) guard with isPrefixOf on a ':'/'=' prefix, so raw always contains a delimiter, rest is non-empty, cs is non-empty; the empty/default arm is unreachable."

    -- b64Decode's `go : List Int -> List Char -> List Int`. The second arg is a
    -- `List Char`; an exhaustive cons match enumerates `::` and `Nil` as branches.
    -- The forked compiler also emits a `default` branch per case-tree level, which
    -- no `List` value reaches (every list is cons OR nil, both already enumerated).
    -- These default arms (#p2 at depth 4, #p4 at depth 3, #p5 at depth 2) are
    -- compiler-synthesised, not user clauses with reachable inputs.
  , MkUnreachablePath
      "EtherClawAndroid.Parse.6844:2451:go#p2"
      StraightLineClause
      "go's depth-4 list match: branch_label 'default' on a List (only :: and Nil exist, both already enumerated as #3:0/#3:1); the synthesised default has no reachable List value."
  , MkUnreachablePath
      "EtherClawAndroid.Parse.6844:2451:go#p4"
      StraightLineClause
      "go's depth-3 list match: branch_label 'default' on a List (only :: and Nil exist, both enumerated as #2:0/#2:1); compiler-synthesised default, no reachable input."
  , MkUnreachablePath
      "EtherClawAndroid.Parse.6844:2451:go#p5"
      StraightLineClause
      "go's depth-2 list match: branch_label 'default' on a List (only :: and Nil exist, both enumerated as #1:0/#1:1); compiler-synthesised default, no reachable input."

    -- ----- EtherClawAndroid.View ------------------------------------------
    -- taskTreeCard and afterMarker and fmtDate are DEAD private helpers: they
    -- have ZERO call sites in the module (postBubble renders a TaskTree as the
    -- canister-backed builderQueueCard, NOT inline taskTreeCard; fmtDate was
    -- superseded by fmtDateTime). The sole module export is `view`, and no path
    -- from `view` reaches them, so every leaf of taskTreeCard/fmtDate has NO
    -- reachable input. (Verified: `grep taskTreeCard|afterMarker|fmtDate ` in
    -- View.idr shows only their own definitions, no callers.)
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in taskTreeCard#p0"
      ConstantFalseGuard
      "taskTreeCard is a DEAD private helper (0 call sites in View.idr; postBubble renders TaskTrees as builderQueueCard, not inline). Unreachable from the sole export `view`."
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in taskTreeCard#p1"
      ConstantFalseGuard
      "taskTreeCard is a DEAD private helper (0 call sites in View.idr). Unreachable from the sole export `view`."
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in fmtDate#p0"
      ConstantFalseGuard
      "fmtDate is a DEAD private helper (0 call sites in View.idr; superseded by fmtDateTime, which IS called). Unreachable from the sole export `view`."
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in fmtDate#p1"
      ConstantFalseGuard
      "fmtDate is a DEAD private helper (0 call sites in View.idr). Unreachable from the sole export `view`."

    -- composer.closedLabel: `case statusBadge st of Just (lbl,_) => lbl; Nothing
    -- => st`. statusBadge (Model.idr) has a TOTAL catch-all `statusBadge _ = Just
    -- (...)` — it returns `Just` for EVERY input and NEVER `Nothing`. So the
    -- `Nothing => st` arm has no reachable input. Type-level unreachable.
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in composer,closedLabel#p1"
      ConstantFalseGuard
      "closedLabel matches `case statusBadge st of ... Nothing => st`; statusBadge has a total catch-all `statusBadge _ = Just (...)` and never returns Nothing, so the Nothing arm is unreachable."

    -- statusChip: `case (status=="plan_proposed", proposalActionBadge p) of
    -- (True, Just lc) => lc; _ => ...`. The two USER arms ((True,Just) and the
    -- wildcard) are both covered; #p3's branch_label is the compiler-synthesised
    -- `default` (branch_index 1) of the tuple/Maybe match — no value reaches a
    -- third arm of an already-exhaustive (Bool, Maybe) match.
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in statusChip#p3"
      StraightLineClause
      "statusChip's (Bool, Maybe) tuple match: branch_label 'default' (branch_index 1) is the compiler-synthesised arm of an already-exhaustive match (the (True,Just) and wildcard user arms are both covered); no reachable input."

    -- diffScreen.fileBody: `case (m.diffBaseContent, m.diffHeadContent) of
    -- (Just b, Just h) => ...; _ => ...`. The (Just,Just) and wildcard user arms
    -- are covered (diffFileBoth / diffFileHalf). #p2 and #p3 are the compiler-
    -- synthesised `default` arms of the nested Maybe-pair match (no value reaches
    -- a default beyond the two enumerated Maybe constructors per side).
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in diffScreen,fileBody#p2"
      StraightLineClause
      "fileBody's (Maybe, Maybe) match: an MkPair then a compiler-synthesised 'default' branch (branch_index 1); both user arms ((Just,Just) + wildcard) are covered, so the default has no reachable input."
  , MkUnreachablePath
      "EtherClawAndroid.View.case block in diffScreen,fileBody#p3"
      StraightLineClause
      "fileBody's (Maybe, Maybe) match: compiler-synthesised 'default' branch (branch_index 1); both user arms are covered, so the default has no reachable Maybe-pair input."
  ]

||| The path-id-keyed classifier: `Just reason` (a canonical `ExclusionReason`) for
||| a proven-unreachable leaf, `Nothing` to leave the path as a normal product
||| obligation. Surgical: matches the exact path_id only, so reachable siblings of
||| the same function are untouched (they still demand a test). The detailed
||| per-path proof lives in `UnreachablePath.reason`; the classification itself goes
||| through the same enumerated `ExclusionReason` every family uses.
public export
classifyUnreachableWebPath : PathObligation -> Maybe ExclusionReason
classifyUnreachableWebPath path =
  case find (\u => u.pathId == path.pathId) unreachablePaths of
    Just u  => Just u.reclassifiedTo
    Nothing => Nothing

||| Reclassify the proven-unreachable leaves on top of an already-(artifact)-
||| reclassified path list. Composed AFTER the function-name reclassifier in
||| `webCoverageImpl.reclassify`. Only ReachableObligation paths are touched
||| (the shared `reclassifyByClassifier` idempotency guard).
public export
reclassifyUnreachableWebPaths : List PathObligation -> List PathObligation
reclassifyUnreachableWebPaths = reclassifyByClassifier classifyUnreachableWebPath
