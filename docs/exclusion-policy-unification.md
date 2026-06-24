# Exclusion Policy Unification — Totality anchor for path-coverage honesty

> Motivation (user insight): the log1 path-coverage **instrumentation** is unified
> (one backend-independent `wrapPathHit` inserts `prim__recordPathHit` at every
> CaseTree leaf), and lowering/collection legitimately differ per family (LOG topic
> / WASM profiling / globalThis — different observation media). BUT the **exclusion
> policy** (what enters the denominator vs is reclassified out) is family-specific,
> ad-hoc, and NOT Totality-enforced. This let the SAME dishonesty recur per family:
> evm's narrow denominator (excluded 321 >> denom 121, via `classifyEvmExclusion`'s
> "Yul branch-label collapse"), web/android's implicit ipkg-modules drop. Each family
> can pass ANY `PathObligation -> Maybe (ObligationClass, String)` to
> `reclassifyByClassifier` — there is no canonical, enumerated set of legitimate
> exclusion reasons, so over-exclusion is structurally possible.

## Current state (verified)
- Unified: `idrislang-idris2/src/Compiler/CompileExpr.idr` `wrapPathHit` (insertion).
- Per-medium (legit): lowering — Chez/RefC/ES (idrislang) + Yul (idris2-yul Codegen);
  collectors — Dfx/Evm PathRuntime, Web PathHitCollector. All import shared
  `Idris2CoverageCore` (`Coverage.Core.Backend.reclassifyByClassifier`).
- Canonical class type EXISTS: `Idris2CoverageStandardization/.../Types.idr`
  `data ObligationClass = ReachableObligation | LogicallyUnreachable |
  UserAdmittedPartialGap | CompilerInsertedArtifact | UnknownClassification`.
- DRIFT: the **classifier function** is family-specific. dfx/web compose
  `artifactClassifier` (proj + test-harness only). evm composes the RICHER
  `classifyEvmExclusion` adding: config/dep exclusion, non-product (.Tests./.Storages./
  .Schema), stdlib, generated-projection, **constant-false→LogicallyUnreachable**,
  **Yul-branch-label-collapse→CompilerInsertedArtifact**, single-ctor-destructure,
  straight-line. The Yul-collapse one is the dishonest lever: it maps a REAL product
  branch (unobservable in bytecode) to CompilerInsertedArtifact = removed from the
  denominator → narrow-denominator "claim True".

## Design (Totality-anchored canonical exclusion)
New canonical type in `Idris2CoverageCore` (or Standardization):
```idris
public export
data ExclusionReason
  = NonProductModule       -- .Tests./.Storages./.Schema/wrapper
  | StandardLibrary        -- prelude/stdlib
  | GeneratedProjection    -- record projection
  | SingleCtorDestructure  -- irrefutable destructure (truly not a branch)
  | StraightLineClause     -- no branch obligation
  | ConstantFalseGuard     -- logically unreachable (constant-false)
  | ObservabilityLimited   -- ★ product branch the medium cannot observe (e.g. Yul collapse)

||| Canonical, single-source-of-truth mapping. Totality-checked: adding an
||| ExclusionReason ctor forces this match (and every classifier) to handle it.
reasonClass : ExclusionReason -> ObligationClass
reasonClass NonProductModule      = CompilerInsertedArtifact
reasonClass StandardLibrary       = CompilerInsertedArtifact
reasonClass GeneratedProjection   = CompilerInsertedArtifact
reasonClass SingleCtorDestructure = CompilerInsertedArtifact
reasonClass StraightLineClause    = CompilerInsertedArtifact
reasonClass ConstantFalseGuard    = LogicallyUnreachable
reasonClass ObservabilityLimited  = UnknownClassification   -- ★ NOT artifact: honest "can't observe"
```
- Family classifiers return `Maybe ExclusionReason` (NOT arbitrary `(ObligationClass,String)`).
  → families cannot invent ad-hoc categories; the legitimate set is enumerated once.
- `reclassifyByClassifier` is rebound to take a `PathObligation -> Maybe ExclusionReason`
  and apply `reasonClass`. Reason string derives from the ctor (stable, no free-text drift).
- **Effect**: evm's Yul-collapse paths become `UnknownClassification` (honestly "observed
  could not confirm"), NOT removed from the denominator. The narrow-denominator "claim True"
  becomes "claim False with N unknowns" until the branches are actually covered (via
  fork-yul log1 + state calldata). web/android implicit ipkg-drop must also route through
  this classifier (no silent module omission).

## Totality enforcement (the anchor, like family-enum / ContextKind)
- `ExclusionReason` is the single enum. `reasonClass` + every family classifier match on it.
- A new exclusion category = new `ExclusionReason` ctor = compile error at `reasonClass`
  and at each family classifier until handled. No family can drift its denominator silently.

## Open question to resolve with evm fork-yul DATA (before final impl)
Does `ObservabilityLimited` even apply under fork-yul? The log1 markers are SOURCE-level
(every CaseTree leaf emits log1), so a "Yul-collapsed" source branch SHOULD still be
observable via its marker. If so, `isKnownYulBranchLabelMismatch` is a RELEASED-yul-era
relic and under fork-yul those paths are simply Reachable-or-Missing (no need for
ObservabilityLimited at all). The evm agent's fork-yul run will show whether any path is
genuinely unobservable. → finalize the ExclusionReason set against that data.

## Scope (cross-cutting refactor; do AFTER evm agent finishes — EvmCoverage overlap)
Files: Idris2CoverageCore (new ExclusionReason + reasonClass + reclassifyByClassifier sig),
Idris2EvmCoverage (classifyEvmExclusion → returns ExclusionReason; drop ad-hoc tuple),
Idris2DfxCoverage (artifactClassifier → ExclusionReason), Idris2WebCoverage (same + route
ipkg-dropped modules through classifier), Idris2AndroidCoverage. parity: all family test
suites must stay green; the canonical mapping must reproduce the LEGITIMATE exclusions
(test/stdlib/proj) unchanged while flipping ObservabilityLimited to Unknown.

---

## RESOLVED by evm fork-yul data (2026-06-23) — ObservabilityLimited NOT needed
The evm agent's honest fork-yul run answered the open question definitively:
- Under fork-yul source-level `log1`, EVERY source CaseTree branch carries its own marker,
  so a "Yul-collapsed" branch IS observable. `isKnownYulBranchLabelMismatch` /
  `isKnownConstantUnreachable` were RELEASED-YUL-ERA RELICS that hid real product branches
  (Tally/Members/Vote) by classifying them CompilerInsertedArtifact.
- Honest result: TextDao denominator 121→**133** (hidden product branches returned to the
  denominator), 70/133 covered, claim True, **unknown 0**. Tokenomics 0→103 observable.
- ∴ **DROP `ObservabilityLimited`** from the design. There is no honest "product branch we
  can't observe" category once source-level markers exist — it's either Reachable (covered)
  or Missing (not yet driven). The dishonest lever was the observability-based exclusion
  ITSELF; the fix is to NOT have it (which the agent did by suppressing those exclusions
  when markers are present).

### Final canonical ExclusionReason (revised — 6 ctors, all genuinely-not-product-branch)
```idris
data ExclusionReason
  = NonProductModule       -- .Tests./.Storages./.Schema/wrapper   → CompilerInsertedArtifact
  | StandardLibrary        -- prelude/stdlib                       → CompilerInsertedArtifact
  | GeneratedProjection    -- record projection                   → CompilerInsertedArtifact
  | SingleCtorDestructure  -- irrefutable destructure (not a branch) → CompilerInsertedArtifact
  | StraightLineClause     -- no branch obligation                 → CompilerInsertedArtifact
  | ConstantFalseGuard     -- literal-false guard, logically dead  → LogicallyUnreachable
```
NO observability/medium-collapse category. Families return `Maybe ExclusionReason`; a family
that wants to exclude an observable product branch simply CANNOT (the type has no ctor for it)
→ the narrow-denominator trick is structurally impossible. A genuinely-dead branch with a
written reachability proof uses `ConstantFalseGuard` (or stays Missing with a documented
no-public-entry note, as evm's addRep-gated branches do — kept in denominator, honest).

### Still also fixed by this unification
- web/android implicit ipkg-modules drop → must route every module through the classifier
  (no silent omission; pure View/Parse stay in the denominator).
- evm `classifyEvmExclusion` → returns `Maybe ExclusionReason`, losing the observability ctors.
- The `(ObligationClass, String)` free-tuple classifier signature → replaced by
  `PathObligation -> Maybe ExclusionReason`, so no family can emit an ad-hoc class/reason.

---

## ⚠️ NAMING COLLISION (must heed during implementation, 2026-06-24)
`Coverage.Core.Exclusions` ALREADY defines a DIFFERENT `ExclusionReason`:
```idris
data ExclusionReason = NotExcluded | ExcludedByPattern String | ExcludedByModule String
```
This existing type answers "was this function excluded, by which RULE" and is USED in
Stats.idr, DependencyExclusion.idr, WebCoverage/UnifiedRunner.idr. DO NOT clobber or repurpose it.

→ **Name the canonical category type `ExclusionCategory`** (NOT `ExclusionReason`):
```idris
public export
data ExclusionCategory
  = NonProductModule | StandardLibrary | GeneratedProjection
  | SingleCtorDestructure | StraightLineClause | ConstantFalseGuard

public export
categoryClass : ExclusionCategory -> ObligationClass   -- Totality-checked, all 6
categoryClass NonProductModule      = CompilerInsertedArtifact
categoryClass StandardLibrary       = CompilerInsertedArtifact
categoryClass GeneratedProjection   = CompilerInsertedArtifact
categoryClass SingleCtorDestructure = CompilerInsertedArtifact
categoryClass StraightLineClause    = CompilerInsertedArtifact
categoryClass ConstantFalseGuard    = LogicallyUnreachable

public export
categoryReason : ExclusionCategory -> String   -- stable text
```
Family classifiers: `PathObligation -> Maybe ExclusionCategory`. `reclassifyByClassifier`
takes that and applies `categoryClass`+`categoryReason`. The pre-existing `ExclusionReason`
stays as-is (orthogonal: pattern/module matching for dependency exclusions).

---

## EffectBoundary fact-grounding — `--dumppathshits` reachVia spec (fork-compiler, scoped 2026-06-24)

Goal: each emitted path carries the EffectBoundary it reaches + a reachVia witness
(the call chain to the %foreign primitive), so denominator-exclusion of harness-
unexecutable paths is asserted by the COMPILER (call-graph fact), never by a human.
Type spine already landed (Coverage.Standardization.Types.EffectBoundary); PoC that
linearity makes it a TYPE fact already landed (poc/LinearBoundaryPoC.idr).

### Verified integration points (idrislang-idris2, fork branch feature/es-source-maps)
- `src/Compiler/Common.idr:528` `collectPathResults : String -> Nat -> CaseTree -> (List PathResult, Nat)`
  — the per-path enumerator. Attach the boundary witness to each PathResult here.
- `src/Compiler/Common.idr:92` `namedDefs : List (Name, FC, NamedDef)` (and getCompileDataWith
  ~:818) — the full compiled-def list = the call-graph source.
- `src/Core/CompileExpr.idr:209` `MkForeign (ccs : List String) ...` — the %foreign decl;
  `ccs` carries the C spec (e.g. "C:popen2,libidris2_support"). Match against
  boundaryPrimitive (popen2 / http_request / ic0.call_new / openFile).
- `CExp` ctors carrying call edges: `CRef fc Name` (:75), `CApp` (:85), `CExtPrim` (:95).

### Algorithm (one fixpoint + per-path lookup)
1. directBoundary : Name -> Maybe EffectBoundary — for each def whose CDef is
   `MkForeign ccs ...`, if any cc matches boundaryPrimitive b → Just b.
2. reachesBoundary : transitive closure over the call graph (edges = CRef/CApp/CExtPrim
   names in each def's body). `reachesBoundary n = directBoundary n` ∪ ⋃ over callees.
   Compute as a fixpoint over namedDefs (worklist; bounded by |defs|).
3. In collectPathResults, for a leaf path whose terminal expr's free CRef set is S,
   its boundary = the strongest b in ⋃ reachesBoundary over S (PureComputation if none),
   and reachVia = a shortest witness chain n₀→…→%foreign(b).
4. Emit per path: existing path_id + classification, PLUS `effect_boundary` and
   `reach_via` (the chain). Gated on `dumppathshits`/a new `--dumppaths-boundaries`
   flag so production builds are unaffected.

### Consumer side (magical-utils, already-typed)
- Parse `effect_boundary`/`reach_via` into PathObligation (add fields).
- denominator = paths with `boundaryExcludable boundary == False` (PureComputation).
  Excluded paths reclassify to `boundaryClass boundary` (UnknownClassification — visible,
  claim-affecting), NEVER deleted. Report the EffectBoundary histogram + reachVia.
- A path can ONLY leave the denominator with a compiler-emitted reachVia proof → no
  observer judgment, no value weight → trick-proof.

### Ideal hardening (separate): linear runProc
Make `Boundary.Sys.runProc : Argv -> IO (Res Output)` (linear Res). Then "reaches
ProcessSpawn" upgrades from a call-graph approximation to a TYPE fact (consumes Res),
independent of the analysis's soundness. PoC proved the compiler enforces exactly-once
(use-twice → "linear name in non-linear context"; drop → "0 uses ... exactly once").
