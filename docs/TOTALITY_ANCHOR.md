# The Totality Anchor — why coverage uses closed enums, not strings

> Part of the coverage-design set. Start at [DELIVERYKIND.md](./DELIVERYKIND.md) for the map.
> This doc explains the *discipline* the enums enforce. For the pluggability that lives
> alongside it, see [PLUGIN_ARCHITECTURE.md](./PLUGIN_ARCHITECTURE.md).

---

## 1. Concept — the anchor in one sentence

> **Adding a constructor to a closed enum makes every exhaustive match on it stop
> compiling until you handle the new case.** We use this on purpose: the compiler becomes
> the checklist. You cannot *forget* to update a coverage family, an exclusion category,
> or a boundary policy, because forgetting is a type error.

This is the user's stated design backbone (memory: *idris2-perspective-and-effectboundary*):
*"抽象度を上げる = ctor を足す → case/totality が破綻して波及が機械的に露出する。"*
Raise the abstraction by adding a ctor, and the totality checker mechanically surfaces
every place that must react. The alternative — a `String` tag with a `_ => default`
fallback — lets a new case slip through silently, and in coverage a silent slip is a
**dishonest number** (a branch quietly dropped from the denominator). The anchor converts
"a human must remember N sites" into "the compiler enumerates N sites."

## 2. Why this matters *specifically* for coverage honesty

Path coverage has one failure mode worth fearing: **the denominator trick** — making the
"100%" easy by quietly shrinking what counts. It recurred once per family before the
anchor (evm's narrow denominator, web's implicit module drop — see
[exclusion-policy-unification.md](./exclusion-policy-unification.md)). Every such trick is
a *silent default*: some classifier returned an ad-hoc `(class, reason)` no one enumerated.

The fix is structural, not vigilance: make the set of legitimate exclusions a **closed
enum**, and make every classifier return *only* a value of that enum. Then "invent a new
excuse to exclude a branch" is not a code smell you must catch in review — it is
impossible to express, because the type has no constructor for it.

**The trick-proof rule** (the load-bearing idea): an exclusion is only trustworthy if its
justification is a *fact the compiler/type-checker already enforces* — not a value
weight, not a free-text reason, not a human's "it's IO so skip it." Values a human can
edit are trick-surfaces; types the compiler rejects are not.

## 3. Type — the three anchored enums

All in `pkgs/Idris2CoverageStandardization/src/Coverage/Standardization/Types.idr`.

### (a) `ObligationClass` — the verdict on one path obligation
```idris
data ObligationClass
  = ReachableObligation      -- a real branch; must be covered or it's Missing
  | LogicallyUnreachable     -- provably dead (constant-false), out of denom WITH proof
  | UserAdmittedPartialGap   -- a human signed off on leaving it uncovered
  | CompilerInsertedArtifact -- not product code (generated/projection/wrapper)
  | UnknownClassification    -- we genuinely cannot confirm — HONEST, claim-affecting
```
`UnknownClassification` is the honesty valve: it does **not** delete the path from the
denominator silently — it keeps it visible and flips the claim to "True with N unknowns."
There is no constructor that means "trust me, exclude it."

### (b) `ExclusionReason` — the *only* legitimate reasons to exclude (6, closed)
```idris
data ExclusionReason
  = NonProductModule | StandardLibrary | GeneratedProjection
  | SingleCtorDestructure | StraightLineClause | ConstantFalseGuard

reasonClass : ExclusionReason -> ObligationClass   -- total, all 6
reasonClass NonProductModule      = CompilerInsertedArtifact
reasonClass StandardLibrary       = CompilerInsertedArtifact
reasonClass GeneratedProjection   = CompilerInsertedArtifact
reasonClass SingleCtorDestructure = CompilerInsertedArtifact
reasonClass StraightLineClause    = CompilerInsertedArtifact
reasonClass ConstantFalseGuard    = LogicallyUnreachable
```
Family classifiers return `Maybe ExclusionReason` — **not** an arbitrary `(class, string)`.
A family that wants to exclude an observable product branch *cannot*: the type has no
ctor for it. (History: a 7th ctor `ObservabilityLimited` was designed, then **dropped**
once fork-yul source-level `log1` proved every branch is observable — see the RESOLVED
section of [exclusion-policy-unification.md](./exclusion-policy-unification.md). The
absence of that ctor *is* the anti-trick.)

> ⚠️ Naming: `Coverage.Core.Exclusions` already has a *different* `ExclusionReason`
> (`NotExcluded | ExcludedByPattern | ExcludedByModule`) for dependency rule-matching.
> The canonical-category type may be named `ExclusionCategory` to avoid the clash — heed
> this when implementing (it bit us; documented at the bottom of the unification doc).

### (c) `EffectBoundary` — the kind of hole a path reaches
```idris
data EffectBoundary
  = PureComputation | ProcessSpawn | NetworkOutcall
  | CanisterCall | FileSystemIO | UnclassifiedForeign String

boundaryExcludable PureComputation         = False  -- no hole → stays in denominator
boundaryExcludable (UnclassifiedForeign _) = True   -- unknown FFI → SAFE side: excludable+Unknown
boundaryClass      PureComputation         = ReachableObligation
boundaryClass      (UnclassifiedForeign _) = UnknownClassification
```
The `UnclassifiedForeign String` ctor is the *soundness* anchor: **any** `%foreign` we
have not catalogued is still captured (it carries the raw cc), so an unknown FFI can never
masquerade as `PureComputation`. The user caught the earlier version that missed unknown
FFIs; this ctor is the fix. The compiler emits this per path from the call-graph (a fact),
never a human (see the `--dumppathshits` reachVia spec in the unification doc).

## 4. How to *see* the anchor bite (probe pattern)

The anchor is only real if a missed case fails to compile. We prove it with **probe
constructors** in tests, not prose. Existing witnesses
(`pkgs/Idris2CoverageCore/src/Tests/AllTests.idr`):
- `test_PATH_BOUNDARY_001` — `effect_boundary` reclassifies `ProcessSpawn` → `Unknown`,
  keeps `Pure` in the denominator.
- `test_BOUNDARY_PERFAMILY_001` — the canonical policy *differs* per family (core vs evm
  vs web), proving `boundarySpecsFor` is a real total match, not a constant.

When you add a `CoverageFamily` or `ExclusionReason` ctor, the build breaks at
`boundarySpecsFor` / `reasonClass` / each family classifier until you state the new case.
That break **is** the feature. Do not add a `_ =>` to silence it — that re-opens the trick.
