# Plugin Architecture — pluggable coverage backends without forking core

> Part of the coverage-design set. Map: [DELIVERYKIND.md](./DELIVERYKIND.md).
> Discipline: [TOTALITY_ANCHOR.md](./TOTALITY_ANCHOR.md). Hands-on: [forkers/README.md](./forkers/README.md).

---

## 1. Concept — the tension this resolves

Two requirements pull against each other:

1. **Totality anchor** (honesty): the set of coverage families must be a *closed* enum so
   nothing is silently dropped. → argues for a closed `match` in core.
2. **Free development** (extensibility): an outside party should be able to fork EtherClaw,
   wire up a *new* delivery backend (a different VM, a different test runner), and measure
   coverage for it — *without* patching `idris2-magical-utils`. → argues against a closed
   match in core.

The user's resolution (memory: *idris2-perspective-and-effectboundary*, decisive message):

> *"公式 DeliveryKind を我々が定めて、それの範囲内で TotalityAnchor するけど、基本的には
> プラガブル設計で、外部者も etherclaw fork を勝手に作成・選定した plugin でやっていいし、
> 本家もよくできたそれらを取り込む。"*

So: **closed enum for the canonical families (anchored), open data for everyone else
(pluggable).** Forks ship a value, not a patch. The upstream may later fold a good plugin
into the canonical enum.

## 2. Type — what makes it pluggable

The hinge is that the per-backend policy is **plain data**, not a closed match.
`pkgs/Idris2CoverageStandardization/src/Coverage/Standardization/Types.idr`:

```idris
record EffectBoundarySpec where
  constructor MkEffectBoundarySpec
  familyTag    : String          -- "evm"/"dfx"/… OR a fork's own tag. OPEN string → forks not gated.
  boundary     : EffectBoundary  -- which hole a matching %foreign reaches
  ccSubstrings : List String     -- cc substrings that identify the hole ON THIS backend
  excludable   : Bool            -- is a path reaching it harness-unexecutable for THIS runner?
```

Why each field is a value and not a hardcoded global:
- **`ccSubstrings`** — the *same* hole is opened by different calling conventions per
  backend: `popen2` on Chez, `child_process` on node, a Yul opcode on EVM. The cc match
  is data because it is backend-relative.
- **`excludable`** — the *same* hole is unrunnable on one runner and runnable on another:
  a precompile is N/A under Chez but **executed by revm** (so on `EvmHash` it is
  `excludable = False` — it stays in the denominator). Excludability is data because it is
  runner-relative.
- **`familyTag : String`** (not `CoverageFamily`) — precisely so a fork is **not gated by
  the closed enum**. A fork uses its own tag; nothing in core needs to know it.

### How canonical and plugin specs combine

The canonical families' specs come from the **Totality-anchored** table
`pkgs/Idris2CoverageCore/src/Coverage/Boundary/Canonical.idr`:

```idris
boundarySpecsFor : CoverageFamily -> List EffectBoundarySpec   -- total, no catch-all
boundarySpecsFor CoreLib = [ MkEffectBoundarySpec "core" ProcessSpawn ["popen2","system"] True, … ]
boundarySpecsFor EvmHash = [ MkEffectBoundarySpec "evm"  CanisterCall ["staticcall","call"] False ] -- revm runs them
boundarySpecsFor Humanoid = []   -- declared-unimplemented, stated explicitly (not skipped)
…
```

A coverage consumer uses **`canonical ++ pluginSpecs`**: the anchored table for official
families, unioned with any `List EffectBoundarySpec` a plugin supplies for its own
`familyTag`. The consumer matches a `%foreign`'s cc against the union; the first spec
whose `ccSubstrings` hits wins. So:
- official families: closed, exhaustive, anchored. Add one → `boundarySpecsFor` breaks
  until you fill the row ([TOTALITY_ANCHOR.md](./TOTALITY_ANCHOR.md)).
- fork families: open, additive, no core edit. Ship `MkEffectBoundarySpec "myvm" …`.

This is "anchor *within* the official set, pluggable *outside* it" — both properties at
once, because the closure is on `CoverageFamily` while the policy is on `EffectBoundarySpec`.

## 3. The honest-extensibility caveat (don't skip this)

Pluggability re-opens the door the anchor closed — at exactly one field: **`excludable :
Bool` is an assertion a plugin makes.** A malicious/careless fork could mark a real product
hole `excludable = True` to shrink its denominator. That is the value-weighting trick,
returned (the comment in `Types.idr` says so verbatim).

Two defenses, in order of strength:

1. **Auditability (shipped now).** The spec *carries the evidence*: `familyTag` +
   `ccSubstrings` + `boundary`. A reviewer reading a fork's specs sees exactly which cc it
   excluded, on which family, and why. No free-text — the cc is the witness. So a
   dishonest exclusion is *visible*, even if not yet *impossible*.
2. **Linear grounding (the honest endpoint, not yet landed).** Make excludability a *fact*,
   not an assertion: a hole is excludable **iff** that backend's runtime cannot construct
   its boundary resource — modelled as a linear `Res` whose constructor is absent from the
   backend's primitive set. Then the compiler enforces excludability and the plugin cannot
   lie. PoC that linearity gives exactly-once enforcement:
   `pkgs/Idris2CoverageStandardization/poc/LinearBoundaryPoC.idr`. Spec:
   "Ideal hardening" section of [exclusion-policy-unification.md](./exclusion-policy-unification.md).

Until (2) lands, the rule for reviewing a plugin is the rule in
[forkers/README.md](./forkers/README.md): **every `excludable = True` must point at a cc
that the family's runner genuinely cannot execute, and a reviewer must be able to confirm
it from the spec alone.**

## 4. Where this lives

| concern | file |
|---|---|
| the pluggable record | `pkgs/Idris2CoverageStandardization/src/Coverage/Standardization/Types.idr` (`EffectBoundarySpec`) |
| the canonical (anchored) table | `pkgs/Idris2CoverageCore/src/Coverage/Boundary/Canonical.idr` (`boundarySpecsFor`) |
| per-family difference is real (test) | `pkgs/Idris2CoverageCore/src/Tests/AllTests.idr` (`test_BOUNDARY_PERFAMILY_001`) |
| the family enum a plugin tag sits beside | `pkgs/Idris2DeliveryKind/src/Idris2/CoverageFamily.idr` |
