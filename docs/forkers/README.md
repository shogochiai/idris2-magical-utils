# For forkers — add a coverage backend without patching core

> The hands-on companion to the design docs:
> [DELIVERYKIND.md](../DELIVERYKIND.md) (map) · [TOTALITY_ANCHOR.md](../TOTALITY_ANCHOR.md)
> (why the rules) · [PLUGIN_ARCHITECTURE.md](../PLUGIN_ARCHITECTURE.md) (the mechanism).
>
> Read this if you are wiring path coverage for a **new delivery backend** (a VM, a
> runtime, a test runner we don't ship). You should NOT need to edit
> `idris2-magical-utils` core to do it.

---

## What "a coverage backend" must provide

Path coverage = **numerator / denominator**, both honest:

- **Denominator** — every branch the product *could* take. Source: the forked Idris2
  compiler's `--dumppaths-json` (a per-path enumeration). You reuse this as-is; you do not
  write a denominator.
- **Numerator** — branches actually executed. Source: a `prim__recordPathHit` marker at
  every CaseTree leaf, *lowered to your backend's observation medium* and *collected* after
  a run. This is the part you write.
- **Exclusions** — paths that legitimately leave the denominator. You do **not** invent
  these; you pick from the closed `ExclusionReason` set and declare effect-holes via
  `EffectBoundarySpec`. See [TOTALITY_ANCHOR.md](../TOTALITY_ANCHOR.md).

The three honesty rules, non-negotiable:

1. **No silent default.** Anything you can't classify is `UnknownClassification` (visible,
   claim-affecting) — never quietly dropped.
2. **No invented exclusion category.** Use the 6 canonical `ExclusionReason` ctors. If you
   think you need a 7th, you almost certainly have a real Missing branch (this is exactly
   how `ObservabilityLimited` got dropped — see the unification doc).
3. **Every `excludable = True` must be auditable** from the spec alone (familyTag + cc).

## Step by step

### 1. Pick your `familyTag`
A plain string, e.g. `"myvm"`. It is **not** added to the `CoverageFamily` enum — that
enum is closed for official families only. Your tag lives in your `EffectBoundarySpec`
values. (If your backend later graduates upstream, *then* it earns a `CoverageFamily` ctor
and a `boundarySpecsFor` row — that is the upstream's "fold a good plugin in" path.)

### 2. Declare your effect-boundary policy as data
For each kind of effect hole your backend has, ship one `EffectBoundarySpec`:

```idris
import Coverage.Standardization.Types

myVmSpecs : List EffectBoundarySpec
myVmSpecs =
  [ MkEffectBoundarySpec "myvm" ProcessSpawn   ["myvm_exec"]        True
  , MkEffectBoundarySpec "myvm" NetworkOutcall ["myvm_http"]        True
    -- a hole your runner CAN execute stays in the denominator → excludable = False:
  , MkEffectBoundarySpec "myvm" CanisterCall   ["myvm_precompile"]  False
  ]
```
- `ccSubstrings` = the substrings that appear in a `%foreign` calling-convention on *your*
  backend for that hole. (e.g. canonical core uses `["popen2","system"]` for ProcessSpawn.)
- `excludable` = **can your test runner actually execute a path that reaches this hole?**
  If yes → `False` (it stays in the denominator and must be covered). If no (needs a
  device, a replica, a live network) → `True` (it reclassifies to `UnknownClassification`,
  stays *visible*, never deleted).
- If a hole is genuinely new to us, the compiler already has a safe net: any uncatalogued
  `%foreign` becomes `UnclassifiedForeign "<cc>"` → excludable + Unknown. You will not
  silently mislabel it as pure.

### 3. Lower the marker + collect hits for your medium
The marker insertion (`wrapPathHit` in the forked compiler's `CompileExpr.idr`) is
backend-independent — you get it for free. What differs per backend is **lowering** (how
`prim__recordPathHit` becomes an observable event) and **collection** (how you read the
events back after a run). Mirror an existing collector:
- string-identity join (most backends): `Coverage.Core.Backend` (`reclassifyByClassifier`).
- LOG-topic join (EVM-style): `Idris2EvmCoverage` PathRuntime.
- `globalThis` join (node/web): `Idris2WebCoverage` PathHitCollector.

Your collector emits the same path-ids the denominator uses (an **identity join** — same
id on both sides), so numerator ⊆ denominator by construction.

### 4. Feed the union to the consumer
The consumer matches a path's `%foreign` cc against
`boundarySpecsFor <canonical families> ++ myVmSpecs`. Your specs are *added*, not merged
into core. Nothing in `idris2-magical-utils` changes.

### 5. Prove your anchor bites (a test, not a claim)
Add a probe test in your package mirroring
`pkgs/Idris2CoverageCore/src/Tests/AllTests.idr`:
- `test_PATH_BOUNDARY_001`-style: a path reaching your `excludable` hole reclassifies to
  `UnknownClassification`; a pure path stays `ReachableObligation` in the denominator.
- assert your spec set actually *differs* from core's where it should (the per-family test
  pattern), so a future refactor can't silently flatten it.

## Review checklist (what an upstream reviewer will check before folding your plugin in)

- [ ] Every `excludable = True` cites a cc the runner genuinely cannot execute — confirmable
      from the spec, no external context needed.
- [ ] No path is dropped from the denominator without an `ExclusionReason` (one of the 6) or
      an `UnknownClassification`. No third path out.
- [ ] Numerator path-ids are identity-equal to denominator path-ids (no remapping that could
      inflate coverage).
- [ ] A probe test demonstrates the reclassification (not just asserts it in prose).
- [ ] Pure functions (View/Parse/Display with no FFI) stay **in** the denominator — "no
      tester for it" is *not* a valid exclusion (this was a real past bug; memory:
      *web-android real path coverage*).

## The honest endpoint (where this is heading)

Today `excludable` is a `Bool` you assert and a reviewer audits. The target is to make it a
**fact the compiler enforces**: a hole is excludable iff your backend's runtime cannot
construct its boundary resource (a linear `Res` whose ctor is absent from your primitive
set), so a plugin physically cannot lie. PoC:
`pkgs/Idris2CoverageStandardization/poc/LinearBoundaryPoC.idr`. Until then, audit is the
backstop — see [PLUGIN_ARCHITECTURE.md](../PLUGIN_ARCHITECTURE.md) §3.
