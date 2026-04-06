# Backend Measurement Strength Matrix

This note summarizes what each downstream backend currently measures, what kind
of claim that measurement supports, and where the remaining semantic gaps are.

## Rule

The denominator and numerator must point at the same obligation layer.

If a backend can only observe runtime functions, then the strongest admissible
claim is a function-level claim unless it can map those runtime observations
back to stable branch obligations.

## Current Matrix

| Backend | Runtime observation | Current measurement layer | Strong claim today | Main remaining gap |
|---|---|---|---|---|
| Chez / `idris2-coverage` | Chez profiler function hits | Function-level semantic obligations | Function-level semantic test obligation coverage | Stable branch-level obligation IDs from Idris2 |
| IC WASM / `idris2-dfx-coverage` | ic-wasm function IDs + cycles plus branch probe hit indices | Branch-level semantic obligations on the materialized runtime denominator | Branch-level semantic test obligation coverage on the materialized runtime denominator | Stronger upstream provenance and a cleaner relation between static canonical branches and runtime-materialized branches |
| EVM / `idris2-evm-coverage` | Chez profiler hits plus dumpcases-derived branch sets | Branch-level semantic obligations | Conservative branch-level semantic test obligation coverage | Dumpcases parsing and runtime mapping are still downstream approximations, so provenance remains weaker than the desired upstream interface |
| Web Idris runner / `LazyWeb` integration | V8 hits mapped through Idris-side static analysis | Function-level semantic obligations | Function-level semantic test obligation coverage | Stable obligation-aware source map for branch-level claims |
| Web JS CLI / `idris2-web-cov` | Raw V8 function observations | Runtime function observation set | Runtime observation measurement only | No static semantic obligation mapping in the JS-only path |

## Interpretation

Two reports may share a JSON schema and still have different semantic strength.

That is acceptable if:

- the measurement layer is stated explicitly
- the admissible claim is stated explicitly
- the tool does not overstate what the backend can justify

## Why This Matters

Without this matrix, it is easy to confuse:

- a backend-specific runtime observation set
- a semantic executable-obligation measurement
- an admissible semantic coverage claim

Those are not the same thing.

## Current Integration Gaps

The matrix is not just about backend behavior. It also tracks standardization
integration status.

Today the main gaps are:

- `idris2-evm-coverage` now exposes shared `CoverageMeasurement` and claim
  admissibility, but its branch parser and runtime mapping are still
  downstream approximations rather than upstream provenance-backed identities.
- `idris2-dfx-coverage` now has a practical branch-level runtime path, but the
  strongest claim is still scoped to the materialized runtime denominator
  rather than the full static canonical branch set.
- The Web JS CLI shares the report schema but not yet the semantic static
  obligation model used by the Idris-side runner.
- Branch-level claims remain downstream approximations until Idris2 exposes
  stable machine-readable obligation IDs with provenance.

## Next Upgrade Path

1. Idris2 upstream exposes stable, machine-readable obligation IDs.
2. Each backend maps runtime observations to those IDs.
3. Branch-level semantic claims become admissible where the runtime mapper is
   strong enough.

Until then, downstream tools should prefer conservative function-level semantic
claims over unsound branch-level claims.
