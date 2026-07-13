# idris2-magical-utils

Typed infrastructure for building and testing Idris2 systems across multiple
backends.

This repo currently has two major threads:

- backend/toolchain packages for EVM, IC WASM, and web-facing runtimes
- coverage/standardization packages for proof-aware test coverage in Idris2

## Package Map

### Execution backends

| Package | Role |
| --- | --- |
| `Idris2Evm` | Idris2 to EVM toolchain and pure EVM runtime support |
| `Idris2IcWasm` | Idris2 to Internet Computer WASM toolchain |
| `Idris2IcWasmSQLite` | SQLite support for IC WASM canisters |
| `Idris2Cdk` | IC canister framework and runtime helpers |
| `Idris2Subcontract` | EVM-side subcontract/UCS support |
| `Idris2BitcoinScript` | Typed Bitcoin Script DSL |
| `Idris2E2eHarness` | Cross-backend end-to-end harness helpers |

### Coverage and standardization

| Package | Role |
| --- | --- |
| `Idris2CoverageCore` | Shared coverage types, obligation maps, and classification helpers |
| `Idris2Coverage` | Chez/native path-level semantic test obligation coverage |
| `Idris2DfxCoverage` | IC WASM branch-aware runtime coverage with branch-level semantic claims on the materialized runtime denominator |
| `Idris2EvmCoverage` | EVM branch-level semantic test obligation coverage |
| `Idris2WebCoverage` | Web/runtime coverage adapters and semantic function-level runner |
| `Idris2CoverageStandardization` | Draft standard, upstream requirements, and maintainer prep material |
| `Idris2ExecutionStandardization` | Backend-aware execution value vocabulary (`Cycles`, `Wei`, `ChainId`, etc.) |
| `Idris2LspRedox` | Forked `idris2-lsp` with Redox path snapshot, diff, lifecycle, and QEQ witness RPCs |

## Coverage Architecture

### Path-obligation-first model

All three coverage families share a single denominator source: **canonical path
obligations** emitted by `idris2 --dumppaths-json` (forked compiler). Each
backend then supplies its own runtime numerator (path hits). This replaces the
older branch-only model.

- **Denominator**: `idris2 --dumppaths-json` path obligations (compiler-derived,
  backend-independent)
- **Numerator**: backend-specific runtime path hits
- **Output contract** (shared by all families): `coverage_percent`,
  `claim_admissible`, `Missing paths`

### 3-family runtime architecture

| Family | Package | Runtime | Instrumentation |
| --- | --- | --- | --- |
| core | `Idris2Coverage` | Chez native execution | `--dumppathshits` path-hit instrumentation |
| evm | `Idris2EvmCoverage` | Yul instrumentation, Anvil deployment | Event trace -> branch -> path mapping |
| dfx | `Idris2DfxCoverage` | ic-wasm profiling, dfx canister calls | Function -> path mapping |

EVM and DFX have their own runtime architectures; only core/Chez uses the
native `--dumppathshits` flag directly.

### Runtime sharding (core family only)

```bash
IDRIS2_COVERAGE_PATH_SHARDS=N idris2-coverage run ...
```

`IDRIS2_COVERAGE_PATH_SHARDS=N` splits test execution into N shards while
keeping a single instrumented build. This is needed because core family
packages with large path counts (e.g. EtherClaw with 9785 paths) OOM when all
tests run in one binary. EVM and DFX families have different runtime
architectures and do not support this env var.

### Measurement strength

The coverage packages do not all make the same strength of claim.

| Backend | Strongest current profile |
| --- | --- |
| Chez / `Idris2Coverage` | path-level semantic test obligation coverage |
| IC WASM / `Idris2DfxCoverage` | branch-level semantic test obligation coverage on the materialized runtime denominator |
| EVM / `Idris2EvmCoverage` | branch-level semantic test obligation coverage |
| Web Idris runner | function-level semantic test obligation coverage |
| Web JS CLI | runtime observation measurement |

See:

- [`pkgs/Idris2CoverageStandardization/docs/BACKEND_MEASUREMENT_STRENGTH_MATRIX.md`](pkgs/Idris2CoverageStandardization/docs/BACKEND_MEASUREMENT_STRENGTH_MATRIX.md)
- [`pkgs/Idris2CoverageStandardization/docs/SEMANTIC_TEST_OBLIGATION_STANDARD.md`](pkgs/Idris2CoverageStandardization/docs/SEMANTIC_TEST_OBLIGATION_STANDARD.md)

## Why The Standardization Packages Exist

Dependent-type-aware coverage needs stricter accounting than line coverage.
The current model in this repo distinguishes:

- what the denominator is
- which obligations are excluded as unreachable or non-semantic artifacts
- which runtime observations map back to the same obligation layer
- whether a numeric measurement is strong enough to be stated as a semantic claim

That last distinction is surfaced as `claim_admissible`.

## Why Idris2, Not Lean

This is a common question, because Lean 4 is the more famous dependently-typed
system and has a larger proof ecosystem. The short answer is **not** that Lean
is less capable. It is that Lean and Idris2 optimized for different goals, and
the work in this repo has the shape Idris2 optimized for.

> Idris2 trims its dependent types toward *bringing proofs into programs*.
> Lean trims its dependent types toward *bringing programs into proofs*.
> A path-obligation coverage toolchain that compiles, runs, and measures real
> backends is the former shape, so Idris2 fits with less friction. That is a
> difference of aim, not a deficiency in Lean.

### The workflow this repo actually runs

Abstracted, the daily loop is:

1. add a constructor to a sum type (e.g. a new backend family)
2. let the totality/coverage checker enumerate every non-exhaustive site
3. close genuinely-unreachable cases with `impossible`
4. require `total` so the compiler is the witness of exhaustiveness
5. **execute** the program and collect `--dumppaths-json` path coverage

The decisive property is that steps 2-4 (type-level exhaustiveness and
termination) and step 5 (real execution, measured) live in **one** working loop.
This repo treats dependent types as an everyday *programming* tool whose programs
are also shipped and instrumented — exactly Edwin Brady's *Type-Driven
Development* framing. The four design divergences below explain why that loop is
smooth in Idris2 and taxed in Lean.

### 1. `impossible` is a first-class programming word

Idris2 has the `impossible` keyword, so "this pattern is unreachable at the type
level" is written directly in ordinary code:

```idris
noNat : (1 = 2) -> Void
noNat Refl impossible
```

Lean has no such keyword. The equivalent is done with proof tactics / terms
(`nomatch`, `Empty.elim`, `absurd`, an empty `cases`). Lean is not weaker here —
for large proofs it is often smoother. The difference is directness: writing one
word `impossible` in a program branch and having downstream exhaustiveness close
is *programming vocabulary* in Idris2 and a *proof tactic* in Lean.

### 2. Totality is a dial you can run with, not a gate you must clear

This is the largest divergence.

- **Idris2**: `%default total` makes "not total ⇒ compile error" a project-wide
  policy, while `partial` definitions still type-check and still run. Totality
  is a practical on/off dial, which enables *gradual rigor*: write `partial`,
  run it, tighten to `total` later.
- **Lean 4**: every definition the kernel accepts must be total (for soundness).
  You either discharge termination (`termination_by` / `decreasing_by`) or write
  `partial def` — but a `partial def` carries no defining equations or `simp`
  lemmas, so it is effectively opaque inside proofs.

| | A `partial` function can be… |
| --- | --- |
| Idris2 | executed, and type-checked (only `covering` is required); merely awkward to reason about |
| Lean 4 | executed, but largely severed from the proof world (opaque) |

So "prototype partial, then tighten" is low-friction in Idris2 and high-friction
in Lean.

### 3. Execution is a first-class target — the soil `dumppaths` grows in

The coverage pipeline assumes you **write a program in Idris2, run it, and
observe its execution paths** (`--dumppaths-json` / `--dumppathshits`, forked
compiler). That assumption pays off because Idris2 treats multiple execution
backends (Chez, RefC→C, JS, and external codegen such as `Idris2Evm` /
`Idris2IcWasm`) as primary deliverables. Lean 4 can compile and `#eval` too, but
its ecosystem's center of gravity is Mathlib — formalized proof, not shipped-and-
profiled applications. Growing a `--dumppaths-json` flag and using it as a
coverage denominator is a natural move when *the executed program is the object
of measurement*. This is an ecosystem-weight difference more than a raw-
capability one.

### 4. Idris2 leans computational; Lean carries proof-grade machinery

For sound mathematics Lean adopts definitional proof irrelevance, quotient types
as kernel primitives, and classical logic with choice. These are decisive for
formalizing mathematics, but they add friction to the *computational content* of
programs (universe `Prop`/`Type` separation, `Decidable`-mediated `if`, etc.).
Idris2 leans constructive/computational, so "case-split a `Type` value and close
with `impossible`" stays lightweight. Idris2 also carries a different weapon Lean
lacks — Quantitative Type Theory (0/1/ω multiplicities) for resource/linearity
typing.

### Where Lean is the better choice

Stated plainly, so this section is a comparison and not a pitch:

- **Proof scale and automation**: `simp`, `omega`, `decide`, `aesop`, and the
  Mathlib corpus make large-scale proving Lean's domain.
- **Metaprogramming**: Lean 4's `macro` / `elab` / `MetaM` (Lean is written in
  Lean) is more unified than Idris2 Elaborator Reflection.
- **Kernel trust**: a small checked kernel plus proof irrelevance gives Lean the
  stronger story as a *proof system*.
- **Mathematical community**: Mathlib is the largest asset in the dependently-
  typed world.

### One-line summary

If your loop is *prove-heavy*, reach for Lean. If your loop is
*enumerate-with-the-typechecker, close-with-`impossible`, require-`total`, then
execute-and-measure* — the loop this repo is built around — Idris2 is the better
fit, and that is a deliberate difference in optimization target, not a gap in
Lean.

## Build Notes

Different packages target different backends.

### Native / Chez packages

```bash
cd /Users/bob/code/idris2-magical-utils
idris2 --build pkgs/Idris2Coverage/idris2-coverage.ipkg
idris2 --build pkgs/Idris2CoverageStandardization/idris2-coverage-standardization.ipkg
```

### EVM packages

`Idris2EvmCoverage` itself is a native CLI, but it analyzes projects that are
compiled via the `Idris2Evm` / `idris2-yul` pipeline.

```bash
cd /Users/bob/code/idris2-magical-utils
idris2 --build pkgs/Idris2EvmCoverage/idris2-evm-coverage.ipkg
```

### IC WASM packages

IC packages often need backend-specific build commands rather than plain
`pack build`. Follow each package README.

## Forked Idris2 Support

All three coverage families depend on a forked Idris2 binary that provides
`--dumppaths-json` (canonical path obligations) and `--dumppathshits` (runtime
path-hit instrumentation for core/Chez). These flags supply the shared
denominator and the core-family numerator respectively.

Typical environment:

```bash
export IDRIS2_BIN=/path/to/forked/idris2
export IDRIS2_PACKAGE_PATH=/path/to/installed/idris2/packages
```

`Idris2EvmCoverage` and `Idris2DfxCoverage` use `--dumppaths-json` for the
denominator but obtain their numerators from backend-specific runtime traces
(Anvil event logs and ic-wasm profiling respectively).

## Recommended Reading

- [`pkgs/Idris2Coverage/README.md`](pkgs/Idris2Coverage/README.md)
- [`pkgs/Idris2DfxCoverage/README.md`](pkgs/Idris2DfxCoverage/README.md)
- [`pkgs/Idris2EvmCoverage/README.md`](pkgs/Idris2EvmCoverage/README.md)
- [`pkgs/Idris2CoverageStandardization/README.md`](pkgs/Idris2CoverageStandardization/README.md)
- [`pkgs/Idris2ExecutionStandardization/`](pkgs/Idris2ExecutionStandardization)

## Status

If you need the shortest accurate description of the current work:

- coverage uses a path-obligation-first model with `--dumppaths-json` as the shared denominator
- core/Chez has path-level coverage with runtime sharding for large packages
- EVM and DFX have practical branch-level runtime paths mapped back to the same obligation layer
- DFX currently makes its strong claim on the materialized runtime denominator
