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
| `Idris2Coverage` | Chez/native function-level semantic test obligation coverage |
| `Idris2DfxCoverage` | IC WASM branch-aware runtime coverage with branch-level semantic claims on the materialized runtime denominator |
| `Idris2EvmCoverage` | EVM branch-level semantic test obligation coverage |
| `Idris2WebCoverage` | Web/runtime coverage adapters and semantic function-level runner |
| `Idris2CoverageStandardization` | Draft standard, upstream requirements, and maintainer prep material |
| `Idris2ExecutionStandardization` | Backend-aware execution value vocabulary (`Cycles`, `Wei`, `ChainId`, etc.) |

## Current Coverage Position

The coverage packages do not all make the same strength of claim.

| Backend | Strongest current profile |
| --- | --- |
| Chez / `Idris2Coverage` | function-level semantic test obligation coverage |
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

Downstream coverage packages can optionally use a forked Idris2 binary that
emits structured dumpcases JSON.

Typical environment:

```bash
export IDRIS2_BIN=/path/to/forked/idris2
export IDRIS2_PACKAGE_PATH=/path/to/installed/idris2/packages
```

This is especially relevant for `Idris2EvmCoverage`, which is already prepared
to consume structured case-tree export when upstream support lands. The same
forked Idris2 support is now useful for `Idris2DfxCoverage`, whose strongest
semantic path also prefers harness-local dumpcases over project-main fallback.

## Recommended Reading

- [`pkgs/Idris2Coverage/README.md`](pkgs/Idris2Coverage/README.md)
- [`pkgs/Idris2DfxCoverage/README.md`](pkgs/Idris2DfxCoverage/README.md)
- [`pkgs/Idris2EvmCoverage/README.md`](pkgs/Idris2EvmCoverage/README.md)
- [`pkgs/Idris2CoverageStandardization/README.md`](pkgs/Idris2CoverageStandardization/README.md)
- [`pkgs/Idris2ExecutionStandardization/`](pkgs/Idris2ExecutionStandardization)

## Status

If you need the shortest accurate description of the current work:

- coverage is obligation-aware and conservative
- EVM and DFX now have practical branch-level runtime paths
- DFX currently makes its strong claim on the materialized runtime denominator
- upstream Idris2 structured export is still the main remaining external dependency
