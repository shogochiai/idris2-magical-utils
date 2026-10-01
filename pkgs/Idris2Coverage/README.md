# idris2-coverage

A path-first, proof-aware coverage tool for Idris2.

[![CI](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml/badge.svg)](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml)
[![Idris2](https://img.shields.io/badge/Idris2-0.7.0+-blue)]()

## What is this?

`idris2-cov` is now a path-first tool.

- primary profile: path obligations from `--dumppaths-json`
- primary runtime evidence: path-id hits from `--dumppathshits`
- strong claim policy: only when `claim_admissible = true`
- implementation style: downstream and proof-aware

If you are integrating coverage into another tool, start from:

- `Coverage.PathCoverage`
- `Coverage.UnifiedRunner.runTestsWithPathCoverageArtifacts`
- `idris2-cov [options] <dir-or-ipkg>`

Legacy `dumpcases` / function-hit APIs still exist in the repo for migration,
but they are no longer the recommended entrypoint.

## Quick Start

```bash
# Build
idris2 --build idris2-coverage.ipkg

# Run on your project
./build/exec/idris2-cov path/to/project/

# Or with an ipkg file
./build/exec/idris2-cov myproject.ipkg

# JSON output for CI
./build/exec/idris2-cov --json path/to/project/

# Path coverage from dumppaths-json + runtime hits
./build/exec/idris2-cov --dumppaths-json path/to/dumppaths.json --path-hits path/to/path-hits.txt

# Path coverage by asking a forked compiler to emit dumppaths-json on demand
IDRIS2_BIN=/path/to/forked/idris2 ./build/exec/idris2-cov myproject.ipkg
```

## Path Coverage

`idris2-cov` itself is now the current entrypoint for missing-path analysis.
The `paths` subcommand remains accepted as a no-op alias.

It consumes the same semantic vocabulary used elsewhere in the repo:

- `Missing paths`
- `coverage_percent`
- `claim_admissible`

The intended use is:

1. generate canonical intrafunction path obligations with a forked Idris2
   exposing `--dumppaths-json`
2. collect runtime path hits
3. report exact missing paths over the same obligation layer

This is the output surface now consumed by `lazy * ask --steps=4` and
EtherClaw HardHarness integration.

### Keeping the instrumented build between runs (`IDRIS2COV_RUNTIME_BUILD_DIR`)

For large packages the runtime hits come from a test executable compiled with
`--dumppathshits`. By default each run compiles it in a fresh
`.idris2-coverage-runtime-<uid>` and deletes it. With
`IDRIS2COV_RUNTIME_BUILD_DIR=<name>`, a directory under the project, the
build is kept. Later runs recompile only changed modules, and they reuse the
executable outright when a digest of everything it is compiled from is
unchanged: the project's `.idr` files, the runner and ipkg, the compiler's
`.so`, and every installed library `.ttc`. Each runtime chunk prints
`Runtime chunk N build: S s in … (executable reused | rebuilt: inputs digest …)`.

The name must be letters, digits, `.`, `_` or `-`. `build` is refused, because
a plain build's TTCs carry uninstrumented code, and so is anything containing
`..`. Refused values fall back to the fresh directory and say so. Use one
directory per tree, and run one measurement at a time in it. Spec:
`REQ_COV_UNI_PERSIST_001`. Measured on luci `pkgs/Luci` (186 modules): with the
executable reused, the instrumented build went from 645 s to 0 s, and the
universe and hit sets were identical.

## Key Features

- **Path-first**: Reports exact missing path obligations rather than only function summaries
- **Proof-aware**: Separates reachable obligations, logical unreachability, partial gaps, artifacts, and unknowns
- **Admissibility-aware**: Distinguishes a numeric measurement from a strong semantic claim
- **CI-friendly**: Exit codes and JSON output for automation
- **Fork-aware**: Uses a forked Idris2 when `IDRIS2_BIN` exposes `--dumppaths-json`

## Legacy APIs

The following modules are legacy migration surfaces and should not be used for
new integrations unless you intentionally want dumpcases/function-hit behavior:

- `Coverage.DumpcasesParser`
- `Coverage.TestCoverage`
- `runTestsWithFunctionHits`
- `runTestsWithTestCoverage`

## Relation To The Draft Standard

This tool is intended to implement the draft standard defined in
`idris2-coverage-standardization`, with path obligations as the preferred
runtime-facing profile.

Accordingly, the current implementation should be described as:

- a conservative downstream implementation
- of the path-obligation profile
- with explicit unknown handling
- and explicit `claim_admissible` reporting

The current JSON/text reports expose a `measurement` block so downstream tools
can inspect:

- `denominator_ids`
- `covered_ids`
- `excluded_ids`
- `unknown_ids`

That keeps the observed numerator and the tested obligation layer explicit.

## Documentation

| Document | Description |
|----------|-------------|
| [Vibe Coding](docs/VIBE_CODING.md) | Why this enables complex software development |
| [Getting Started](docs/GETTING_STARTED.md) | Installation and first steps |
| [User Guide](docs/USER_GUIDE.md) | Full CLI reference and configuration |
| [FAQ](docs/FAQ.md) | Common questions and answers |
| [Contributing](docs/CONTRIBUTING.md) | Development setup and PR guidelines |
| [Architecture](docs/ARCHITECTURE.md) | Code structure and design decisions |
| [Internals](docs/INTERNALS.md) | Technical details (CRASH classification, name mangling) |

## How It Works

The primary path-coverage flow is:

```
idris2 --dumppaths-json  →  canonical path obligations
runtime path hits        →  covered_ids on the same path_id layer
```

The result is a missing-path list over the same path-id layer.

Legacy dumpcases/function-hit analysis remains in the repo only for migration.

## What To Cite

If you need a concept name in a paper, note, or issue, prefer:

- `path test obligation coverage`

If you need implementation status, say:

- `idris2-coverage currently implements the path-obligation profile conservatively`

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (no coverage gaps) |
| 1 | Coverage gaps found |
| 2 | Analysis error |

## Requirements

- Idris2 0.7.0+
- Chez Scheme (for profiling)

Optional:

- forked Idris2 with `--dumppaths-json` / `--dumppathshits` via `IDRIS2_BIN`

## Related Projects

- [idris2-sos-test-coverage-experiment](https://github.com/shogochiai/idris2-sos-test-coverage-experiment) - Demonstrates how GFR Monad + dependent types reduce System of Systems test complexity from O(2^N) to O(N)

## License

MIT

### Static per-module memo (`IDRIS2COV_STATIC_MEMO=1`)

With `IDRIS2COV_STATIC_MEMO=1` **and** a persistent `IDRIS2COV_RUNTIME_BUILD_DIR`, the
chunked static denominator keeps one row per module in `<runtime dir>/static-memo`
and builds only the modules whose row key changed. A key covers the module's source,
the source of every project module in its import closure, this idris2-cov's `.so`,
the fork compiler's `.so` and every installed library `.ttc`. Rows are committed only
at the end of a run that produced covered hits and whose served universe shows no
moved compiler counter (`staleHitIds`); otherwise the memo is discarded and the
denominator recomputed. Each chunk prints its time and memo/recomputed counts.

Measured on luci pkgs/Luci (37 chunks, 503.5 s cold), one unreferenced export added:

| edited module | recomputed modules | static chunks: memo / cold | paths memo / recomputed | universe vs cold |
|---|---|---|---|---|
| `Luci.Boundary.DaemonOps` (imported by nobody) | 1 | 20.3 s / 507.2 s | 29724 / 62 | sorted diff 0 |
| `Luci.Workspace` | 12 | 265.7 s / 512.2 s | 20513 / 9810 | sorted diff 0 |
| `Luci.Types` (most imported) | 57 | 432.4 s / 500.9 s | 10759 / 19483 | sorted diff 0 |
