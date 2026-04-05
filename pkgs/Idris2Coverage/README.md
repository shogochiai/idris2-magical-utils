# idris2-coverage

A pragmatic, proof-aware test coverage tool for Idris2.

[![CI](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml/badge.svg)](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml)
[![Idris2](https://img.shields.io/badge/Idris2-0.7.0+-blue)]()

## What is this?

A coverage tool that interprets Idris2 compiler output conservatively in the
presence of dependent types.

Current positioning:

- profile: function-level semantic test obligation coverage
- implementation style: downstream and proof-aware
- strong claim policy: only when `claim_admissible = true`

This tool may emit a numeric coverage measurement even when compiler provenance
is incomplete, but it does not treat every measurement as a strong semantic
coverage claim.

```bash
$ idris2-cov myproject/
Coverage: 847/901 (94%)
Profile: function-level semantic test obligation coverage
Claim admissible: true

Excluded (unreachable):
  - void/absurd patterns: 45
  - optimizer artifacts: 12
```

## Quick Start

```bash
# Build
idris2 --build idris2-coverage.ipkg

# Run on your project
./build/exec/idris2-cov path/to/project/

# Or with an ipkg file
./build/exec/idris2-cov myproject.ipkg

# Show only uncovered functions
./build/exec/idris2-cov --uncovered path/to/project/

# JSON output for CI
./build/exec/idris2-cov --json path/to/project/
```

## Key Features

- **Proof-aware**: Separates reachable obligations, logical unreachability, partial gaps, artifacts, and unknowns
- **Admissibility-aware**: Distinguishes a numeric measurement from a strong semantic claim
- **CRASH classification**: Distinguishes multiple compiler/runtime artifact sources instead of treating all `CRASH` nodes alike
- **CI-friendly**: Exit codes and JSON output for automation
- **Zero compiler changes**: Works with vanilla Idris2

## Relation To The Draft Standard

This tool is intended to implement the draft standard defined in
`idris2-coverage-standardization`, but current Idris2 compiler interfaces are
not yet sufficient for a fully upstream-backed branch-level standard.

Accordingly, the current implementation should be described as:

- a conservative downstream implementation
- of the function-level profile
- with explicit unknown handling
- and explicit `claim_admissible` reporting

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

```
idris2 --dumpcases  →  Parse case trees  →  Classify branches
                                                    ↓
                              Obligation classification + admissibility
                                                    ↓
Chez Scheme profiler  →  Runtime hits  →  Coverage measurement + claim status
```

## What To Cite

If you need a concept name in a paper, note, or issue, prefer:

- `semantic test obligation coverage`

If you need implementation status, say:

- `idris2-coverage currently implements the function-level profile conservatively`

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (no coverage gaps) |
| 1 | Coverage gaps found |
| 2 | Analysis error |

## Requirements

- Idris2 0.7.0+
- Chez Scheme (for profiling)

## Related Projects

- [idris2-sos-test-coverage-experiment](https://github.com/shogochiai/idris2-sos-test-coverage-experiment) - Demonstrates how GFR Monad + dependent types reduce System of Systems test complexity from O(2^N) to O(N)

## License

MIT
