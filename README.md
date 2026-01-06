# idris2-coverage

A pragmatic, proof-aware test coverage tool for Idris2.

[![CI](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml/badge.svg)](https://github.com/shogochiai/idris2-coverage/actions/workflows/ci.yml)
[![Idris2](https://img.shields.io/badge/Idris2-0.7.0+-blue)]()

## What is this?

A coverage tool that understands dependent types. It excludes provably unreachable branches (like `void` cases) from your coverage denominator, so **100% coverage is actually achievable**.

```bash
$ idris2-cov myproject/
Coverage: 847/901 (94%)

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

- **Proof-aware**: Excludes `impossible`/`void` branches from denominator
- **CRASH classification**: Distinguishes bugs from unreachable code
- **CI-friendly**: Exit codes and JSON output for automation
- **Zero compiler changes**: Works with vanilla Idris2

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
                              Canonical (testable) vs Excluded (void/absurd)
                                                    ↓
Chez Scheme profiler  →  Runtime hits  →  Coverage = executed / canonical
```

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
