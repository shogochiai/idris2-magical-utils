# idris2-coverage Project Guidelines

## Build Commands

```bash
# Build the library and CLI
idris2 --build idris2-coverage.ipkg

# Install as package (for use as dependency)
idris2 --install idris2-coverage.ipkg

# Build self-analysis tool
idris2 --build self-analyze.ipkg
```

## Running Coverage Analysis

```bash
# Analyze current directory (default)
idris2-cov                              # defaults to .
idris2-cov .                            # explicit current dir

# Analyze specific project
idris2-cov path/to/project/             # finds .ipkg automatically
idris2-cov myproject.ipkg               # explicit ipkg

# Show only functions with coverage gaps
idris2-cov --uncovered
idris2-cov --uncovered path/to/project/
```

## Test Discovery

Tests are discovered automatically:
1. **ipkg-based**: Modules ending in `AllTests` from ipkg's `modules` field
2. **Filesystem fallback**: `*/Tests/*AllTests.idr` files

Test modules must export `runAllTests : IO ()`.

## Self-Coverage

Current self-coverage: **220/901 branches (24%)**

```bash
# Measure self-coverage
idris2-cov
```

## Project Architecture

```
src/
├── Main.idr                    # CLI entry point
└── Coverage/
    ├── Types.idr               # Core types (CaseKind, SemanticCoverage)
    ├── SemanticCoverage.idr    # High-level API
    ├── DumpcasesParser.idr     # --dumpcases output parser
    ├── UnifiedRunner.idr       # Test execution with profiling
    ├── TestRunner.idr          # Test discovery utilities
    ├── Aggregator.idr          # Coverage aggregation
    ├── Report.idr              # Output formatting
    └── Tests/
        └── AllTests.idr        # Unit tests (87 tests)
```

## CRASH Classification (dunham's classification)

| CRASH Message | Classification | Action |
|--------------|----------------|--------|
| `"No clauses in..."` | `CrashNoClauses` | **Exclude** (void/absurd) |
| `"Unhandled input for..."` | `CrashUnhandledInput` | **Bug** - fix it |
| `"Nat case not covered"` | `CrashOptimizerNat` | **Ignore** (optimizer artifact) |
| Other | `CrashUnknown` | **Never exclude** (conservative) |

## Adding Tests

1. Add test functions to `src/Coverage/Tests/AllTests.idr`
2. Register in `allTests` list with pattern `("REQ_XXX_NNN", test_func)`
3. Run: `./build/exec/idris2-cov .` to verify

## CI / Portability Testing

GitHub Actions CI (`.github/workflows/ci.yml`) runs on push/PR to main:

1. **Build**: `pack build idris2-coverage.ipkg` on Ubuntu + macOS
2. **Smoke test**: `./build/exec/idris2-cov --help`
3. **Integration test**: Clones and analyzes `shogochiai/idris2-yul`

The integration test verifies that `generateTempPackToml` correctly uses GitHub
references (not hardcoded local paths), ensuring the tool works in clean environments.

If you modify `UnifiedRunner.idr` or pack.toml generation logic, ensure CI passes
to avoid environment-dependent bugs that only surface on other machines.

## Key APIs

```idris
-- High-level: analyze project
analyzeProject : String -> IO (Either String SemanticAnalysis)

-- With runtime profiler hits
analyzeProjectWithHits : String -> List String -> IO (Either String SemanticCoverage)

-- Discover tests from filesystem
discoverTestModules : String -> IO (List String)
```
