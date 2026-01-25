# User Guide

Complete reference for idris2-coverage CLI and configuration.

## CLI Reference

### Basic Usage

```bash
idris2-cov [OPTIONS] [PATH]
```

`PATH` can be:
- A directory containing an `.ipkg` file
- An `.ipkg` file directly
- `.` for current directory (default)

### Options

| Option | Description |
|--------|-------------|
| `--json` | Output in JSON format |
| `--uncovered` | Show only uncovered functions |
| `--top N` | Limit to top N high-impact targets (default: 20) |
| `--version` | Show version information |
| `--help` | Show help message |

### Examples

```bash
# Analyze current directory
idris2-cov

# Analyze specific project
idris2-cov ~/projects/myapp/

# JSON output with top 50 targets
idris2-cov --json --top 50 myproject.ipkg

# Only show functions with no test coverage
idris2-cov --uncovered .
```

## Configuration File

Create `.idris2-cov.toml` in your project root to customize behavior.

### Full Example

```toml
[exclusions]
# Exclude modules by prefix from high-impact targets
# (they still count in totals, but won't appear as suggestions)
module_prefixes = [
    "MyProject.Internal.",
    "MyProject.Generated."
]

# Additional packages to exclude (beyond ipkg depends)
packages = ["contrib", "network"]
```

### Exclusion Behavior

1. **Automatic exclusions** (always applied):
   - Standard library: `Prelude.*`, `Data.*`, `System.*`, `Control.*`
   - Compiler-generated: `{csegen:*}`, `{eta:*}`, `prim__*`
   - Type constructors: Names ending with `.`

2. **From ipkg `depends`**: Packages listed in your `.ipkg` are auto-excluded

3. **From config**: Additional prefixes/packages you specify

## Output Formats

### Text Output (Default)

```
=== Branch Classification ===
canonical:           2883
excluded_void:         45
bugs:                   3
optimizer_artifacts:   12
unknown:                0

=== High Impact Targets ===
1. Coverage.Parser.parseCase       (15 branches, 0 tested)
2. Coverage.Analyzer.classify      (12 branches, 4 tested)
...
```

### JSON Output

```json
{
  "summary": {
    "total_canonical": 2883,
    "excluded_void": 45,
    "bugs": 3,
    "optimizer_artifacts": 12,
    "unknown": 0
  },
  "exclusion_breakdown": {
    "compiler_generated": 234,
    "standard_library": 891,
    "type_constructors": 85,
    "dependencies": 156
  },
  "high_impact_targets": [
    {
      "funcName": "Coverage.Parser.parseCase",
      "branchCount": 15,
      "executedCount": 0,
      "kind": "untested_canonical"
    }
  ]
}
```

## CI Integration

### GitHub Actions

```yaml
name: Coverage Check
on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Idris2
        run: |
          # Your Idris2 installation steps

      - name: Build coverage tool
        run: idris2 --build idris2-coverage.ipkg

      - name: Run coverage analysis
        run: ./build/exec/idris2-cov --json . > coverage.json

      - name: Check for bugs
        run: |
          bugs=$(jq '.summary.bugs' coverage.json)
          if [ "$bugs" -gt 0 ]; then
            echo "Found $bugs partial functions!"
            exit 1
          fi
```

### Exit Codes

| Code | Meaning | CI Action |
|------|---------|-----------|
| 0 | No coverage gaps | Pass |
| 1 | Coverage gaps found | Warn or fail |
| 2 | Analysis error | Fail |

## Understanding Branch Classification

| Classification | Meaning | Action |
|---------------|---------|--------|
| `canonical` | Normal, testable branches | Write tests |
| `excluded_void` | Provably unreachable (void/absurd) | Ignore |
| `bugs` | Partial functions with missing cases | Fix code |
| `optimizer_artifacts` | Compiler optimization residue | Ignore |
| `unknown` | Unclassified CRASH | Investigate |

### The "Bugs" Category

When you see `bugs > 0`, it means your code has partial functions:

```idris
-- This will show as a "bug":
unsafeHead : List a -> a
unsafeHead (x :: _) = x
-- Missing: unsafeHead [] = ???
```

These are real coverage gaps you should fix, typically by:
1. Adding the missing case with appropriate error handling
2. Changing the type to make the case impossible (e.g., `NonEmpty a`)

## Test Discovery

idris2-coverage automatically discovers test modules:

1. **From ipkg**: Modules ending in `AllTests` in the `modules` field
2. **Filesystem fallback**: `*/Tests/*AllTests.idr` files

Test modules must export `runAllTests : IO ()`.

### Test Module Format

```idris
module MyProject.Tests.AllTests

import MyProject.Tests.ParserTests
import MyProject.Tests.AnalyzerTests

export
runAllTests : IO ()
runAllTests = do
  ParserTests.runTests
  AnalyzerTests.runTests
```

## Performance Tips

1. **Use `--top N`** to limit output when analyzing large projects
2. **Use `--uncovered`** to focus on gaps rather than full report
3. **Exclude internal modules** via config to reduce noise

## See Also

- [FAQ](FAQ.md) - Common questions
- [Internals](INTERNALS.md) - Technical details on classification
- [Architecture](ARCHITECTURE.md) - How the tool works internally
