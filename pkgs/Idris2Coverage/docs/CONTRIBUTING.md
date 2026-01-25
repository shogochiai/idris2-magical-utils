# Contributing Guide

Thank you for your interest in improving idris2-coverage!

## Development Setup

### Prerequisites

- Idris2 0.7.0+
- Git

### Clone and Build

```bash
git clone https://github.com/yourname/idris2-coverage.git
cd idris2-coverage
idris2 --build idris2-coverage.ipkg
```

### Run Tests

```bash
# Self-coverage analysis (also runs unit tests)
./build/exec/idris2-cov .
```

## Project Structure

```
src/
├── Main.idr                    # CLI entry point
└── Coverage/
    ├── Types.idr               # Core data types
    ├── DumpcasesParser.idr     # --dumpcases output parser
    ├── TestCoverage.idr        # High-level coverage API
    ├── UnifiedRunner.idr       # Test execution with profiling
    ├── Config.idr              # .idris2-cov.toml parser
    ├── Report.idr              # Output formatting
    └── Tests/
        └── AllTests.idr        # Unit tests
exclusions/
├── base.txt                    # Version-independent patterns
└── 0.8.0.txt                   # Idris2 0.8.0-specific patterns
scripts/
├── detect-leaks.sh             # Find missing exclusion patterns
└── report-leak.sh              # Auto-create PR for new patterns
```

## Making Changes

### 1. Create a Branch

```bash
git checkout -b fix/your-fix-description
# or
git checkout -b feat/your-feature-description
```

### 2. Make Your Changes

- Follow existing code style
- Add tests for new functionality
- Update documentation if needed

### 3. Run Tests

```bash
idris2 --build idris2-coverage.ipkg
./build/exec/idris2-cov .
```

### 4. Commit

```bash
git add .
git commit -m "fix: description of your fix"
```

Commit message prefixes:
- `fix:` - Bug fixes
- `feat:` - New features
- `docs:` - Documentation changes
- `refactor:` - Code restructuring
- `test:` - Test additions/changes

### 5. Open a PR

Push your branch and open a pull request with:
- Description of changes
- Any relevant issue numbers
- Test results

## Common Contribution Types

### Adding Exclusion Patterns

When Idris2 releases a new version, new compiler-generated patterns may appear.

**Detection**:
```bash
./scripts/detect-leaks.sh path/to/project 1000
```

**If leaks are found**:
1. Identify the pattern type (compiler-generated, stdlib, etc.)
2. Add to appropriate file in `exclusions/`
3. Or update `src/Coverage/DumpcasesParser.idr` for code-based patterns

**Pattern file format** (`exclusions/*.txt`):
```
# Comments start with #
{newpattern:*}    # Wildcard suffix
exact_match       # Exact string match
```

### Adding CLI Options

1. Update `parseArgs` in `src/Main.idr`
2. Add option to help text
3. Implement the feature
4. Document in `docs/USER_GUIDE.md`

### Adding Tests

Tests go in `src/Coverage/Tests/AllTests.idr`:

```idris
test_myFeature : IO Bool
test_myFeature = do
  result <- myFunction testInput
  pure $ result == expectedOutput

-- Add to allTests list:
allTests = [
  ...
  ("REQ_XXX_001", test_myFeature)
]
```

Test naming convention: `REQ_<AREA>_<NUMBER>`
- `COV` - Coverage analysis
- `PRS` - Parsing
- `CFG` - Configuration
- `MGL` - Name mangling

## Code Style

### General

- 2-space indentation
- 80-character line limit (soft)
- Explicit type signatures for top-level functions

### Naming

- Functions: `camelCase`
- Types/Records: `PascalCase`
- Modules: `PascalCase` with `.` separators

### Documentation

- Document exported functions with `|||` doc comments
- Add `-- Note:` for non-obvious implementation details

## Reporting Issues

### Bug Reports

Include:
1. Idris2 version: `idris2 --version`
2. idris2-cov version: `idris2-cov --version`
3. Minimal reproduction case
4. Expected vs actual behavior

### Feature Requests

Describe:
1. Use case
2. Proposed behavior
3. Any alternatives considered

## Version Tracking

When updating for new Idris2 versions:

1. Run `./scripts/detect-leaks.sh` on a large project
2. Add new patterns to `exclusions/<version>.txt`
3. Update compatibility matrix in docs
4. Release new idris2-coverage version

## Questions?

Open an issue with the `question` label.
