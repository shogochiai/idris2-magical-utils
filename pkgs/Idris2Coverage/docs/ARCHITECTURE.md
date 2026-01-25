# Architecture

This document describes the high-level architecture of idris2-coverage.

## Overview

idris2-coverage is a two-phase coverage tool:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        ANALYSIS PIPELINE                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  Phase 1: Static Analysis (Denominator)                              │
│  ────────────────────────────────────────                            │
│  idris2 --dumpcases project.ipkg                                     │
│          │                                                           │
│          ▼                                                           │
│  DumpcasesParser.idr  ──→  List CompiledFunction                     │
│          │                                                           │
│          ▼                                                           │
│  Classify each CRASH node by message                                 │
│          │                                                           │
│          ▼                                                           │
│  BranchClass: Canonical | Excluded | Bug | ...                       │
│                                                                       │
│  Phase 2: Runtime Analysis (Numerator)                               │
│  ──────────────────────────────────────                              │
│  Build test binary with --profile                                    │
│          │                                                           │
│          ▼                                                           │
│  Execute tests (Chez Scheme profiler active)                         │
│          │                                                           │
│          ▼                                                           │
│  Parse .ss.html profiler output                                      │
│          │                                                           │
│          ▼                                                           │
│  Map Scheme function hits → Idris function names                     │
│                                                                       │
│  Final: Coverage = executed_canonical / total_canonical              │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

## Module Responsibilities

### Core Modules

| Module | Responsibility |
|--------|----------------|
| `Main.idr` | CLI parsing, orchestration, output |
| `Types.idr` | Core data types (BranchId, BranchClass, etc.) |
| `DumpcasesParser.idr` | Parse `--dumpcases` output, classify branches |
| `TestCoverage.idr` | High-level API, target filtering |
| `UnifiedRunner.idr` | Build and run tests with profiling |
| `Collector.idr` | Parse .ss.html, map to Idris functions |
| `Report.idr` | Format output (text, JSON) |
| `Config.idr` | Parse .idris2-cov.toml |

### Data Flow

```
                    ┌──────────────┐
                    │   Main.idr   │
                    └──────┬───────┘
                           │
           ┌───────────────┼───────────────┐
           │               │               │
           ▼               ▼               ▼
    ┌──────────┐    ┌───────────┐    ┌──────────┐
    │ Config   │    │ TestCov   │    │ Report   │
    │ .idr     │    │ erage.idr │    │ .idr     │
    └──────────┘    └─────┬─────┘    └──────────┘
                          │
              ┌───────────┼───────────┐
              │           │           │
              ▼           ▼           ▼
       ┌──────────┐ ┌──────────┐ ┌──────────┐
       │ Dumpcases│ │ Unified  │ │Collector │
       │ Parser   │ │ Runner   │ │ .idr     │
       └──────────┘ └──────────┘ └──────────┘
              │           │           │
              ▼           ▼           ▼
         Types.idr    (subprocess)  Types.idr
```

## Key Data Types

### BranchId

Uniquely identifies a case branch:

```idris
record BranchId where
  constructor MkBranchId
  moduleName  : String    -- "Coverage.Parser"
  funcName    : String    -- "parseCase"
  caseIndex   : Nat       -- Which %case block (0-indexed)
  branchIndex : Nat       -- Which branch in that case
```

### BranchClass

Classification of each branch:

```idris
data BranchClass
  = BCCanonical              -- Normal, testable branch
  | BCExcludedNoClauses      -- void/absurd (excluded from denominator)
  | BCOptimizerNat           -- Nat→Integer artifact (excluded)
  | BCBugUnhandledInput      -- Partial function (counts as bug)
  | BCUnknownCrash           -- Unrecognized CRASH (conservative: counted)
```

### CompiledFunction

Aggregated function data:

```idris
record CompiledFunction where
  constructor MkCompiledFunction
  funcName     : String
  totalCases   : Nat
  branchCounts : List (BranchClass, Nat)
```

## Exclusion System

### Three-Layer Exclusion

1. **Code-based** (DumpcasesParser.idr):
   - `isCompilerGenerated`: `{csegen:*}`, `prim__*`, `_builtin.*`
   - `isStandardLibrary`: `Prelude.*`, `Data.*`, `System.*`
   - `isTypeConstructor`: Names ending with `.`

2. **File-based** (exclusions/):
   - `base.txt`: Version-independent patterns
   - `<version>.txt`: Version-specific patterns

3. **Config-based** (.idris2-cov.toml):
   - `module_prefixes`: User-defined exclusions
   - `packages`: Additional package exclusions

### Exclusion Flow

```
Function Name
     │
     ├── Is compiler-generated? ──→ Exclude (compiler_generated)
     │
     ├── Is standard library? ──→ Exclude (standard_library)
     │
     ├── Is type constructor? ──→ Exclude (type_constructors)
     │
     ├── Is from depends? ──→ Exclude (dependencies)
     │
     ├── Matches config prefix? ──→ Exclude (user-defined)
     │
     └── Otherwise ──→ Include in targets
```

## Name Mangling

Idris2 compiles to Chez Scheme with mangled names:

```
Idris:  Coverage.Parser.parseCase
Scheme: CoverageC-45Parser-parseCase
```

### Encoding Rules

- Alphanumeric and `_`: unchanged
- All other characters: `C-<ASCII_CODE>`
- `.` (module separator): `-` or `C-45`

Implementation in `Collector.idr`:

```idris
chezEncodeChar : Char -> String
chezEncodeChar c =
  if isAlphaNum c || c == '_'
     then singleton c
     else "C-" ++ show (ord c)

chezMangle : String -> String
-- "Prelude.IO" → "PreludeC-45IO"
```

## Test Discovery

### Algorithm

1. **ipkg-based**: Look for modules ending in `AllTests` in ipkg's `modules` field
2. **Filesystem fallback**: Search for `*/Tests/*AllTests.idr`

### Execution

```
1. Discover test modules
2. Generate test runner that imports all modules
3. Compile with --profile flag
4. Execute, collecting profiler output
5. Parse .ss.html for execution counts
6. Map to Idris functions via chezMangle
```

## Output Generation

### Target Ranking

Functions are ranked by "severity" (impact score):

```
severity = branchCount / (executedCount + 1)
```

Higher severity = more branches with less coverage = higher priority.

### JSON Schema

```json
{
  "summary": {
    "total_canonical": 2883,
    "excluded_void": 45,
    "bugs": 3
  },
  "exclusion_breakdown": {
    "compiler_generated": 234,
    "standard_library": 891,
    "type_constructors": 85,
    "dependencies": 156
  },
  "high_impact_targets": [
    {
      "funcName": "Module.function",
      "branchCount": 15,
      "executedCount": 0,
      "kind": "untested_canonical",
      "severity": "Inf"
    }
  ]
}
```

## Design Decisions

### Why Downstream-Only?

Modifying the Idris2 compiler would:
- Create maintenance burden
- Risk rejection by upstream
- Limit compatibility with different Idris2 versions

By using only `--dumpcases` and profiler output, we stay independent.

### Why Conservative Classification?

Unknown CRASH messages are counted (not excluded) because:
- False negatives (missing real bugs) are worse than false positives
- Users can investigate and add patterns
- Transparency is more important than optimistic numbers

### Why Chez Scheme Profiler?

- Already installed with standard Idris2
- Provides expression-level granularity
- No additional dependencies needed

## Future Considerations

1. **Source maps**: If Idris2 adds source map support, we could achieve perfect function mapping
2. **Alternative backends**: Currently Chez-specific; could extend to other backends
3. **Incremental analysis**: Cache results for unchanged files
