# EVM Coverage Architecture

## Formula

```
Coverage = Profiler HitCount (numerator)
           ────────────────────────────────
           Dumpcases - Exclusions (denominator)
```

## Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│                   STATIC ANALYSIS (Denominator)                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  idris2-yul --dumpcases cases.txt --build project.ipkg         │
│                        │                                        │
│                        ▼                                        │
│  ┌───────────────────────────────────────────┐                 │
│  │ DumpcasesParser.parseDumpcases            │                 │
│  │ → List ClassifiedBranch                   │                 │
│  │   • BranchId (module, func, branchIdx)    │                 │
│  │   • BranchClass (Canonical | Excluded)    │                 │
│  └───────────────────────────────────────────┘                 │
│                        │                                        │
│                        ▼                                        │
│  canonicalCount = |{b | isCanonical(b.branchClass)}|           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                   DYNAMIC ANALYSIS (Numerator)                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  idris2-yul compile → Yul source + asm.json                    │
│                        │                                        │
│  ┌─────────────────────┴─────────────────────┐                 │
│  │                                           │                  │
│  ▼                                           ▼                  │
│  ┌─────────────────┐               ┌─────────────────┐         │
│  │ Yul Source      │               │ asm.json        │         │
│  │ /* Mod:L:C */   │               │ PC → YulOffset  │         │
│  │ comments        │               │                 │         │
│  └────────┬────────┘               └────────┬────────┘         │
│           │                                  │                  │
│           ▼                                  │                  │
│  ┌─────────────────┐                        │                  │
│  │ SourceMap       │                        │                  │
│  │ parseYulComments│                        │                  │
│  │ → YulComment[]  │                        │                  │
│  └────────┬────────┘                        │                  │
│           │                                  │                  │
│           └──────────────┬──────────────────┘                  │
│                          │                                      │
│  idris2-evm-run --trace  │                                      │
│           │              │                                      │
│           ▼              │                                      │
│  ┌─────────────────┐     │                                      │
│  │ TraceParser     │     │                                      │
│  │ → (PC, hitCnt)  │     │                                      │
│  └────────┬────────┘     │                                      │
│           │              │                                      │
│           ▼              ▼                                      │
│  ┌───────────────────────────────────────────┐                 │
│  │ mapPcToIdris(YulComments, asm, PC)        │                 │
│  │ → IdrisLoc (module, line)                 │                 │
│  └───────────────────────────────────────────┘                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                      CORRELATION                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  For each IdrisLoc from trace:                                  │
│    1. Find BranchId where func matches IdrisLoc.module.func     │
│    2. Mark BranchId as hit                                      │
│                                                                 │
│  hitCount = |{b : BranchId | b.hit > 0}|                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                      AGGREGATION                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Aggregator.aggregateCoverage(static, testRuns)                │
│                                                                 │
│  Coverage% = (hitCount / canonicalCount) × 100                 │
│                                                                 │
│  Output: AggregatedCoverage                                     │
│    • totalCanonical: Nat                                        │
│    • hitCount: Nat                                              │
│    • bugsCount: Nat (UnhandledInput)                           │
│    • coveragePercent: Double                                    │
│    • uncoveredBranches: List ClassifiedBranch                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Existing Components

| Component | File | Status |
|-----------|------|--------|
| DumpcasesParser | DumpcasesParser.idr | ✅ Implemented |
| SourceMap | SourceMap.idr | ✅ Implemented |
| YulMapper | YulMapper.idr | ✅ Implemented |
| TraceParser | TraceParser.idr | ✅ Implemented |
| Aggregator | Aggregator.idr | ✅ Implemented |
| BranchId ↔ IdrisLoc correlation | - | ⚠️ Needs work |

## Missing Piece: Branch-Line Correlation

Currently:
- **DumpcasesParser** gives: `BranchId (module, func, branchIdx)`
- **SourceMap** gives: `IdrisLoc (module, line, col)`

Needed:
- Map `BranchId → SourceSpan (startLine, endLine)`
- Then check: `IdrisLoc.line ∈ BranchId.SourceSpan`

### Options

1. **Modify idris2-yul to emit source spans in dumpcases**
   - Requires changes to Idris2 compiler
   - Most accurate

2. **Use Yul comments to infer branch locations**
   - Parse Yul function bodies
   - Map case expressions to Yul offsets
   - Infer line ranges from surrounding comments

3. **Function-level approximation**
   - Simpler: just check if function was hit
   - Less granular but immediately usable

## Recommended Implementation

Phase 1: Function-level coverage
- Use existing YulMapper + TraceParser
- Coverage = hitFunctions / totalProjectFunctions

Phase 2: Branch-level coverage
- Enhance DumpcasesParser to track source spans
- Or use Yul-based inference
