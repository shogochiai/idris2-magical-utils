# Option B: EVM-specific Coverage Implementation

## Overview

Use Yul source comments + asm.json to correlate EVM execution traces with Idris2 functions.

```
Yul comments: /* Module:line:col--line:col */
      +
asm.json: PC → Yul offset
      +
EVM trace: PC → hitCount
      ↓
Function-level coverage (expandable to branch-level)
```

## Phase 1: Function-Level Coverage

### Data Flow

```
1. idris2-yul compile project.ipkg
   → build/exec/output.yul (with /* loc */ comments)
   → build/exec/output.json (asm.json with PC mapping)

2. idris2-evm-run --trace contract.evm < input
   → trace.csv (step, pc, opcode, name, stack_depth)

3. Coverage calculation:
   trace.csv → TraceParser → List (PC, hitCount)
   output.yul → SourceMap.parseYulComments → List YulComment
   output.json → AsmJsonParser → List AsmInstr

   mapPcToIdris(comments, asm, pc) → IdrisLoc (module, line)

   Group by module.function → hitFunctions

   Coverage = |hitFunctions| / |projectFunctions|
```

### Files to Modify

1. **EvmCoverage/FunctionCoverage.idr** (NEW)
   - `aggregateFunctionHits : List IdrisLoc -> List (String, Nat)`
   - `calculateFunctionCoverage : StaticBranchAnalysis -> List (String, Nat) -> CoverageResult`

2. **EvmCoverage/EvmCoverage.idr** (MODIFY)
   - Add `runFunctionCoverage : Config -> IO CoverageResult`
   - Integrate Yul + asm.json + trace

3. **Evm/Ask/Ask.idr** (MODIFY)
   - Update Step 4 to use function-level coverage with runtime data

### Required Inputs

| Input | Source | Parser |
|-------|--------|--------|
| Yul source | idris2-yul compile | SourceMap.parseYulComments |
| asm.json | idris2-yul compile | AsmJsonParser |
| EVM trace | idris2-evm-run --trace | TraceParser |
| Dumpcases | idris2-yul --dumpcases | DumpcasesParser |

## Phase 2: Branch-Level Coverage (Future)

### Yul Case Expression Pattern

```yul
/* Main.Functions.Factory:70:3--75:10 */
switch sload(slot)
case 0 {
    /* Main.Functions.Factory:72:5--73:20 */
    ...
}
case 1 {
    /* Main.Functions.Factory:74:5--75:10 */
    ...
}
```

### Implementation

1. Parse Yul `switch/case` blocks with their source comments
2. Map each case to a line range
3. Correlate with dumpcases `%case` branches by function + order
4. Track hit counts per branch

## Testing

1. Create test contract with known branches
2. Run with specific inputs to hit certain branches
3. Verify coverage matches expected

## Example Output

```
$ lazy evm ask --steps=4

[Step 4] EVM function coverage... Result: OK
  Static analysis (dumpcases):
    Canonical branches: 45
    Excluded (impossible): 12
    Bugs (unhandled): 0

  Runtime coverage (trace):
    Functions hit: 8 / 12 (66.7%)
    Branches hit: 32 / 45 (71.1%)

  Uncovered functions:
    Main.Functions.Tally.executeTally
    Main.Functions.Vote.revokeVote
    Main.Functions.AssignAuditor.removeAuditor
    Main.Functions.Fee.refundFee
```
