# Issue: Add source spans to --dumpcases output

## Summary

Add source location information to `--dumpcases` output to enable accurate branch-level code coverage tools.

## Current Behavior

```
Main.Functions.Factory.createUpgrader = [{ext:0}]: (%case (sload [...]) [(%constcase 0 ...), (%constcase 1 ...)] Nothing)
```

## Proposed Behavior

```
Main.Functions.Factory.createUpgrader [src/Main/Functions/Factory.idr:65:1--87:8] = [{ext:0}]: (%case [src/Main/Functions/Factory.idr:70:3--75:10] (sload [...]) [(%constcase 0 [72:5--73:20] ...), (%constcase 1 [74:5--75:10] ...)] Nothing)
```

## Motivation

Coverage tools need to correlate runtime execution data with static branch analysis. Currently:

1. `--dumpcases` provides function names and case tree structure
2. Runtime profilers provide hit counts at source locations (line:col)
3. **Missing**: The mapping between case branches and source locations

Without source spans, coverage tools must use heuristics to correlate branches with executed code, leading to inaccurate coverage percentages.

## Use Cases

1. **idris2-coverage** - Chez Scheme backend coverage
2. **idris2-evm-coverage** - EVM/Yul backend coverage
3. **IDE integration** - Highlighting uncovered branches
4. **CI/CD** - Coverage thresholds and reporting

## Implementation Notes

The `FC` (File Context) information is already available in the compiler during case tree generation. This proposal is to include it in the `--dumpcases` serialization.

Relevant modules:
- `Idris.REPL.Opts` - Where `--dumpcases` is processed
- `Core.Case.CaseTree` - Case tree representation
- `Core.FC` - File context (source spans)

## Backwards Compatibility

This is an additive change to the output format. Existing parsers that don't need source spans can ignore them.

## Related

- https://github.com/idris-lang/Idris2/pull/3708 (recent dumpcases improvements)
