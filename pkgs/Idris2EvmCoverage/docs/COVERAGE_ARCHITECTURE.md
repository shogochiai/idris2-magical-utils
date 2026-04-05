# EVM Coverage Architecture

This document describes the current architecture of `Idris2EvmCoverage` as it
exists today, not the older function-hit approximation.

## Coverage Layers

The EVM backend now tracks three layers explicitly.

### 1. Canonical branch obligations

These come from static dumpcases analysis.

- source: Idris2 dumpcases text or structured JSON
- identity: downstream branch obligation IDs, prepared for compiler-issued IDs
- classes: reachable, excluded, unknown, partial-gap, artifact

This is the largest semantic denominator the backend currently knows about.

### 2. Materialized branch obligations

Not every canonical obligation survives lowering as an independently observable
runtime branch in generated Yul/EVM code.

The tool therefore computes a second denominator:

- branch obligations that still materialize as labeled runtime branch counters

This is the denominator used for the strongest current runtime branch-level
claim.

### 3. Runtime observed branches

These are the materialized branch obligations whose counters were actually hit
during test execution.

## Pipeline

```text
Idris2 source
  -> dumpcases analysis
  -> branch obligation classification
  -> idris2-yul / Yul generation
  -> branch counter instrumentation
  -> solc runtime bytecode extraction
  -> bounded idris2-evm-run execution
  -> trace / label recovery
  -> counter index -> branch ID mapping
  -> runtime observed branch set
```

## Static Side

Primary modules:

- `EvmCoverage.DumpcasesParser`
- `EvmCoverage.Types`
- `EvmCoverage.StructuredExport`

Responsibilities:

- produce canonical branch obligations
- preserve obligation classes
- retain enough identity to swap to compiler-issued branch IDs later

## Runtime Side

Primary modules:

- `EvmCoverage.YulInstrumentor`
- `EvmCoverage.Runtime`

Responsibilities:

- insert branch labels/counters into Yul
- extract runtime bytecode correctly from `solc --bin`
- emit runtime label files and diagnostics CSV
- execute `idris2-evm-run` under a bounded timeout
- recover covered branch IDs from trace output

## Diagnostics

The runtime side emits:

- `coverage-trace.csv`
- `coverage-labels.csv`
- `branch-counter-diagnostics.csv`

The diagnostics CSV is the fastest way to answer:

- which branch counter corresponds to which branch ID
- whether a branch is materialized
- whether a branch was hit
- whether an uncovered branch is a likely test gap or a mapping problem

## Auxiliary Harness Merge

The CLI can merge coverage from auxiliary harnesses into the same report.

Use this when:

- the main test suite reaches most of the contract
- but small direct harnesses are needed for branch-heavy helpers

The merge rule is conservative:

- keep the same materialized denominator
- union only covered branch IDs

## Claim Boundary

The runtime branch-level claim is admissible only when the report can stay on a
single obligation layer.

In practice that means:

- use materialized branch obligations as the runtime denominator
- count only runtime observations mapped back to those same materialized branch IDs
- keep unknown handling explicit

That is why the report can legitimately say:

```text
Claim admissible (runtime branch-level): True
```

even when the canonical static denominator is larger than the materialized
runtime denominator.

## Remaining Upstream Dependency

The backend is already prepared for structured compiler export, but the final
strong form still depends on Idris2 exposing stable provenance-tagged branch
obligation IDs.

Until that lands, `Idris2EvmCoverage` remains:

- branch-level
- highly useful
- but still downstream in provenance origin
