# Full Pipeline And Diagnostics

This note explains how to run `Idris2EvmCoverage` in its strongest current
mode and how to read the generated diagnostics.

## Recommended Invocation

```bash
cd /path/to/project
IDRIS2_BIN=/path/to/forked-or-local/idris2 \
IDRIS2_PACKAGE_PATH=/path/to/idris2/packages \
IDRIS2_EVM_RUN_TIMEOUT_SECS=20 \
idris2-evm-cov --full-pipeline --ipkg path/to/project.ipkg .
```

## What `--full-pipeline` Does

`--full-pipeline` is not just report formatting. It runs the whole coverage
pipeline:

1. static dumpcases extraction
2. branch classification
3. Yul generation
4. branch counter instrumentation
5. runtime bytecode extraction
6. bounded EVM execution
7. runtime trace recovery
8. branch hit aggregation
9. auxiliary harness merge, when present

## Important Environment Variables

### `IDRIS2_BIN`

Optional Idris2 binary override.

Use this when:

- testing a fork with `--dumpcases-json`
- keeping coverage analysis pinned to a specific compiler build

### `IDRIS2_PACKAGE_PATH`

Optional package path override for the Idris2 binary selected above.

### `IDRIS2_EVM_RUN_TIMEOUT_SECS`

Controls how long the EVM execution phase is allowed to run before it is
terminated and downgraded to a conservative lower bound.

This is important because the runtime phase should fail conservatively, not hang
the entire pipeline indefinitely.

## Generated Files

The output directory typically contains:

- `coverage-report.json`
- `coverage-trace.csv`
- `coverage-labels.csv`
- `branch-counter-diagnostics.csv`

## How To Read `branch-counter-diagnostics.csv`

This file correlates runtime counters with semantic branch identity.

Typical columns answer questions like:

- which counter index maps to which branch obligation ID
- which function owns that branch
- whether the branch was materialized
- whether the branch was hit

This is the first file to inspect when:

- coverage seems unexpectedly low
- a branch appears uncovered even though a test should hit it
- materialized branch count looks suspicious

## Canonical vs Materialized vs Covered

Three counts matter:

- `canonical branches`
  static semantic branches from dumpcases
- `materialized branches`
  branches that remain directly observable after EVM lowering
- `covered branches`
  materialized branches actually hit at runtime

Example:

```text
Canonical branches: 574
Materialized branches: 64
Branches hit: 53 / 64
Coverage: 82%
Claim admissible (runtime branch-level): True
```

Interpretation:

- static semantic model sees 574 branch obligations
- only 64 survive as direct runtime-observable branch counters
- 53 of those 64 were hit
- the runtime branch-level claim is admissible because numerator and denominator
  live on the same materialized branch layer

## Auxiliary Harness Merge

If the tool finds auxiliary coverage harnesses, it can merge their covered
branch IDs into the same materialized denominator.

This is useful for projects where:

- the main harness covers realistic flows
- a small direct harness is needed for helper-heavy corner cases

The merge is conservative:

- denominator stays fixed
- only covered branch IDs are unioned

## Failure Modes

### Runtime times out

The run is bounded. The tool should:

- terminate the subprocess group
- keep partial trace data if available
- report a lower-bound result instead of hanging

### Branch expected but not hit

Check:

1. `branch-counter-diagnostics.csv`
2. `coverage-labels.csv`
3. whether the branch is canonical but not materialized
4. whether the test should move to an auxiliary harness

### Static and runtime counts disagree

This is expected to some degree on EVM.

The key question is not "why are canonical and materialized identical?" but
"does the runtime claim stay on the materialized obligation layer?".

## Best Current Use

Today the best use of the tool is:

- branch-level guidance
- high-impact uncovered targets
- conservative runtime branch measurement over the materialized denominator

It is already useful for real projects, but the final provenance story still
improves further if upstream Idris2 structured export is merged.
