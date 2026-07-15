# idris2-evm-coverage

Branch- and path-aware semantic coverage tooling for Idris2 programs compiled
to EVM.

## Positioning

`Idris2EvmCoverage` is currently the strongest downstream backend in this repo.
It reports:

- branch-level semantic test obligation coverage
- explicit `measurement`
- explicit `coverage_model`
- explicit `unknown_policy`
- explicit `claim_admissible`

It also distinguishes three important branch sets:

- `canonical branches`
  static reachable branch obligations extracted from dumpcases
- `materialized branches`
  branch obligations that still exist as observable runtime targets after EVM
  lowering/instrumentation
- `runtime observed branches`
  materialized branches actually hit during test execution

The strongest current runtime claim is therefore:

`runtime branch-level semantic coverage over the materialized denominator`

Path-level support now also exists as a downstream layer:

- `idris2-evm-cov paths --dumppaths-json ... --path-hits ...`
- `idris2-evm-cov paths --runtime-trace ... --runtime-labels ...`

The EVM path report uses the same contract as the other backends:

- `Missing paths`
- `coverage_percent`
- `claim_admissible`

## What The Tool Actually Does

At a high level:

1. extract branch obligations from Idris2 dumpcases
2. classify them into canonical / excluded / unknown buckets
3. instrument generated Yul with branch labels and counters
4. execute the EVM program under a bounded runtime
5. map runtime branch hits back to obligation IDs
6. report both the numeric measurement and whether the branch-level claim is
   admissible

## Full Pipeline

The main entrypoint is:

```bash
idris2-evm-cov --full-pipeline --ipkg path/to/project.ipkg .
```

This pipeline performs:

- static dumpcases analysis
- Yul generation
- branch counter insertion
- `solc` assembly/runtime extraction
- bounded `idris2-evm-run` execution
- runtime trace parsing
- branch hit aggregation

For path coverage, the full-pipeline story is:

1. obtain canonical paths from `--dumppaths-json`
2. recover runtime evidence from EVM labels / traces
3. emit exact missing-path lists over that same semantic denominator

## Requirements

- `idris2-yul` / `Idris2Evm`
- `solc`
- `python3`
- a working Idris2 installation

Optional but recommended:

- `IDRIS2_BIN=/path/to/forked/idris2`
- `IDRIS2_PACKAGE_PATH=/path/to/idris2/packages`

Those are used when you want static analysis to consume a forked Idris2 with
structured dumpcases JSON support.

## Runtime Controls

The runtime pipeline is intentionally bounded so that bad executions degrade to
a conservative lower bound instead of hanging forever.

Environment variables:

```bash
export IDRIS2_EVM_RUN_TIMEOUT_SECS=20
export IDRIS2_BIN=/path/to/idris2
export IDRIS2_PACKAGE_PATH=/path/to/packages
```

If the interpreter times out, the tool keeps any partial trace it can recover
and reports a lower-bound measurement rather than silently pretending success.

## Instrumentation Cost And Build Separation

Runtime branch coverage is implemented with explicit instrumentation.
That means:

- extra runtime instructions
- larger generated code
- higher execution cost during coverage runs

For EVM this cost is real gas / interpreter work. The branch-level runtime path
is therefore intended for coverage builds, not production deployments.

The intended operating model is:

- production artifact: no branch coverage instrumentation
- coverage artifact: branch counters / labels enabled

This distinction is not optional polish. It is part of the backend milestone
for making branch-level coverage operationally safe.

## Differential check: revm-run vs idris2-evm-run

The pipeline runs ONE EVM backend (`resolveEvmRunner` picks revm-run if on PATH,
else the pure-Idris idris2-evm-run). Picking one discards the strongest
correctness signal available — two INDEPENDENT EVM implementations agreeing on
the same input. If they disagree, one is wrong, and that *includes revm-run
itself*, which nothing else here cross-checks. (A whole class of bugs found
while raising TextDAO coverage were *codegen* miscompiles: a backend executing
correct bytecode wrong, or correct source compiled to wrong bytecode — exactly
what a backend-vs-backend diff catches.)

`scripts/diff-runners.sh` runs the SAME runtime bytecode + calldata through both
and asserts the SET of fired path-marker LOG topics is identical:

```bash
scripts/diff-runners.sh <runtime-bytecode> <calldata-hex>
scripts/diff-runners.sh <runtime-bytecode> --calls-file <file>   # revm-side full;
    # pure interp lacks --calls-file, so calls-file mode diffs the FIRST call only
```

PASS = both backends fired the same marker topics. FAIL = the sets differ → one
backend miscompiles/misexecutes that path. Run it across the contract's
selectors as a backend-conformance gate before trusting a single-runner coverage
number.

A differential check proves the two backends AGREE; it cannot prove either is
CORRECT (both could share a bug). `scripts/conformance-revm.sh` pins the
absolute correctness of the revm-run side against HAND-COMPUTED EVM vectors —
small programs whose RETURN / LOG / storage / status are computable by hand
(incl. `LT 2<3 == 1` and `GT 2>3 == 0`, the EVM-side counterpart to the
`<`-vs-`>` codegen bug). Together:

- `diff-runners.sh`      — relative: the two backends agree on a real contract.
- `conformance-revm.sh`  — absolute: revm-run matches the EVM spec on knowns.

A third tier — `e2e-lifecycle-oracle.sh` (lives with the contract) — asserts the
contract's full lifecycle reaches the right OBSERVABLE STATE on-chain (e.g.
approvedHeader==1), the VALUE check that branch-reachability coverage cannot do.

`scripts/verify-evm-backend.sh` BUNDLES all three into one gate — run it before
trusting a single-runner coverage number:

```bash
E2E_CALLS=<contract>/coverage-happy-path.calls REVM_TIME_STEP=10 \
  scripts/verify-evm-backend.sh /tmp/runtime-dispatch.bin <sel1> <sel2> …
# → "ALL CHECKS PASS — coverage from this backend is trustworthy."  (exit 0)
# A failure means do NOT trust the coverage %. SKIP_E2E=1 runs engine checks only.
```

Demonstrated to discriminate: with a reintroduced newtype codegen bug, coverage
merely drops 81.68%→70.99% (reads as "low coverage"), but the gate's E2E oracle
FAILS loudly (approvedHeader unset). (`REVM_SHOW_RETURN=1` surfaces a call's
RETURN data for assertions without changing default output.)

## Diagnostics

Full-pipeline runs now emit branch diagnostics data:

- `coverage-trace.csv`
- `coverage-labels.csv`
- `branch-counter-diagnostics.csv`

`branch-counter-diagnostics.csv` is the most useful when coverage looks wrong.
It lets you inspect:

- counter index
- branch obligation ID
- owning function
- whether that branch was materialized
- whether it was hit at runtime

See:

- [`docs/FULL_PIPELINE_AND_DIAGNOSTICS.md`](docs/FULL_PIPELINE_AND_DIAGNOSTICS.md)
- [`docs/COVERAGE_ARCHITECTURE.md`](docs/COVERAGE_ARCHITECTURE.md)

## Auxiliary Harness Merge

Some projects need a primary test harness plus one or more auxiliary coverage
harnesses. `Idris2EvmCoverage` can merge covered branch IDs from those auxiliary
runs into the main report.

This is useful when:

- the normal test suite does not expose all branch-heavy helper paths
- a debug harness can exercise missing semantic cases directly

The main denominator is not widened by these helpers. Only covered branch IDs
for the same materialized obligation space are unioned.

## Output Shape

The report schema is aligned with the shared standardization packages. The most
important fields are:

```json
{
  "coverage_model": "semantic test obligation coverage (branch-level)",
  "unknown_policy": "block_claim",
  "claim_admissible": true,
  "measurement": {
    "denominator_ids": ["TextDAO.Functions.Tally.finalTally#0:0"],
    "covered_ids": ["TextDAO.Functions.Tally.finalTally#0:0"],
    "excluded_ids": [],
    "unknown_ids": []
  }
}
```

When the full runtime branch mapping is available, the tool also reports:

- canonical branch count
- materialized branch count
- runtime hit count
- high-impact uncovered targets
- per-function branch summaries

## Example

```bash
cd /Users/bob/code/etherclaw/pkgs/Idris2TextDao
IDRIS2_BIN=/Users/bob/code/idrislang-idris2/build/exec/idris2 \
IDRIS2_PACKAGE_PATH=/Users/bob/.local/state/pack/install/.../idris2-0.8.0 \
idris2-evm-cov --full-pipeline --ipkg idris2-textdao.ipkg .
```

Typical text output now looks like:

```text
Coverage model: Semantic Test Obligation Coverage (Branch-Level)
Canonical branches: 574
Materialized branches: 64
Branches hit: 53 / 64
Coverage: 82%
Claim admissible (runtime branch-level): True
```

Path-focused example:

```bash
./build/exec/idris2-evm-cov paths \
  --dumppaths-json ./fixtures/path-demo/golden.paths.json \
  --path-hits ./fixtures/path-demo/sample.path-hits.txt
```

See the reproducible fixture under:

- `fixtures/path-demo/README.md`
- `fixtures/path-demo/expected.txt`

## Current Limits

This package is still downstream. It is prepared for upstream structured export,
but the final strong form still depends on Idris2 exposing stable provenance.

What is already in place:

- branch obligation IDs in downstream tooling
- optional structured dumpcases consumption via forked `IDRIS2_BIN`
- runtime branch labels in Yul instrumentation
- materialized-denominator reporting

What remains upstream-sensitive:

- stable compiler-issued obligation IDs
- fully provenance-tagged branch identity through lowering

## Milestones

The remaining milestones for the EVM backend are:

1. upstream structured dumpcases export with stable branch obligation IDs
2. stronger lowering provenance from Idris2 / idris2-yul
3. explicit production vs coverage artifact split in build workflows
4. documented instrumentation overhead budget and expected runtime cost
5. guarded enablement so branch-level runtime coverage never silently affects
   production deployments

## Related Docs

- [`../Idris2CoverageStandardization/README.md`](../Idris2CoverageStandardization/README.md)
- [`../Idris2CoverageStandardization/docs/BACKEND_MEASUREMENT_STRENGTH_MATRIX.md`](../Idris2CoverageStandardization/docs/BACKEND_MEASUREMENT_STRENGTH_MATRIX.md)
- [`docs/FULL_PIPELINE_AND_DIAGNOSTICS.md`](docs/FULL_PIPELINE_AND_DIAGNOSTICS.md)
- [`docs/COVERAGE_ARCHITECTURE.md`](docs/COVERAGE_ARCHITECTURE.md)
