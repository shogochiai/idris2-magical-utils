# Idris2 Upstream Merge-Ready Checklist

Status: downstream preparation checklist

This is the checklist for the point where the upstream work is no longer
"design discussion" and is instead "reviewable patch".

## Patch Scope

- add a new experimental structured export flag
- do not change existing `--dumpcases` text behavior
- do not change totality checking
- do not change elaboration semantics
- do not change backend code generation

## Output Shape

- machine-readable JSON
- top-level `compiler_version`
- top-level `functions`
- per-node `node_id`
- per-node `function_name` through function object
- per-node `branch_index`
- coarse provenance via `origin`
- explicit `unknown` category

## Review-Surface Constraints

- one small CLI plumbing change
- one writer path beside existing text dumpcases
- no downstream package dependency
- no public promise beyond same-version stability

## Tests Required For "Only Merge Remains"

- one golden JSON fixture for a tiny project
- one golden JSON fixture containing an impossible clause
- one golden JSON fixture containing compiler partial completion
- one golden JSON fixture containing an optimizer artifact or explicit `unknown`
- one same-version determinism test for `node_id`

## Downstream Readiness Already In Place

These are already implemented downstream:

- structured JSON adapter in `Idris2EvmCoverage`
- same report schema as current legacy parser
- `lazy evm ask --steps=4` unchanged at the API layer
- conservative fallback to legacy text parser when structured export is absent

## Remaining Truly Upstream-Blocked Capability

The only capability still blocked on upstream is:

- compiler-backed stable branch/node provenance

Once that lands, downstream can stop using legacy text recovery for the static
branch layer. Runtime branch counters are a separate backend task.
