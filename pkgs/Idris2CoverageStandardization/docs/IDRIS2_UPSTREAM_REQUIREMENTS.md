# Idris2 Upstream Requirements For Coverage Standardization

Status: Draft 0.1

## Why Upstream Work Is Needed

Downstream tooling can classify current `--dumpcases` output pragmatically, but
that is not sufficient for a standard that the community can treat as formal,
stable, and citable.

The weak point is compiler interface quality, not downstream ambition.

## Required Compiler Support

### 1. Machine-readable elaboration output

Human-oriented dump text is too unstable for standardization. Idris2 should
expose a structured format for elaborated case trees.

Minimum fields:

- node id
- source span
- constructor or branch label
- clause origin
- impossible status
- partial completion status
- backend insertion status

### 2. Stable obligation identifiers

Coverage must be measured against obligations that exist before optimization.
Without stable identifiers, backend artifacts can be counted as covered and
inflate the numerator.

### 3. Provenance tags

Each branch-like node should state whether it comes from:

- user-written clause
- impossible clause
- compiler-generated partial completion
- no-clause body
- optimization or backend artifact

### 4. Explicit unknown state

When the compiler cannot justify a stronger classification, it should expose an
unknown state rather than forcing downstream tools to guess.

## What Downstream Tooling Can Still Do Now

- adopt a conservative unknown bucket
- report heuristic exclusions explicitly
- block strong coverage claims when provenance is insufficient
- collect empirical evidence on real projects

## Suggested Upstream Path

1. Draft an Idris2 issue or RFC around structured case-tree export.
2. Separate semantic branches from backend-generated artifacts.
3. Add stable obligation ids to the structured output.
4. Document intended invariants for impossible clauses and partial completion.

Until then, downstream tools should describe themselves as pragmatic and
proof-aware, but not as fully standardized semantic coverage implementations.
