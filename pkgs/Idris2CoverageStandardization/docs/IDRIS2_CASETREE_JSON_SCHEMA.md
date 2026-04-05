# Idris2 Structured Case-Tree JSON Schema

Status: downstream-ready, upstream-proposed

This is the smallest JSON shape that current downstream semantic coverage
tooling can consume without changing its reporting or gating layers.

## Goal

Provide enough structured case-tree data for:

- branch-level denominator construction
- conservative origin classification
- stable same-version node identity
- future runtime branch-id instrumentation

## Top-Level Shape

```json
{
  "compiler_version": "0.8.0",
  "functions": [
    {
      "function_name": "Main.f",
      "nodes": [
        {
          "node_id": "Main.f#0:0",
          "source_span": "Main.idr:10:1-12:10",
          "case_index": 0,
          "branch_index": 0,
          "branch_label": "Cons",
          "origin": "user_clause",
          "impossible_status": "reachable",
          "partial_status": "complete",
          "backend_artifact_status": "none"
        }
      ]
    }
  ]
}
```

## Required Fields

### Top level

- `compiler_version`
- `functions`

### Function object

- `function_name`
- `nodes`

### Node object

- `node_id`
- `branch_index`
- `origin`

## Strongly Recommended Fields

- `source_span`
- `case_index`
- `branch_label`
- `impossible_status`
- `partial_status`
- `backend_artifact_status`

## Minimum Semantics

### `node_id`

- stable within one compiler version for the same elaborated definition
- unique within one export file

### `origin`

The first useful set is:

- `user_clause`
- `impossible_clause`
- `compiler_partial_completion`
- `no_clause_body`
- `optimizer_artifact`
- `compiler_generated_helper`
- `unknown`

### status fields

These do not need to be sophisticated in PR 1.
It is enough if they allow conservative downstream classification.

- `impossible_status`: `reachable` | `impossible` | `unknown`
- `partial_status`: `complete` | `compiler_partial_completion` | `unknown`
- `backend_artifact_status`: `none` | `optimizer_artifact` | `compiler_generated` | `unknown`

## Downstream Consumption Today

Current `Idris2EvmCoverage` is already prepared to:

- parse this schema
- preserve `node_id`
- map origin tags into current branch classes
- keep existing report and `lazy evm ask --steps=4` APIs unchanged

That means an upstream export matching this schema would remove the current
text-parser heuristic from the static branch layer immediately.
