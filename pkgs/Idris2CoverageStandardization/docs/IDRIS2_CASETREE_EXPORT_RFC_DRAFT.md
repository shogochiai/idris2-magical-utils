# RFC Draft: Structured Case-Tree Export For Semantic Coverage

Status: Draft for upstream discussion

## Summary

Idris2 should expose a machine-readable export of elaborated case trees with
stable obligation identifiers and provenance tags.

The goal is not limited to coverage tooling. This also improves downstream
analysis, auditability, and tool interoperability around totality, partiality,
and elaboration artifacts.

## Problem

Today, downstream tools rely on `--dumpcases`, which is useful but not stable
enough for standardization.

Current limitations:

- output is human-oriented text rather than structured data
- branch provenance must be inferred heuristically
- backend or optimizer artifacts can leak into downstream metrics
- stable pre-optimization obligation identifiers are unavailable

This makes semantic coverage possible as a pragmatic experiment, but difficult
to formalize as a reproducible and citable metric.

## Minimal Request

This RFC is intentionally narrow. It does not ask Idris2 to endorse a single
coverage tool or metric. It asks Idris2 to expose enough structured
compiler-side provenance for downstream tools to make conservative, auditable
claims.

## Proposal

Add a structured export mode for elaborated case trees, for example:

```text
idris2 --dumpcases-json out.json --build mypkg.ipkg
```

The exact flag name is not important. The required capability is.

## Required Fields Per Node

- `node_id`: stable identifier for the branch-like node
- `function_name`
- `source_span`
- `case_index`
- `branch_index`
- `branch_label`
- `origin`
- `impossible_status`
- `partial_status`
- `backend_artifact_status`

At minimum, `node_id`, `origin`, and a source span are required for downstream
semantic coverage tooling to avoid heuristic-only classification.

## Required Provenance Categories

The `origin` field should distinguish at least:

- `user_clause`
- `impossible_clause`
- `compiler_partial_completion`
- `no_clause_body`
- `optimizer_artifact`
- `compiler_generated_helper`
- `unknown`

## Stability Requirement

`node_id` must be assigned before backend optimization so runtime hits can map
back to the same obligation identity without numerator inflation.

Without this, a downstream tool can observe more covered nodes than true source
obligations and produce values above 100%.

Version-stability across compiler releases is desirable, but same-version
stability is the minimum requirement.

## Example Shape

```json
{
  "compiler_version": "0.8.0",
  "functions": [
    {
      "function_name": "Main.safeHead",
      "nodes": [
        {
          "node_id": "Main.safeHead#0:0",
          "source_span": "Main.idr:10:1-12:20",
          "case_index": 0,
          "branch_index": 0,
          "branch_label": "Nil",
          "origin": "impossible_clause",
          "impossible_status": "proven",
          "partial_status": "not_partial",
          "backend_artifact_status": "not_artifact"
        },
        {
          "node_id": "Main.safeHead#0:1",
          "source_span": "Main.idr:10:1-12:20",
          "case_index": 0,
          "branch_index": 1,
          "branch_label": "Cons",
          "origin": "user_clause",
          "impossible_status": "reachable",
          "partial_status": "not_partial",
          "backend_artifact_status": "not_artifact"
        }
      ]
    }
  ]
}
```

## Non-Goals

- redefining totality checking
- changing elaboration semantics
- forcing one downstream coverage metric

This RFC only asks Idris2 to expose enough provenance and identity for sounder
downstream analysis.

## Acceptance Criteria

An upstream implementation is sufficient for this RFC if all of the following
are true:

1. Case-tree output is machine-readable.
2. Each branch-like node has a stable same-version identifier.
3. Clause provenance distinguishes user clauses, impossible clauses, partial
   completion, and backend artifacts.
4. Unknown provenance can be represented explicitly instead of being collapsed
   into stronger categories.

## Expected Downstream Benefits

- semantic coverage metrics with explicit uncertainty handling
- stronger CI gating for dependently typed projects
- reproducible benchmark suites
- easier separation of logical unreachability from runtime test obligations

## Open Questions

1. Should `node_id` be guaranteed stable across compiler versions, or only
   within one compiler version?
2. Should impossible-status be binary, or should it distinguish stronger and
   weaker compiler justifications?
3. Should helper-generated local functions be exported inline or as separate
   nodes?
4. Is JSON enough, or should Idris2 expose a typed binary format as well?
