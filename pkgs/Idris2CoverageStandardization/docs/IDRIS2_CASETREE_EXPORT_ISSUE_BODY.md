# Issue Draft: Structured Case-Tree Export For Downstream Semantic Coverage

Idris2 already exposes useful elaboration information through `--dumpcases`,
but downstream tools currently have to recover semantic branch provenance
heuristically from human-oriented text output.

That is enough for pragmatic experiments, but not enough for a reproducible and
citable semantic coverage metric.

## Request

Please expose a machine-readable case-tree export with:

- stable same-version branch/node identifiers
- source spans
- clause provenance
- explicit impossible / partial / artifact status
- explicit unknown state when the compiler cannot justify a stronger category

A hypothetical interface could look like:

```text
idris2 --dumpcases-json out.json --build mypkg.ipkg
```

The exact flag name is not important. The capability is.

## Why This Matters

Today, downstream proof-aware coverage tooling can conservatively classify:

- reachable obligations
- logically unreachable obligations
- user-admitted partial gaps
- compiler-inserted artifacts
- unknown classifications

However, without compiler-provided provenance and stable obligation identity,
the strongest semantic claims still depend on heuristics.

In particular, downstream tools need to avoid:

- treating backend artifacts as real obligations
- silently collapsing unknowns into stronger categories
- inflating the numerator above the true source/elaboration obligations

## Minimal Per-Node Fields

- `node_id`
- `function_name`
- `source_span`
- `case_index`
- `branch_index`
- `branch_label`
- `origin`
- `impossible_status`
- `partial_status`
- `backend_artifact_status`

## Minimal Provenance Categories

- `user_clause`
- `impossible_clause`
- `compiler_partial_completion`
- `no_clause_body`
- `optimizer_artifact`
- `compiler_generated_helper`
- `unknown`

## Acceptance Criteria

This would already be sufficient for downstream semantic coverage tooling if:

1. the export is machine-readable
2. each branch-like node has a stable same-version identifier
3. provenance distinguishes user clauses, impossible clauses, partial
   completion, and backend artifacts
4. unknown provenance can be represented explicitly

## Non-Goals

This request does not ask Idris2 to:

- change totality checking
- change elaboration semantics
- endorse one specific coverage tool

It only asks Idris2 to expose enough structured provenance for conservative,
auditable downstream analysis.

## Related Drafts

- `pkgs/Idris2CoverageStandardization/docs/SEMANTIC_TEST_OBLIGATION_STANDARD.md`
- `pkgs/Idris2CoverageStandardization/docs/IDRIS2_UPSTREAM_REQUIREMENTS.md`
- `pkgs/Idris2CoverageStandardization/docs/IDRIS2_CASETREE_EXPORT_RFC_DRAFT.md`
- `pkgs/Idris2CoverageStandardization/docs/IDRIS2_CASETREE_JSON_SCHEMA.md`
