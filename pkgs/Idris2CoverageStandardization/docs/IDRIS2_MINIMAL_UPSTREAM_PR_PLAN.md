# Idris2 Minimal Upstream PR Plan

Status: pre-implementation plan  
Goal: make the maintainer review surface as small as possible

## Intent

This plan is not "implement semantic coverage in Idris2".

This plan is only:

- add a machine-readable case-tree export path
- preserve existing elaboration and totality behavior
- keep the initial review surface small enough that a maintainer can reject or
  accept it on interface grounds alone

## Review Strategy

The most important constraint is review cost.

So the first upstream PR should be intentionally narrow:

1. no public semantic metric in Idris2
2. no downstream tool coupling
3. no cross-version stability promise
4. no change to existing `--dumpcases` text behavior
5. new export behind a separate experimental flag

Recommended framing:

"This adds a structured export for case-tree data already produced internally.
It does not change elaboration, totality, or backend code generation."

## Minimum Viable PR

### New CLI Surface

Add one experimental flag, for example:

```text
--dumpcases-json <output-file>
```

This should coexist with existing:

- `--dumpcases`

and should not alter current text output.

Likely touch point:

- `/Users/bob/code/idrislang-idris2/src/Idris/CommandLine.idr`

## Output Scope For PR 1

PR 1 should export only what is already cheap and reviewable to expose.

Required:

- `function_name`
- per-node local identifier
- source span if available
- branch label if available
- a coarse origin tag

Allowed for PR 1:

- same-version stability only
- coarse provenance categories
- `unknown` whenever classification is not cleanly available

Deferred:

- stronger impossible proof grades
- cross-version stability
- typed binary format
- direct runtime-hit integration

## Suggested Provenance For PR 1

Do not try to solve every category in the first patch.

It is enough if PR 1 can distinguish:

- `user_clause`
- `impossible_clause`
- `compiler_partial_completion`
- `optimizer_artifact`
- `unknown`

If some categories are not yet recoverable cleanly, emit `unknown`.
That is preferable to a broader but shakier first export.

## Likely Internal Touch Points

### 1. Flag plumbing

- `/Users/bob/code/idrislang-idris2/src/Idris/CommandLine.idr`
- `/Users/bob/code/idrislang-idris2/src/Idris/SetOptions.idr`

Purpose:

- add a new CLI option
- thread it through session options without affecting existing behavior

### 2. Compile pipeline emission

- `/Users/bob/code/idrislang-idris2/src/Compiler/Common.idr`

Current relevant behavior:

- existing `dumpcases` is emitted from `namedDefs`

Purpose:

- add parallel structured emission beside current text emission

### 3. Provenance recovery and impossible classification

- `/Users/bob/code/idrislang-idris2/src/TTImp/ProcessDef.idr`

Current relevant behavior:

- `impossibleOK`
- `impossibleErrOK`

Purpose:

- reuse only enough internal information to tag impossible-related branches
- avoid changing the checker itself

### 4. Optimizer artifact note

- `/Users/bob/code/idrislang-idris2/src/Compiler/Opts/Constructor.idr`

Current relevant behavior:

- Nat optimization can introduce crash-like fallback nodes such as
  `"Nat case not covered"`

Purpose:

- either tag these as artifacts in export or leave them `unknown` in PR 1
- do not attempt optimizer redesign

## Proposed PR Sequence

### PR 1: Structured Export Skeleton

Contents:

- new experimental flag
- JSON writer
- function names
- per-node IDs
- basic branch labels
- source spans where already available

Claim:

- machine-readable export exists
- no promise yet that provenance is complete

Review burden:

- lowest

### PR 2: Coarse Provenance Tags

Contents:

- `user_clause`
- `impossible_clause`
- `compiler_partial_completion`
- `optimizer_artifact`
- `unknown`

Claim:

- enough provenance for conservative downstream classification

Review burden:

- moderate

### PR 3: Stabilization And Tests

Contents:

- same-version stability tests
- golden JSON fixtures
- explicit documentation of what is and is not stable

Claim:

- downstream tools can rely on the export within one compiler version

Review burden:

- moderate, but now mechanical

## Suggested JSON Shape For PR 1

Keep it flat and boring.

The proposed downstream-ready schema is written out in:

- `docs/IDRIS2_CASETREE_JSON_SCHEMA.md`

```json
{
  "compiler_version": "x.y.z",
  "functions": [
    {
      "function_name": "Main.f",
      "nodes": [
        {
          "node_id": "Main.f#0:0",
          "source_span": "Main.idr:10:1-12:10",
          "branch_label": "Cons",
          "origin": "unknown"
        }
      ]
    }
  ]
}
```

This is sufficient for a first PR. Do not overdesign the schema.

## What To Test In The Upstream PR

Minimum tests:

1. `--dumpcases` text output remains unchanged.
2. `--dumpcases-json` writes valid JSON.
3. A function with ordinary clauses appears in the export.
4. An `impossible` example can be tagged or explicitly remains `unknown`.
5. Nat optimization artifacts do not silently appear as ordinary user clauses.

If 4 or 5 are hard in PR 1, they may remain `unknown`, but the tests should
assert that they are not misclassified as user clauses.

## What Not To Do In The First PR

- do not change totality behavior
- do not redesign optimizer output
- do not claim branch-level semantic coverage is solved
- do not block on perfect provenance
- do not ask maintainers to review downstream parser changes in the same PR

## Maintainer-Friendly Promise

The right promise to make is:

"If this structured export lands, the downstream parser, migration, and
coverage-layer validation stay on our side. The upstream change stays limited to
exporting structured compiler-side information."

## Exit Criteria For Opening The PR

Open the PR only when all of these are true:

1. The new flag is isolated and optional.
2. Existing `--dumpcases` behavior is unchanged.
3. JSON shape is simple and documented.
4. Any uncertain provenance is represented as `unknown`.
5. Golden tests make the review mostly mechanical.
