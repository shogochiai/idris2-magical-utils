# idris2-coverage-standardization

Standardization work for a proof-aware, citation-friendly test coverage concept
for Idris2 and other dependently typed languages.

## Scope

This package is not a coverage runner. It defines the vocabulary, boundaries,
and interface requirements needed to turn pragmatic downstream tooling into a
concept that can be evaluated, compared, and cited.

The current focus is to standardize:

- executable test obligations
- obligation classification
- denominator and numerator rules
- soundness envelope
- reproducibility protocol
- upstream compiler interface requirements

## Current Position

This package defines the draft standard and the vocabulary around it.
It does not claim that Idris2 already exposes a fully sufficient upstream
interface.

Today the practical downstream target is:

- explicit obligation classes
- explicit profile selection
- explicit unknown handling
- explicit `claim_admissible` reporting

That is enough for conservative experiments and CI gating, but not yet enough
for a compiler-backed final standard.

## Non-Goal

This package does not patch Idris2. In particular, stable provenance-tagged
compiler output is an upstream requirement, not something downstream tooling can
fully solve on its own.

## Initial Deliverables

- A typed core model in `Coverage.Standardization.*`
- A written draft standard in `docs/SEMANTIC_TEST_OBLIGATION_STANDARD.md`
- A backend measurement comparison note in
  `docs/BACKEND_MEASUREMENT_STRENGTH_MATRIX.md`
- An Idris2 upstream requirements memo in `docs/IDRIS2_UPSTREAM_REQUIREMENTS.md`
- An upstream RFC draft in `docs/IDRIS2_CASETREE_EXPORT_RFC_DRAFT.md`
- A GitHub-issue-ready upstream request in `docs/IDRIS2_CASETREE_EXPORT_ISSUE_BODY.md`
- A proposed structured JSON schema in `docs/IDRIS2_CASETREE_JSON_SCHEMA.md`
- A pre-issue briefing in `docs/UPSTREAM_ISSUE_PREP_PACKET.md`
- A maintainer-response training pack in
  `docs/UPSTREAM_RESPONSE_TRAINING_PACK.md`
- A maintainer-friendly minimal PR plan in
  `docs/IDRIS2_MINIMAL_UPSTREAM_PR_PLAN.md`
- A merge-ready upstream checklist in
  `docs/IDRIS2_UPSTREAM_MERGE_READY_CHECKLIST.md`

## Recommended Citation Surface

If you need one short concept name, use:

`semantic test obligation coverage`

If you need to be more precise, include the profile:

- `branch-level semantic test obligation coverage`
- `function-level semantic test obligation coverage`

## Build

```bash
idris2 --build idris2-coverage-standardization.ipkg
```
