# Upstream Issue Prep Packet

Status: pre-issue briefing  
Audience: Idris2 maintainers, downstream tool authors, issue author

## One-Page Briefing

### Problem

Idris2 already exposes useful elaboration information through `--dumpcases`,
but downstream tools still have to infer semantic provenance from human-oriented
text output.

That is enough to build pragmatic coverage tooling, but not enough to make a
strong, citation-friendly semantic coverage claim without caveats.

The missing piece is not "more runtime profiling". The missing piece is a
stable, machine-readable upstream interface for obligation identity and
provenance.

### Current Downstream Workaround

Current downstream tooling can already classify observed entities into:

- `ReachableObligation`
- `LogicallyUnreachable`
- `UserAdmittedPartialGap`
- `CompilerInsertedArtifact`
- `UnknownClassification`

It can also report:

- the selected profile
- the unknown-handling policy
- whether the semantic claim is admissible

This is enough for conservative experiments and CI gating.

### Why The Workaround Is Insufficient

Three things still depend on heuristics:

1. Provenance recovery from text-oriented `--dumpcases` output.
2. Stable identity for branch-like obligations before backend artifacts appear.
3. Separation between compiler-generated artifacts and true source/elaboration
   obligations.

Without an upstream interface, a downstream tool can still report a number, but
it cannot always justify the strongest semantic claim.

### Minimal Upstream Ask

Expose a machine-readable case-tree export with:

- stable same-version node identifiers
- source spans
- clause provenance
- explicit impossible / partial / artifact status
- explicit `unknown` when the compiler cannot justify a stronger category

This does not require changing totality checking or elaboration semantics.

## Concrete Examples

### Example 1: Unknown Must Remain Public

Downstream classification already has a first-class unknown bucket.

If a tool collapses unknowns into stronger categories, it can overstate the
semantic meaning of its reported coverage.

That is why the standard exposes:

- `UnknownClassification`
- `UnknownPolicy`
- `claim_admissible`

This is not cosmetic. It is the boundary between a numeric measurement and a
semantic claim.

### Example 2: Function-Level Is Currently Stronger Than Branch-Level

Today a downstream tool can usually map runtime evidence back to executable
functions more defensibly than to stable branch identities.

That is why the current practical profile is:

- `function-level semantic test obligation coverage`

and not an unconditional branch-level claim.

The branch-level profile still needs upstream node identity and provenance.

### Example 3: Artifact Leakage Must Be Prevented

`--dumpcases` and backend pipelines can surface crash-like or helper-like nodes
that are not genuine source/elaboration obligations.

If those leak into the numerator, a tool can report values that do not match the
true obligation set, including values above the intended source-level coverage
space.

That is why provenance-tagged upstream export matters.

## What We Are Claiming Today

- A conservative downstream implementation exists.
- It can support useful CI gating today.
- It can report profile, unknown policy, and claim admissibility explicitly.
- The strongest current practical profile is function-level semantic test
  obligation coverage.

## What We Are Not Claiming Today

- We are not claiming that current Idris2 output is already a complete
  compiler-backed semantic coverage interface.
- We are not claiming that branch-level semantic coverage is fully solved
  downstream.
- We are not asking Idris2 to endorse one specific tool.
- We are not asking Idris2 to change totality semantics.

## Minimal Evidence To Have Ready Before Filing

1. A short definition of the metric.
2. A short explanation of `claim_admissible`.
3. One example where unknown must remain explicit.
4. One example where function-level is currently the strongest safe profile.
5. One example where artifact provenance matters.

## Likely Maintainer Questions

See:

- `docs/UPSTREAM_RESPONSE_TRAINING_PACK.md`

