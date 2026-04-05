# Upstream Response Training Pack

Status: issue follow-up support  
Audience: issue author and downstream implementer

## Goal

This pack is for two jobs:

1. answering maintainer follow-up questions clearly
2. implementing the smallest reasonable downstream follow-up after feedback

It is intentionally conservative. Prefer "we do not claim that yet" over
stretching the current evidence.

## Core 30-Second Answer

`idris2-coverage` is not asking Idris2 to adopt a coverage tool. It is asking
Idris2 to expose enough machine-readable case-tree provenance that downstream
tools can make conservative, auditable semantic coverage claims without relying
on text-parsing heuristics.

## Short Answers To Likely Questions

### What exactly are you asking Idris2 to add?

A machine-readable case-tree export with stable same-version node IDs, source
spans, provenance tags, and an explicit `unknown` state.

### Why is `--dumpcases` not enough?

Because it is human-oriented text. Downstream tools currently have to recover
provenance heuristically, which is not a strong enough basis for a citable
semantic coverage interface.

### Why do you need stable IDs?

Because the denominator and numerator must refer to the same obligation layer.
Without stable pre-optimization IDs, runtime hits cannot always be mapped back
to the correct branch-like obligation safely.

### Why not just use line coverage?

Because dependent pattern matching introduces logically unreachable branches and
compiler artifacts that line coverage does not distinguish. The metric here is
about executable obligations, not text lines.

### Why does `unknown` need to be explicit?

Because otherwise a tool is forced either to guess or to silently collapse
unknowns into stronger classes. Both make the semantic claim less trustworthy.

### Why is the current profile function-level?

Because function-level obligation identity is currently safer to justify
downstream than stable branch-level identity.

### Are you asking Idris2 to change coverage checking?

No. The request is about structured export, not about changing totality or
coverage semantics.

## Medium-Length Answers

### Why separate numeric measurement from admissible claim?

A tool can often compute a number from the available runtime and compiler
outputs. But when provenance is incomplete, that number does not automatically
justify a strong semantic statement.

The standard therefore separates:

- `measurement`
- `claim_admissible`

This lets tools remain useful in practice while being honest about semantic
limits.

### Why is this relevant beyond one tool?

Because the need is structural. Any downstream tool that wants proof-aware,
semantically defensible coverage for Idris2 needs:

- obligation identity
- provenance
- explicit unknown

Those are compiler interface needs, not private implementation details.

## Evidence Anchors

Use these when someone asks "where is that encoded today?"

- Standard vocabulary:
  - `src/Coverage/Standardization/Types.idr`
- Admissibility model:
  - `src/Coverage/Standardization/Model.idr`
- Downstream branch classification:
  - `../Idris2CoverageCore/src/Coverage/Classification/BranchClass.idr`
- Current tool reporting:
  - `../Idris2Coverage/src/Coverage/TestCoverage.idr`
  - `../Idris2Coverage/src/Main.idr`

## Safe Phrases

Use these when answering conservatively.

- "The current downstream implementation can measure this conservatively, but we
  do not claim that the upstream interface is complete yet."
- "Today the strongest practical profile is function-level semantic test
  obligation coverage."
- "Unknown is intentionally public because it marks the boundary of the current
  semantic claim."
- "This request is about structured provenance export, not about changing
  totality."

## Phrases To Avoid

- "Idris2 already supports semantic coverage."
- "`--dumpcases` is enough."
- "Branch-level coverage is solved."
- "Unknowns can be ignored."
- "This is just a formatting change."

## Implementation Follow-Up Playbook

If maintainers ask for a smaller or more concrete prototype, do this in order.

1. Reduce the request to same-version stability only.
2. Accept a provisional export shape if it includes explicit provenance.
3. Wire the export into downstream tooling without changing the public metric.
4. Keep `UnknownClassification` and `claim_admissible` in place until the new
   export fully replaces heuristics.

## If They Ask For Examples

Give one example from each category:

1. Unknown handling
2. function-level vs branch-level profile
3. artifact leakage risk

Do not try to explain the entire downstream tool.

## If They Push Back On Scope

Narrow the ask to:

- machine-readable export
- provenance tags
- stable same-version IDs

Avoid broad design discussion about all future coverage tooling.

## If They Ask For Immediate Downstream Changes

The reasonable downstream follow-ups are:

1. replace text parsing with structured parsing
2. preserve the same obligation classes
3. preserve `claim_admissible`
4. keep function-level profile as default until branch-level mapping is proven

## Exit Criterion Before Filing

You are ready to post only if you can answer, in one or two sentences each:

1. What is the minimal upstream ask?
2. Why is `--dumpcases` text not enough?
3. Why is `unknown` explicit?
4. Why is the current profile function-level?
5. What are you explicitly not asking Idris2 to do?

## Rehearsal Drills

Use these before filing the issue. Answer them aloud or in writing without
looking at the docs first.

### Drill 1: 60-Second Pitch

Explain:

1. what the metric is
2. why current `--dumpcases` is not enough
3. what the minimum upstream ask is

Success condition:

- no tool-specific detours
- explicit mention of provenance and stable IDs
- no claim that branch-level is already solved

### Drill 2: Skeptical Maintainer

Prompt:

"Why can this not stay downstream? Why should the compiler change at all?"

Success condition:

- answer in 3 sentences or fewer
- mention that obligation identity and provenance are compiler-interface facts
- avoid asking Idris2 to endorse a tool

### Drill 3: Scope Pushback

Prompt:

"This sounds too broad. What is the smallest thing that would help?"

Success condition:

- reduce the ask to machine-readable export, provenance tags, and same-version
  stable IDs
- explicitly say that exact flag shape is not important

## Implementation Drills

These are the smallest reasonable follow-up tasks if maintainers ask for proof
that the request is actionable.

### Drill A: Structured Parser Swap

Task:

Replace one text-parsed downstream path with a structured parser while keeping
the same public classes:

- `ReachableObligation`
- `LogicallyUnreachable`
- `UserAdmittedPartialGap`
- `CompilerInsertedArtifact`
- `UnknownClassification`

Success condition:

- no public metric change
- `claim_admissible` still behaves the same

### Drill B: Same-Version Stability Adapter

Task:

Assume upstream gives same-version stable node IDs only. Update downstream
mapping to use those IDs without claiming cross-version stability.

Success condition:

- docs stay conservative
- reports explicitly state the stability scope

### Drill C: Branch-Level Upgrade Gate

Task:

Keep function-level as default and add a guarded branch-level path that is only
enabled when upstream provenance fields are present.

Success condition:

- no silent upgrade from function-level to branch-level
- profile remains explicit in every report

## Escalation Rule

If a maintainer question would force one of these claims, stop and answer
conservatively instead:

- "we have fully solved branch-level semantic coverage"
- "current text output is sufficient for a final standard"
- "unknowns can be hidden once the number looks reasonable"

The correct fallback is:

"The current downstream implementation is useful, but this exact point is why
we are asking for a structured upstream interface."
