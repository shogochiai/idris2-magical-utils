# Semantic Test Obligation Coverage

Status: Draft 0.1  
Target ecosystem: Idris2 first, dependently typed languages second

## Purpose

This document defines a coverage concept that remains meaningful in the
presence of dependent pattern matching, logical unreachability, and
compiler-inserted artifacts.

The goal is not to replace totality checking. The goal is to define a test
coverage metric whose numerator and denominator are both semantically
defensible.

This standard separates two things that are often conflated:

- a numeric coverage measurement
- an admissible semantic coverage claim

A tool may report a number even when provenance is incomplete, but it must not
silently present that number as a strong semantic coverage claim unless the
admissibility conditions below are met.

## Core Definition

Coverage is defined as:

`covered executable obligations / all executable obligations`

where an executable obligation is a source-level or elaboration-level program
obligation that may be taken in program execution under the language semantics.

This standard admits multiple obligation granularities, provided the granularity
is stated explicitly and the numerator and denominator are defined at the same
semantic layer.

Current profiles:

- branch-level semantic test obligation coverage
- function-level semantic test obligation coverage

Backend-specific runtime tools may also emit weaker observation-level
measurements, but those must not be presented as semantic test obligation
coverage unless they satisfy the admissibility conditions in this document.

Branch-level tools count elaborated branches directly.
Function-level tools count executable functions induced by elaborated case trees.
The function-level profile is valid when runtime observation can be mapped
soundly to function obligations but not yet to stable branch obligations.

Every report must state its profile explicitly. A branch-level tool and a
function-level tool are not directly comparable unless the comparison is made at
the profile level first.

## Obligation Classes

Every observed obligation-like entity must be assigned exactly one class:

- `ReachableObligation`
- `LogicallyUnreachable`
- `UserAdmittedPartialGap`
- `CompilerInsertedArtifact`
- `UnknownClassification`

The metric is only admissible if this classification is explicit.

`UnknownClassification` is not an implementation detail. It is part of the
public semantic boundary of the metric.

## Counting Rules

### Denominator

Include:

- `ReachableObligation`
- `UserAdmittedPartialGap`

Exclude:

- `LogicallyUnreachable`
- `CompilerInsertedArtifact`

Do not silently normalize:

- `UnknownClassification`

An implementation may either block the coverage claim, count unknowns as gaps,
or report them separately, but it must state which policy it uses.

### Numerator

An obligation contributes to the numerator only if:

- it is hit at runtime, and
- the hit maps back to a stable obligation identifier defined before backend
  optimization artifacts are introduced

This prevents coverage values above 100 percent.

## Admissibility

A report is admissible only if all of the following hold:

1. The obligation profile is stated explicitly.
2. All observed entities are classified into the obligation classes above.
3. The unknown-handling policy is stated explicitly.
4. Runtime hits are mapped back to pre-optimization obligation identities at
   the same semantic layer as the denominator.

If any of these conditions fail, the tool may still emit a measurement, but the
semantic coverage claim is not admissible.

## Soundness Envelope

At minimum, a tool claiming this metric must state:

1. Why excluded obligations are semantically unreachable.
2. Why counted runtime hits correspond to pre-optimization obligations.
3. How compiler artifacts are prevented from leaking into the numerator.
4. What happens when classification is unknown.

## Reproducibility Requirements

A report must include:

- Idris2 version
- backend
- profiling mode
- project commit or source revision
- classification policy for unknowns
- exclusion tables or heuristics used

## Evaluation Criteria

A candidate implementation should be evaluated on:

1. No values above 100 percent on benchmark suites.
2. Stable detection of admitted partial gaps.
3. No false test obligations from logically unreachable branches.
4. Reproducible results across repeated runs on the same toolchain.
5. Clear failure mode when provenance information is insufficient.

## Relation To Idris2

Idris2 already exposes enough information for useful downstream experiments,
but not yet enough for a strong standard without caveats.

In particular, provenance-tagged machine-readable elaboration output and stable
obligation identifiers are upstream compiler interface requirements.

## Current Downstream Interpretation

Downstream Idris2 tools can already approximate this standard conservatively.
The current best-supported profile is function-level semantic test obligation
coverage with explicit unknown handling and claim admissibility reporting.

Backend support is uneven. Chez, IC WASM, EVM, and Web do not currently sit at
the same integration depth. A shared schema does not by itself imply a shared
semantic strength. Backend-specific strength and gaps should be stated
explicitly.

That is already valuable, but it should still be described as a proof-aware
downstream implementation of this draft standard, not as a completed upstream
semantic coverage interface.
