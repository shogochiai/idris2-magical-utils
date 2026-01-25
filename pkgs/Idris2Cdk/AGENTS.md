# Lazy Agent Workflow Guidelines

## Philosophy

Implementation-first is valid. Order emerges from tests.
Type sedimentation is a human intuition domain.

## Growth Phases

### Phase 1: Vibe Bootstrap

- Run `lazy core ask --steps=4` (testandcoverage)
- Select high-branch functions from recommendations
- Write tests (Spec comes later)
- Iterate until coverage stabilizes

### Phase 2: Spec Emergence

- Run `lazy core ask --steps=1,2` (stparity + testorphans)
- OrphanTest detected → Add to SPEC.toml
- SpecGap detected → Write test
- Bidirectional consistency grows

### Phase 3: Type-Driven Vibe Coding (TDVC)

- Run `lazy core ask --steps=3,5` (stsemantic + fuzzing)
- Recurring Spec constraints → Candidate for type sedimentation
- Spec encoded in type → Test becomes optional
- Fuzzing detects type boundary blind spots

## Phase Detection

| Indicator       | Phase 1 | Phase 2 | Phase 3 |
|-----------------|---------|---------|---------|
| OrphanTest count| High    | Decreasing | Low  |
| SpecGap count   | N/A     | High    | Decreasing |
| Coverage %      | Low     | Rising  | High (type-augmented) |

## Autonomous Loop

```
while true:
  result = lazy core ask .
  if result.urgent.empty:
    break
  execute(result.urgent.first)
```
