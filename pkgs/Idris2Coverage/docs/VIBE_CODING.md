# Vibe Coding with idris2-coverage

## The Problem with Vibe Coding

Vibe coding is fast. You sketch ideas, iterate quickly, and ship. But as complexity grows, you lose track of what's tested and what's not. Traditional coverage tools don't help—they drown you in noise about unreachable branches and compiler internals.

## The Solution

idris2-coverage tells you exactly what you missed:

```bash
$ idris2-cov .

=== High Impact Targets ===
1. Parser.handleEdgeCase     (8 branches, 0 tested)
2. Validator.checkConstraint (6 branches, 1 tested)
3. Engine.processQuery       (12 branches, 4 tested)
```

No noise. No impossible branches. Just the gaps.

## Workflow

### 1. Vibe First

Write code the way you want:

```idris
processCommand : Command -> IO Result
processCommand Help = printHelp
processCommand (Run args) = runWithArgs args
processCommand (Config path) = loadConfig path
-- ... more cases as you think of them
```

### 2. Check Coverage

```bash
idris2-cov --uncovered .
```

Output:
```
Uncovered:
  - processCommand: missing test for (Config path)
  - loadConfig: 4 branches untested
```

### 3. Fill the Gaps

Write tests for what matters. Skip what the type system already guarantees.

### 4. Ship with Confidence

```bash
idris2-cov --json . | jq '.summary'
{
  "total_canonical": 847,
  "executed": 823,
  "coverage": "97%"
}
```

## Why This Matters for Complex Software

As your vibe-coded project grows:

| Without coverage tool | With idris2-coverage |
|-----------------------|----------------------|
| "Did I test that edge case?" | Exact list of untested branches |
| Fear of refactoring | Refactor freely, gaps are visible |
| Manual test tracking | Automated, CI-integrated |
| 100% coverage impossible | 100% is achievable (proof-aware) |

## The Legendary Part

Traditional coverage tools in dependently-typed languages are useless—they count impossible branches, giving you 60% when you've actually tested everything reachable.

idris2-coverage understands your types:

```idris
safeDiv : (n : Nat) -> (d : Nat) -> Not (d = 0) -> Nat
safeDiv n d prf = n `div` d
-- The "divide by zero" case doesn't exist. We don't count it.
```

This means:
- **100% is real** — not a fantasy metric
- **Gaps are actionable** — every listed target is something you can actually test
- **Types + Tests = Confidence** — the combination catches what neither alone can

## For Teams

When multiple vibe coders work together:

```bash
# CI check: no new untested code
idris2-cov --json . > coverage.json
if [ $(jq '.summary.bugs' coverage.json) -gt 0 ]; then
  echo "Someone added partial functions!"
  exit 1
fi
```

## Quick Start

```bash
# Build the tool
idris2 --build idris2-coverage.ipkg

# Run on your project
./build/exec/idris2-cov path/to/your/vibe/project/

# See only what needs attention
./build/exec/idris2-cov --uncovered .
```

## Go Build Something Complex

The whole point of dependent types is to build software that would be impossible to get right otherwise. idris2-coverage closes the loop—vibe code fearlessly, then verify you didn't miss anything.

Now go ship something legendary.
