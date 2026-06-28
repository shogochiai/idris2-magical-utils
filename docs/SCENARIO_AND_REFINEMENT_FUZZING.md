# Stateful Scenarios & Refinement-Guided Fuzzing — making "state-carrying path coverage" the path of least resistance

> Part of the coverage-design set. Start at [DELIVERYKIND.md](./DELIVERYKIND.md) for the map.
> This doc is the design for the *next* layer: how a state-carrying EVM (or any-runtime)
> path-coverage scenario should be **written**, and how the un-covered arms should be
> **reached by deduction, not by hand**.
>
> It builds directly on:
> - [TOTALITY_ANCHOR.md](./TOTALITY_ANCHOR.md) — closed enums; the compiler is the checklist.
> - [type-first-path-coverage] (memory) — un-covered paths are *instruments of the type's holes*.
> - [exclusion-policy-unification.md](./exclusion-policy-unification.md) — the honesty discipline.

---

## 0. The complaint this design answers (the affordance failure)

State-carrying coverage today is written as a file of **raw calldata hex** fed through
`EVM_COV_HAPPYPATH`, plus a constellation of implicit knowledge:

- the proposal id must be threaded by hand (concatenation with the generic phases
  pollutes state → wrong pid → silent no-op);
- the time must advance *just so* (`REVM_TIME_STEP`) for a vote to be not-expired but a
  tally to be expired — encoded nowhere, discoverable only by tracing;
- valid header/command ids must be guessed (ranking a non-existent id → silent `Fail`);
- to *see* why a call reverted you must know `REVM_DUMP_STORAGE` exists (not `REVM_DEBUG`).

Every one of those is a **trap that the default falls into and the correct path is a
narrow trail only the initiated can walk.** That is the exact inverse of the project's
backbone (TOTALITY_ANCHOR: *"you cannot forget, because forgetting is a type error"*).
A multi-hour debugging session was spent re-deriving facts the design should have made
unforgettable. The lesson: **doing the right thing must require less knowledge than doing
the wrong thing, and "wrong" should not even typecheck.**

Two capabilities close the gap:

1. **A typed `Scenario` DSL** — write state-carrying coverage as a *typed call sequence*,
   not raw hex. State threads implicitly; preconditions are demanded as proofs; failure
   auto-attaches the storage dump. The traps become type errors or vanish entirely.
2. **Refinement-guided fuzzing** — reach an un-covered guard arm by *constructing a
   witness for its predicate from the type*, not by random mutation. Deduction over a
   constraint, not a random walk.

The two are one idea seen from two sides: **the contract's guard predicates are already
refinement types** (`IsRep pid addr`, `NotExpired pid`, `IsApproved pid`, `NotExecuted
pid`, `IsMember pid` — these literally exist in `Security/AccessControl.idr`). A guard is
a function that *constructs* such a proof from on-chain state. Coverage of a guard's two
arms = "a state where the proof holds" and "a state where it doesn't." Both the writing
(Scenario) and the reaching (Fuzzer) are about **manipulating these proofs**.

---

## 1. Concept — the refinement is already there; surface it

A *refinement type* is a value paired with a proof of a predicate over it:
`(x : A ** P x)`. The TextDAO contract already speaks this language:

```idris
data IsRep      : (pid : Integer) -> (addr : Integer) -> Type
data NotExpired : (pid : Integer) -> Type
data IsApproved : (pid : Integer) -> Type
data NotExecuted: (pid : Integer) -> Type
data IsMember   : (pid : Integer) -> Type
```

and the guards are *constructors of these proofs from state*:

```idris
requireRepProof   : (pid, addr : Integer) -> IO (Outcome (IsRep pid addr))
requireNotExpired : (pid : Integer)       -> IO (Outcome (NotExpired pid))
requireApproved   : (pid : Integer)       -> IO (Outcome (IsApproved pid))
```

A guard branches two ways: **proof constructed (Ok)** vs **proof refused (Fail)**. Path
coverage of that guard = exercising *both* arms. So:

> **Covering a guard arm ⇔ reaching an on-chain state in which its predicate does, or
> does not, hold.** The predicate is the thing the type already names. Coverage is a
> reachability problem *over predicates that are already types*.

This reframes both layers:

- **Writing** a scenario = chaining actions where each action's precondition is one of
  these proofs, threaded forward. `forkCommand` needs `IsRep p caller ∧ NotExpired p`;
  `execute` needs `IsApproved p ∧ NotExecuted p`. If the DSL *demands* those proofs, you
  cannot write a scenario that calls `execute` on an unapproved proposal — it won't
  typecheck. The pid-mismatch / invalid-id / wrong-timing traps are gone by construction.
- **Reaching** an un-covered arm = *constructing the state that makes the predicate
  hold (or fail)*. This is not random; it's solving "which prefix of actions
  establishes `IsApproved p`?" — a constraint over the predicates, deducible.

---

## 2. Why this matters — the trap-proof rule, extended to writing tests

TOTALITY_ANCHOR gives the **trap-proof rule** for *exclusions*: a path may only leave the
denominator if its justification is a fact the type-checker already enforces. The same
rule should govern *test inputs*:

> **A coverage scenario may only reach an arm via inputs whose validity the type-checker
> already enforces.** A ranked vote may only name ids the proposal actually has; a
> `forkCommand` may only be issued by an established rep; an `execute` may only follow an
> approval. Anything else is unwritable, not merely discouraged.

Raw calldata hex is the *anti*-pattern here, exactly as a free-text exclusion reason was:
it is a value a human edits, with no proof attached, so it silently does the wrong thing
(rank id 1 on a proposal with only id 0 → `Fail`, counted as a "successful" call that
covers nothing). Replacing it with a typed action whose argument is `(h : HeaderId **
ValidFor p h)` makes the wrong rank *inexpressible*. The numerator can no longer be
inflated by a call that looks like it exercised a branch but took the reject path.

This is the **covering-direction** sibling of the `StubbedReach` discipline
(exclusion-policy-unification.md §StubbedReach): there we stopped a meaningless *read*
from counting as covered; here we stop a meaningless *call* from masquerading as a
reached arm. Both say: a hit only counts if a *type* vouched for the input that produced
it.

---

## 3. Type — the two artifacts

### (a) `Scenario` — a typed, state-threading call sequence

A free monad (or indexed monad) over typed actions. The index carries the *facts
established so far* so an action can demand its preconditions:

```idris
-- Sketch. The index is the set of proofs currently in scope.
data Scenario : (pre : World) -> (post : World) -> Type -> Type where
  Config   : DeliberationConfig -> Scenario w w ()
  AddMember: (a : Addr) -> Scenario w (w `with` Member a) (MemberId)
  Propose  : (a : Addr) -> {auto _ : Member a `In` w} -> Metadata
           -> Scenario w (w `with` Proposal) ProposalId      -- assigns reps as a post-fact
  Fork     : (p : ProposalId) -> (a : Addr)
           -> {auto _ : Rep p a `In` w} -> {auto _ : NotExpired p `In` w}
           -> Cmd -> Scenario w (w `with` Command p) CommandId
  Vote     : (p : ProposalId) -> (a : Addr) -> {auto _ : Rep p a `In` w}
           -> Ranking p                                       -- only ids that exist on p
           -> Scenario w (w `with` Voted p a) ()
  Advance  : (secs : Nat) -> Scenario w (tick secs w) ()      -- explicit; no implicit step
  Tally    : (p : ProposalId) -> {auto _ : Expired p `In` w}  -- finalTally needs expiry
           -> Scenario w (w `with` Approved p) ()
  Execute  : (p : ProposalId)
           -> {auto _ : Approved p `In` w} -> {auto _ : NotExecuted p `In` w}
           -> Scenario w (w `with` Executed p) ()
  (>>=)    : Scenario a b r -> (r -> Scenario b c s) -> Scenario a c s
```

The `auto` implicits are the refinements. Writing `Execute p` before an approval is in
`w` is a type error, not a runtime `Fail` or a silent miss. The interpreter
(`runScenario`) lowers each action to calldata against a **fresh, isolated DB** (pid 0 is
the scenario's own proposal — the isolation that had to be bolted on by hand), advances
time explicitly per `Advance`, and **on any unexpected revert dumps storage
automatically** (it sets `REVM_DUMP_STORAGE` for you; you never need to know it exists).

The point is not the exact shape above — it's that **the four traps (pid, ids, timing,
storage-visibility) each become either a type obligation or an interpreter
responsibility, so a human cannot fall into them.**

### (b) `Reach` — a refinement-guided witness for an un-covered arm

Given a target predicate `P` (a guard's proof type) and a sense (hold / fail), produce a
`Scenario` prefix that establishes a state in which `P` holds (or fails):

```idris
-- For each guard predicate, a *constructive* reach strategy: the prefix that
-- establishes it, derived from the predicate's own definition — not random.
reach : (goal : Predicate) -> (sense : Bool) -> Maybe (Scenario Empty w ())
-- e.g. reach (Approved p) True
--        = do c <- Config ...; a <- AddMember alice; p <- Propose a ...
--             cmd <- Fork p a ...; Vote p a (singleWinner p cmd); Advance 30; Tally p
--      reach (NotExpired p) False   -- the *fail* arm: same prefix + Advance past expiry
```

`reach` reads the predicate's establishing conditions off its definition (or a small
hand-written strategy table keyed by predicate, initially) and emits a *typed* Scenario —
so the produced witness is itself trap-proof. The fuzzer's job shrinks from "mutate hex
until a branch flips" to "ask `reach` for each missing arm's predicate." Coverage-guided
fuzzing becomes **predicate-guided synthesis**.

---

## 4. Why this is efficient — deduction, not a random walk

AFL-style fuzzing is dynamic and probabilistic: mutate inputs, observe runtime coverage,
steer toward novelty. It wastes enormous effort generating inputs that fail an early
guard (an invalid id, a non-rep caller) and never reach the interesting branch — exactly
the wasted trials of this session's manual attempts.

Refinement-guided fuzzing inverts the loop:

- The **search space is narrowed by the type before generation.** `Ranking p` can only
  contain ids on `p`, so no invalid-id input is ever produced. The fuzzer never spends a
  trial on a revert that the type already forbids.
- The **target is a predicate, and the predicate carries its own reachability.** To hit
  `finalTally`'s single-winner branch you need `SingleWinner votes`; the type says what
  that means, so you *construct* a satisfying `votes` rather than wait for a random one to
  appear. This is the difference between solving a constraint and rolling dice against it.

This is the type-theoretic form of coverage-guided fuzzing: instead of the *runtime*
telling you which branch you stumbled into, the *type* tells you which inputs can reach a
branch, and you enumerate witnesses constructively. For a contract whose guards are
already proof-returning, the speedup is not marginal — whole classes of wasted trials
(every early-guard reject) are removed at the type level.

It is also the natural completion of [type-first-path-coverage] (memory): there, three
classes of un-covered path — *mechanical/totality* (kill with `map`/deriving), *truly
dead defensive* (excise with a smart type or reason-exclude), and *the real state space*
(test + dependent-type proof). This design **mechanises the third class**: the "real
state space" arms are reached by synthesising the dependent-type witness the arm's guard
demands, instead of a human hand-crafting calldata to stumble into it.

---

## 5. How to see it bite (the probe, same spirit as TOTALITY_ANCHOR §4)

- Try to write `Execute p` with no `Approved p` in scope → **type error**. (Today: a
  runtime revert that silently covers nothing.)
- Try to `Vote p a (rank [bogusId])` where `bogusId` is not on `p` → **type error** (the
  `Ranking p` constructor only admits ids of `p`). (Today: silent `Fail`.)
- A scenario that forgets `Advance` before `Tally` → `Expired p` unprovable → **type
  error**. (Today: `finalTally` no-ops, no winner, execute reverts, hours lost.)
- Ask `reach (Approved 0) True` → get back a *typed* Scenario that, run, covers the whole
  execute guard chain — the 8 arms this session could not reach by hand.

If any of those *doesn't* bite, the design has a hole — fix the type, not the test.

---

## 6. Build order

1. **`Scenario` for one contract (TextDAO)** — the indexed monad + `runScenario` with
   isolated DB, explicit time, auto storage-dump. Re-express the current
   `coverage-happy-path.calls` as a typed Scenario; the pid/id/timing traps disappear and
   the execute-chain arms either get covered or fail with an *automatic storage dump that
   shows exactly which slot mismatched* (the `checkRep` vs `isRep` rep-slot bug this
   session pinned by hand). This alone is the affordance fix.
   — **✅ indexed monad PoC done**: `pkgs/Idris2EvmCoverage/src/EvmCoverage/Scenario.idr`
     (commit `28d3eea3`). `validExecuteChain` typechecks; the 4 raw-calldata traps
     (execute-without-approval, tally-before-expiry, fork/vote-by-non-rep, vote-with-no-cmd)
     are now compile-time type errors. **Remaining for step 1**: `runScenario` interpreter
     (lower each action to calldata against an isolated revm DB, explicit time, auto
     storage-dump), then swap `EVM_COV_HAPPYPATH` to consume a typed Scenario.
2. **`reach` table** — a per-predicate establishing-prefix, hand-written first, then
   derived. Feeds Scenario prefixes for each missing arm.
   — **✅ PoC done**: `pkgs/Idris2EvmCoverage/src/EvmCoverage/Reach.idr` (commit
     `6235629b`). `reach : Fact -> Maybe (Reached target)` synthesises the establishing
     prefix; `Reached.evidence : Holds target post` makes it trap-proof (reach cannot
     return a scenario that misses its target). `reach (Approved 0)` derives the exact
     propose→fork→vote→expire→tally chain the manual session couldn't hand-craft;
     `allGuardFactsReachable` proves all execute-chain arms derive from their predicates.
3. **The fuzzer loop** — for every un-covered guard arm in the coverage report, call
   `reach predicate sense`, run the synthesised Scenario, re-measure. Replaces the manual
   calldata grind end-to-end. **Blocked on step 1's `runScenario`** (needs a runnable
   interpreter to turn a `Reached` into measured coverage).

The boundary with existing machinery: `Scenario`/`reach` produce the *input sequence*;
the existing `Idris2EvmCoverage` runner (revm-run, fork-yul `--dumppathshits`) measures
the *coverage* of running it. This layer sits **above** `EVM_COV_HAPPYPATH` — eventually
replacing the raw-hex hook with a typed-Scenario hook — and **below** the coverage report.
