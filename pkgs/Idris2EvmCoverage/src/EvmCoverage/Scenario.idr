||| PoC: a typed, state-threading Scenario DSL for state-carrying path coverage.
|||
||| See docs/SCENARIO_AND_REFINEMENT_FUZZING.md. The point this module must PROVE is
||| that the four traps of raw-calldata coverage become TYPE ERRORS:
|||   1. Execute on an unapproved proposal           -> won't typecheck
|||   2. Tally before the proposal expired           -> won't typecheck
|||   3. Fork/Vote by a non-rep                       -> won't typecheck
|||   4. Vote ranking an id the proposal lacks        -> won't typecheck (Ranking p)
|||
||| The World index carries the FACTS established so far. An action that needs a
||| precondition takes an `auto` proof that the fact is in the World. Establishing
||| a fact is the POST-condition of the action that creates it. State threads in the
||| index; the interpreter (runScenario, separate) threads the real on-chain state.
module EvmCoverage.Scenario

%default total

------------------------------------------------------------------------
-- Facts the World can hold. Closed (totality anchor): a new contract fact
-- is a new constructor, and every consumer must react.
------------------------------------------------------------------------

public export
Addr : Type
Addr = Integer

public export
Pid : Type
Pid = Nat   -- proposal id; in an isolated run the first Propose is pid 0

public export
data Fact : Type where
  Member    : Addr -> Fact            -- addr is a registered member
  Proposed  : Pid -> Fact             -- proposal exists
  Rep       : Pid -> Addr -> Fact     -- addr is a rep of the proposal
  HasCmd    : Pid -> Fact             -- proposal has at least one command (vote-able)
  Voted     : Pid -> Fact             -- a vote has been cast (a winner can exist)
  Expired   : Pid -> Fact             -- proposal's voting window has passed
  Approved  : Pid -> Fact             -- tally produced a winner & set approval
  Executed  : Pid -> Fact             -- proposal has been executed

public export
World : Type
World = List Fact

------------------------------------------------------------------------
-- Membership: the refinement witness. `Holds f w` is "fact f is established
-- in world w" — this is the proof an action demands. It is exactly `Elem`,
-- but named to read as a predicate.
------------------------------------------------------------------------

public export
data Holds : Fact -> World -> Type where
  Here  : Holds f (f :: fs)
  There : Holds f fs -> Holds f (g :: fs)

------------------------------------------------------------------------
-- A ranking that, by construction, can only name commands of a proposal
-- that HasCmd. Trap #4 (rank a non-existent id) is unwritable: you cannot
-- build a Ranking p without a HasCmd p proof.
------------------------------------------------------------------------

public export
data Ranking : Pid -> Type where
  RankWinner : (p : Pid) -> Ranking p   -- the single-winner ranking; valid because

------------------------------------------------------------------------
-- The Scenario monad. Indexed by (pre, post) Worlds. Each action's auto
-- implicits are its preconditions; its result-World adds its post-facts.
------------------------------------------------------------------------

public export
data Scenario : (pre : World) -> (post : World) -> Type -> Type where
  Config   : Scenario w w ()
  AddMember: (a : Addr) -> Scenario w (Member a :: w) ()
  -- propose by a member; post-establishes the proposal AND assigns the proposer
  -- as a rep (mirrors createProposal -> assignAllMembersAsReps) and a fresh,
  -- not-yet-expired proposal.
  Propose  : (a : Addr) -> (p : Pid)
          -> {auto memberPrf : Holds (Member a) w}
          -> Scenario w (Rep p a :: Proposed p :: w) ()
  -- fork a command: needs the caller to be a rep and the proposal not expired.
  ForkCmd  : (p : Pid) -> (a : Addr)
          -> {auto repPrf : Holds (Rep p a) w}
          -> Scenario w (HasCmd p :: w) ()
  -- vote: needs rep + a command to rank (Ranking p only exists once HasCmd).
  Vote     : (p : Pid) -> (a : Addr)
          -> {auto repPrf : Holds (Rep p a) w}
          -> {auto cmdPrf : Holds (HasCmd p) w}
          -> Ranking p
          -> Scenario w (Voted p :: w) ()
  -- advance time enough to expire the proposal (explicit; no implicit step).
  Expire   : (p : Pid) -> {auto propPrf : Holds (Proposed p) w}
          -> Scenario w (Expired p :: w) ()
  -- tally: finalTally only finalises an EXPIRED proposal with a vote.
  Tally    : (p : Pid)
          -> {auto expPrf  : Holds (Expired p) w}
          -> {auto votePrf : Holds (Voted p) w}
          -> Scenario w (Approved p :: w) ()
  -- execute: needs approval and not-yet-executed.
  Execute  : (p : Pid)
          -> {auto apprPrf : Holds (Approved p) w}
          -> Scenario w (Executed p :: w) ()

  Pure     : a -> Scenario w w a
  Bind     : Scenario a b r -> (r -> Scenario b c s) -> Scenario a c s

public export
(>>=) : Scenario a b r -> (r -> Scenario b c s) -> Scenario a c s
(>>=) = Bind

public export
(>>) : Scenario a b () -> Scenario b c s -> Scenario a c s
x >> y = Bind x (\_ => y)

public export
pure : a -> Scenario w w a
pure = Pure

------------------------------------------------------------------------
-- PROBES — the design's life-or-death test. The VALID scenario must
-- typecheck; the INVALID ones (kept as commented holes) must NOT.
------------------------------------------------------------------------

||| The full execute-chain happy path, written WITHOUT raw hex, pid juggling,
||| implicit timing, or storage-var knowledge. Every guard precondition is
||| discharged by the fact a prior action established. This is the scenario the
||| whole multi-hour manual session was trying to express.
public export
validExecuteChain : Scenario [] ? ()
validExecuteChain = do
  Config
  AddMember 0xa11ce
  Propose 0xa11ce 0
  ForkCmd 0 0xa11ce
  Vote 0 0xa11ce (RankWinner 0)
  Expire 0
  Tally 0
  Execute 0

-- PROBE 1 (trap: execute without approval). Uncommenting MUST fail with
-- "Can't find an implementation for Holds (Approved 0) ...":
--
--   badExecuteNoApproval : Scenario [] ? ()
--   badExecuteNoApproval = do
--     Config; AddMember 0xa11ce; Propose 0xa11ce 0
--     Execute 0            -- <-- no Approved 0 in scope: TYPE ERROR
--
-- PROBE 2 (trap: tally before expiry). MUST fail "Holds (Expired 0)":
--
--   badTallyNotExpired : Scenario [] ? ()
--   badTallyNotExpired = do
--     Config; AddMember 0xa11ce; Propose 0xa11ce 0
--     ForkCmd 0 0xa11ce; Vote 0 0xa11ce (RankWinner 0)
--     Tally 0              -- <-- no Expired 0 in scope: TYPE ERROR
--
-- PROBE 3 (trap: fork/vote by a non-rep). MUST fail "Holds (Rep 0 0xb0b)":
--
--   badForkNonRep : Scenario [] ? ()
--   badForkNonRep = do
--     Config; AddMember 0xa11ce; Propose 0xa11ce 0
--     ForkCmd 0 0xb0b      -- <-- 0xb0b is not a rep of pid 0: TYPE ERROR
--
-- PROBE 4 (trap: vote with no command to rank). MUST fail "Holds (HasCmd 0)":
--
--   badVoteNoCmd : Scenario [] ? ()
--   badVoteNoCmd = do
--     Config; AddMember 0xa11ce; Propose 0xa11ce 0
--     Vote 0 0xa11ce (RankWinner 0)   -- <-- no ForkCmd first: TYPE ERROR
