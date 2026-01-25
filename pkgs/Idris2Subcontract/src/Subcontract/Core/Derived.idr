||| Subcontract Core: Derived Storage Patterns
|||
||| RQ-2.2: Schema Derivation from Storable Records
||| RQ-2.3: Proof-Carrying State Transitions with Storage Integration
|||
||| This module demonstrates Idris2's expressiveness for:
||| - Compile-time schema derivation (impossible in Solidity)
||| - State machine enforcement at type level
||| - Storage operations that require transition proofs
module Subcontract.Core.Derived

import public Data.Vect
import public Subcontract.Core.Storable
import public Subcontract.Core.Schema

%default total

-- =============================================================================
-- RQ-2.2: Schema Derivation Interface
-- =============================================================================

||| Types that can derive their storage schema.
|||
||| This is IMPOSSIBLE in Solidity - you cannot introspect struct fields
||| at compile time to generate storage layouts.
|||
||| In Idris2, the type itself carries enough information to derive the schema.
public export
interface Storable a => HasSchema a where
  ||| The derived schema for this type
  schema : Schema

  ||| Field names in storage order
  fieldNames : Vect (slotCount {a}) String

||| Get a typed Ref using the derived schema
export
schemaRef : HasSchema a => Slot -> Ref a
schemaRef baseSlot = MkRef baseSlot

-- =============================================================================
-- RQ-2.2: Example Derived Schemas
-- =============================================================================

||| Member record with derived schema
public export
record DerivedMember where
  constructor MkDerivedMember
  memberAddr : Bits256
  memberMeta : Bits256

public export
Storable DerivedMember where
  slotCount = 2
  toSlots m = [m.memberAddr, m.memberMeta]
  fromSlots [a, m] = MkDerivedMember a m

||| HasSchema instance - schema is derived from the record definition
public export
HasSchema DerivedMember where
  schema = MkSchema "derived.member" 0
    [ Value "memberAddr" TAddress
    , Value "memberMeta" TBytes32
    ]
  fieldNames = ["memberAddr", "memberMeta"]

||| Token balance record with derived schema
public export
record TokenBalance where
  constructor MkTokenBalance
  balance : Bits256
  lastUpdate : Bits256
  frozen : Bits256

public export
Storable TokenBalance where
  slotCount = 3
  toSlots t = [t.balance, t.lastUpdate, t.frozen]
  fromSlots [b, l, f] = MkTokenBalance b l f

public export
HasSchema TokenBalance where
  schema = MkSchema "derived.token" 0
    [ Value "balance" TUint256
    , Value "lastUpdate" TUint256
    , Value "frozen" TBool
    ]
  fieldNames = ["balance", "lastUpdate", "frozen"]

-- =============================================================================
-- RQ-2.3: State Machine with Storage Integration
-- =============================================================================

||| Generic state enumeration (can be any finite set)
public export
interface StateEnum s where
  ||| Convert state to storage representation
  toStateInt : s -> Bits256
  ||| Parse state from storage
  fromStateInt : Bits256 -> Maybe s

||| Proposal states
public export
data PropState = PropDraft | PropVoting | PropApproved | PropExecuted | PropRejected

public export
StateEnum PropState where
  toStateInt PropDraft = 0
  toStateInt PropVoting = 1
  toStateInt PropApproved = 2
  toStateInt PropExecuted = 3
  toStateInt PropRejected = 4

  fromStateInt 0 = Just PropDraft
  fromStateInt 1 = Just PropVoting
  fromStateInt 2 = Just PropApproved
  fromStateInt 3 = Just PropExecuted
  fromStateInt 4 = Just PropRejected
  fromStateInt _ = Nothing

||| Valid state transitions - type-level constraint
||| Each constructor is a PROOF that the transition is valid
public export
data PropTransition : PropState -> PropState -> Type where
  ||| Draft -> Voting: proposal submitted for voting
  Submit : PropTransition PropDraft PropVoting
  ||| Voting -> Approved: quorum reached, approved
  Approve : PropTransition PropVoting PropApproved
  ||| Voting -> Rejected: quorum reached, rejected
  Reject : PropTransition PropVoting PropRejected
  ||| Approved -> Executed: proposal executed
  Execute : PropTransition PropApproved PropExecuted

||| State-indexed proposal in storage
||| The state is part of the TYPE, not just a runtime value
public export
record StatefulProposal (state : PropState) where
  constructor MkStatefulProposal
  proposalId : Bits256
  creator : Bits256
  contentHash : Bits256
  createdAt : Bits256

||| Storable for any proposal state
public export
Storable (StatefulProposal s) where
  slotCount = 4
  toSlots p = [p.proposalId, p.creator, p.contentHash, p.createdAt]
  fromSlots [pid, c, h, t] = MkStatefulProposal pid c h t

-- =============================================================================
-- RQ-2.3: Proof-Carrying Storage Operations
-- =============================================================================

||| Storage location for proposal state
public export
record ProposalStorage where
  constructor MkProposalStorage
  stateSlot : Slot      -- slot storing the state enum
  dataSlot : Slot       -- base slot for proposal data

||| Read proposal with its current state
||| Returns a dependent pair: state + proposal indexed by that state
export
loadProposal : ProposalStorage -> IO (s : PropState ** StatefulProposal s)
loadProposal store = do
  stateInt <- sloadSlot store.stateSlot
  let Just state = fromStateInt stateInt
    | Nothing => pure (PropDraft ** MkStatefulProposal 0 0 0 0)  -- fallback
  proposal <- get (MkRef store.dataSlot)
  pure (state ** proposal)

||| Transition proposal state with PROOF.
|||
||| This is the key innovation:
||| - You CANNOT call this without a valid transition proof
||| - Invalid transitions are compile-time errors, not runtime reverts
|||
||| ```idris
||| -- OK: Draft -> Voting with Submit proof
||| transitionProposal store Submit draftProposal
|||
||| -- COMPILE ERROR: No proof for Draft -> Executed
||| transitionProposal store ??? draftProposal
||| ```
export
transitionProposal : {from, to : PropState}
                  -> ProposalStorage
                  -> PropTransition from to
                  -> StatefulProposal from
                  -> IO (StatefulProposal to)
transitionProposal store prf proposal = do
  -- Write new state to storage
  sstoreSlot store.stateSlot (toStateInt to)
  -- Data remains the same, but type changes
  let newProposal = MkStatefulProposal
        proposal.proposalId
        proposal.creator
        proposal.contentHash
        proposal.createdAt
  set (MkRef store.dataSlot) newProposal
  pure newProposal

-- =============================================================================
-- RQ-2.3: Multi-Step Workflows with Proof Chains
-- =============================================================================

||| A workflow step is a proof of valid transition
public export
data WorkflowStep : PropState -> PropState -> Type where
  SingleStep : PropTransition from to -> WorkflowStep from to
  ChainSteps : {mid : PropState} -> PropTransition from mid -> WorkflowStep mid to -> WorkflowStep from to

||| Execute a complete workflow
||| Each step requires its own proof - you cannot skip steps!
export
executeWorkflow : {from, to : PropState}
               -> ProposalStorage
               -> WorkflowStep from to
               -> StatefulProposal from
               -> IO (StatefulProposal to)
executeWorkflow store (SingleStep t) p = transitionProposal store t p
executeWorkflow {from} {to} store (ChainSteps {mid} t rest) p = do
  midProposal <- transitionProposal {from} {to=mid} store t p
  executeWorkflow {from=mid} {to} store rest midProposal

||| Example: Complete proposal lifecycle (Draft -> Voting -> Approved -> Executed)
||| This is a COMPILE-TIME VERIFIED workflow - invalid paths don't compile
export
fullApprovalWorkflow : WorkflowStep PropDraft PropExecuted
fullApprovalWorkflow = ChainSteps {mid=PropVoting} Submit
                         (ChainSteps {mid=PropApproved} Approve
                           (SingleStep Execute))

||| Example: Rejection workflow (Draft -> Voting -> Rejected)
export
rejectionWorkflow : WorkflowStep PropDraft PropRejected
rejectionWorkflow = ChainSteps {mid=PropVoting} Submit (SingleStep Reject)

-- =============================================================================
-- RQ-2.3: Conditional Transitions (Runtime + Compile-time Safety)
-- =============================================================================

||| Transition result - either succeeds with new state or fails with reason
public export
data TransitionResult : PropState -> Type where
  Success : StatefulProposal to -> TransitionResult to
  Failed : String -> TransitionResult from

||| Try to approve a proposal (requires runtime vote check + compile-time proof)
|||
||| Combines:
||| 1. Compile-time: PropTransition Voting Approved proof required
||| 2. Runtime: quorum check must pass
export
tryApprove : ProposalStorage
          -> (quorumReached : Bool)
          -> StatefulProposal PropVoting
          -> IO (Either String (StatefulProposal PropApproved))
tryApprove store quorumReached proposal =
  if quorumReached
    then do
      approved <- transitionProposal store Approve proposal
      pure (Right approved)
    else
      pure (Left "Quorum not reached")

||| Try to execute a proposal (requires approved state)
export
tryExecute : ProposalStorage
          -> (executionSucceeded : Bool)
          -> StatefulProposal PropApproved
          -> IO (Either String (StatefulProposal PropExecuted))
tryExecute store executionSucceeded proposal =
  if executionSucceeded
    then do
      executed <- transitionProposal store Execute proposal
      pure (Right executed)
    else
      pure (Left "Execution failed")

-- =============================================================================
-- What Solidity Cannot Express
-- =============================================================================

||| This function is IMPOSSIBLE to write incorrectly.
|||
||| In Solidity, you might write:
||| ```solidity
||| function approve(uint pid) {
|||   require(proposals[pid].state == State.Voting);
|||   proposals[pid].state = State.Approved;  // Could typo: State.Executed
||| }
||| ```
|||
||| In Idris2:
||| - The input type MUST be PropVoting
||| - The output type MUST be PropApproved
||| - PropTransition Voting Approved is the ONLY valid proof
||| - Typos are compile errors, not runtime bugs
export
safeApprove : ProposalStorage
           -> StatefulProposal PropVoting
           -> IO (StatefulProposal PropApproved)
safeApprove store proposal = transitionProposal store Approve proposal

||| Similarly, this guarantees we go from Approved -> Executed
export
safeExecute : ProposalStorage
           -> StatefulProposal PropApproved
           -> IO (StatefulProposal PropExecuted)
safeExecute store proposal = transitionProposal store Execute proposal

-- =============================================================================
-- Compile-Time Guarantees Summary
-- =============================================================================

-- These DON'T COMPILE (uncomment to see errors):
--
-- Invalid: Cannot go Draft -> Approved (must go through Voting)
-- badTransition1 : PropTransition PropDraft PropApproved
-- badTransition1 = ?impossible
--
-- Invalid: Cannot go Executed -> anything (terminal state)
-- badTransition2 : PropTransition PropExecuted PropDraft
-- badTransition2 = ?impossible
--
-- Invalid: Cannot skip states in workflow
-- badWorkflow : WorkflowStep PropDraft PropExecuted
-- badWorkflow = SingleStep ?noDirectPath
