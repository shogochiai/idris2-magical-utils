||| Inception Test Suite
|||
||| SPEC-Test Parity for Inception text governance.
module Subcontract.Standards.ERC7546.Inception.Tests.AllTests

import Subcontract.Standards.ERC7546.Inception.Storages.Schema
import Subcontract.Standards.ERC7546.Inception.Functions.Propose
import Subcontract.Standards.ERC7546.Inception.Functions.Fork
import Subcontract.Standards.ERC7546.Inception.Functions.Vote
import Subcontract.Standards.ERC7546.Inception.Functions.Resolve

%default covering

-- =============================================================================
-- Test Fixtures
-- =============================================================================

||| Sample InceptionSpec for testing
testInceptionSpec : InceptionSpec
testInceptionSpec = MkInceptionSpec
  (MkIntentKeywords ["security", "efficiency", "decentralization"])
  (MkNonGoals ["KYC", "centralized-custody"])
  (MkBoundary ["no-admin-keys", "max-upgrade-7d"])
  (MkAllowedChangeKinds [Bugfix, GasOptimization, Documentation])
  1
  "QmTestHash123456789012345678901234567890123456"

||| Initial test state
testInitialState : InceptionState
testInitialState = initInceptionState testInceptionSpec

||| Sample proposer address
testProposer : Integer
testProposer = 0x1234567890abcdef

||| Sample voter addresses
testVoter1 : Integer
testVoter1 = 0xaaaaaaaaaaaa

testVoter2 : Integer
testVoter2 = 0xbbbbbbbbbbbb

testVoter3 : Integer
testVoter3 = 0xcccccccccccc

-- =============================================================================
-- Schema Tests (INCEP-SPEC, PROP-TEXT, VOTE-RCV)
-- =============================================================================

||| Test: InceptionSpec initialization
test_INCEP_SPEC_init : Bool
test_INCEP_SPEC_init =
  let spec = testInceptionSpec
  in spec.version == 1
     && length spec.intentKeywords.keywords == 3
     && length spec.nonGoals.excluded == 2

||| Test: ChangeKind equality
test_INTENT_ACK_eq : Bool
test_INTENT_ACK_eq =
  Bugfix == Bugfix
  && GasOptimization /= Bugfix
  && isAutoAdoptable testInceptionSpec Bugfix
  && not (isAutoAdoptable testInceptionSpec FeatureAddition)

||| Test: ProposalStatus show
test_PROP_STATUS_show : Bool
test_PROP_STATUS_show =
  show Pending == "Pending"
  && show Active == "Active"
  && show Accepted == "Accepted"

-- =============================================================================
-- Propose Tests (PROP-SUBMIT, PROP-VALIDATE)
-- =============================================================================

||| Test: Submit valid proposal
test_PROP_SUBMIT_valid : Bool
test_PROP_SUBMIT_valid =
  let params = MkProposeParams
        "QmValidHash1234567890123456789012345678901234"
        604800  -- 7 days
        FeatureAddition
      (result, newState) = submitProposal testInitialState testProposer params 1000
  in case result of
       ProposalCreated 1 => newState.proposalCount == 1
       _ => False

||| Test: Submit with invalid IPFS hash
test_PROP_SUBMIT_invalid_hash : Bool
test_PROP_SUBMIT_invalid_hash =
  let params = MkProposeParams "short" 604800 Bugfix
      (result, _) = submitProposal testInitialState testProposer params 1000
  in case result of
       InvalidText _ => True
       _ => False

||| Test: Submit with short voting period
test_PROP_SUBMIT_short_period : Bool
test_PROP_SUBMIT_short_period =
  let params = MkProposeParams
        "QmValidHash1234567890123456789012345678901234"
        3600  -- 1 hour (too short)
        Bugfix
      (result, _) = submitProposal testInitialState testProposer params 1000
  in case result of
       InvalidText _ => True
       _ => False

-- =============================================================================
-- Fork Tests (FORK-CREATE, FORK-DEPTH, FORK-LINEAGE)
-- =============================================================================

||| Create state with proposal for forking
stateWithProposal : InceptionState
stateWithProposal =
  let params = MkProposeParams
        "QmParentHash12345678901234567890123456789012"
        604800
        FeatureAddition
      (_, state) = submitProposal testInitialState testProposer params 1000
  in case activateProposal state 1 of
       Right activated => activated
       Left _ => state

||| Test: Create valid fork
test_FORK_CREATE_valid : Bool
test_FORK_CREATE_valid =
  let forkParams = MkForkParams
        1
        "QmForkHash123456789012345678901234567890123"
        "Alternative approach to feature"
        604800
      (result, newState) = createFork stateWithProposal testVoter1 forkParams 2000
  in case result of
       ForkCreated 2 => newState.proposalCount == 2
       _ => False

||| Test: Fork non-existent parent
test_FORK_CREATE_no_parent : Bool
test_FORK_CREATE_no_parent =
  let forkParams = MkForkParams
        999  -- Non-existent
        "QmForkHash123456789012345678901234567890123"
        "Fork reason"
        604800
      (result, _) = createFork stateWithProposal testVoter1 forkParams 2000
  in case result of
       ParentNotFound => True
       _ => False

||| Test: Fork depth calculation
test_FORK_DEPTH : Bool
test_FORK_DEPTH =
  let forkParams = MkForkParams 1 "QmFork1234567890123456789012345678901234567" "Reason" 604800
  in case createFork stateWithProposal testVoter1 forkParams 2000 of
       (ForkCreated 2, state2) =>
         getForkDepth stateWithProposal 1 == 0 && getForkDepth state2 2 == 1
       _ => False

-- =============================================================================
-- Vote Tests (VOTE-CAST, VOTE-EFFECTIVE)
-- =============================================================================

||| Test: Cast valid vote
test_VOTE_CAST_valid : Bool
test_VOTE_CAST_valid =
  let voteParams = MkVoteParams 1 0 0
      (result, newState) = castVote stateWithProposal testVoter1 voteParams 1500
  in case result of
       VoteCast 1 => length newState.votes == 1
       _ => False

||| Test: Prevent double voting
test_VOTE_CAST_double : Bool
test_VOTE_CAST_double =
  let voteParams = MkVoteParams 1 0 0
  in case castVote stateWithProposal testVoter1 voteParams 1500 of
       (VoteCast _, state1) =>
         case castVote state1 testVoter1 voteParams 1600 of
           (AlreadyVoted, _) => True
           _ => False
       _ => False

||| Test: Vote on closed proposal
test_VOTE_CAST_closed : Bool
test_VOTE_CAST_closed =
  let voteParams = MkVoteParams 1 0 0
      -- Vote after deadline (proposal.votingEnds = 1000 + 604800 = 605800)
      (result, _) = castVote stateWithProposal testVoter1 voteParams 700000
  in case result of
       VotingClosed => True
       _ => False

-- =============================================================================
-- Resolve Tests (RESOLVE-PROP, RESOLVE-QUORUM, RESOLVE-SUPER)
-- =============================================================================

||| Test: Has quorum check
test_RESOLVE_QUORUM : Bool
test_RESOLVE_QUORUM =
  -- 10% quorum: 100 eligible, need 10 votes
  hasQuorum testInitialState 10 100
  && not (hasQuorum testInitialState 9 100)

||| Test: Has super majority check
test_RESOLVE_SUPER : Bool
test_RESOLVE_SUPER =
  -- 66.67% super majority
  hasSuperMajority 67 100
  && not (hasSuperMajority 66 100)
  && hasSuperMajority 2 3

||| Test: Resolve still voting
test_RESOLVE_still_voting : Bool
test_RESOLVE_still_voting =
  let (result, _) = resolveProposal stateWithProposal 1 1500
  in case result of
       StillVoting => True
       _ => False

-- =============================================================================
-- Drift Detection Tests (DRIFT-DETECT, DRIFT-VERDICT)
-- =============================================================================

||| Test: Drift verdict show
test_DRIFT_VERDICT_show : Bool
test_DRIFT_VERDICT_show =
  show Match == "Match"
  && show (DriftDetected "reason") == "DriftDetected(reason)"
  && show InsufficientEvidence == "InsufficientEvidence"

||| Test: No drift detection (placeholder)
test_DRIFT_DETECT_none : Bool
test_DRIFT_DETECT_none =
  case detectDrift testInceptionSpec "any text" of
    NoDrift => True
    _ => False

-- =============================================================================
-- Integration Tests
-- =============================================================================

||| Test: Full proposal lifecycle
test_INTEGRATION_lifecycle : Bool
test_INTEGRATION_lifecycle =
  -- 1. Submit proposal
  let params = MkProposeParams
        "QmProposalHash123456789012345678901234567890"
        604800
        FeatureAddition
  in case submitProposal testInitialState testProposer params 1000 of
       (ProposalCreated 1, state1) =>
         -- 2. Activate proposal
         case activateProposal state1 1 of
           Right state2 =>
             -- 3. Cast votes
             case castVote state2 testVoter1 (MkVoteParams 1 0 0) 2000 of
               (VoteCast _, state3) =>
                 case castVote state3 testVoter2 (MkVoteParams 1 0 0) 2100 of
                   (VoteCast _, state4) =>
                     case castVote state4 testVoter3 (MkVoteParams 1 0 0) 2200 of
                       (VoteCast _, state5) =>
                         state5.proposalCount == 1 && length state5.votes == 3
                       _ => False
                   _ => False
               _ => False
           Left _ => False
       _ => False

-- =============================================================================
-- Test Runner
-- =============================================================================

||| All tests
export
allTests : List (String, Bool)
allTests =
  [ ("INCEP-SPEC init", test_INCEP_SPEC_init)
  , ("INTENT-ACK equality", test_INTENT_ACK_eq)
  , ("PROP-STATUS show", test_PROP_STATUS_show)
  , ("PROP-SUBMIT valid", test_PROP_SUBMIT_valid)
  , ("PROP-SUBMIT invalid hash", test_PROP_SUBMIT_invalid_hash)
  , ("PROP-SUBMIT short period", test_PROP_SUBMIT_short_period)
  , ("FORK-CREATE valid", test_FORK_CREATE_valid)
  , ("FORK-CREATE no parent", test_FORK_CREATE_no_parent)
  , ("FORK-DEPTH", test_FORK_DEPTH)
  , ("VOTE-CAST valid", test_VOTE_CAST_valid)
  , ("VOTE-CAST double", test_VOTE_CAST_double)
  , ("VOTE-CAST closed", test_VOTE_CAST_closed)
  , ("RESOLVE-QUORUM", test_RESOLVE_QUORUM)
  , ("RESOLVE-SUPER", test_RESOLVE_SUPER)
  , ("RESOLVE still voting", test_RESOLVE_still_voting)
  , ("DRIFT-VERDICT show", test_DRIFT_VERDICT_show)
  , ("DRIFT-DETECT none", test_DRIFT_DETECT_none)
  , ("INTEGRATION lifecycle", test_INTEGRATION_lifecycle)
  ]

||| Run all tests and return pass count
export
runTests : (Nat, Nat)  -- (passed, total)
runTests =
  let results = map snd allTests
      passed = length (filter id results)
  in (passed, length results)
