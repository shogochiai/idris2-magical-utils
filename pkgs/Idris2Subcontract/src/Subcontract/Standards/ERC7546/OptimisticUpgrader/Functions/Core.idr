||| OptimisticUpgrader - n-of-n Multisig Admin for ERC-7546 Dictionary
|||
||| Manages upgrade proposals with n-of-n auditor approval.
||| When all auditors approve, automatically executes upgrade on Dictionary.
|||
||| Flow:
||| 1. Proposer creates proposal with target, newImpl, selector
||| 2. Auditors cast votes (approve/reject)
||| 3. Proposer submits signature
||| 4. When threshold met + proposer sig, auto-execute upgrade
module Subcontract.Standards.ERC7546.OptimisticUpgrader.Functions.Core

import public EVM.Primitives
import EVM.Storage.Namespace
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Slots
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Schema
import Subcontract.Standards.ERC7546.Dictionary

%default covering

-- =============================================================================
-- Decision Types
-- =============================================================================

||| Vote decision enum
public export
DECISION_NONE : Integer
DECISION_NONE = 0

public export
DECISION_APPROVE : Integer
DECISION_APPROVE = 1

public export
DECISION_REJECT : Integer
DECISION_REJECT = 2

public export
DECISION_REQUEST_CHANGES : Integer
DECISION_REQUEST_CHANGES = 3

-- =============================================================================
-- Access Control
-- =============================================================================

||| Require caller to be proposer
requireProposer : IO ()
requireProposer = do
  proposer <- getProposer
  callerAddr <- caller
  if proposer == callerAddr
    then pure ()
    else evmRevert 0 0

||| Require caller to be auditor
requireAuditor : IO ()
requireAuditor = do
  callerAddr <- caller
  isAud <- isAuditor callerAddr
  if isAud
    then pure ()
    else evmRevert 0 0

||| Require proposal not expired
requireNotExpired : Integer -> IO ()
requireNotExpired proposalId = do
  deadline <- getDeadline proposalId
  now <- timestamp
  if now <= deadline
    then pure ()
    else evmRevert 0 0

||| Require proposal not executed
requireNotExecuted : Integer -> IO ()
requireNotExecuted proposalId = do
  executed <- getExecuted proposalId
  if executed == 0
    then pure ()
    else evmRevert 0 0

||| Require caller has not voted
requireNotVoted : Integer -> Integer -> IO ()
requireNotVoted proposalId auditorAddr = do
  decision <- getVoteDecision proposalId auditorAddr
  if decision == DECISION_NONE
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Core Functions (mutual block for forward references)
-- =============================================================================

mutual
  ||| Get voting status
  ||| Returns (currentVotes, requiredVotes, isComplete)
  export
  getVotingStatus : Integer -> IO (Integer, Integer, Integer)
  getVotingStatus proposalId = do
    currentVotes <- getVoteCount proposalId
    threshold <- getThreshold proposalId
    proposerSig <- getProposerSig proposalId
    executed <- getExecuted proposalId

    let isComplete = if currentVotes >= threshold && proposerSig /= 0 && executed == 0
                       then 1
                       else 0
    pure (currentVotes, threshold, isComplete)

  ||| Execute the upgrade on Dictionary
  ||| Internal: called when all conditions met
  executeUpgrade : Integer -> IO ()
  executeUpgrade proposalId = do
    -- Mark as executed first (reentrancy protection)
    setExecuted proposalId

    -- Get proposal data
    targetProxy <- getTargetProxy proposalId
    newImpl <- getNewImpl proposalId
    selector <- getProposalSelector proposalId

    -- Get Dictionary address
    dictAddr <- getDictionary

    -- Build calldata for Dictionary.setImplementation(selector, impl)
    mstore 0 SEL_SET_IMPL
    mstore 4 selector
    mstore 36 newImpl

    -- Call Dictionary
    success <- call 100000 dictAddr 0 0 68 0 0

    if success /= 0
      then do
        -- Emit success event
        mstore 0 newImpl
        log3 0 32 EVENT_UPGRADE_EXECUTED proposalId targetProxy
      else evmRevert 0 0  -- Dictionary call failed

  ||| Try to execute upgrade if conditions met
  ||| Called after each vote and proposer sig submission
  tryExecuteUpgrade : Integer -> IO ()
  tryExecuteUpgrade proposalId = do
    (currentVotes, threshold, isComplete) <- getVotingStatus proposalId
    if isComplete == 1
      then executeUpgrade proposalId
      else pure ()

  ||| Create a new upgrade proposal
  ||| Only proposer can call
  export
  createProposal : Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
  createProposal proposalId targetProxy newImpl selector deadline = do
    requireProposer
    -- Get threshold from auditor count (n-of-n)
    auditorCount <- getAuditorCount
    if auditorCount == 0
      then evmRevert 0 0  -- Need at least one auditor
      else do
        storeProposal proposalId targetProxy newImpl selector auditorCount deadline
        -- Emit event
        mstore 0 targetProxy
        mstore 32 newImpl
        log2 0 64 EVENT_PROPOSAL_CREATED proposalId

  ||| Cast a vote on a proposal
  ||| Only auditors can call, once per proposal
  export
  castVote : Integer -> Integer -> Integer -> IO ()
  castVote proposalId decision sigHash = do
    requireAuditor
    requireNotExpired proposalId
    requireNotExecuted proposalId
    callerAddr <- caller
    requireNotVoted proposalId callerAddr

    -- Store vote
    storeVote proposalId callerAddr decision sigHash

    -- If approve, increment count and check threshold
    if decision == DECISION_APPROVE
      then do
        newCount <- incrementVoteCount proposalId
        -- Emit event
        mstore 0 decision
        log3 0 32 EVENT_VOTE_CAST proposalId callerAddr
        -- Check if ready to execute
        tryExecuteUpgrade proposalId
      else do
        -- Emit event for non-approve votes
        mstore 0 decision
        log3 0 32 EVENT_VOTE_CAST proposalId callerAddr

  ||| Submit proposer signature
  ||| Required for execution
  export
  submitProposerSignature : Integer -> Integer -> IO ()
  submitProposerSignature proposalId sigHash = do
    requireProposer
    requireNotExecuted proposalId

    -- Check not already submitted
    existingSig <- getProposerSig proposalId
    if existingSig /= 0
      then evmRevert 0 0  -- Already submitted
      else do
        setProposerSig proposalId sigHash
        -- Check if ready to execute
        tryExecuteUpgrade proposalId

-- =============================================================================
-- View Functions
-- =============================================================================

||| Check if proposal exists (has non-zero threshold)
export
proposalExists : Integer -> IO Bool
proposalExists proposalId = do
  threshold <- getThreshold proposalId
  pure (threshold /= 0)

||| Check if voting is complete
export
isVotingComplete : Integer -> IO Bool
isVotingComplete proposalId = do
  (_, _, isComplete) <- getVotingStatus proposalId
  pure (isComplete == 1)

||| Check if proposal is executed
export
isExecuted : Integer -> IO Bool
isExecuted proposalId = do
  executed <- getExecuted proposalId
  pure (executed /= 0)

||| Get auditor's vote on proposal
export
getAuditorVote : Integer -> Integer -> IO Integer
getAuditorVote proposalId auditorAddr = getVoteDecision proposalId auditorAddr

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Main entry point for OU contract
export
ouMain : IO ()
ouMain = do
  selector <- getSelector

  if selector == SEL_CREATE_PROPOSAL
    then do
      -- createProposal(uint256, address, address, bytes4, uint256)
      proposalId <- calldataload 4
      targetProxy <- calldataload 36
      newImpl <- calldataload 68
      sel <- calldataload 100
      deadline <- calldataload 132
      createProposal proposalId targetProxy newImpl sel deadline
      evmReturn 0 0
    else if selector == SEL_CAST_VOTE
      then do
        -- castVote(uint256, uint8, bytes32)
        proposalId <- calldataload 4
        decision <- calldataload 36
        sigHash <- calldataload 68
        castVote proposalId decision sigHash
        evmReturn 0 0
      else if selector == SEL_SUBMIT_PROPOSER_SIG
        then do
          -- submitProposerSignature(uint256, bytes32)
          proposalId <- calldataload 4
          sigHash <- calldataload 36
          submitProposerSignature proposalId sigHash
          evmReturn 0 0
        else if selector == SEL_GET_VOTING_STATUS
          then do
            -- getVotingStatus(uint256) -> (uint256, uint256, bool)
            proposalId <- calldataload 4
            (current, required, complete) <- getVotingStatus proposalId
            mstore 0 current
            mstore 32 required
            mstore 64 complete
            evmReturn 0 96
          else evmRevert 0 0  -- Unknown selector
