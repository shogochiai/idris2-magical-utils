||| OptimisticUpgrader Storage Schema
|||
||| Storage access functions for OU multisig admin contract.
module Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Schema

import public EVM.Primitives
import EVM.Storage.Namespace
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Slots

%default covering

-- =============================================================================
-- Slot Calculation
-- =============================================================================

||| Calculate proposal storage slot
||| slot = keccak256(proposalId . SLOT_PROPOSALS_BASE)
export
getProposalSlot : Integer -> IO Integer
getProposalSlot proposalId = mappingSlot SLOT_PROPOSALS_BASE proposalId

||| Calculate vote storage slot
||| slot = keccak256(auditorAddr . keccak256(proposalId . SLOT_VOTES_BASE))
export
getVoteSlot : Integer -> Integer -> IO Integer
getVoteSlot proposalId auditorAddr = do
  innerSlot <- mappingSlot SLOT_VOTES_BASE proposalId
  mappingSlot innerSlot auditorAddr

||| Calculate auditor list slot
||| slot = keccak256(index . SLOT_AUDITORS_BASE)
export
getAuditorSlot : Integer -> IO Integer
getAuditorSlot idx = mappingSlot SLOT_AUDITORS_BASE idx

-- =============================================================================
-- Proposal Storage Access
-- =============================================================================

||| Get proposal target proxy
export
getTargetProxy : Integer -> IO Integer
getTargetProxy proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_TARGET_PROXY)

||| Get proposal new implementation
export
getNewImpl : Integer -> IO Integer
getNewImpl proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_NEW_IMPL)

||| Get proposal function selector
export
getProposalSelector : Integer -> IO Integer
getProposalSelector proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_SELECTOR)

||| Get proposer signature hash
export
getProposerSig : Integer -> IO Integer
getProposerSig proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_PROPOSER_SIG)

||| Set proposer signature hash
export
setProposerSig : Integer -> Integer -> IO ()
setProposerSig proposalId sigHash = do
  slot <- getProposalSlot proposalId
  sstore (slot + OFFSET_PROPOSER_SIG) sigHash

||| Get current vote count
export
getVoteCount : Integer -> IO Integer
getVoteCount proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_VOTE_COUNT)

||| Increment vote count
export
incrementVoteCount : Integer -> IO Integer
incrementVoteCount proposalId = do
  slot <- getProposalSlot proposalId
  current <- sload (slot + OFFSET_VOTE_COUNT)
  let newCount = current + 1
  sstore (slot + OFFSET_VOTE_COUNT) newCount
  pure newCount

||| Get vote threshold
export
getThreshold : Integer -> IO Integer
getThreshold proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_THRESHOLD)

||| Get voting deadline
export
getDeadline : Integer -> IO Integer
getDeadline proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_DEADLINE)

||| Get executed flag
export
getExecuted : Integer -> IO Integer
getExecuted proposalId = do
  slot <- getProposalSlot proposalId
  sload (slot + OFFSET_EXECUTED)

||| Set executed flag
export
setExecuted : Integer -> IO ()
setExecuted proposalId = do
  slot <- getProposalSlot proposalId
  sstore (slot + OFFSET_EXECUTED) 1

||| Store full proposal data
export
storeProposal : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
storeProposal proposalId targetProxy newImpl selector threshold deadline = do
  slot <- getProposalSlot proposalId
  sstore (slot + OFFSET_TARGET_PROXY) targetProxy
  sstore (slot + OFFSET_NEW_IMPL) newImpl
  sstore (slot + OFFSET_SELECTOR) selector
  sstore (slot + OFFSET_PROPOSER_SIG) 0
  sstore (slot + OFFSET_VOTE_COUNT) 0
  sstore (slot + OFFSET_THRESHOLD) threshold
  sstore (slot + OFFSET_DEADLINE) deadline
  sstore (slot + OFFSET_EXECUTED) 0

-- =============================================================================
-- Vote Storage Access
-- =============================================================================

||| Get vote decision for auditor
export
getVoteDecision : Integer -> Integer -> IO Integer
getVoteDecision proposalId auditorAddr = do
  slot <- getVoteSlot proposalId auditorAddr
  sload (slot + OFFSET_VOTE_DECISION)

||| Get vote signature for auditor
export
getVoteSig : Integer -> Integer -> IO Integer
getVoteSig proposalId auditorAddr = do
  slot <- getVoteSlot proposalId auditorAddr
  sload (slot + OFFSET_VOTE_SIG)

||| Store vote
export
storeVote : Integer -> Integer -> Integer -> Integer -> IO ()
storeVote proposalId auditorAddr decision sigHash = do
  slot <- getVoteSlot proposalId auditorAddr
  sstore (slot + OFFSET_VOTE_DECISION) decision
  sstore (slot + OFFSET_VOTE_SIG) sigHash

-- =============================================================================
-- Auditor List Access
-- =============================================================================

||| Get auditor count
export
getAuditorCount : IO Integer
getAuditorCount = sload SLOT_AUDITOR_COUNT

||| Set auditor count
export
setAuditorCount : Integer -> IO ()
setAuditorCount count = sstore SLOT_AUDITOR_COUNT count

||| Get auditor address by index
export
getAuditorAddr : Integer -> IO Integer
getAuditorAddr idx = do
  slot <- getAuditorSlot idx
  sload slot

||| Add auditor to list
export
addAuditor : Integer -> IO Integer
addAuditor auditorAddr = do
  idx <- getAuditorCount
  slot <- getAuditorSlot idx
  sstore slot auditorAddr
  setAuditorCount (idx + 1)
  pure idx

||| Check if address is auditor
export
isAuditor : Integer -> IO Bool
isAuditor addr = do
  count <- getAuditorCount
  checkAuditor addr 0 count
  where
    checkAuditor : Integer -> Integer -> Integer -> IO Bool
    checkAuditor target idx cnt =
      if idx >= cnt
        then pure False
        else do
          auditorAddr <- getAuditorAddr idx
          if auditorAddr == target
            then pure True
            else checkAuditor target (idx + 1) cnt

-- =============================================================================
-- Config Access
-- =============================================================================

||| Get Dictionary contract address
export
getDictionary : IO Integer
getDictionary = sload SLOT_DICTIONARY

||| Set Dictionary contract address (only during init)
export
setDictionary : Integer -> IO ()
setDictionary addr = sstore SLOT_DICTIONARY addr

||| Get proposer address
export
getProposer : IO Integer
getProposer = sload SLOT_PROPOSER

||| Set proposer address
export
setProposer : Integer -> IO ()
setProposer addr = sstore SLOT_PROPOSER addr
