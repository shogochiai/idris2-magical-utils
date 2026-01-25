||| TextDAO Function: Tally
||| Idris2 port of textdao-monorepo/.../functions/Tally.sol
|||
||| Tallies votes using Ranked Choice Voting (RCV) algorithm
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout
-- =============================================================================

SLOT_DELIBERATION : Integer
SLOT_DELIBERATION = 0x1000

SLOT_CONFIG_QUORUM_SCORE : Integer
SLOT_CONFIG_QUORUM_SCORE = 0x1103

-- Meta field offsets
META_OFFSET_EXPIRATION : Integer
META_OFFSET_EXPIRATION = 1

META_OFFSET_HEADER_COUNT : Integer
META_OFFSET_HEADER_COUNT = 3

META_OFFSET_CMD_COUNT : Integer
META_OFFSET_CMD_COUNT = 4

META_OFFSET_APPROVED_HEADER : Integer
META_OFFSET_APPROVED_HEADER = 5

META_OFFSET_APPROVED_CMD : Integer
META_OFFSET_APPROVED_CMD = 6

-- Vote storage offsets
VOTE_OFFSET_HEADER_RANK_0 : Integer
VOTE_OFFSET_HEADER_RANK_0 = 0

VOTE_OFFSET_CMD_RANK_0 : Integer
VOTE_OFFSET_CMD_RANK_0 = 3

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| ProposalTallied(uint256 indexed pid, uint256 approvedHeaderId, uint256 approvedCmdId)
EVENT_TALLIED : Integer
EVENT_TALLIED = 0xcf5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8ec

||| Snapped(uint256 indexed pid, uint256 epoch)
EVENT_SNAPPED : Integer
EVENT_SNAPPED = 0xdf5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8ed

-- =============================================================================
-- Storage Helpers
-- =============================================================================

getProposalSlot : Integer -> IO Integer
getProposalSlot pid = do
  mstore 0 pid
  mstore 32 SLOT_DELIBERATION
  keccak256 0 64

getProposalMetaSlot : Integer -> IO Integer
getProposalMetaSlot pid = do
  baseSlot <- getProposalSlot pid
  pure (baseSlot + 0x30)

getRepsSlot : Integer -> IO Integer
getRepsSlot pid = do
  metaSlot <- getProposalMetaSlot pid
  pure (metaSlot + 0x08)

getVoteSlot : Integer -> Integer -> IO Integer
getVoteSlot pid repAddr = do
  metaSlot <- getProposalMetaSlot pid
  let votesBaseSlot = metaSlot + 0x10
  mstore 0 repAddr
  mstore 32 votesBaseSlot
  keccak256 0 64

-- =============================================================================
-- Expiration Check
-- =============================================================================

isExpired : Integer -> IO Bool
isExpired pid = do
  metaSlot <- getProposalMetaSlot pid
  expiration <- sload (metaSlot + META_OFFSET_EXPIRATION)
  now <- timestamp
  pure (now > expiration)

-- =============================================================================
-- RCV Score Calculation (Simplified)
-- =============================================================================

||| Calculate RCV score for a header
||| Simplified: 1st choice = 3 points, 2nd = 2 points, 3rd = 1 point
calculateHeaderScore : Integer -> Integer -> IO Integer
calculateHeaderScore pid headerId = do
  repsSlot <- getRepsSlot pid
  calcScoreLoop headerId repsSlot 0 5 0
  where
    calcScoreLoop : Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
    calcScoreLoop hid repsSlot idx maxReps accScore =
      if idx >= maxReps
        then pure accScore
        else do
          repAddr <- sload (repsSlot + idx)
          if repAddr == 0
            then pure accScore
            else do
              voteSlot <- getVoteSlot pid repAddr
              rank0 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_0)
              rank1 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_0 + 1)
              rank2 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_0 + 2)
              let points = if rank0 == hid then 3
                          else if rank1 == hid then 2
                          else if rank2 == hid then 1
                          else 0
              calcScoreLoop hid repsSlot (idx + 1) maxReps (accScore + points)

||| Calculate RCV score for a command
calculateCmdScore : Integer -> Integer -> IO Integer
calculateCmdScore pid cmdId = do
  repsSlot <- getRepsSlot pid
  calcScoreLoop cmdId repsSlot 0 5 0
  where
    calcScoreLoop : Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
    calcScoreLoop cid repsSlot idx maxReps accScore =
      if idx >= maxReps
        then pure accScore
        else do
          repAddr <- sload (repsSlot + idx)
          if repAddr == 0
            then pure accScore
            else do
              voteSlot <- getVoteSlot pid repAddr
              rank0 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_0)
              rank1 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_0 + 1)
              rank2 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_0 + 2)
              let points = if rank0 == cid then 3
                          else if rank1 == cid then 2
                          else if rank2 == cid then 1
                          else 0
              calcScoreLoop cid repsSlot (idx + 1) maxReps (accScore + points)

-- =============================================================================
-- Find Winner
-- =============================================================================

||| Find header with highest score
findWinningHeader : Integer -> IO (Integer, Integer)  -- (headerId, score)
findWinningHeader pid = do
  metaSlot <- getProposalMetaSlot pid
  headerCount <- sload (metaSlot + META_OFFSET_HEADER_COUNT)
  findBest pid 0 headerCount 0 0
  where
    findBest : Integer -> Integer -> Integer -> Integer -> Integer -> IO (Integer, Integer)
    findBest pid idx count bestId bestScore =
      if idx >= count
        then pure (bestId, bestScore)
        else do
          score <- calculateHeaderScore pid idx
          if score > bestScore
            then findBest pid (idx + 1) count idx score
            else findBest pid (idx + 1) count bestId bestScore

||| Find command with highest score
findWinningCmd : Integer -> IO (Integer, Integer)  -- (cmdId, score)
findWinningCmd pid = do
  metaSlot <- getProposalMetaSlot pid
  cmdCount <- sload (metaSlot + META_OFFSET_CMD_COUNT)
  if cmdCount == 0
    then pure (0, 0)
    else findBest pid 0 cmdCount 0 0
  where
    findBest : Integer -> Integer -> Integer -> Integer -> Integer -> IO (Integer, Integer)
    findBest pid idx count bestId bestScore =
      if idx >= count
        then pure (bestId, bestScore)
        else do
          score <- calculateCmdScore pid idx
          if score > bestScore
            then findBest pid (idx + 1) count idx score
            else findBest pid (idx + 1) count bestId bestScore

-- =============================================================================
-- Final Tally
-- =============================================================================

||| Perform final tally and approve winning header/command
finalTally : Integer -> IO ()
finalTally pid = do
  quorumScore <- sload SLOT_CONFIG_QUORUM_SCORE
  -- Find winners
  (winningHeaderId, headerScore) <- findWinningHeader pid
  (winningCmdId, cmdScore) <- findWinningCmd pid
  -- Check quorum
  if headerScore >= quorumScore
    then do
      metaSlot <- getProposalMetaSlot pid
      -- Approve header (+1 to distinguish from "not set")
      sstore (metaSlot + META_OFFSET_APPROVED_HEADER) (winningHeaderId + 1)
      -- Approve command if quorum met
      if cmdScore >= quorumScore
        then sstore (metaSlot + META_OFFSET_APPROVED_CMD) (winningCmdId + 1)
        else pure ()
      -- Emit event
      mstore 0 winningHeaderId
      mstore 32 winningCmdId
      log2 0 64 EVENT_TALLIED pid
    else pure ()  -- Quorum not met, no approval

-- =============================================================================
-- Main Tally Function
-- =============================================================================

||| Tally a proposal
||| Can be called by anyone (keepers)
tally : Integer -> IO ()
tally pid = do
  expired <- isExpired pid
  if expired
    then finalTally pid
    else pure ()  -- Snap functionality omitted for simplicity

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- tally(uint256)
SEL_TALLY : Integer
SEL_TALLY = 0xd8bff5a5

-- getApprovedHeader(uint256) returns (uint256)
SEL_GET_APPROVED_HEADER : Integer
SEL_GET_APPROVED_HEADER = 0x7a1e7ab3

-- getApprovedCmd(uint256) returns (uint256)
SEL_GET_APPROVED_CMD : Integer
SEL_GET_APPROVED_CMD = 0x8b2e8bc4

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_TALLY
    then do
      pid <- calldataload 4
      tally pid
      evmReturn 0 0

    else if selector == SEL_GET_APPROVED_HEADER
    then do
      pid <- calldataload 4
      metaSlot <- getProposalMetaSlot pid
      approved <- sload (metaSlot + META_OFFSET_APPROVED_HEADER)
      -- Subtract 1 to get actual ID (0 means not approved)
      let actualId = if approved == 0 then 0 else approved - 1
      returnUint actualId

    else if selector == SEL_GET_APPROVED_CMD
    then do
      pid <- calldataload 4
      metaSlot <- getProposalMetaSlot pid
      approved <- sload (metaSlot + META_OFFSET_APPROVED_CMD)
      let actualId = if approved == 0 then 0 else approved - 1
      returnUint actualId

    else evmRevert 0 0
