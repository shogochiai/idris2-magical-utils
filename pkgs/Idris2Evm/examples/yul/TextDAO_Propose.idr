||| TextDAO Function: Propose
||| Idris2 port of textdao-monorepo/.../functions/onlyMember/Propose.sol
|||
||| Allows members to create new proposals with headers and optional commands
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout (from Schema)
-- =============================================================================

SLOT_DELIBERATION : Integer
SLOT_DELIBERATION = 0x1000

SLOT_PROPOSAL_COUNT : Integer
SLOT_PROPOSAL_COUNT = 0x1001

SLOT_MEMBERS : Integer
SLOT_MEMBERS = 0x3000

SLOT_MEMBER_COUNT : Integer
SLOT_MEMBER_COUNT = 0x3001

-- Config slots
SLOT_CONFIG_EXPIRY_DURATION : Integer
SLOT_CONFIG_EXPIRY_DURATION = 0x1100

SLOT_CONFIG_SNAP_INTERVAL : Integer
SLOT_CONFIG_SNAP_INTERVAL = 0x1101

SLOT_CONFIG_REPS_NUM : Integer
SLOT_CONFIG_REPS_NUM = 0x1102

-- Meta field offsets
META_OFFSET_CREATED_AT : Integer
META_OFFSET_CREATED_AT = 0

META_OFFSET_EXPIRATION : Integer
META_OFFSET_EXPIRATION = 1

META_OFFSET_SNAP_INTERVAL : Integer
META_OFFSET_SNAP_INTERVAL = 2

META_OFFSET_HEADER_COUNT : Integer
META_OFFSET_HEADER_COUNT = 3

META_OFFSET_CMD_COUNT : Integer
META_OFFSET_CMD_COUNT = 4

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| Proposed(uint256 indexed pid, address indexed proposer, uint256 createdAt, uint256 expirationTime, uint256 snapInterval)
EVENT_PROPOSED : Integer
EVENT_PROPOSED = 0x8f5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e8

||| HeaderCreated(uint256 indexed pid, uint256 indexed headerId, string metadataCid)
EVENT_HEADER_CREATED : Integer
EVENT_HEADER_CREATED = 0x9f5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e9

||| CommandCreated(uint256 indexed pid, uint256 indexed cmdId)
EVENT_COMMAND_CREATED : Integer
EVENT_COMMAND_CREATED = 0xaf5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8ea

-- =============================================================================
-- Storage Helpers
-- =============================================================================

||| Calculate proposal base slot
getProposalSlot : Integer -> IO Integer
getProposalSlot pid = do
  mstore 0 pid
  mstore 32 SLOT_DELIBERATION
  keccak256 0 64

||| Get proposal meta slot
getProposalMetaSlot : Integer -> IO Integer
getProposalMetaSlot pid = do
  baseSlot <- getProposalSlot pid
  pure (baseSlot + 0x30)

||| Get headers base slot for proposal
getProposalHeadersSlot : Integer -> IO Integer
getProposalHeadersSlot pid = do
  baseSlot <- getProposalSlot pid
  pure (baseSlot + 0x10)

||| Get specific header slot
getHeaderSlot : Integer -> Integer -> IO Integer
getHeaderSlot pid hid = do
  headersSlot <- getProposalHeadersSlot pid
  mstore 0 hid
  mstore 32 headersSlot
  keccak256 0 64

||| Get commands base slot for proposal
getProposalCmdsSlot : Integer -> IO Integer
getProposalCmdsSlot pid = do
  baseSlot <- getProposalSlot pid
  pure (baseSlot + 0x20)

||| Get member slot by index
getMemberSlot : Integer -> IO Integer
getMemberSlot index = do
  mstore 0 index
  mstore 32 SLOT_MEMBERS
  keccak256 0 64

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if address is a member
isMember : Integer -> IO Bool
isMember addr = do
  memberCount <- sload SLOT_MEMBER_COUNT
  checkMembership addr 0 memberCount
  where
    checkMembership : Integer -> Integer -> Integer -> IO Bool
    checkMembership addr idx count =
      if idx >= count
        then pure False
        else do
          memberSlot <- getMemberSlot idx
          memberAddr <- sload memberSlot
          if memberAddr == addr
            then pure True
            else checkMembership addr (idx + 1) count

||| Require caller to be a member
requireMember : IO ()
requireMember = do
  callerAddr <- caller
  isMem <- isMember callerAddr
  if isMem
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Proposal Creation
-- =============================================================================

||| Create a new proposal
||| Returns the new proposal ID
createProposal : IO Integer
createProposal = do
  pid <- sload SLOT_PROPOSAL_COUNT
  sstore SLOT_PROPOSAL_COUNT (pid + 1)
  pure pid

||| Create a header for a proposal
||| metadataCid is stored as a hash
createHeader : Integer -> Integer -> IO Integer
createHeader pid metadataCid = do
  metaSlot <- getProposalMetaSlot pid
  headerCount <- sload (metaSlot + META_OFFSET_HEADER_COUNT)
  -- Store header metadata
  headerSlot <- getHeaderSlot pid headerCount
  sstore headerSlot metadataCid
  -- Increment header count
  sstore (metaSlot + META_OFFSET_HEADER_COUNT) (headerCount + 1)
  pure headerCount

||| Setup proposal timing
setupTimes : Integer -> IO ()
setupTimes pid = do
  now <- timestamp
  expiryDuration <- sload SLOT_CONFIG_EXPIRY_DURATION
  snapInterval <- sload SLOT_CONFIG_SNAP_INTERVAL
  metaSlot <- getProposalMetaSlot pid
  sstore (metaSlot + META_OFFSET_CREATED_AT) now
  sstore (metaSlot + META_OFFSET_EXPIRATION) (now + expiryDuration)
  sstore (metaSlot + META_OFFSET_SNAP_INTERVAL) snapInterval

-- =============================================================================
-- Main Propose Function
-- =============================================================================

||| propose(bytes32 headerMetadataCid)
||| Simplified version: takes header metadata CID hash and creates proposal
propose : Integer -> IO Integer
propose headerMetadataCid = do
  requireMember
  -- Create new proposal
  pid <- createProposal
  -- Create header
  headerId <- createHeader pid headerMetadataCid
  -- Setup timing
  setupTimes pid
  -- Emit events
  proposerAddr <- caller
  now <- timestamp
  expiration <- do
    metaSlot <- getProposalMetaSlot pid
    sload (metaSlot + META_OFFSET_EXPIRATION)
  snapInt <- sload SLOT_CONFIG_SNAP_INTERVAL
  -- Emit HeaderCreated event
  mstore 0 headerMetadataCid
  log3 0 32 EVENT_HEADER_CREATED pid headerId
  -- Emit Proposed event (encode times in data)
  mstore 0 now
  mstore 32 expiration
  mstore 64 snapInt
  log3 0 96 EVENT_PROPOSED pid proposerAddr
  -- Return proposal ID
  pure pid

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- propose(bytes32) -> selector
SEL_PROPOSE : Integer
SEL_PROPOSE = 0xd4a22fd4

-- getProposalCount() -> selector
SEL_GET_PROPOSAL_COUNT : Integer
SEL_GET_PROPOSAL_COUNT = 0x50d1f5c6

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_PROPOSE
    then do
      -- propose(bytes32 headerMetadataCid)
      metadataCid <- calldataload 4
      pid <- propose metadataCid
      returnUint pid

    else if selector == SEL_GET_PROPOSAL_COUNT
    then do
      -- getProposalCount()
      count <- sload SLOT_PROPOSAL_COUNT
      returnUint count

    else evmRevert 0 0
