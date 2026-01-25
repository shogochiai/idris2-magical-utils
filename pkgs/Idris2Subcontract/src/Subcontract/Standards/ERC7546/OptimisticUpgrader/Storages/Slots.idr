||| OptimisticUpgrader Storage Slots
|||
||| Storage slot constants for OU multisig admin contract.
module Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Slots

-- =============================================================================
-- OU Storage Slots (ERC-7201 namespaced)
-- =============================================================================

||| Base slot for proposals mapping
||| keccak256("erc7546.ou.proposals") - 1
public export
SLOT_PROPOSALS_BASE : Integer
SLOT_PROPOSALS_BASE = 0x1000

||| Base slot for votes mapping
||| keccak256("erc7546.ou.votes") - 1
public export
SLOT_VOTES_BASE : Integer
SLOT_VOTES_BASE = 0x2000

||| Slot for auditor list
public export
SLOT_AUDITORS_BASE : Integer
SLOT_AUDITORS_BASE = 0x3000

||| Slot for auditor count
public export
SLOT_AUDITOR_COUNT : Integer
SLOT_AUDITOR_COUNT = 0x3001

||| Slot for Dictionary contract address
public export
SLOT_DICTIONARY : Integer
SLOT_DICTIONARY = 0x4000

||| Slot for proposer address
public export
SLOT_PROPOSER : Integer
SLOT_PROPOSER = 0x4001

-- =============================================================================
-- Proposal Data Offsets (8 slots per proposal)
-- =============================================================================

||| Offset: Target proxy address
public export
OFFSET_TARGET_PROXY : Integer
OFFSET_TARGET_PROXY = 0

||| Offset: New implementation address
public export
OFFSET_NEW_IMPL : Integer
OFFSET_NEW_IMPL = 1

||| Offset: Function selector to update
public export
OFFSET_SELECTOR : Integer
OFFSET_SELECTOR = 2

||| Offset: Proposer signature (hash)
public export
OFFSET_PROPOSER_SIG : Integer
OFFSET_PROPOSER_SIG = 3

||| Offset: Current approve vote count
public export
OFFSET_VOTE_COUNT : Integer
OFFSET_VOTE_COUNT = 4

||| Offset: Required votes (threshold)
public export
OFFSET_THRESHOLD : Integer
OFFSET_THRESHOLD = 5

||| Offset: Voting deadline (timestamp)
public export
OFFSET_DEADLINE : Integer
OFFSET_DEADLINE = 6

||| Offset: Executed flag (0 or 1)
public export
OFFSET_EXECUTED : Integer
OFFSET_EXECUTED = 7

-- =============================================================================
-- Vote Data Offsets (2 slots per vote)
-- =============================================================================

||| Offset: Vote decision (0=none, 1=approve, 2=reject, 3=changes)
public export
OFFSET_VOTE_DECISION : Integer
OFFSET_VOTE_DECISION = 0

||| Offset: Auditor signature (hash)
public export
OFFSET_VOTE_SIG : Integer
OFFSET_VOTE_SIG = 1

-- =============================================================================
-- Function Selectors
-- =============================================================================

||| castVote(uint256 proposalId, uint8 decision, bytes signature)
||| keccak256("castVote(uint256,uint8,bytes)")[:4]
public export
SEL_CAST_VOTE : Integer
SEL_CAST_VOTE = 0x5c19a95c

||| submitProposerSignature(uint256 proposalId, bytes signature)
public export
SEL_SUBMIT_PROPOSER_SIG : Integer
SEL_SUBMIT_PROPOSER_SIG = 0x7d4b1d9e

||| getVotingStatus(uint256 proposalId) -> (uint256, uint256, bool)
public export
SEL_GET_VOTING_STATUS : Integer
SEL_GET_VOTING_STATUS = 0x8a2c7b5e

||| createProposal(address proxy, address newImpl, bytes4 selector, uint256 deadline)
public export
SEL_CREATE_PROPOSAL : Integer
SEL_CREATE_PROPOSAL = 0x3b2d5c8a

||| setImplementation(bytes4 selector, address impl) on Dictionary
public export
SEL_SET_IMPL : Integer
SEL_SET_IMPL = 0x2c3c3e4e

-- =============================================================================
-- Events
-- =============================================================================

||| VoteCast(uint256 indexed proposalId, address indexed auditor, uint8 decision)
public export
EVENT_VOTE_CAST : Integer
EVENT_VOTE_CAST = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef

||| UpgradeExecuted(uint256 indexed proposalId, address proxy, address newImpl)
public export
EVENT_UPGRADE_EXECUTED : Integer
EVENT_UPGRADE_EXECUTED = 0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890

||| ProposalCreated(uint256 indexed proposalId, address proxy, address newImpl)
public export
EVENT_PROPOSAL_CREATED : Integer
EVENT_PROPOSAL_CREATED = 0x567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234
