||| Inception Storage Slots
|||
||| ERC-7201 namespaced storage slots for Inception text governance.
||| Stores intent vocabulary, text proposals, and voting state.
module Subcontract.Standards.ERC7546.Inception.Storages.Slots

%default total

-- =============================================================================
-- ERC-7201 Namespace
-- =============================================================================

||| Inception namespace: keccak256("inception.storage.v1") - 1
||| Used as base for all Inception storage slots
public export
INCEPTION_NAMESPACE : Integer
INCEPTION_NAMESPACE = 0x1a2b3c4d5e6f7890  -- Placeholder, compute actual hash

-- =============================================================================
-- InceptionSpec Storage (The Constitution)
-- =============================================================================

||| Current InceptionSpec IPFS hash (bytes32)
public export
SLOT_INCEPTION_HASH : Integer
SLOT_INCEPTION_HASH = INCEPTION_NAMESPACE + 0

||| InceptionSpec version counter
public export
SLOT_INCEPTION_VERSION : Integer
SLOT_INCEPTION_VERSION = INCEPTION_NAMESPACE + 1

||| Last update timestamp
public export
SLOT_INCEPTION_UPDATED_AT : Integer
SLOT_INCEPTION_UPDATED_AT = INCEPTION_NAMESPACE + 2

-- =============================================================================
-- Text Proposal Storage
-- =============================================================================

||| Total proposal count
public export
SLOT_PROPOSAL_COUNT : Integer
SLOT_PROPOSAL_COUNT = INCEPTION_NAMESPACE + 10

||| Proposals mapping base: proposalId => TextProposal
public export
SLOT_PROPOSALS_BASE : Integer
SLOT_PROPOSALS_BASE = INCEPTION_NAMESPACE + 11

-- TextProposal struct offsets
public export
OFFSET_TEXT_HASH : Integer      -- IPFS hash of proposed text
OFFSET_TEXT_HASH = 0

public export
OFFSET_PROPOSER : Integer       -- Address of proposer
OFFSET_PROPOSER = 1

public export
OFFSET_PARENT_ID : Integer      -- Parent proposal (0 = root, >0 = fork)
OFFSET_PARENT_ID = 2

public export
OFFSET_STATUS : Integer         -- 0=Pending, 1=Active, 2=Accepted, 3=Rejected
OFFSET_STATUS = 3

public export
OFFSET_CREATED_AT : Integer     -- Block timestamp
OFFSET_CREATED_AT = 4

public export
OFFSET_VOTING_ENDS : Integer    -- Voting deadline
OFFSET_VOTING_ENDS = 5

public export
OFFSET_TOTAL_VOTES : Integer    -- Total vote weight cast
OFFSET_TOTAL_VOTES = 6

-- =============================================================================
-- Vote Storage (RCV - Ranked Choice Voting)
-- =============================================================================

||| Votes mapping base: proposalId => voter => RankedVote
public export
SLOT_VOTES_BASE : Integer
SLOT_VOTES_BASE = INCEPTION_NAMESPACE + 100

-- RankedVote struct offsets (up to 3 ranked choices)
public export
OFFSET_RANK_1 : Integer         -- First choice proposal ID
OFFSET_RANK_1 = 0

public export
OFFSET_RANK_2 : Integer         -- Second choice proposal ID
OFFSET_RANK_2 = 1

public export
OFFSET_RANK_3 : Integer         -- Third choice proposal ID
OFFSET_RANK_3 = 2

public export
OFFSET_VOTE_WEIGHT : Integer    -- Voter's weight (e.g., token balance)
OFFSET_VOTE_WEIGHT = 3

-- =============================================================================
-- Fork Tree Storage
-- =============================================================================

||| Fork children mapping base: parentId => childIndex => childId
public export
SLOT_FORK_CHILDREN_BASE : Integer
SLOT_FORK_CHILDREN_BASE = INCEPTION_NAMESPACE + 200

||| Fork children count: parentId => count
public export
SLOT_FORK_COUNT_BASE : Integer
SLOT_FORK_COUNT_BASE = INCEPTION_NAMESPACE + 201

-- =============================================================================
-- Config Storage
-- =============================================================================

||| Minimum voting period (seconds)
public export
SLOT_MIN_VOTING_PERIOD : Integer
SLOT_MIN_VOTING_PERIOD = INCEPTION_NAMESPACE + 300

||| Quorum threshold (basis points, 10000 = 100%)
public export
SLOT_QUORUM_BPS : Integer
SLOT_QUORUM_BPS = INCEPTION_NAMESPACE + 301

||| Super majority threshold for Inception updates (basis points)
public export
SLOT_SUPER_MAJORITY_BPS : Integer
SLOT_SUPER_MAJORITY_BPS = INCEPTION_NAMESPACE + 302

||| Authorized proposers mapping base
public export
SLOT_AUTHORIZED_PROPOSERS_BASE : Integer
SLOT_AUTHORIZED_PROPOSERS_BASE = INCEPTION_NAMESPACE + 310
