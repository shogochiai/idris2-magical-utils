||| Inception Storage Schema
|||
||| Type definitions and storage access for Inception text governance.
||| Inception = the human intent injection point for Self-Amending Protocols.
module Subcontract.Standards.ERC7546.Inception.Storages.Schema

import Data.List

%default total

-- =============================================================================
-- InceptionSpec Types (The Constitution)
-- =============================================================================

||| Intent keywords that guide LLM proposal generation
||| e.g., ["security", "efficiency", "user-privacy"]
public export
record IntentKeywords where
  constructor MkIntentKeywords
  keywords : List String

||| Things the protocol explicitly will NOT do
||| e.g., ["KYC", "centralized-custody", "MEV-extraction"]
public export
record NonGoals where
  constructor MkNonGoals
  excluded : List String

||| Hard boundaries that must never be crossed
||| e.g., ["no-admin-keys", "max-upgrade-frequency-7d"]
public export
record Boundary where
  constructor MkBoundary
  constraints : List String

||| Change kinds that can be auto-adopted without human review
||| e.g., ["bugfix", "gas-optimization", "documentation"]
public export
data ChangeKind
  = Bugfix
  | GasOptimization
  | Documentation
  | SecurityPatch
  | FeatureAddition
  | FeatureRemoval
  | ParameterChange
  | ArchitecturalChange

public export
Show ChangeKind where
  show Bugfix = "bugfix"
  show GasOptimization = "gas-optimization"
  show Documentation = "documentation"
  show SecurityPatch = "security-patch"
  show FeatureAddition = "feature-addition"
  show FeatureRemoval = "feature-removal"
  show ParameterChange = "parameter-change"
  show ArchitecturalChange = "architectural-change"

public export
Eq ChangeKind where
  Bugfix == Bugfix = True
  GasOptimization == GasOptimization = True
  Documentation == Documentation = True
  SecurityPatch == SecurityPatch = True
  FeatureAddition == FeatureAddition = True
  FeatureRemoval == FeatureRemoval = True
  ParameterChange == ParameterChange = True
  ArchitecturalChange == ArchitecturalChange = True
  _ == _ = False

||| Allowed change kinds for auto-adoption
public export
record AllowedChangeKinds where
  constructor MkAllowedChangeKinds
  allowed : List ChangeKind

||| The complete Inception specification
||| This is the "constitution" that defines protocol intent
public export
record InceptionSpec where
  constructor MkInceptionSpec
  intentKeywords     : IntentKeywords
  nonGoals           : NonGoals
  boundary           : Boundary
  allowedChangeKinds : AllowedChangeKinds
  version            : Nat
  ipfsHash           : String  -- Content-addressed storage

-- =============================================================================
-- Text Proposal Types
-- =============================================================================

||| Proposal status
public export
data ProposalStatus
  = Pending       -- Waiting for voting to start
  | Active        -- Voting in progress
  | Accepted      -- Passed quorum and threshold
  | Rejected      -- Did not pass
  | Superseded    -- Replaced by accepted fork

public export
Show ProposalStatus where
  show Pending = "Pending"
  show Active = "Active"
  show Accepted = "Accepted"
  show Rejected = "Rejected"
  show Superseded = "Superseded"

public export
Eq ProposalStatus where
  Pending == Pending = True
  Active == Active = True
  Accepted == Accepted = True
  Rejected == Rejected = True
  Superseded == Superseded = True
  _ == _ = False

||| Text proposal for Inception amendment
public export
record TextProposal where
  constructor MkTextProposal
  proposalId   : Nat
  textHash     : String      -- IPFS hash of proposed text
  proposer     : Integer     -- Address
  parentId     : Nat         -- 0 = root proposal, >0 = fork of parent
  status       : ProposalStatus
  createdAt    : Nat         -- Block timestamp
  votingEnds   : Nat         -- Voting deadline
  totalVotes   : Nat         -- Total vote weight cast

||| Ranked choice vote (up to 3 preferences)
public export
record RankedVote where
  constructor MkRankedVote
  voter       : Integer      -- Voter address
  rank1       : Nat          -- First choice proposal ID
  rank2       : Nat          -- Second choice (0 = none)
  rank3       : Nat          -- Third choice (0 = none)
  weight      : Nat          -- Vote weight

-- =============================================================================
-- Fork Tree
-- =============================================================================

||| Fork relationship
public export
record ForkInfo where
  constructor MkForkInfo
  parentId    : Nat
  childId     : Nat
  forkReason  : String       -- Why this fork was created

-- =============================================================================
-- Inception State
-- =============================================================================

||| Complete Inception governance state
public export
record InceptionState where
  constructor MkInceptionState
  currentSpec    : InceptionSpec
  proposals      : List TextProposal
  votes          : List RankedVote
  forks          : List ForkInfo
  proposalCount  : Nat

||| Initialize empty Inception state
export
initInceptionState : InceptionSpec -> InceptionState
initInceptionState spec = MkInceptionState spec [] [] [] 0

-- =============================================================================
-- Query Functions
-- =============================================================================

||| Find proposal by ID
export
findProposal : InceptionState -> Nat -> Maybe TextProposal
findProposal state pid = find (\p => p.proposalId == pid) state.proposals

||| Get all active proposals
export
getActiveProposals : InceptionState -> List TextProposal
getActiveProposals state = filter (\p => p.status == Active) state.proposals

||| Get forks of a proposal
export
getForksOf : InceptionState -> Nat -> List TextProposal
getForksOf state parentId =
  filter (\p => p.parentId == parentId) state.proposals

||| Check if change kind is auto-adoptable
export
isAutoAdoptable : InceptionSpec -> ChangeKind -> Bool
isAutoAdoptable spec kind = elem kind spec.allowedChangeKinds.allowed

||| Check if proposal violates boundaries
export
violatesBoundary : InceptionSpec -> TextProposal -> Bool
violatesBoundary spec proposal =
  -- In practice, this would parse the proposal text and check against boundaries
  -- For now, return False (no violation)
  False

-- =============================================================================
-- Drift Detection
-- =============================================================================

||| Drift detection result
public export
data DriftVerdict
  = NoDrift
  | MinorDrift String    -- Warning but acceptable
  | MajorDrift String    -- Requires human review
  | BoundaryViolation String  -- Rejected outright

public export
Show DriftVerdict where
  show NoDrift = "NoDrift"
  show (MinorDrift r) = "MinorDrift(" ++ r ++ ")"
  show (MajorDrift r) = "MajorDrift(" ++ r ++ ")"
  show (BoundaryViolation r) = "BoundaryViolation(" ++ r ++ ")"

||| Check proposal text against Inception for drift
||| In practice, this would use NLP/LLM analysis
export
detectDrift : InceptionSpec -> String -> DriftVerdict
detectDrift spec proposalText =
  -- Placeholder: actual implementation would analyze text
  -- against IntentKeywords, NonGoals, and Boundary
  NoDrift

-- =============================================================================
-- Intent Audit Types
-- =============================================================================

||| Auditor verdict on proposal-Inception alignment
public export
data AuditorVerdict
  = Match                      -- Proposal aligns with Inception
  | DriftDetected String       -- Proposal drifts from intent
  | InsufficientEvidence       -- Cannot determine alignment

public export
Show AuditorVerdict where
  show Match = "Match"
  show (DriftDetected r) = "DriftDetected(" ++ r ++ ")"
  show InsufficientEvidence = "InsufficientEvidence"
