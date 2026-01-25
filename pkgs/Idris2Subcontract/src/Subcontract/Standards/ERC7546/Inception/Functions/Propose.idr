||| Inception Proposal Submission
|||
||| Submit new text proposals for Inception amendment.
||| Proposals are stored on IPFS, only hash is on-chain.
module Subcontract.Standards.ERC7546.Inception.Functions.Propose

import Subcontract.Standards.ERC7546.Inception.Storages.Schema

%default total

-- =============================================================================
-- Proposal Submission
-- =============================================================================

||| Proposal submission parameters
public export
record ProposeParams where
  constructor MkProposeParams
  textHash      : String    -- IPFS hash of proposed Inception text
  votingPeriod  : Nat       -- Voting duration in seconds
  changeKind    : ChangeKind -- Type of change being proposed

||| Proposal submission result
public export
data ProposeResult
  = ProposalCreated Nat     -- Success: new proposal ID
  | Unauthorized String     -- Proposer not authorized
  | InvalidText String      -- Text validation failed
  | BoundaryViolated String -- Proposal violates Inception boundary

public export
Show ProposeResult where
  show (ProposalCreated pid) = "ProposalCreated(" ++ show pid ++ ")"
  show (Unauthorized r) = "Unauthorized(" ++ r ++ ")"
  show (InvalidText r) = "InvalidText(" ++ r ++ ")"
  show (BoundaryViolated r) = "BoundaryViolated(" ++ r ++ ")"

-- =============================================================================
-- Validation
-- =============================================================================

||| Validate proposal against current Inception
export
validateProposal : InceptionSpec -> ProposeParams -> Either String ()
validateProposal spec params =
  -- Check if text hash is valid format
  if length params.textHash < 46
    then Left "Invalid IPFS hash format"
    else
      -- Check voting period minimum
      if params.votingPeriod < 86400  -- 1 day minimum
        then Left "Voting period too short"
        else Right ()

||| Check if proposer is authorized
export
isAuthorizedProposer : InceptionState -> Integer -> Bool
isAuthorizedProposer state proposer =
  -- In practice, check against authorized proposers list
  -- For now, anyone can propose
  True

-- =============================================================================
-- Submission Logic
-- =============================================================================

||| Submit a new text proposal
|||
||| @state     Current Inception state
||| @proposer  Address of proposer
||| @params    Proposal parameters
||| @timestamp Current block timestamp
export
submitProposal :
  InceptionState ->
  Integer ->
  ProposeParams ->
  Nat ->
  (ProposeResult, InceptionState)
submitProposal state proposer params timestamp =
  -- Check authorization
  if not (isAuthorizedProposer state proposer)
    then (Unauthorized "Proposer not in authorized list", state)
    else
      case validateProposal state.currentSpec params of
        Left err => (InvalidText err, state)
        Right () =>
          -- Check for boundary violations
          case detectDrift state.currentSpec params.textHash of
            BoundaryViolation reason =>
              (BoundaryViolated reason, state)
            _ =>
              -- Create new proposal
              let newId = state.proposalCount + 1
                  proposal = MkTextProposal
                    newId
                    params.textHash
                    proposer
                    0  -- Root proposal (not a fork)
                    Pending
                    timestamp
                    (timestamp + params.votingPeriod)
                    0
                  newState = { proposals := proposal :: state.proposals
                             , proposalCount := newId
                             } state
              in (ProposalCreated newId, newState)

||| Activate a pending proposal (start voting)
export
activateProposal :
  InceptionState ->
  Nat ->
  Either String InceptionState
activateProposal state proposalId =
  case findProposal state proposalId of
    Nothing => Left "Proposal not found"
    Just p =>
      if p.status /= Pending
        then Left "Proposal not in Pending status"
        else
          let updated = { status := Active } p
              newProposals = map (\q => if q.proposalId == proposalId then updated else q)
                                 state.proposals
          in Right $ { proposals := newProposals } state

-- =============================================================================
-- Query Functions
-- =============================================================================

||| Get pending proposals
export
getPendingProposals : InceptionState -> List TextProposal
getPendingProposals state =
  filter (\p => p.status == Pending) state.proposals

||| Get proposal by ID
export
getProposal : InceptionState -> Nat -> Maybe TextProposal
getProposal = findProposal
