||| Inception Fork Mechanism
|||
||| Fork existing proposals to create alternative versions.
||| Enables deliberative text evolution (TextDAO pattern).
module Subcontract.Standards.ERC7546.Inception.Functions.Fork

import Subcontract.Standards.ERC7546.Inception.Storages.Schema
import Data.List
import Data.String

%default total

-- =============================================================================
-- Fork Types
-- =============================================================================

||| Fork submission parameters
public export
record ForkParams where
  constructor MkForkParams
  parentId      : Nat       -- Proposal to fork from
  textHash      : String    -- IPFS hash of forked text
  forkReason    : String    -- Why creating this fork
  votingPeriod  : Nat       -- Voting duration

||| Fork result
public export
data ForkResult
  = ForkCreated Nat         -- Success: new proposal ID
  | ParentNotFound          -- Parent proposal doesn't exist
  | ParentNotForkable String -- Parent not in forkable state
  | InvalidFork String      -- Fork validation failed

public export
Show ForkResult where
  show (ForkCreated pid) = "ForkCreated(" ++ show pid ++ ")"
  show ParentNotFound = "ParentNotFound"
  show (ParentNotForkable r) = "ParentNotForkable(" ++ r ++ ")"
  show (InvalidFork r) = "InvalidFork(" ++ r ++ ")"

-- =============================================================================
-- Fork Validation
-- =============================================================================

||| Check if proposal can be forked
export
isForkable : TextProposal -> Bool
isForkable p =
  case p.status of
    Pending => True
    Active => True
    _ => False

||| Validate fork parameters
export
validateFork : InceptionState -> ForkParams -> Either String TextProposal
validateFork state params =
  case findProposal state params.parentId of
    Nothing => Left "Parent proposal not found"
    Just parent =>
      if not (isForkable parent)
        then Left "Parent proposal is not forkable"
        else
          if length params.textHash < 46
            then Left "Invalid IPFS hash"
            else
              if length params.forkReason < 10
                then Left "Fork reason too short"
                else Right parent

-- =============================================================================
-- Fork Creation
-- =============================================================================

||| Create a fork of an existing proposal
|||
||| @state     Current Inception state
||| @forker    Address of forker
||| @params    Fork parameters
||| @timestamp Current block timestamp
export
createFork :
  InceptionState ->
  Integer ->
  ForkParams ->
  Nat ->
  (ForkResult, InceptionState)
createFork state forker params timestamp =
  case validateFork state params of
    Left "Parent proposal not found" => (ParentNotFound, state)
    Left err =>
      if isPrefixOf "Parent proposal is not forkable" err
        then (ParentNotForkable err, state)
        else (InvalidFork err, state)
    Right parent =>
      let newId = state.proposalCount + 1
          forkProposal = MkTextProposal
            newId
            params.textHash
            forker
            params.parentId  -- Link to parent
            Pending
            timestamp
            (timestamp + params.votingPeriod)
            0
          forkInfo = MkForkInfo params.parentId newId params.forkReason
          newState = { proposals := forkProposal :: state.proposals
                     , forks := forkInfo :: state.forks
                     , proposalCount := newId
                     } state
      in (ForkCreated newId, newState)

-- =============================================================================
-- Fork Tree Queries
-- =============================================================================

||| Get all forks of a proposal (direct children)
export
getDirectForks : InceptionState -> Nat -> List TextProposal
getDirectForks state parentId =
  filter (\p => p.parentId == parentId) state.proposals

||| Get fork depth (0 = root, 1 = first fork, etc.)
export covering
getForkDepth : InceptionState -> Nat -> Nat
getForkDepth state proposalId =
  case findProposal state proposalId of
    Nothing => 0
    Just p =>
      if p.parentId == 0
        then 0
        else 1 + getForkDepth state p.parentId

||| Get fork lineage (from root to current)
export covering
getForkLineage : InceptionState -> Nat -> List Nat
getForkLineage state proposalId =
  case findProposal state proposalId of
    Nothing => []
    Just p =>
      if p.parentId == 0
        then [proposalId]
        else getForkLineage state p.parentId ++ [proposalId]

||| Get all proposals in fork tree (root + all descendants)
export covering
getForkTree : InceptionState -> Nat -> List TextProposal
getForkTree state rootId =
  case findProposal state rootId of
    Nothing => []
    Just root =>
      let children = getDirectForks state rootId
          descendants = concatMap (\c => getForkTree state c.proposalId) children
      in root :: descendants

-- =============================================================================
-- Fork Competition
-- =============================================================================

||| Get competing proposals (same parent, all active)
export
getCompetingForks : InceptionState -> Nat -> List TextProposal
getCompetingForks state parentId =
  filter (\p => p.status == Active) (getDirectForks state parentId)

||| Check if fork has siblings
export
hasSiblings : InceptionState -> Nat -> Bool
hasSiblings state proposalId =
  case findProposal state proposalId of
    Nothing => False
    Just p =>
      length (getDirectForks state p.parentId) > 1
