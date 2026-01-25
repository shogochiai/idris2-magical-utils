||| Inception Ranked Choice Voting
|||
||| RCV voting for text proposals.
||| Voters rank preferences; votes transfer if first choice eliminated.
module Subcontract.Standards.ERC7546.Inception.Functions.Vote

import Subcontract.Standards.ERC7546.Inception.Storages.Schema
import Data.List

%default total

-- =============================================================================
-- Vote Types
-- =============================================================================

||| Vote submission parameters
public export
record VoteParams where
  constructor MkVoteParams
  rank1   : Nat     -- First choice proposal ID
  rank2   : Nat     -- Second choice (0 = none)
  rank3   : Nat     -- Third choice (0 = none)

||| Vote result
public export
data VoteResult
  = VoteCast Nat            -- Success: vote weight recorded
  | AlreadyVoted            -- Voter already cast vote
  | InvalidProposal String  -- Proposal not votable
  | VotingClosed            -- Voting period ended
  | InvalidRanking String   -- Invalid preference ranking

public export
Show VoteResult where
  show (VoteCast w) = "VoteCast(" ++ show w ++ ")"
  show AlreadyVoted = "AlreadyVoted"
  show (InvalidProposal r) = "InvalidProposal(" ++ r ++ ")"
  show VotingClosed = "VotingClosed"
  show (InvalidRanking r) = "InvalidRanking(" ++ r ++ ")"

-- =============================================================================
-- Vote Validation
-- =============================================================================

||| Check if voter has already voted on any proposal in set
export
hasVoted : InceptionState -> Integer -> List Nat -> Bool
hasVoted state voter proposalIds =
  any (\v => v.voter == voter && elem v.rank1 proposalIds) state.votes

||| Validate ranking (no duplicates, valid proposal IDs)
export
validateRanking : InceptionState -> VoteParams -> Either String ()
validateRanking state params =
  let ranks = filter (/= 0) [params.rank1, params.rank2, params.rank3]
      uniqueRanks = nub ranks
  in if length ranks /= length uniqueRanks
       then Left "Duplicate proposals in ranking"
       else
         -- Check all ranked proposals exist and are active
         if not (all (isActiveProposal state) ranks)
           then Left "Ranked proposal not active"
           else Right ()
  where
    isActiveProposal : InceptionState -> Nat -> Bool
    isActiveProposal st pid =
      case findProposal st pid of
        Nothing => False
        Just p => p.status == Active

||| Check if voting is still open
export
isVotingOpen : TextProposal -> Nat -> Bool
isVotingOpen proposal currentTime =
  proposal.status == Active && currentTime < proposal.votingEnds

-- =============================================================================
-- Vote Casting
-- =============================================================================

||| Calculate vote weight for voter
||| In practice, this would query token balance or staking
export
getVoteWeight : Integer -> Nat
getVoteWeight voter = 1  -- Placeholder: 1 vote per address

||| Cast a ranked choice vote
|||
||| @state     Current Inception state
||| @voter     Address of voter
||| @params    Vote parameters (ranked choices)
||| @timestamp Current block timestamp
export
castVote :
  InceptionState ->
  Integer ->
  VoteParams ->
  Nat ->
  (VoteResult, InceptionState)
castVote state voter params timestamp =
  -- Check first choice proposal exists
  case findProposal state params.rank1 of
    Nothing => (InvalidProposal "First choice not found", state)
    Just proposal =>
      -- Check voting is open
      if not (isVotingOpen proposal timestamp)
        then (VotingClosed, state)
        else
          -- Get all proposals in this voting round (siblings)
          let siblings = if proposal.parentId == 0
                           then [params.rank1]
                           else map (\p => p.proposalId)
                                    (getForksOf state proposal.parentId)
          in
          -- Check if already voted
          if hasVoted state voter siblings
            then (AlreadyVoted, state)
            else
              case validateRanking state params of
                Left err => (InvalidRanking err, state)
                Right () =>
                  let weight = getVoteWeight voter
                      vote = MkRankedVote voter params.rank1 params.rank2 params.rank3 weight
                      -- Update vote counts on ranked proposals
                      newProposals = updateVoteCounts state.proposals params weight
                      newState = { votes := vote :: state.votes
                                 , proposals := newProposals
                                 } state
                  in (VoteCast weight, newState)
  where
    updateVoteCounts : List TextProposal -> VoteParams -> Nat -> List TextProposal
    updateVoteCounts proposals vp w =
      map (\p =>
        if p.proposalId == vp.rank1
          then { totalVotes := p.totalVotes + w } p
          else p
      ) proposals

-- =============================================================================
-- Vote Queries
-- =============================================================================

||| Get votes for a proposal
export
getVotesFor : InceptionState -> Nat -> List RankedVote
getVotesFor state proposalId =
  filter (\v => v.rank1 == proposalId) state.votes

||| Get total vote weight for a proposal (first choice only)
export
getFirstChoiceWeight : InceptionState -> Nat -> Nat
getFirstChoiceWeight state proposalId =
  sum $ map (\v => v.weight) (getVotesFor state proposalId)

||| Get voter's vote
export
getVoterVote : InceptionState -> Integer -> Maybe RankedVote
getVoterVote state voter =
  find (\v => v.voter == voter) state.votes

-- =============================================================================
-- RCV Transfer Logic
-- =============================================================================

||| Transfer votes from eliminated proposal to next preference
|||
||| @state       Current state
||| @eliminated  Proposal ID being eliminated
||| Returns updated votes with transferred preferences
export
transferVotes : InceptionState -> Nat -> List RankedVote
transferVotes state eliminated =
  map transferVote state.votes
  where
    transferVote : RankedVote -> RankedVote
    transferVote v =
      if v.rank1 == eliminated
        then { rank1 := v.rank2, rank2 := v.rank3, rank3 := 0 } v
        else v

||| Get effective vote count after elimination
||| Counts first-choice votes, accounting for eliminations
export
getEffectiveVotes : List RankedVote -> List Nat -> Nat -> Nat
getEffectiveVotes votes eliminatedIds proposalId =
  let validVotes = filter (isValidVoteFor proposalId eliminatedIds) votes
  in sum $ map (\v => v.weight) validVotes
  where
    isValidVoteFor : Nat -> List Nat -> RankedVote -> Bool
    isValidVoteFor pid elims v =
      -- Find first non-eliminated choice
      let firstValid = if not (elem v.rank1 elims) then v.rank1
                       else if not (elem v.rank2 elims) then v.rank2
                       else if not (elem v.rank3 elims) then v.rank3
                       else 0
      in firstValid == pid
