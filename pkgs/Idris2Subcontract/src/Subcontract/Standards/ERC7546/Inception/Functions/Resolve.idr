||| Inception Proposal Resolution
|||
||| Tally votes and resolve proposals using RCV elimination.
||| Updates Inception spec when proposal is accepted.
module Subcontract.Standards.ERC7546.Inception.Functions.Resolve

import Subcontract.Standards.ERC7546.Inception.Storages.Schema
import Subcontract.Standards.ERC7546.Inception.Functions.Vote
import Data.List

%default total

-- =============================================================================
-- Resolution Types
-- =============================================================================

||| Resolution result
public export
data ResolveResult
  = Winner Nat              -- Proposal won with majority
  | NoQuorum                -- Quorum not reached
  | Elimination Nat         -- Proposal eliminated, continue RCV
  | Tie (List Nat)          -- Tied proposals
  | StillVoting             -- Voting period not ended

public export
Show ResolveResult where
  show (Winner pid) = "Winner(" ++ show pid ++ ")"
  show NoQuorum = "NoQuorum"
  show (Elimination pid) = "Elimination(" ++ show pid ++ ")"
  show (Tie pids) = "Tie(" ++ show pids ++ ")"
  show StillVoting = "StillVoting"

||| RCV round state
public export
record RCVRound where
  constructor MkRCVRound
  roundNumber   : Nat
  remaining     : List Nat  -- Remaining proposal IDs
  eliminated    : List Nat  -- Eliminated proposal IDs
  voteCounts    : List (Nat, Nat)  -- (proposalId, vote count)

-- =============================================================================
-- Quorum Check
-- =============================================================================

||| Default quorum: 10% of eligible voters
public export
DEFAULT_QUORUM_BPS : Nat
DEFAULT_QUORUM_BPS = 1000  -- 10%

||| Default super majority for Inception changes: 66.7%
public export
SUPER_MAJORITY_BPS : Nat
SUPER_MAJORITY_BPS = 6667  -- 66.67%

||| Check if quorum is reached
export
hasQuorum : InceptionState -> Nat -> Nat -> Bool
hasQuorum state totalVotes eligibleVoters =
  let quorumRequired = (cast eligibleVoters * cast DEFAULT_QUORUM_BPS) `div` 10000
  in cast totalVotes >= the Integer quorumRequired

||| Check if proposal has super majority
export
hasSuperMajority : Nat -> Nat -> Bool
hasSuperMajority proposalVotes totalVotes =
  if totalVotes == 0 then False
  else (cast proposalVotes * 10000) `div` cast totalVotes >= cast SUPER_MAJORITY_BPS

-- =============================================================================
-- RCV Resolution
-- =============================================================================

||| Get vote counts for competing proposals
export
getCompetingVoteCounts :
  InceptionState ->
  List Nat ->     -- Remaining proposals
  List Nat ->     -- Eliminated proposals
  List (Nat, Nat)
getCompetingVoteCounts state remaining eliminated =
  map (\pid => (pid, getEffectiveVotes state.votes eliminated pid)) remaining

||| Find proposal with fewest votes (for elimination)
export
findLowestVotes : List (Nat, Nat) -> Maybe Nat
findLowestVotes [] = Nothing
findLowestVotes counts =
  let sorted = sortBy (\(_, a), (_, b) => compare a b) counts
  in map fst (head' sorted)

||| Run one round of RCV
export
runRCVRound :
  InceptionState ->
  RCVRound ->
  (ResolveResult, RCVRound)
runRCVRound state round =
  case round.remaining of
    [] => (NoQuorum, round)  -- No proposals left
    [winner] => (Winner winner, round)  -- Single proposal remaining
    _ =>
      let counts = getCompetingVoteCounts state round.remaining round.eliminated
          totalVotes = sum (map snd counts)
      in case find (\(_, v) => hasSuperMajority v totalVotes) counts of
           Just (winner, _) => (Winner winner, round)
           Nothing =>
             -- Eliminate lowest
             case findLowestVotes counts of
               Nothing => (NoQuorum, round)
               Just lowest =>
                 let newRemaining = filter (/= lowest) round.remaining
                     newEliminated = lowest :: round.eliminated
                     newRound = { roundNumber := round.roundNumber + 1
                                , remaining := newRemaining
                                , eliminated := newEliminated
                                , voteCounts := counts
                                } round
                 in (Elimination lowest, newRound)

||| Run full RCV resolution until winner or completion
export covering
runFullRCV :
  InceptionState ->
  List Nat ->     -- Competing proposal IDs
  Nat ->          -- Max rounds (safety limit)
  ResolveResult
runFullRCV state proposals maxRounds =
  let initialRound = MkRCVRound 0 proposals [] []
  in runRounds state initialRound maxRounds
  where
    runRounds : InceptionState -> RCVRound -> Nat -> ResolveResult
    runRounds st rd 0 = Tie rd.remaining
    runRounds st rd fuel =
      case runRCVRound st rd of
        (Winner w, _) => Winner w
        (NoQuorum, _) => NoQuorum
        (Tie ts, _) => Tie ts
        (Elimination _, newRd) => runRounds st newRd (minus fuel 1)
        (StillVoting, _) => StillVoting

-- =============================================================================
-- Proposal Resolution
-- =============================================================================

||| Resolve a proposal (or fork competition)
|||
||| @state     Current Inception state
||| @proposalId  Proposal to resolve (or root of fork tree)
||| @timestamp Current block timestamp
export covering
resolveProposal :
  InceptionState ->
  Nat ->
  Nat ->
  (ResolveResult, InceptionState)
resolveProposal state proposalId timestamp =
  case findProposal state proposalId of
    Nothing => (NoQuorum, state)
    Just proposal =>
      -- Check if voting has ended
      if timestamp < proposal.votingEnds
        then (StillVoting, state)
        else
          -- Get all competing proposals (proposal + siblings)
          let competing = if proposal.parentId == 0
                            then [proposalId]
                            else map (\p => p.proposalId)
                                     (getForksOf state proposal.parentId ++ [proposal])
              result = runFullRCV state competing 10
              newState = updateProposalStatuses state result
          in (result, newState)
  where
    updateProposalStatuses : InceptionState -> ResolveResult -> InceptionState
    updateProposalStatuses st (Winner winnerId) =
      let updateStatus : TextProposal -> TextProposal
          updateStatus p =
            if p.proposalId == winnerId
              then { status := Accepted } p
              else if elem p.proposalId (map (\q => q.proposalId)
                          (getForksOf st p.parentId))
              then { status := Rejected } p
              else p
      in { proposals := map updateStatus st.proposals } st
    updateProposalStatuses st _ = st

-- =============================================================================
-- Inception Update
-- =============================================================================

||| Apply accepted proposal to update Inception spec
|||
||| @state     Current Inception state
||| @winnerId  Accepted proposal ID
||| @newSpec   New Inception spec from proposal
export
applyInceptionUpdate :
  InceptionState ->
  Nat ->
  InceptionSpec ->
  Either String InceptionState
applyInceptionUpdate state winnerId newSpec =
  case findProposal state winnerId of
    Nothing => Left "Proposal not found"
    Just proposal =>
      if proposal.status /= Accepted
        then Left "Proposal not accepted"
        else
          let updatedSpec : InceptionSpec
              updatedSpec = { version := state.currentSpec.version + 1 } newSpec
          in Right $ { currentSpec := updatedSpec } state

-- =============================================================================
-- Query Functions
-- =============================================================================

||| Get current resolution status for proposal
export covering
getResolutionStatus : InceptionState -> Nat -> Nat -> ResolveResult
getResolutionStatus state proposalId timestamp =
  case findProposal state proposalId of
    Nothing => NoQuorum
    Just proposal =>
      if timestamp < proposal.votingEnds
        then StillVoting
        else
          case proposal.status of
            Accepted => Winner proposalId
            Rejected => Elimination proposalId
            _ =>
              let competing = if proposal.parentId == 0
                                then [proposalId]
                                else map (\p => p.proposalId)
                                         (getForksOf state proposal.parentId)
              in runFullRCV state competing 10

||| Get all resolved proposals
export
getResolvedProposals : InceptionState -> List TextProposal
getResolvedProposals state =
  filter (\p => p.status == Accepted || p.status == Rejected) state.proposals
