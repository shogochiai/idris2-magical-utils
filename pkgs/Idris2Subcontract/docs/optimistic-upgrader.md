# OptimisticUpgrader (OU) — SPEC-Test Parity

The OptimisticUpgrader implements an n-of-n multisig admin for the ERC-7546
Dictionary. This document is the doc layer of the OU spec-test parity chain:
every `[[spec]]` id declared in
`src/Subcontract/Standards/ERC7546/OptimisticUpgrader/SPEC.toml` is cited here.

## Proposal Creation

- **OU_PROP_001** — Only proposer can create proposals (`createProposal` reverts if caller != proposer).
- **OU_PROP_002** — Proposal requires auditors (`createProposal` reverts if auditorCount == 0).
- **OU_PROP_003** — Threshold equals auditor count (n-of-n) (new proposal threshold == getAuditorCount()).
- **OU_PROP_004** — Proposal starts with zero votes (voteCount == 0 and executed == 0).

## Vote Casting

- **OU_VOTE_001** — Only auditors can vote (`castVote` reverts if caller not in auditor list).
- **OU_VOTE_002** — Double voting prevented (`castVote` reverts if caller already has decision != 0).
- **OU_VOTE_003** — Expired voting rejected (`castVote` reverts if block.timestamp > deadline).
- **OU_VOTE_004** — Executed proposal cannot receive votes (`castVote` reverts if executed == 1).
- **OU_VOTE_005** — Approval increments vote count (`castVote` with decision==1 increments voteCount by 1).
- **OU_VOTE_006** — VoteCast event emitted (`castVote` emits VoteCast(proposalId, auditor, decision)).

## Proposer Signature

- **OU_SIG_001** — Only proposer can submit signature (`submitProposerSignature` reverts if caller != proposer).
- **OU_SIG_002** — Signature submitted once (`submitProposerSignature` reverts if proposerSig != 0).
- **OU_SIG_003** — Executed proposal cannot receive signature (`submitProposerSignature` reverts if executed == 1).

## Automatic Execution

- **OU_EXEC_001** — Execute when threshold met with proposer sig (`tryExecuteUpgrade` calls executeUpgrade iff voteCount >= threshold AND proposerSig != 0).
- **OU_EXEC_002** — Reentrancy protection via executed flag (`executeUpgrade` sets executed = 1 before external call).
- **OU_EXEC_003** — Dictionary.setImplementation called (`executeUpgrade` calls dictionary.setImplementation(selector, newImpl)).
- **OU_EXEC_004** — UpgradeExecuted event on success (`executeUpgrade` emits UpgradeExecuted(proposalId, targetProxy, newImpl)).
- **OU_EXEC_005** — Revert on Dictionary call failure (`executeUpgrade` reverts if Dictionary call returns 0).

## View Functions

- **OU_VIEW_001** — getVotingStatus returns correct values ((currentVotes, threshold, isComplete)).
- **OU_VIEW_002** — isComplete true iff threshold met with proposer sig (isComplete == 1 iff voteCount >= threshold AND proposerSig != 0 AND executed == 0).

## Access Control

- **OU_ACL_001** — isAuditor iterates auditor list (isAuditor returns true iff addr in auditors[0..auditorCount]).
- **OU_ACL_002** — Proposer is single address (only address at SLOT_PROPOSER can create proposals/submit sig).

## Test Harness

The test harness distinguishes *executed* assertions from *declared but not yet
verified* entries. Every `TestDef` carries a `TestBody` of `Runs (IO Bool)` or
`Pending String`. The runner tallies passed / failed / pending separately and
exits non-zero only when a test actually fails; pending entries never fail the
run but are always reported prominently.

- **OU_TEST_001** — Pending entries are not counted as passed or failed (a `Pending` body contributes to the pending count only).
- **OU_TEST_002** — Runs body returning False increments failed (a `Runs` body that returns False is tallied as failed).
- **OU_TEST_003** — Pending-only run exits 0 (the runner exits non-zero only when failed > 0; pending never fails the run).
