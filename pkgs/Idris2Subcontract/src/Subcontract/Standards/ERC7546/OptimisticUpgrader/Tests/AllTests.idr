||| OptimisticUpgrader Tests
|||
||| SPEC-Test Parity for OptimisticUpgrader module.
|||
||| Every [[spec]] entry in SPEC.toml is declared here as a `TestDef`.
||| A `TestDef` either runs real assertions (`Runs`) or is explicitly
||| pending (`Pending`) with a documented reason — a declared spec must
||| never be silently counted as verified. The runner tallies three
||| numbers (passed / failed / pending) and only exits non-zero when a
||| test actually fails; pending entries never fail the run, but a
||| non-zero pending count is always reported prominently.
module Subcontract.Standards.ERC7546.OptimisticUpgrader.Tests.AllTests

import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Slots
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Schema
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Functions.Core

import System
import System.Coverage

%default covering

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

||| Body of a test definition: executable assertions or a pending marker.
public export
data TestBody
  = Runs (IO Bool)
  | Pending String

||| A named test definition that either runs assertions or is pending.
||| The `specId` is the exact REQ id from SPEC.toml.
public export
record TestDef where
  constructor MkTest
  specId      : String
  description : String
  body        : TestBody

||| Construct a test definition with a body.
public export
test : String -> String -> TestBody -> TestDef
test sid desc body = MkTest sid desc body

||| Outcome of running a single test definition.
public export
data Verdict
  = VerdictPass
  | VerdictFail
  | VerdictPending

||| Run a test body, producing a verdict.
||| `Runs` executes the assertions; `Pending` never runs and never fails.
public export
verdictOf : TestBody -> IO Verdict
verdictOf (Pending _) = pure VerdictPending
verdictOf (Runs io) = do
  ok <- io
  pure (if ok then VerdictPass else VerdictFail)

||| Aggregate verdicts into (passed, failed, pending) counts.
export
tally : List Verdict -> (Nat, Nat, Nat)
tally = go 0 0 0
  where
    go : Nat -> Nat -> Nat -> List Verdict -> (Nat, Nat, Nat)
    go p f q [] = (p, f, q)
    go p f q (VerdictPass :: rest) = go (S p) f q rest
    go p f q (VerdictFail :: rest) = go p (S f) q rest
    go p f q (VerdictPending :: rest) = go p f (S q) rest

||| Count pending entries in a list of test definitions.
||| Pending entries are separate from the runnable pool: they are never
||| counted as passed nor as failed.
export
countPending : List TestDef -> Nat
countPending [] = 0
countPending (MkTest _ _ (Pending _) :: rest) = S (countPending rest)
countPending (MkTest _ _ (Runs _) :: rest) = countPending rest

||| The run must exit non-zero iff at least one test failed.
export
shouldExitNonZero : Nat -> Bool
shouldExitNonZero failed = failed > 0

-- =============================================================================
-- Test Harness Tests (Runs bodies)
-- =============================================================================

||| OU_TEST_001: a Pending entry is never counted as passed or failed;
||| only the runnable pool is eligible for pass/fail.
export
test_OU_TEST_001_pending_not_counted : IO Bool
test_OU_TEST_001_pending_not_counted = do
  let (passed, failed, pending) = tally [VerdictPending, VerdictPending, VerdictPending]
  let defs = [ MkTest "a" "" (Pending "not implemented")
             , MkTest "b" "" (Runs (pure True))
             ]
  pure (passed == 0
        && failed == 0
        && pending == 3
        && countPending defs == 1
        && length defs == 2)

||| OU_TEST_002: a Runs body that returns False tallies as failed.
export
test_OU_TEST_002_runs_false_increments_failed : IO Bool
test_OU_TEST_002_runs_false_increments_failed = do
  v <- verdictOf (Runs (pure False))
  let (passed, failed, _) = tally [v]
  pure $ case v of
    VerdictFail => failed == 1 && passed == 0
    _ => False

||| OU_TEST_003: a run with only pending entries never fails — exit is 0
||| because failed == 0.
export
test_OU_TEST_003_pending_only_exits_zero : IO Bool
test_OU_TEST_003_pending_only_exits_zero = do
  let (passed, failed, pending) = tally [VerdictPending, VerdictPending]
  pure (pending == 2
        && failed == 0
        && passed == 0
        && not (shouldExitNonZero failed))

-- =============================================================================
-- Test Definitions (SPEC-Test Parity)
-- =============================================================================

public export
allTests : List TestDef
allTests = [
  -- Proposal Creation
  test "OU_PROP_001" "Only proposer can create proposals" (Pending "body not implemented"),
  test "OU_PROP_002" "Proposal requires auditors" (Pending "body not implemented"),
  test "OU_PROP_003" "Threshold equals auditor count (n-of-n)" (Pending "body not implemented"),
  test "OU_PROP_004" "Proposal starts with zero votes" (Pending "body not implemented"),

  -- Vote Casting
  test "OU_VOTE_001" "Only auditors can vote" (Pending "body not implemented"),
  test "OU_VOTE_002" "Double voting prevented" (Pending "body not implemented"),
  test "OU_VOTE_003" "Expired voting rejected" (Pending "body not implemented"),
  test "OU_VOTE_004" "Executed proposal cannot receive votes" (Pending "body not implemented"),
  test "OU_VOTE_005" "Approval increments vote count" (Pending "body not implemented"),
  test "OU_VOTE_006" "VoteCast event emitted" (Pending "body not implemented"),

  -- Proposer Signature
  test "OU_SIG_001" "Only proposer can submit signature" (Pending "body not implemented"),
  test "OU_SIG_002" "Signature submitted once" (Pending "body not implemented"),
  test "OU_SIG_003" "Executed proposal cannot receive signature" (Pending "body not implemented"),

  -- Automatic Execution
  test "OU_EXEC_001" "Execute when threshold met with proposer sig" (Pending "body not implemented"),
  test "OU_EXEC_002" "Reentrancy protection via executed flag" (Pending "body not implemented"),
  test "OU_EXEC_003" "Dictionary.setImplementation called" (Pending "body not implemented"),
  test "OU_EXEC_004" "UpgradeExecuted event on success" (Pending "body not implemented"),
  test "OU_EXEC_005" "Revert on Dictionary call failure" (Pending "body not implemented"),

  -- View Functions
  test "OU_VIEW_001" "getVotingStatus returns correct values" (Pending "body not implemented"),
  test "OU_VIEW_002" "isComplete true iff threshold met with proposer sig" (Pending "body not implemented"),

  -- Access Control
  test "OU_ACL_001" "isAuditor iterates auditor list" (Pending "body not implemented"),
  test "OU_ACL_002" "Proposer is single address" (Pending "body not implemented"),

  -- Test Harness
  test "OU_TEST_001" "Pending entries are not counted as passed or failed" (Runs test_OU_TEST_001_pending_not_counted),
  test "OU_TEST_002" "Runs body returning False increments failed" (Runs test_OU_TEST_002_runs_false_increments_failed),
  test "OU_TEST_003" "Pending-only run exits 0" (Runs test_OU_TEST_003_pending_only_exits_zero)
]

-- =============================================================================
-- Test Runner
-- =============================================================================

||| Short tag for a verdict, used in result lines.
export
showVerdict : Verdict -> String
showVerdict VerdictPass = "PASS"
showVerdict VerdictFail = "FAIL"
showVerdict VerdictPending = "PENDING"

||| Run one test definition, producing its verdict and a result line.
||| The coverage label is set to `test_<specId>` immediately before running
||| a `Runs` body so that path hits are attributed per SpecId.
runOne : TestDef -> IO (Verdict, String)
runOne (MkTest sid desc (Pending reason)) =
  pure (VerdictPending, "[PENDING] " ++ sid ++ " " ++ desc ++ " (" ++ reason ++ ")")
runOne (MkTest sid desc (Runs io)) = do
  enterTest ("test_" ++ sid)
  ok <- io
  let v = if ok then VerdictPass else VerdictFail
  pure (v, "[" ++ showVerdict v ++ "] " ++ sid ++ " " ++ desc)

||| Run all tests, counting passed / failed / pending.
|||
||| Prints one `[PASS]` / `[FAIL]` / `[PENDING]` line per row, then a
||| `Results: P passed, F failed, N pending` summary. Exits non-zero only
||| when at least one test failed; pending never fails the run. A non-zero
||| pending count is always reported prominently.
export
runAllTests : IO ()
runAllTests = do
  results <- traverse runOne allTests
  let (passed, failed, pending) = tally (map fst results)
  _ <- traverse (putStrLn . snd) results
  if pending > 0
    then putStrLn ("[WARNING] " ++ show pending ++ " pending test(s): declared but not yet verified")
    else pure ()
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ " passed, "
          ++ show failed ++ " failed, "
          ++ show pending ++ " pending"
  if failed > 0 then exitFailure else exitSuccess

export
main : IO ()
main = runAllTests
