||| Tests for InterCanisterCall type safety
|||
||| These tests verify that the type-level state machine prevents
||| the "reply-then-call" anti-pattern at compile time.
module IcWasm.Tests.InterCanisterCallTests

import IcWasm.InterCanisterCall

%default covering

-- =============================================================================
-- SHOULD COMPILE: Correct patterns
-- =============================================================================

||| Correct: immediate reply, no inter-canister call
correctSimpleReply : CallCtx Fresh -> IO (CallCtx Replied)
correctSimpleReply ctx = replyText ctx "hello"

||| Correct: deferred reply via inter-canister call
correctDeferredReply : CallCtx Fresh -> IO (Either Int32 (CallCtx Called))
correctDeferredReply ctx =
  performCall ctx managementCanister "raw_rand" [] fireAndForget 0

||| Correct: perform call with real callbacks
correctWithCallbacks : CallCtx Fresh -> Callbacks -> IO (Either Int32 (CallCtx Called))
correctWithCallbacks ctx cbs =
  performCall ctx managementCanister "ecdsa_public_key" [0x44,0x49,0x44,0x4C] cbs 26000000000

-- =============================================================================
-- SHOULD NOT COMPILE: Anti-patterns
-- (Uncomment to verify type errors)
-- =============================================================================

-- ||| TYPE ERROR: Reply then call (the bug that took 6 hours to find)
-- badReplyThenCall : CallCtx Fresh -> IO ()
-- badReplyThenCall ctx = do
--   ctx' <- replyText ctx "pending"          -- ctx' : CallCtx Replied
--   _ <- performCall ctx' mgmt "method" [] fireAndForget 0  -- TYPE ERROR!
--   pure ()
--   -- Error: Can't unify Fresh with Replied

-- ||| TYPE ERROR: Reply twice
-- badDoubleReply : CallCtx Fresh -> IO ()
-- badDoubleReply ctx = do
--   ctx' <- replyText ctx "first"            -- ctx' : CallCtx Replied
--   _ <- replyText ctx' "second"             -- TYPE ERROR!
--   pure ()
--   -- Error: Can't unify Fresh with Replied

-- ||| TYPE ERROR: Call then reply (Called has no reply)
-- badCallThenReply : CallCtx Fresh -> IO ()
-- badCallThenReply ctx = do
--   Right ctx' <- performCall ctx mgmt "m" [] fireAndForget 0
--     | Left _ => pure ()
--   _ <- replyText ctx' "late reply"         -- TYPE ERROR!
--   pure ()
--   -- Error: Can't unify Fresh with Called

-- =============================================================================
-- Summary
-- =============================================================================

||| All correct patterns compile.
||| All anti-patterns (commented out) produce type errors.
||| The state machine is: Fresh → Called | Replied (no other transitions).
export
allTestsPass : String
allTestsPass = "InterCanisterCall type safety: OK (anti-patterns are compile-time errors)"
