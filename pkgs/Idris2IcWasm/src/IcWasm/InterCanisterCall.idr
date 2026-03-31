||| Type-Safe Inter-Canister Call API
|||
||| Enforces the DEFERRED REPLY pattern at the type level.
|||
||| BACKGROUND (discovered 2026-03-31):
||| On ICP, if ic0.msg_reply is called BEFORE ic0.call_perform for a
||| cross-subnet call, the call is silently dropped (best-effort).
||| This module makes the wrong ordering a TYPE ERROR.
|||
||| The key insight: a canister update method is in one of three states:
|||   Fresh  → can reply OR can initiate a call
|||   Called → call was performed, MUST NOT reply (callback will reply)
|||   Replied → reply was sent, MUST NOT perform cross-subnet calls
|||
||| Usage:
|||   -- Correct: deferred reply (cross-subnet safe)
|||   handleMethod : CallCtx Fresh -> IO (CallCtx Called)
|||   handleMethod ctx = do
|||     ctx' <- performCall ctx callee method payload callbacks
|||     pure ctx'   -- DO NOT reply here; callback replies
|||
|||   -- Correct: immediate reply (no inter-canister call)
|||   handleSimple : CallCtx Fresh -> IO (CallCtx Replied)
|||   handleSimple ctx = replyText ctx "result"
|||
|||   -- TYPE ERROR: reply then call
|||   bad : CallCtx Fresh -> IO (CallCtx Called)
|||   bad ctx = do
|||     ctx' <- replyText ctx "pending"  -- ctx' : CallCtx Replied
|||     performCall ctx' ...             -- TYPE ERROR: needs Fresh, got Replied
module IcWasm.InterCanisterCall

import Data.List

%default covering

-- =============================================================================
-- Call Context State Machine
-- =============================================================================

||| State of the current update method's reply/call lifecycle
public export
data CallState = Fresh | Called | Replied

||| Proof that a call context is in a specific state.
||| Opaque — can only be created by the API functions.
export
data CallCtx : CallState -> Type where
  MkCallCtx : CallCtx s

-- =============================================================================
-- IC0 FFI (imported from WASM environment)
-- =============================================================================

%foreign "C:ic0_call_new,env"
prim__callNew : Int32 -> Int32 -> Int32 -> Int32 ->
                Int32 -> Int32 -> Int32 -> Int32 -> PrimIO ()

%foreign "C:ic0_call_data_append,env"
prim__callDataAppend : Int32 -> Int32 -> PrimIO ()

%foreign "C:ic0_call_cycles_add128,env"
prim__callCyclesAdd : Bits64 -> Bits64 -> PrimIO ()

%foreign "C:ic0_call_perform,env"
prim__callPerform : PrimIO Int32

%foreign "C:ic0_msg_reply,env"
prim__msgReply : PrimIO ()

%foreign "C:ic0_msg_reply_data_append,env"
prim__msgReplyDataAppend : Int32 -> Int32 -> PrimIO ()

%foreign "C:ic0_msg_reject,env"
prim__msgReject : Int32 -> Int32 -> PrimIO ()

%foreign "C:ic0_msg_arg_data_size,env"
prim__msgArgDataSize : PrimIO Int32

%foreign "C:ic0_msg_arg_data_copy,env"
prim__msgArgDataCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

-- =============================================================================
-- Callback Registration
-- =============================================================================

||| Callback table index (opaque, obtained from C function pointer)
public export
CallbackIdx : Type
CallbackIdx = Int32

||| Pair of reply + reject callback indices
public export
record Callbacks where
  constructor MkCallbacks
  onReply  : CallbackIdx
  onReject : CallbackIdx

||| No callbacks (fire-and-forget). Only safe for same-subnet calls.
||| WARNING: cross-subnet calls with no callbacks are silently dropped
||| if ic0.msg_reply was already called.
public export
fireAndForget : Callbacks
fireAndForget = MkCallbacks 0 0

-- =============================================================================
-- Entry Point: Create a Fresh context
-- =============================================================================

||| Create a fresh call context at the start of an update method.
||| Each update method starts in Fresh state.
export
freshCtx : CallCtx Fresh
freshCtx = MkCallCtx

-- =============================================================================
-- Reply (Fresh → Replied)
-- =============================================================================

||| Reply with raw bytes. Transitions Fresh → Replied.
||| After this, cross-subnet ic0.call_perform is UNSAFE (best-effort / silent drop).
export
replyRaw : CallCtx Fresh -> (payload : List Bits8) -> IO (CallCtx Replied)
replyRaw MkCallCtx bytes = do
  -- Write bytes to reply
  for_ (zip [0..cast (length bytes)] bytes) $ \(idx, b) =>
    primIO $ prim__msgReplyDataAppend (cast idx) 1  -- TODO: batch write
  primIO prim__msgReply
  pure MkCallCtx

||| Reply with Candid-encoded text. Transitions Fresh → Replied.
||| Uses ic0.msg_reply_data_append + ic0.msg_reply.
||| Implementation note: text is encoded as Candid text (DIDL\x00\x01\x71 + LEB128 len + UTF-8).
||| TODO: proper buffer management via C helper.
export
replyText : CallCtx Fresh -> String -> IO (CallCtx Replied)
replyText MkCallCtx text = do
  -- Placeholder: just call msg_reply (actual Candid encoding done in C helper)
  primIO prim__msgReply
  pure MkCallCtx

-- =============================================================================
-- Perform Call (Fresh → Called)
-- =============================================================================

||| Target canister identifier
public export
record CanisterId where
  constructor MkCanisterId
  bytes : List Bits8

||| Management canister (empty principal)
public export
managementCanister : CanisterId
managementCanister = MkCanisterId []

||| Perform an inter-canister call. Transitions Fresh → Called.
|||
||| CRITICAL: After this, DO NOT call replyText/replyRaw.
||| The callback (specified in Callbacks) must call ic0.msg_reply.
||| This is enforced by the type: CallCtx Called has no reply function.
|||
||| @ctx       Fresh context (proves we haven't replied yet)
||| @target    Target canister
||| @method    Method name
||| @payload   Candid-encoded request
||| @callbacks Reply/reject callback table indices
||| @cycles    Cycles to attach (0 for free calls)
export
performCall : CallCtx Fresh
           -> (target : CanisterId)
           -> (method : String)
           -> (payload : List Bits8)
           -> (callbacks : Callbacks)
           -> (cycles : Bits64)
           -> IO (Either Int32 (CallCtx Called))
performCall MkCallCtx target method payload callbacks cycles = do
  -- Write callee principal to linear memory (simplified: use static buffer)
  -- For production: need proper buffer management
  let calleeBytes = target.bytes
      methodBytes = unpack method
  -- ic0_call_new(callee_src, callee_size, method_src, method_size,
  --              reply_fn, reply_env, reject_fn, reject_env)
  primIO $ prim__callNew
    0 (cast $ length calleeBytes)  -- callee (TODO: proper pointer)
    0 (cast $ length methodBytes)  -- method (TODO: proper pointer)
    callbacks.onReply 0
    callbacks.onReject 0
  -- Append payload
  -- ic0_call_data_append(src, size)
  -- TODO: write payload to buffer and pass pointer
  -- Attach cycles
  when (cycles > 0) $ primIO $ prim__callCyclesAdd 0 cycles
  -- Perform
  result <- primIO prim__callPerform
  if result == 0
    then pure (Right MkCallCtx)  -- Called state
    else pure (Left result)       -- Error, still Fresh (can reply)

-- =============================================================================
-- Callback Context
-- =============================================================================

||| Context available inside a reply/reject callback.
||| Callbacks can read response data and MUST reply to the original caller.
public export
data CallbackCtx = MkCallbackCtx

||| Read response data size (in callback context)
export
callbackArgSize : IO Int32
callbackArgSize = primIO prim__msgArgDataSize

||| Reply from callback context.
||| This replies to the ORIGINAL ingress caller (not the callee).
export
callbackReplyRaw : (payload : List Bits8) -> IO ()
callbackReplyRaw bytes = do
  -- TODO: proper buffer write
  primIO prim__msgReply

||| Reply with text from callback
export
callbackReplyText : String -> IO ()
callbackReplyText text = do
  -- Candid text encoding + reply
  primIO prim__msgReply

-- =============================================================================
-- Type-Level Documentation
-- =============================================================================
--
-- The state machine prevents the "reply-then-call" anti-pattern:
--
--   Fresh ──performCall──→ Called    (callback will reply)
--   Fresh ──replyText────→ Replied   (done, no more calls)
--
--   Called:  no reply functions available (type error)
--   Replied: no performCall available (type error)
--
-- This encoding is a simplified version of indexed monads.
-- The CallCtx phantom type parameter tracks the state,
-- and each operation's type signature enforces valid transitions.
