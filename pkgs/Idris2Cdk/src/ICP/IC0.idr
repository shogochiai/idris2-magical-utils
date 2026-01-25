||| ICP IC0 System API - FFI Boundary Layer
|||
||| This module contains the raw system calls to the IC0 API.
||| All primIO and FFI operations are confined here.
||| Higher layers (ICP.API) should use these primitives through safe wrappers.
|||
||| Reference: https://internetcomputer.org/docs/current/references/ic-interface-spec#system-api
module ICP.IC0

import Data.Buffer

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Principal ID as raw bytes (max 29 bytes)
public export
record Principal where
  constructor MkPrincipal
  bytes : List Bits8

||| Canister ID (alias for Principal)
public export
CanisterId : Type
CanisterId = Principal

||| Cycle amount (128-bit represented as two 64-bit values)
public export
record Cycles where
  constructor MkCycles
  high : Bits64
  low  : Bits64

||| Timestamp in nanoseconds since Unix epoch
public export
Timestamp : Type
Timestamp = Bits64

||| Certification (for certified variables)
public export
record Certification where
  constructor MkCertification
  certificate : List Bits8
  tree        : List Bits8

--------------------------------------------------------------------------------
-- Message Information (ic0.msg_*)
--------------------------------------------------------------------------------

||| Get size of argument data in bytes
||| ic0.msg_arg_data_size : () -> i32
export
%foreign "C:ic0_msg_arg_data_size,libic0"
prim__msgArgDataSize : PrimIO Int32

||| Copy argument data to buffer
||| ic0.msg_arg_data_copy : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_msg_arg_data_copy,libic0"
prim__msgArgDataCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Get size of caller principal in bytes
||| ic0.msg_caller_size : () -> i32
export
%foreign "C:ic0_msg_caller_size,libic0"
prim__msgCallerSize : PrimIO Int32

||| Copy caller principal to buffer
||| ic0.msg_caller_copy : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_msg_caller_copy,libic0"
prim__msgCallerCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Reject the message with a message
||| ic0.msg_reject : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_msg_reject,libic0"
prim__msgReject : Int32 -> Int32 -> PrimIO ()

||| Get reject code (0 = no error)
||| ic0.msg_reject_code : () -> i32
export
%foreign "C:ic0_msg_reject_code,libic0"
prim__msgRejectCode : PrimIO Int32

||| Get size of reject message
||| ic0.msg_reject_msg_size : () -> i32
export
%foreign "C:ic0_msg_reject_msg_size,libic0"
prim__msgRejectMsgSize : PrimIO Int32

||| Copy reject message to buffer
||| ic0.msg_reject_msg_copy : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_msg_reject_msg_copy,libic0"
prim__msgRejectMsgCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

--------------------------------------------------------------------------------
-- Reply (ic0.msg_reply_*)
--------------------------------------------------------------------------------

||| Append data to reply buffer
||| ic0.msg_reply_data_append : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_msg_reply_data_append,libic0"
prim__msgReplyDataAppend : Int32 -> Int32 -> PrimIO ()

||| Send the reply
||| ic0.msg_reply : () -> ()
export
%foreign "C:ic0_msg_reply,libic0"
prim__msgReply : PrimIO ()

--------------------------------------------------------------------------------
-- Canister Information (ic0.canister_*)
--------------------------------------------------------------------------------

||| Get size of own canister ID
||| ic0.canister_self_size : () -> i32
export
%foreign "C:ic0_canister_self_size,libic0"
prim__canisterSelfSize : PrimIO Int32

||| Copy own canister ID to buffer
||| ic0.canister_self_copy : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_canister_self_copy,libic0"
prim__canisterSelfCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Get canister cycle balance (128-bit)
||| ic0.canister_cycle_balance128 : (dst: i32) -> ()
export
%foreign "C:ic0_canister_cycle_balance128,libic0"
prim__canisterCycleBalance128 : Int32 -> PrimIO ()

||| Get canister status (1=running, 2=stopping, 3=stopped)
||| ic0.canister_status : () -> i32
export
%foreign "C:ic0_canister_status,libic0"
prim__canisterStatus : PrimIO Int32

--------------------------------------------------------------------------------
-- Time (ic0.time)
--------------------------------------------------------------------------------

||| Get current timestamp in nanoseconds
||| ic0.time : () -> i64
export
%foreign "C:ic0_time,libic0"
prim__time : PrimIO Bits64

--------------------------------------------------------------------------------
-- Stable Memory (ic0.stable_*)
--------------------------------------------------------------------------------

||| Get stable memory size in WebAssembly pages (64KiB each)
||| ic0.stable_size : () -> i32
export
%foreign "C:ic0_stable_size,libic0"
prim__stableSize : PrimIO Int32

||| Grow stable memory by given number of pages
||| ic0.stable_grow : (new_pages: i32) -> i32 (returns -1 on failure)
export
%foreign "C:ic0_stable_grow,libic0"
prim__stableGrow : Int32 -> PrimIO Int32

||| Read from stable memory
||| ic0.stable_read : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_stable_read,libic0"
prim__stableRead : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Write to stable memory
||| ic0.stable_write : (offset: i32, src: i32, size: i32) -> ()
export
%foreign "C:ic0_stable_write,libic0"
prim__stableWrite : Int32 -> Int32 -> Int32 -> PrimIO ()

||| Get stable memory size (64-bit version)
||| ic0.stable64_size : () -> i64
export
%foreign "C:ic0_stable64_size,libic0"
prim__stable64Size : PrimIO Bits64

||| Grow stable memory (64-bit version)
||| ic0.stable64_grow : (new_pages: i64) -> i64
export
%foreign "C:ic0_stable64_grow,libic0"
prim__stable64Grow : Bits64 -> PrimIO Bits64

||| Read from stable memory (64-bit version)
||| ic0.stable64_read : (dst: i64, offset: i64, size: i64) -> ()
export
%foreign "C:ic0_stable64_read,libic0"
prim__stable64Read : Bits64 -> Bits64 -> Bits64 -> PrimIO ()

||| Write to stable memory (64-bit version)
||| ic0.stable64_write : (offset: i64, src: i64, size: i64) -> ()
export
%foreign "C:ic0_stable64_write,libic0"
prim__stable64Write : Bits64 -> Bits64 -> Bits64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Certified Data (ic0.certified_*)
--------------------------------------------------------------------------------

||| Set certified data (max 32 bytes)
||| ic0.certified_data_set : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_certified_data_set,libic0"
prim__certifiedDataSet : Int32 -> Int32 -> PrimIO ()

||| Get size of data certificate
||| ic0.data_certificate_size : () -> i32
export
%foreign "C:ic0_data_certificate_size,libic0"
prim__dataCertificateSize : PrimIO Int32

||| Copy data certificate to buffer
||| ic0.data_certificate_copy : (dst: i32, offset: i32, size: i32) -> ()
export
%foreign "C:ic0_data_certificate_copy,libic0"
prim__dataCertificateCopy : Int32 -> Int32 -> Int32 -> PrimIO ()

--------------------------------------------------------------------------------
-- Inter-Canister Calls (ic0.call_*)
--------------------------------------------------------------------------------

||| Create a new call context
||| ic0.call_new : (callee_src, callee_size, name_src, name_size, reply_fun, reply_env, reject_fun, reject_env) -> ()
export
%foreign "C:ic0_call_new,libic0"
prim__callNew : Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> PrimIO ()

||| Set call data
||| ic0.call_data_append : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_call_data_append,libic0"
prim__callDataAppend : Int32 -> Int32 -> PrimIO ()

||| Attach cycles to call (128-bit)
||| ic0.call_cycles_add128 : (high: i64, low: i64) -> ()
export
%foreign "C:ic0_call_cycles_add128,libic0"
prim__callCyclesAdd128 : Bits64 -> Bits64 -> PrimIO ()

||| Perform the call
||| ic0.call_perform : () -> i32 (returns 0 on success)
export
%foreign "C:ic0_call_perform,libic0"
prim__callPerform : PrimIO Int32

--------------------------------------------------------------------------------
-- Cycles (ic0.msg_cycles_*)
--------------------------------------------------------------------------------

||| Get cycles available in message (128-bit)
||| ic0.msg_cycles_available128 : (dst: i32) -> ()
export
%foreign "C:ic0_msg_cycles_available128,libic0"
prim__msgCyclesAvailable128 : Int32 -> PrimIO ()

||| Accept cycles from message (128-bit)
||| ic0.msg_cycles_accept128 : (max_high: i64, max_low: i64, dst: i32) -> ()
export
%foreign "C:ic0_msg_cycles_accept128,libic0"
prim__msgCyclesAccept128 : Bits64 -> Bits64 -> Int32 -> PrimIO ()

||| Get cycles refunded (128-bit)
||| ic0.msg_cycles_refunded128 : (dst: i32) -> ()
export
%foreign "C:ic0_msg_cycles_refunded128,libic0"
prim__msgCyclesRefunded128 : Int32 -> PrimIO ()

--------------------------------------------------------------------------------
-- Debugging (ic0.debug_print, ic0.trap)
--------------------------------------------------------------------------------

||| Print debug message (only in local replica)
||| ic0.debug_print : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_debug_print,libic0"
prim__debugPrint : Int32 -> Int32 -> PrimIO ()

||| Trap with message
||| ic0.trap : (src: i32, size: i32) -> ()
export
%foreign "C:ic0_trap,libic0"
prim__trap : Int32 -> Int32 -> PrimIO ()

--------------------------------------------------------------------------------
-- Performance Counter
--------------------------------------------------------------------------------

||| Get performance counter
||| ic0.performance_counter : (type: i32) -> i64
export
%foreign "C:ic0_performance_counter,libic0"
prim__performanceCounter : Int32 -> PrimIO Bits64

--------------------------------------------------------------------------------
-- Global Timer
--------------------------------------------------------------------------------

||| Set global timer
||| ic0.global_timer_set : (timestamp: i64) -> i64
export
%foreign "C:ic0_global_timer_set,libic0"
prim__globalTimerSet : Bits64 -> PrimIO Bits64

--------------------------------------------------------------------------------
-- Instruction Counter
--------------------------------------------------------------------------------

||| Check if instruction limit is approaching
||| ic0.instruction_counter : () -> i64
export
%foreign "C:ic0_instruction_counter,libic0"
prim__instructionCounter : PrimIO Bits64

--------------------------------------------------------------------------------
-- Is Controller
--------------------------------------------------------------------------------

||| Check if caller is a controller
||| ic0.is_controller : (src: i32, size: i32) -> i32
export
%foreign "C:ic0_is_controller,libic0"
prim__isController : Int32 -> Int32 -> PrimIO Int32
