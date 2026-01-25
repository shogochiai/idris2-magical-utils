||| ICP Safe API Layer
|||
||| This module provides safe, high-level wrappers around the raw IC0 system calls.
||| Application code should use this module instead of ICP.IC0 directly.
|||
||| Design principles:
||| - All IO is wrapped in IO monad
||| - Errors are represented as Either types where possible
||| - Buffer management is handled internally
||| - Types are domain-appropriate (Principal, Cycles, etc.)
module ICP.API

import public ICP.IC0
import public ICP.Types
import Data.List
import Data.String
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Canister Status
--------------------------------------------------------------------------------

||| Canister runtime status
public export
data CanisterStatus = Running | Stopping | Stopped | Unknown

||| Convert status code to CanisterStatus
statusFromCode : Int32 -> CanisterStatus
statusFromCode 1 = Running
statusFromCode 2 = Stopping
statusFromCode 3 = Stopped
statusFromCode _ = Unknown

--------------------------------------------------------------------------------
-- Reject Codes (use ICP.Types.RejectCode)
--------------------------------------------------------------------------------

-- RejectCode is now imported from ICP.Types

--------------------------------------------------------------------------------
-- Message Information
--------------------------------------------------------------------------------

||| Get the caller's principal
export
caller : IO Principal
caller = do
  size <- primIO prim__msgCallerSize
  -- In real implementation, would allocate buffer and copy
  -- For now, return placeholder
  pure (MkPrincipal [])

||| Get the argument data as raw bytes
export
argData : IO (List Bits8)
argData = do
  size <- primIO prim__msgArgDataSize
  -- In real implementation, would allocate buffer and copy
  pure []

||| Reject the current message
export
reject : String -> IO ()
reject msg = do
  -- In real implementation, would copy string to linear memory
  -- and call prim__msgReject
  pure ()

||| Get the reject code from a failed call
export
rejectCode : IO RejectCode
rejectCode = do
  code <- primIO prim__msgRejectCode
  pure (rejectCodeFromInt code)

--------------------------------------------------------------------------------
-- Reply
--------------------------------------------------------------------------------

||| Reply with raw bytes
export
reply : List Bits8 -> IO ()
reply bytes = do
  -- In real implementation, would copy to linear memory
  -- call prim__msgReplyDataAppend, then prim__msgReply
  primIO prim__msgReply

||| Reply with empty response
export
replyEmpty : IO ()
replyEmpty = primIO prim__msgReply

--------------------------------------------------------------------------------
-- Canister Information
--------------------------------------------------------------------------------

||| Get this canister's own ID
export
self : IO CanisterId
self = do
  size <- primIO prim__canisterSelfSize
  -- In real implementation, would allocate buffer and copy
  pure (MkPrincipal [])

||| Get current cycle balance
export
cycleBalance : IO Cycles
cycleBalance = do
  -- In real implementation, would read 128-bit value from memory
  pure (MkCycles 0 0)

||| Get canister status
export
canisterStatus : IO CanisterStatus
canisterStatus = do
  code <- primIO prim__canisterStatus
  pure (statusFromCode code)

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

||| Get current timestamp in nanoseconds
export
time : IO Timestamp
time = primIO prim__time

||| Safe division for Nat
safeDiv : Nat -> Nat -> Nat
safeDiv n Z = 0
safeDiv n (S k) = divNatNZ n (S k) SIsNonZero

||| Get current timestamp in seconds (convenience)
export
timeSeconds : IO Nat
timeSeconds = do
  ns <- time
  pure (safeDiv (cast ns) 1000000000)

--------------------------------------------------------------------------------
-- Cycles (Message-level)
--------------------------------------------------------------------------------

||| Get cycles available in the current message
export
cyclesAvailable : IO Cycles
cyclesAvailable = do
  -- In real implementation, would read 128-bit value
  pure (MkCycles 0 0)

||| Accept cycles from the current message
||| Returns the amount actually accepted
export
cyclesAccept : Cycles -> IO Cycles
cyclesAccept c = do
  -- In real implementation, would call prim__msgCyclesAccept128
  pure (MkCycles 0 0)

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

||| Print debug message (only visible in local replica)
export
debugPrint : String -> IO ()
debugPrint msg = do
  -- In real implementation, would copy string to linear memory
  -- and call prim__debugPrint
  pure ()

||| Trap (abort) with error message
export
trap : String -> IO a
trap msg = do
  -- In real implementation, would copy string and call prim__trap
  -- This will never return
  believe_me ()

--------------------------------------------------------------------------------
-- Performance
--------------------------------------------------------------------------------

||| Performance counter types
public export
data PerfCounterType = Instructions | CallContextInstructions

perfCounterCode : PerfCounterType -> Int32
perfCounterCode Instructions = 0
perfCounterCode CallContextInstructions = 1

||| Get performance counter value
export
performanceCounter : PerfCounterType -> IO Bits64
performanceCounter t = primIO (prim__performanceCounter (perfCounterCode t))

||| Get instruction counter (alias for convenience)
export
instructionCounter : IO Bits64
instructionCounter = primIO prim__instructionCounter

--------------------------------------------------------------------------------
-- Global Timer
--------------------------------------------------------------------------------

||| Set the global timer to fire at given timestamp
||| Returns the previously set timer value
export
setTimer : Timestamp -> IO Timestamp
setTimer ts = primIO (prim__globalTimerSet ts)

||| Cancel the global timer
||| Returns the previously set timer value
export
cancelTimer : IO Timestamp
cancelTimer = setTimer 0

--------------------------------------------------------------------------------
-- Controller Check
--------------------------------------------------------------------------------

||| Check if a principal is a controller of this canister
export
isController : Principal -> IO Bool
isController p = do
  -- In real implementation, would copy bytes to linear memory
  -- and call prim__isController
  result <- primIO (prim__isController 0 0)
  pure (result /= 0)

--------------------------------------------------------------------------------
-- Certified Data
--------------------------------------------------------------------------------

||| Set certified data (max 32 bytes)
export
setCertifiedData : List Bits8 -> IO ()
setCertifiedData bytes = do
  -- In real implementation, would copy to linear memory
  -- and call prim__certifiedDataSet
  pure ()

||| Get data certificate (if in certified query)
export
dataCertificate : IO (Maybe (List Bits8))
dataCertificate = do
  size <- primIO prim__dataCertificateSize
  if size == 0
    then pure Nothing
    else do
      -- In real implementation, would allocate and copy
      pure (Just [])
