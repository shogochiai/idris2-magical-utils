||| Mock Execution Context
|||
||| Provides a mock execution context for running ICP canisters locally.
||| Manages canister state, message context, and IC0 stub behavior.
module DfxCoverage.Ic0Mock.MockContext

import Data.List
import Data.Maybe
import Data.Bits
import System

import DfxCoverage.Ic0Mock.Ic0Stubs

%default covering

-- =============================================================================
-- Message Types
-- =============================================================================

||| Type of canister message
public export
data MessageType
  = QueryMsg
  | UpdateMsg
  | InitMsg
  | PreUpgradeMsg
  | PostUpgradeMsg

public export
Show MessageType where
  show QueryMsg = "query"
  show UpdateMsg = "update"
  show InitMsg = "init"
  show PreUpgradeMsg = "pre_upgrade"
  show PostUpgradeMsg = "post_upgrade"

public export
Eq MessageType where
  QueryMsg == QueryMsg = True
  UpdateMsg == UpdateMsg = True
  InitMsg == InitMsg = True
  PreUpgradeMsg == PreUpgradeMsg = True
  PostUpgradeMsg == PostUpgradeMsg = True
  _ == _ = False

-- =============================================================================
-- Mock Context
-- =============================================================================

||| Execution context for mocked canister
public export
record MockContext where
  constructor MkMockContext
  -- Message context
  messageType : MessageType
  methodName : String
  argData : List Bits8        -- Candid-encoded arguments

  -- Caller information
  caller : List Bits8         -- 29-byte principal

  -- Canister state
  canisterId : List Bits8     -- 10-byte canister ID
  cycleBalance : Bits64

  -- Reply buffer (accumulated msg_reply_data_append calls)
  replyBuffer : List Bits8

  -- Stable memory (simplified - would be larger in practice)
  stableMemory : List Bits8
  stablePages : Nat           -- Number of 64KB pages

  -- Time
  currentTime : Bits64        -- Nanoseconds

  -- Execution flags
  hasReplied : Bool
  hasTrapped : Bool
  trapMessage : String

public export
Show MockContext where
  show ctx = "MockContext{" ++
             show ctx.messageType ++ " " ++ ctx.methodName ++
             ", cycles=" ++ show ctx.cycleBalance ++
             ", replied=" ++ show ctx.hasReplied ++
             ", trapped=" ++ show ctx.hasTrapped ++ "}"

-- =============================================================================
-- Context Construction
-- =============================================================================

||| Create default mock context for query
export
defaultQueryContext : String -> List Bits8 -> MockContext
defaultQueryContext method args = MkMockContext
  { messageType = QueryMsg
  , methodName = method
  , argData = args
  , caller = replicate 29 0  -- Anonymous
  , canisterId = [0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01]
  , cycleBalance = 1_000_000_000_000
  , replyBuffer = []
  , stableMemory = []
  , stablePages = 0
  , currentTime = 1700000000_000_000_000
  , hasReplied = False
  , hasTrapped = False
  , trapMessage = ""
  }

||| Create default mock context for update
export
defaultUpdateContext : String -> List Bits8 -> MockContext
defaultUpdateContext method args =
  { messageType := UpdateMsg } (defaultQueryContext method args)

||| Create context with Candid empty args
export
emptyArgsContext : MessageType -> String -> MockContext
emptyArgsContext msgType method =
  let emptyCandid = [0x44, 0x49, 0x44, 0x4C, 0x00, 0x00]  -- "DIDL\x00\x00"
  in case msgType of
       QueryMsg => defaultQueryContext method emptyCandid
       _ => defaultUpdateContext method emptyCandid

-- =============================================================================
-- Context Modification
-- =============================================================================

||| Set caller principal
export
withCaller : List Bits8 -> MockContext -> MockContext
withCaller principal ctx = { caller := principal } ctx

||| Set cycle balance
export
withCycles : Bits64 -> MockContext -> MockContext
withCycles cycles ctx = { cycleBalance := cycles } ctx

||| Set current time
export
withTime : Bits64 -> MockContext -> MockContext
withTime time ctx = { currentTime := time } ctx

||| Add data to reply buffer
export
appendReply : List Bits8 -> MockContext -> MockContext
appendReply bytes ctx = { replyBuffer $= (++ bytes) } ctx

||| Mark as replied
export
markReplied : MockContext -> MockContext
markReplied ctx = { hasReplied := True } ctx

||| Mark as trapped
export
markTrapped : String -> MockContext -> MockContext
markTrapped msg ctx = { hasTrapped := True, trapMessage := msg } ctx

-- =============================================================================
-- Stub Config from Context
-- =============================================================================

||| Convert MockContext to Ic0StubConfig
export
contextToStubConfig : MockContext -> Ic0StubConfig
contextToStubConfig ctx = MkIc0StubConfig
  { msgArgData = ctx.argData
  , msgCaller = ctx.caller
  , canisterSelf = ctx.canisterId
  , cycleBalance = ctx.cycleBalance
  , currentTime = ctx.currentTime
  , stableMemory = ctx.stableMemory
  }

-- =============================================================================
-- Execution
-- =============================================================================

||| Result of executing canister method
public export
data ExecutionResult
  = ExecSuccess (List Bits8)  -- Reply data
  | ExecReject Int String     -- Reject code and message
  | ExecTrap String           -- Trap message

public export
Show ExecutionResult where
  show (ExecSuccess bytes) = "Success: " ++ show (length bytes) ++ " bytes"
  show (ExecReject code msg) = "Reject(" ++ show code ++ "): " ++ msg
  show (ExecTrap msg) = "Trap: " ++ msg

||| Check if execution was successful
export
isSuccess : ExecutionResult -> Bool
isSuccess (ExecSuccess _) = True
isSuccess _ = False

||| Get reply bytes if successful
export
getReplyData : ExecutionResult -> Maybe (List Bits8)
getReplyData (ExecSuccess bytes) = Just bytes
getReplyData _ = Nothing

-- =============================================================================
-- Execution Commands
-- =============================================================================

||| Commands that can be executed against mock context
public export
data MockCommand
  = CmdMsgReply                           -- msg_reply
  | CmdMsgReplyAppend (List Bits8)        -- msg_reply_data_append
  | CmdMsgReject Int String               -- msg_reject
  | CmdDebugPrint String                  -- debug_print
  | CmdTrap String                        -- trap
  | CmdStableGrow Nat                     -- stable_grow
  | CmdStableWrite Nat (List Bits8)       -- stable_write
  | CmdCallPerform                        -- call_perform (simplified)

public export
Show MockCommand where
  show CmdMsgReply = "msg_reply"
  show (CmdMsgReplyAppend bytes) = "msg_reply_data_append(" ++ show (length bytes) ++ " bytes)"
  show (CmdMsgReject code msg) = "msg_reject(" ++ show code ++ ", " ++ msg ++ ")"
  show (CmdDebugPrint msg) = "debug_print: " ++ msg
  show (CmdTrap msg) = "trap: " ++ msg
  show (CmdStableGrow pages) = "stable_grow(" ++ show pages ++ ")"
  show (CmdStableWrite offset bytes) = "stable_write(" ++ show offset ++ ", " ++ show (length bytes) ++ " bytes)"
  show CmdCallPerform = "call_perform"

||| Apply command to context
export
applyCommand : MockCommand -> MockContext -> MockContext
applyCommand cmd ctx = case cmd of
  CmdMsgReply => markReplied ctx
  CmdMsgReplyAppend bytes => appendReply bytes ctx
  CmdMsgReject _ msg => { hasReplied := True } ctx  -- Reject also ends message
  CmdDebugPrint _ => ctx  -- No state change
  CmdTrap msg => markTrapped msg ctx
  CmdStableGrow pages => { stablePages $= (+ pages) } ctx
  CmdStableWrite _ _ => ctx  -- Would need more complex memory handling
  CmdCallPerform => ctx  -- Would need async handling

-- =============================================================================
-- Execution Log
-- =============================================================================

||| Entry in execution log
public export
record LogEntry where
  constructor MkLogEntry
  timestamp : Nat           -- Instruction counter
  command : MockCommand
  funcIdx : Maybe Nat       -- Which function called this (if known)

public export
Show LogEntry where
  show le = "[" ++ show le.timestamp ++ "] " ++ show le.command ++
            maybe "" (\f => " (func " ++ show f ++ ")") le.funcIdx

||| Execution log
public export
record ExecutionLog where
  constructor MkExecutionLog
  entries : List LogEntry
  ic0Calls : List (String, Nat)  -- (function name, call count)

||| Empty execution log
export
emptyLog : ExecutionLog
emptyLog = MkExecutionLog [] []

||| Add entry to log
export
logCommand : Nat -> Maybe Nat -> MockCommand -> ExecutionLog -> ExecutionLog
logCommand ts funcIdx cmd log =
  { entries $= (MkLogEntry ts cmd funcIdx ::) } log

||| Get IC0 call statistics from log
export
ic0CallStats : ExecutionLog -> List (String, Nat)
ic0CallStats log = log.ic0Calls
