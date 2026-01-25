||| IC0 System API Stubs
|||
||| Defines stub implementations for IC System API functions.
||| These stubs allow running ICP canisters locally for coverage testing
||| without a real IC network.
|||
||| Reference: https://internetcomputer.org/docs/current/references/ic-interface-spec
module DfxCoverage.Ic0Mock.Ic0Stubs

import Data.List
import Data.String
import Data.Bits
import Data.Buffer

%default covering

-- =============================================================================
-- IC0 Function Categories
-- =============================================================================

||| Categories of IC System API functions
public export
data Ic0Category
  = MsgInput          -- msg_arg_data_*, msg_caller_*
  | MsgOutput         -- msg_reply_*, msg_reject
  | Canister          -- canister_self, canister_cycle_balance
  | Stable            -- stable_*, stable64_*
  | Debug             -- debug_print, trap
  | Time              -- time
  | Certified         -- certified_data_set, data_certificate_*
  | Performance       -- performance_counter
  | Call              -- call_*, call_data_*, call_cycles_*

public export
Show Ic0Category where
  show MsgInput = "msg_input"
  show MsgOutput = "msg_output"
  show Canister = "canister"
  show Stable = "stable"
  show Debug = "debug"
  show Time = "time"
  show Certified = "certified"
  show Performance = "performance"
  show Call = "call"

||| Categorize ic0 function name
export
categorizeIc0 : String -> Ic0Category
categorizeIc0 name =
  if isInfixOf "msg_arg" name || isInfixOf "msg_caller" name
    then MsgInput
    else if isInfixOf "msg_reply" name || isInfixOf "msg_reject" name
      then MsgOutput
      else if isInfixOf "canister" name
        then Canister
        else if isInfixOf "stable" name
          then Stable
          else if isInfixOf "debug" name || isInfixOf "trap" name
            then Debug
            else if isInfixOf "time" name
              then Time
              else if isInfixOf "certified" name || isInfixOf "certificate" name
                then Certified
                else if isInfixOf "performance" name
                  then Performance
                  else if isInfixOf "call" name
                    then Call
                    else Debug  -- Default

-- =============================================================================
-- Stub Return Values
-- =============================================================================

||| Stub behavior configuration
public export
record Ic0StubConfig where
  constructor MkIc0StubConfig
  msgArgData : List Bits8       -- Data to return for msg_arg_data_copy
  msgCaller : List Bits8        -- 29-byte principal
  canisterSelf : List Bits8     -- 10-byte canister ID
  cycleBalance : Bits64         -- Cycle balance
  currentTime : Bits64          -- Nanoseconds since epoch
  stableMemory : List Bits8     -- Stable memory contents

||| Default stub configuration
export
defaultStubConfig : Ic0StubConfig
defaultStubConfig = MkIc0StubConfig
  { msgArgData = [0x44, 0x49, 0x44, 0x4C, 0x00, 0x00]  -- Empty Candid: "DIDL\x00\x00"
  , msgCaller = replicate 29 0                          -- Anonymous principal
  , canisterSelf = [0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01]  -- aaaaa-aa
  , cycleBalance = 1_000_000_000_000                    -- 1T cycles
  , currentTime = 1700000000_000_000_000                -- Fixed timestamp
  , stableMemory = []
  }

-- =============================================================================
-- IC0 Function Signatures (for verification)
-- =============================================================================

||| IC0 function signature
public export
record Ic0Sig where
  constructor MkIc0Sig
  name : String
  params : List String    -- WASM types: i32, i64
  results : List String   -- WASM types: i32, i64
  category : Ic0Category

||| All IC0 function signatures
export
ic0Signatures : List Ic0Sig
ic0Signatures =
  -- Message Input
  [ MkIc0Sig "msg_arg_data_size" [] ["i32"] MsgInput
  , MkIc0Sig "msg_arg_data_copy" ["i32", "i32", "i32"] [] MsgInput
  , MkIc0Sig "msg_caller_size" [] ["i32"] MsgInput
  , MkIc0Sig "msg_caller_copy" ["i32", "i32", "i32"] [] MsgInput
  , MkIc0Sig "msg_method_name_size" [] ["i32"] MsgInput
  , MkIc0Sig "msg_method_name_copy" ["i32", "i32", "i32"] [] MsgInput

  -- Message Output
  , MkIc0Sig "msg_reply_data_append" ["i32", "i32"] [] MsgOutput
  , MkIc0Sig "msg_reply" [] [] MsgOutput
  , MkIc0Sig "msg_reject_code" [] ["i32"] MsgOutput
  , MkIc0Sig "msg_reject_msg_size" [] ["i32"] MsgOutput
  , MkIc0Sig "msg_reject_msg_copy" ["i32", "i32", "i32"] [] MsgOutput
  , MkIc0Sig "msg_reject" ["i32", "i32"] [] MsgOutput

  -- Canister
  , MkIc0Sig "canister_self_size" [] ["i32"] Canister
  , MkIc0Sig "canister_self_copy" ["i32", "i32", "i32"] [] Canister
  , MkIc0Sig "canister_cycle_balance" [] ["i64"] Canister
  , MkIc0Sig "canister_cycle_balance128" ["i32"] [] Canister

  -- Stable Memory
  , MkIc0Sig "stable_size" [] ["i32"] Stable
  , MkIc0Sig "stable_grow" ["i32"] ["i32"] Stable
  , MkIc0Sig "stable_read" ["i32", "i32", "i32"] [] Stable
  , MkIc0Sig "stable_write" ["i32", "i32", "i32"] [] Stable
  , MkIc0Sig "stable64_size" [] ["i64"] Stable
  , MkIc0Sig "stable64_grow" ["i64"] ["i64"] Stable
  , MkIc0Sig "stable64_read" ["i64", "i64", "i64"] [] Stable
  , MkIc0Sig "stable64_write" ["i64", "i64", "i64"] [] Stable

  -- Debug
  , MkIc0Sig "debug_print" ["i32", "i32"] [] Debug
  , MkIc0Sig "trap" ["i32", "i32"] [] Debug

  -- Time
  , MkIc0Sig "time" [] ["i64"] Time

  -- Performance
  , MkIc0Sig "performance_counter" ["i32"] ["i64"] Performance

  -- Inter-canister calls
  , MkIc0Sig "call_new" ["i32", "i32", "i32", "i32", "i32", "i32", "i32", "i32"] [] Call
  , MkIc0Sig "call_on_cleanup" ["i32", "i32"] [] Call
  , MkIc0Sig "call_data_append" ["i32", "i32"] [] Call
  , MkIc0Sig "call_cycles_add" ["i64"] [] Call
  , MkIc0Sig "call_cycles_add128" ["i64", "i64"] [] Call
  , MkIc0Sig "call_perform" [] ["i32"] Call
  ]

||| Look up IC0 function signature
export
lookupIc0Sig : String -> Maybe Ic0Sig
lookupIc0Sig name = find (\s => s.name == name) ic0Signatures

-- =============================================================================
-- Stub Execution
-- =============================================================================

||| Result of executing a stub
public export
data StubResult
  = StubOk (List Bits64)    -- Return values (as i64s)
  | StubTrap String         -- Trapped with message
  | StubNoOp                -- Function executed with no observable effect

public export
Show StubResult where
  show (StubOk vals) = "OK: " ++ show vals
  show (StubTrap msg) = "TRAP: " ++ msg
  show StubNoOp = "NoOp"

||| Execute IC0 stub function
|||
||| @config  Stub configuration
||| @name    IC0 function name
||| @args    Function arguments (as i64s)
export
executeStub : Ic0StubConfig -> String -> List Bits64 -> StubResult
executeStub config name args =
  case name of
    -- Message Input
    "msg_arg_data_size" => StubOk [cast $ length config.msgArgData]
    "msg_arg_data_copy" => StubNoOp  -- Would need memory access
    "msg_caller_size" => StubOk [cast $ length config.msgCaller]
    "msg_caller_copy" => StubNoOp

    -- Message Output
    "msg_reply_data_append" => StubNoOp
    "msg_reply" => StubOk []

    -- Canister
    "canister_self_size" => StubOk [cast $ length config.canisterSelf]
    "canister_self_copy" => StubNoOp
    "canister_cycle_balance" => StubOk [config.cycleBalance]

    -- Stable Memory
    "stable_size" => StubOk [0]
    "stable_grow" => StubOk [0]  -- Success
    "stable64_size" => StubOk [0]
    "stable64_grow" => StubOk [0]

    -- Debug
    "debug_print" => StubNoOp
    "trap" => StubTrap "trap called"

    -- Time
    "time" => StubOk [config.currentTime]

    -- Performance
    "performance_counter" => StubOk [0]

    -- Inter-canister calls
    "call_perform" => StubOk [0]  -- Success (would need async handling)

    -- Default: no-op for unknown functions
    _ => StubNoOp

-- =============================================================================
-- Verification
-- =============================================================================

||| Check if all expected IC0 functions are present in WASM
|||
||| @expected List of expected IC0 function names
||| @actual   List of IC0 functions found in WASM
export
verifyIc0Imports : List String -> List String -> List String
verifyIc0Imports expected actual =
  filter (\e => not (e `elem` actual)) expected

||| Get list of commonly used IC0 functions
export
commonIc0Functions : List String
commonIc0Functions =
  [ "msg_arg_data_size", "msg_arg_data_copy"
  , "msg_caller_size", "msg_caller_copy"
  , "msg_reply_data_append", "msg_reply"
  , "canister_self_size", "canister_self_copy"
  , "debug_print", "trap"
  , "time"
  , "stable_size", "stable_grow", "stable_read", "stable_write"
  ]
