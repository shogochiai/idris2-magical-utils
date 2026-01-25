||| Shared Types for ICP Canister Development
|||
||| This module provides common types used across both low-level (ICP.*)
||| and high-level (FRC.*) modules.
module ICP.Types

import public ICP.IC0

%default total

-- =============================================================================
-- Phase: Execution context boundaries for ICP canisters
-- =============================================================================

||| ICP canister lifecycle phases
||| Each phase represents a distinct execution boundary with different capabilities
public export
data Phase
  = Init           -- Canister initialization
  | PreUpgrade     -- Before upgrade (save state)
  | PostUpgrade    -- After upgrade (restore state)
  | Update         -- State-modifying call
  | Query          -- Read-only call
  | Heartbeat      -- Periodic execution
  | Inspect        -- Message inspection
  | Timer          -- Timer callback
  | HttpRequest    -- HTTP outcall context
  | Composite      -- Composite query

public export
Show Phase where
  show Init        = "Init"
  show PreUpgrade  = "PreUpgrade"
  show PostUpgrade = "PostUpgrade"
  show Update      = "Update"
  show Query       = "Query"
  show Heartbeat   = "Heartbeat"
  show Inspect     = "Inspect"
  show Timer       = "Timer"
  show HttpRequest = "HttpRequest"
  show Composite   = "Composite"

public export
Eq Phase where
  Init        == Init        = True
  PreUpgrade  == PreUpgrade  = True
  PostUpgrade == PostUpgrade = True
  Update      == Update      = True
  Query       == Query       = True
  Heartbeat   == Heartbeat   = True
  Inspect     == Inspect     = True
  Timer       == Timer       = True
  HttpRequest == HttpRequest = True
  Composite   == Composite   = True
  _ == _ = False

-- =============================================================================
-- RejectCode: IC protocol reject codes (consolidated)
-- =============================================================================

||| IC reject codes with semantic meaning
||| Maps to IC protocol specification
public export
data RejectCode
  = NoError              -- 0: No error
  | SysFatal             -- 1: Fatal system error
  | SysTransient         -- 2: Transient error, may retry
  | DestinationInvalid   -- 3: Destination canister invalid
  | CanisterReject       -- 4: Canister explicitly rejected
  | CanisterError        -- 5: Canister execution error

public export
Show RejectCode where
  show NoError            = "NoError"
  show SysFatal           = "SysFatal"
  show SysTransient       = "SysTransient"
  show DestinationInvalid = "DestinationInvalid"
  show CanisterReject     = "CanisterReject"
  show CanisterError      = "CanisterError"

public export
Eq RejectCode where
  NoError            == NoError            = True
  SysFatal           == SysFatal           = True
  SysTransient       == SysTransient       = True
  DestinationInvalid == DestinationInvalid = True
  CanisterReject     == CanisterReject     = True
  CanisterError      == CanisterError      = True
  _ == _ = False

||| Convert Int32 to RejectCode
public export
rejectCodeFromInt : Int32 -> RejectCode
rejectCodeFromInt 0 = NoError
rejectCodeFromInt 1 = SysFatal
rejectCodeFromInt 2 = SysTransient
rejectCodeFromInt 3 = DestinationInvalid
rejectCodeFromInt 4 = CanisterReject
rejectCodeFromInt _ = CanisterError  -- 5 and unknown -> CanisterError

||| Convert RejectCode to Int
public export
rejectCodeToInt : RejectCode -> Int
rejectCodeToInt NoError            = 0
rejectCodeToInt SysFatal           = 1
rejectCodeToInt SysTransient       = 2
rejectCodeToInt DestinationInvalid = 3
rejectCodeToInt CanisterReject     = 4
rejectCodeToInt CanisterError      = 5
