||| Failure Classification for FR Monad
|||
||| Based on the FR Monad paper: "Recovery-Preserving Kleisli Semantics for World-Computer Virtual Machines"
|||
||| F is the set of classified failure modes.
||| Key principle: classify failures instead of hiding them (no "Unknown" failures).
||| Every failure has a severity (impact level) and category (failure domain).
|||
||| This enables:
||| - Automated retry decisions based on severity
||| - Category-specific recovery strategies
||| - Proper observability and metrics
module FRMonad.Failure

%default total

-- =============================================================================
-- Severity: Impact level of failure
-- =============================================================================

||| Severity indicates how serious a failure is and guides recovery strategy
||| - Critical: System-wide impact, immediate attention needed
||| - High: Significant impact, operation cannot proceed
||| - Medium: Localized impact, operation degraded
||| - Low: Minor impact, can continue with fallback
||| - Info: Not really a failure, informational only
public export
data Severity
  = Critical    -- System invariant violated, canister may be in bad state
  | High        -- Operation failed, cannot proceed
  | Medium      -- Operation degraded, partial success possible
  | Low         -- Minor issue, fallback available
  | Info        -- Informational, not a true failure

public export
Show Severity where
  show Critical = "CRITICAL"
  show High     = "HIGH"
  show Medium   = "MEDIUM"
  show Low      = "LOW"
  show Info     = "INFO"

public export
Eq Severity where
  Critical == Critical = True
  High     == High     = True
  Medium   == Medium   = True
  Low      == Low      = True
  Info     == Info     = True
  _ == _ = False

||| Severity ordering: Critical > High > Medium > Low > Info
public export
Ord Severity where
  compare Critical Critical = EQ
  compare Critical _        = GT
  compare High     Critical = LT
  compare High     High     = EQ
  compare High     _        = GT
  compare Medium   Critical = LT
  compare Medium   High     = LT
  compare Medium   Medium   = EQ
  compare Medium   _        = GT
  compare Low      Info     = GT
  compare Low      Low      = EQ
  compare Low      _        = LT
  compare Info     Info     = EQ
  compare Info     _        = LT

-- =============================================================================
-- Category: Domain classification of failure
-- =============================================================================

||| Category indicates the domain where failure occurred
||| This guides which recovery strategy to use
public export
data FailCategory
  = SecurityFail    -- Auth/permission failures
  | StateFail       -- State machine/consistency violations
  | ResourceFail    -- Cycles/memory/storage limits
  | ExecutionFail   -- Runtime/trap failures
  | NetworkFail     -- Inter-canister/HTTP call failures
  | EncodingFail    -- Candid/serialization failures
  | PolicyFail      -- Business rule violations
  | UpgradeFail     -- Upgrade/migration failures

public export
Show FailCategory where
  show SecurityFail  = "SECURITY"
  show StateFail     = "STATE"
  show ResourceFail  = "RESOURCE"
  show ExecutionFail = "EXECUTION"
  show NetworkFail   = "NETWORK"
  show EncodingFail  = "ENCODING"
  show PolicyFail    = "POLICY"
  show UpgradeFail   = "UPGRADE"

public export
Eq FailCategory where
  SecurityFail  == SecurityFail  = True
  StateFail     == StateFail     = True
  ResourceFail  == ResourceFail  = True
  ExecutionFail == ExecutionFail = True
  NetworkFail   == NetworkFail   = True
  EncodingFail  == EncodingFail  = True
  PolicyFail    == PolicyFail    = True
  UpgradeFail   == UpgradeFail   = True
  _ == _ = False

-- =============================================================================
-- Fail: Classified failure types (the set F in the paper)
-- =============================================================================

||| ICP-specific failure classifications
||| These form a closed sum type - no "Unknown" failures allowed
||| Every constructor maps to a specific severity and category
public export
data Fail
  -- Execution failures (traps, runtime errors)
  = Trap String                         -- ic0.trap equivalent
  | SysInvariant String                 -- System invariant violation
  | Internal String                     -- Internal error (last resort)

  -- Network failures (inter-canister, HTTP)
  | Reject Int String                   -- Canister rejection (code, message)
  | CallError String                    -- Inter-canister call failure
  | Timeout String                      -- Operation timeout
  | HttpError Int String                -- HTTP outcall error (status, message)

  -- Encoding failures (Candid, serialization)
  | DecodeError String                  -- Candid/data decoding failure
  | EncodeError String                  -- Candid/data encoding failure

  -- State failures (consistency, concurrency)
  | StableMemError String               -- Stable memory operation failure
  | Conflict String                     -- Optimistic concurrency conflict
  | InvalidState String                 -- State machine violation
  | NotFound String                     -- Resource not found

  -- Security failures (auth, permissions)
  | Unauthorized String                 -- Permission/auth failure
  | PrincipalMismatch String            -- Expected vs actual principal

  -- Resource failures (cycles, memory, limits)
  | RateLimited String                  -- Rate/resource limit exceeded
  | InsufficientCycles Nat String       -- Need N cycles, message
  | MemoryExhausted String              -- Memory allocation failed

  -- Policy failures (business rules)
  | PolicyViolation String              -- Business rule violated
  | ValidationError String              -- Input validation failed

  -- Upgrade failures (migration, versioning)
  | UpgradeError String                 -- Upgrade operation failed
  | VersionMismatch Nat Nat             -- Expected vs actual version
  | MigrationError String               -- State migration failed

public export
Show Fail where
  show (Trap s)                = "Trap: " ++ s
  show (SysInvariant s)        = "SysInvariant: " ++ s
  show (Internal s)            = "Internal: " ++ s
  show (Reject c m)            = "Reject(" ++ show c ++ "): " ++ m
  show (CallError s)           = "CallError: " ++ s
  show (Timeout s)             = "Timeout: " ++ s
  show (HttpError c m)         = "HttpError(" ++ show c ++ "): " ++ m
  show (DecodeError s)         = "DecodeError: " ++ s
  show (EncodeError s)         = "EncodeError: " ++ s
  show (StableMemError s)      = "StableMemError: " ++ s
  show (Conflict s)            = "Conflict: " ++ s
  show (InvalidState s)        = "InvalidState: " ++ s
  show (NotFound s)            = "NotFound: " ++ s
  show (Unauthorized s)        = "Unauthorized: " ++ s
  show (PrincipalMismatch s)   = "PrincipalMismatch: " ++ s
  show (RateLimited s)         = "RateLimited: " ++ s
  show (InsufficientCycles n s)= "InsufficientCycles(" ++ show n ++ "): " ++ s
  show (MemoryExhausted s)     = "MemoryExhausted: " ++ s
  show (PolicyViolation s)     = "PolicyViolation: " ++ s
  show (ValidationError s)     = "ValidationError: " ++ s
  show (UpgradeError s)        = "UpgradeError: " ++ s
  show (VersionMismatch e a)   = "VersionMismatch: expected " ++ show e ++ ", got " ++ show a
  show (MigrationError s)      = "MigrationError: " ++ s

-- =============================================================================
-- Classification functions
-- =============================================================================

||| Get severity of a failure
||| Critical: System invariants, stable memory, upgrade failures
||| High: Traps, auth failures, insufficient cycles
||| Medium: Call errors, timeouts, conflicts
||| Low: Not found, validation errors
public export
severity : Fail -> Severity
severity (Trap _)              = High
severity (SysInvariant _)      = Critical
severity (Internal _)          = High
severity (Reject _ _)          = Medium
severity (CallError _)         = Medium
severity (Timeout _)           = Medium
severity (HttpError _ _)       = Medium
severity (DecodeError _)       = Medium
severity (EncodeError _)       = Medium
severity (StableMemError _)    = Critical
severity (Conflict _)          = Medium
severity (InvalidState _)      = High
severity (NotFound _)          = Low
severity (Unauthorized _)      = High
severity (PrincipalMismatch _) = High
severity (RateLimited _)       = Medium
severity (InsufficientCycles _ _) = High
severity (MemoryExhausted _)   = Critical
severity (PolicyViolation _)   = Medium
severity (ValidationError _)   = Low
severity (UpgradeError _)      = Critical
severity (VersionMismatch _ _) = Critical
severity (MigrationError _)    = Critical

||| Get category of a failure
public export
category : Fail -> FailCategory
category (Trap _)              = ExecutionFail
category (SysInvariant _)      = ExecutionFail
category (Internal _)          = ExecutionFail
category (Reject _ _)          = NetworkFail
category (CallError _)         = NetworkFail
category (Timeout _)           = NetworkFail
category (HttpError _ _)       = NetworkFail
category (DecodeError _)       = EncodingFail
category (EncodeError _)       = EncodingFail
category (StableMemError _)    = StateFail
category (Conflict _)          = StateFail
category (InvalidState _)      = StateFail
category (NotFound _)          = StateFail
category (Unauthorized _)      = SecurityFail
category (PrincipalMismatch _) = SecurityFail
category (RateLimited _)       = ResourceFail
category (InsufficientCycles _ _) = ResourceFail
category (MemoryExhausted _)   = ResourceFail
category (PolicyViolation _)   = PolicyFail
category (ValidationError _)   = PolicyFail
category (UpgradeError _)      = UpgradeFail
category (VersionMismatch _ _) = UpgradeFail
category (MigrationError _)    = UpgradeFail

-- =============================================================================
-- Recovery guidance (Section 3.1: Boundaries and Recovery Interfaces)
-- =============================================================================

||| Should this failure be retried?
||| Only Medium/Low severity + certain categories should retry
public export
isRetryable : Fail -> Bool
isRetryable f = case (severity f, category f) of
  (Medium, NetworkFail)  => True   -- Network issues may be transient
  (Medium, StateFail)    => True   -- Conflicts may resolve
  (Low, _)               => True   -- Low severity can retry
  _                      => False  -- Critical/High or other categories

||| Can user recover from this failure?
||| User can fix: validation, policy violations, auth (re-authenticate)
public export
isUserRecoverable : Fail -> Bool
isUserRecoverable (ValidationError _)   = True
isUserRecoverable (PolicyViolation _)   = True
isUserRecoverable (Unauthorized _)      = True  -- User can re-authenticate
isUserRecoverable (NotFound _)          = True  -- User can provide valid ID
isUserRecoverable (InsufficientCycles _ _) = True  -- User can top up
isUserRecoverable _                     = False

||| Does this failure require developer attention?
public export
requiresDevAttention : Fail -> Bool
requiresDevAttention f = severity f >= High

-- =============================================================================
-- Predicate helpers for recovery handlers
-- =============================================================================

||| Match security-related failures
public export
isSecurityFail : Fail -> Bool
isSecurityFail f = category f == SecurityFail

||| Match state-related failures
public export
isStateFail : Fail -> Bool
isStateFail f = category f == StateFail

||| Match resource-related failures
public export
isResourceFail : Fail -> Bool
isResourceFail f = category f == ResourceFail

||| Match network-related failures
public export
isNetworkFail : Fail -> Bool
isNetworkFail f = category f == NetworkFail

||| Match encoding-related failures
public export
isEncodingFail : Fail -> Bool
isEncodingFail f = category f == EncodingFail

||| Match upgrade-related failures
public export
isUpgradeFail : Fail -> Bool
isUpgradeFail f = category f == UpgradeFail

||| Match critical failures
public export
isCriticalFail : Fail -> Bool
isCriticalFail f = severity f == Critical
