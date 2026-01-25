||| Subcontract Core: Conflict (Failure Normal Form)
|||
||| MVP-1: Failure classification - no "unknown" failures allowed.
|||
||| Key insight: EVM failures are currently untyped (revert with bytes).
||| This module creates a finite sum of all possible failure modes,
||| making every failure observable, classifiable, and recoverable.
|||
||| Solidity: revert("some string") - unstructured, untyped
||| Idris2: Fail Reentrancy evidence - structured, typed, traceable
module Subcontract.Core.Conflict

%default total

-- =============================================================================
-- Conflict: Finite Sum of Failure Modes
-- =============================================================================

||| All possible failure modes in a contract.
||| "Unknown" is intentionally NOT included - every failure must be classified.
public export
data Conflict : Type where
  ||| Explicit revert (reason captured in Evidence)
  Revert : Conflict
  ||| Reentrancy detected
  Reentrancy : Conflict
  ||| Authorization/access control violation
  AuthViolation : Conflict
  ||| Wrong epoch (upgrade safety)
  EpochMismatch : Conflict
  ||| Contract already initialized
  InitAlready : Conflict
  ||| Contract not yet initialized
  NotInitialized : Conflict
  ||| Storage slot collision/alias
  StorageAlias : Conflict
  ||| External call not allowed in this context
  ExternalCallForbidden : Conflict
  ||| Unsafe entry context (receive/fallback/etc)
  UnsafeEntryContext : Conflict
  ||| Gas exhausted
  GasExhausted : Conflict
  ||| ABI decode error
  DecodeError : Conflict
  ||| Arithmetic overflow/underflow/division by zero
  ArithmeticError : Conflict
  ||| Upgrade not allowed (epoch/auth/state)
  UpgradeNotAllowed : Conflict
  ||| Rollback not allowed
  RollbackNotAllowed : Conflict
  ||| Asset already spent (linear asset violation)
  AssetAlreadySpent : Conflict
  ||| Asset not owned by caller
  AssetNotOwned : Conflict
  ||| CEI violation (write before call)
  CEIViolation : Conflict
  ||| Invariant violation (bounds, balance, etc)
  InvariantViolation : Conflict
  ||| State machine invalid transition
  InvalidTransition : Conflict
  ||| Proof not provided (access control, bounds, etc)
  ProofMissing : Conflict

||| Conflict equality
public export
Eq Conflict where
  Revert == Revert = True
  Reentrancy == Reentrancy = True
  AuthViolation == AuthViolation = True
  EpochMismatch == EpochMismatch = True
  InitAlready == InitAlready = True
  NotInitialized == NotInitialized = True
  StorageAlias == StorageAlias = True
  ExternalCallForbidden == ExternalCallForbidden = True
  UnsafeEntryContext == UnsafeEntryContext = True
  GasExhausted == GasExhausted = True
  DecodeError == DecodeError = True
  ArithmeticError == ArithmeticError = True
  UpgradeNotAllowed == UpgradeNotAllowed = True
  RollbackNotAllowed == RollbackNotAllowed = True
  AssetAlreadySpent == AssetAlreadySpent = True
  AssetNotOwned == AssetNotOwned = True
  CEIViolation == CEIViolation = True
  InvariantViolation == InvariantViolation = True
  InvalidTransition == InvalidTransition = True
  ProofMissing == ProofMissing = True
  _ == _ = False

||| Convert conflict to human-readable tag
public export
conflictTag : Conflict -> String
conflictTag Revert = "REVERT"
conflictTag Reentrancy = "REENTRANCY"
conflictTag AuthViolation = "AUTH_VIOLATION"
conflictTag EpochMismatch = "EPOCH_MISMATCH"
conflictTag InitAlready = "INIT_ALREADY"
conflictTag NotInitialized = "NOT_INITIALIZED"
conflictTag StorageAlias = "STORAGE_ALIAS"
conflictTag ExternalCallForbidden = "EXTERNAL_CALL_FORBIDDEN"
conflictTag UnsafeEntryContext = "UNSAFE_ENTRY_CONTEXT"
conflictTag GasExhausted = "GAS_EXHAUSTED"
conflictTag DecodeError = "DECODE_ERROR"
conflictTag ArithmeticError = "ARITHMETIC_ERROR"
conflictTag UpgradeNotAllowed = "UPGRADE_NOT_ALLOWED"
conflictTag RollbackNotAllowed = "ROLLBACK_NOT_ALLOWED"
conflictTag AssetAlreadySpent = "ASSET_ALREADY_SPENT"
conflictTag AssetNotOwned = "ASSET_NOT_OWNED"
conflictTag CEIViolation = "CEI_VIOLATION"
conflictTag InvariantViolation = "INVARIANT_VIOLATION"
conflictTag InvalidTransition = "INVALID_TRANSITION"
conflictTag ProofMissing = "PROOF_MISSING"

-- =============================================================================
-- Conflict Severity
-- =============================================================================

||| Severity level for conflicts
public export
data Severity = Critical | High | Medium | Low | Info

public export
Eq Severity where
  Critical == Critical = True
  High == High = True
  Medium == Medium = True
  Low == Low = True
  Info == Info = True
  _ == _ = False

||| Get severity of a conflict
public export
severity : Conflict -> Severity
severity Reentrancy = Critical
severity AuthViolation = Critical
severity CEIViolation = Critical
severity AssetAlreadySpent = Critical
severity StorageAlias = High
severity EpochMismatch = High
severity UpgradeNotAllowed = High
severity InvariantViolation = High
severity InvalidTransition = Medium
severity InitAlready = Medium
severity NotInitialized = Medium
severity ExternalCallForbidden = Medium
severity UnsafeEntryContext = Medium
severity DecodeError = Low
severity ArithmeticError = Low
severity Revert = Low
severity GasExhausted = Info
severity RollbackNotAllowed = Medium
severity AssetNotOwned = Medium
severity ProofMissing = Low

-- =============================================================================
-- Conflict Categories
-- =============================================================================

||| Conflict category for grouping
public export
data ConflictCategory
  = SecurityConflict      -- Reentrancy, Auth, CEI
  | StateConflict         -- Init, Epoch, Transition
  | ResourceConflict      -- Asset, Storage
  | ExecutionConflict     -- Gas, Decode, Arithmetic
  | PolicyConflict        -- EntryContext, ExternalCall

public export
Eq ConflictCategory where
  SecurityConflict == SecurityConflict = True
  StateConflict == StateConflict = True
  ResourceConflict == ResourceConflict = True
  ExecutionConflict == ExecutionConflict = True
  PolicyConflict == PolicyConflict = True
  _ == _ = False

||| Categorize a conflict
public export
category : Conflict -> ConflictCategory
category Reentrancy = SecurityConflict
category AuthViolation = SecurityConflict
category CEIViolation = SecurityConflict
category InitAlready = StateConflict
category NotInitialized = StateConflict
category EpochMismatch = StateConflict
category InvalidTransition = StateConflict
category UpgradeNotAllowed = StateConflict
category RollbackNotAllowed = StateConflict
category AssetAlreadySpent = ResourceConflict
category AssetNotOwned = ResourceConflict
category StorageAlias = ResourceConflict
category InvariantViolation = ResourceConflict
category GasExhausted = ExecutionConflict
category DecodeError = ExecutionConflict
category ArithmeticError = ExecutionConflict
category Revert = ExecutionConflict
category ProofMissing = ExecutionConflict
category ExternalCallForbidden = PolicyConflict
category UnsafeEntryContext = PolicyConflict
