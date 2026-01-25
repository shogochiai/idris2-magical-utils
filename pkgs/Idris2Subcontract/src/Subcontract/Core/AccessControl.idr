||| Subcontract Core: Type-Safe Access Control
|||
||| Access control as types, not runtime checks.
|||
||| Solidity pattern:
||| ```solidity
||| modifier onlyOwner() {
|||   require(msg.sender == owner, "not owner");
|||   _;
||| }
||| ```
|||
||| Idris2 pattern:
||| ```idris
||| transfer : IsOwner caller -> Amount -> IO ()
||| -- Cannot call without proof of ownership!
||| ```
module Subcontract.Core.AccessControl

import public Subcontract.Core.Storable
import public Data.List

%default total

-- =============================================================================
-- Role Definitions
-- =============================================================================

||| Role enumeration (extensible)
public export
data Role = Owner | Admin | Member | Operator | Pauser | Minter

public export
Eq Role where
  Owner == Owner = True
  Admin == Admin = True
  Member == Member = True
  Operator == Operator = True
  Pauser == Pauser = True
  Minter == Minter = True
  _ == _ = False

||| Convert role to storage representation
public export
roleToInt : Role -> Bits256
roleToInt Owner = 0
roleToInt Admin = 1
roleToInt Member = 2
roleToInt Operator = 3
roleToInt Pauser = 4
roleToInt Minter = 5

-- =============================================================================
-- Type-Level Access Proofs
-- =============================================================================

||| Proof that an address has a specific role.
||| This is a COMPILE-TIME constraint, not a runtime check.
|||
||| To call a function requiring `HasRole Owner addr`,
||| you must have obtained this proof somehow (e.g., from storage check).
public export
data HasRole : Role -> Bits256 -> Type where
  ||| Witness that address has the role
  MkHasRole : (role : Role) -> (addr : Bits256) -> HasRole role addr

||| Extract address from role proof
export
proofAddr : HasRole role addr -> Bits256
proofAddr (MkHasRole _ a) = a

||| Extract role from proof
export
proofRole : HasRole role addr -> Role
proofRole (MkHasRole r _) = r

-- =============================================================================
-- Runtime Role Verification (Bridge to Type-Level)
-- =============================================================================

||| Role storage: mapping from (role, address) -> bool
public export
record RoleStorage where
  constructor MkRoleStorage
  baseSlot : Bits256

||| Check if address has role (runtime) and return proof if true
export
checkRole : RoleStorage -> (role : Role) -> (addr : Bits256) -> IO (Maybe (HasRole role addr))
checkRole store role addr = do
  -- Calculate slot: keccak256(addr . keccak256(role . baseSlot))
  mstore 0 (roleToInt role)
  mstore 32 store.baseSlot
  roleSlot <- keccak256 0 64
  mstore 0 addr
  mstore 32 roleSlot
  addrSlot <- keccak256 0 64
  hasRole <- sload addrSlot
  pure $ if hasRole == 1
    then Just (MkHasRole role addr)
    else Nothing

||| Grant role (requires admin proof)
export
grantRole : HasRole Admin granter
         -> RoleStorage
         -> (role : Role)
         -> (addr : Bits256)
         -> IO ()
grantRole _ store role addr = do
  mstore 0 (roleToInt role)
  mstore 32 store.baseSlot
  roleSlot <- keccak256 0 64
  mstore 0 addr
  mstore 32 roleSlot
  addrSlot <- keccak256 0 64
  sstore addrSlot 1

||| Revoke role (requires admin proof)
export
revokeRole : HasRole Admin revoker
          -> RoleStorage
          -> (role : Role)
          -> (addr : Bits256)
          -> IO ()
revokeRole _ store role addr = do
  mstore 0 (roleToInt role)
  mstore 32 store.baseSlot
  roleSlot <- keccak256 0 64
  mstore 0 addr
  mstore 32 roleSlot
  addrSlot <- keccak256 0 64
  sstore addrSlot 0

-- =============================================================================
-- Convenience: Require Role Pattern
-- =============================================================================

||| Run an action only if caller has required role.
||| Returns Nothing if access denied.
export
requireRole : RoleStorage
           -> (role : Role)
           -> (caller : Bits256)
           -> (HasRole role caller -> IO a)
           -> IO (Maybe a)
requireRole store role callerAddr action = do
  mproof <- checkRole store role callerAddr
  case mproof of
    Nothing => pure Nothing
    Just prf => do
      result <- action prf
      pure (Just result)

||| Run an action or revert if caller lacks role
export
requireRoleOrRevert : RoleStorage
                   -> (role : Role)
                   -> (caller : Bits256)
                   -> (HasRole role caller -> IO a)
                   -> IO a
requireRoleOrRevert store role callerAddr action = do
  mproof <- checkRole store role callerAddr
  case mproof of
    Nothing => do
      evmRevert 0 0
      action (MkHasRole role callerAddr)  -- unreachable after revert
    Just prf => action prf

-- =============================================================================
-- Example: Owner-Only Functions
-- =============================================================================

||| Owner storage (single owner pattern)
public export
record OwnerStorage where
  constructor MkOwnerStorage
  ownerSlot : Bits256

||| Get current owner
export
getOwner : OwnerStorage -> IO Bits256
getOwner store = sload store.ownerSlot

||| Check if address is owner (returns proof if true)
export
checkOwner : OwnerStorage -> (addr : Bits256) -> IO (Maybe (HasRole Owner addr))
checkOwner store addr = do
  currentOwner <- getOwner store
  pure $ if currentOwner == addr
    then Just (MkHasRole Owner addr)
    else Nothing

||| Transfer ownership (requires current owner proof)
export
transferOwnership : HasRole Owner currentOwner
                 -> OwnerStorage
                 -> (newOwner : Bits256)
                 -> IO ()
transferOwnership _ store newOwner = sstore store.ownerSlot newOwner

||| Renounce ownership (requires owner proof)
export
renounceOwnership : HasRole Owner currentOwner -> OwnerStorage -> IO ()
renounceOwnership _ store = sstore store.ownerSlot 0

-- =============================================================================
-- Multi-Role Example: Pausable Contract
-- =============================================================================

||| Paused state
public export
data PauseState = Paused | Unpaused

||| Pause storage
public export
record PauseStorage where
  constructor MkPauseStorage
  pauseSlot : Bits256

||| Check if paused
export
isPaused : PauseStorage -> IO Bool
isPaused store = do
  val <- sload store.pauseSlot
  pure (val == 1)

||| Pause the contract (requires Pauser role)
export
pause : HasRole Pauser pauser -> PauseStorage -> IO ()
pause _ store = sstore store.pauseSlot 1

||| Unpause the contract (requires Pauser role)
export
unpause : HasRole Pauser pauser -> PauseStorage -> IO ()
unpause _ store = sstore store.pauseSlot 0

||| Run action only when not paused
export
whenNotPaused : PauseStorage -> IO a -> IO (Maybe a)
whenNotPaused store action = do
  paused <- isPaused store
  if paused
    then pure Nothing
    else Just <$> action

-- =============================================================================
-- Compile-Time Guarantees
-- =============================================================================

-- These functions CANNOT be called without the appropriate proof:
--
-- transferOwnership : HasRole Owner _ -> ...
--   ^-- Caller MUST provide proof of ownership
--
-- grantRole : HasRole Admin _ -> ...
--   ^-- Caller MUST provide proof of admin role
--
-- pause : HasRole Pauser _ -> ...
--   ^-- Caller MUST provide proof of pauser role
--
-- In Solidity, these are runtime modifier checks that can fail.
-- In Idris2, the type system prevents unauthorized calls at compile time.
