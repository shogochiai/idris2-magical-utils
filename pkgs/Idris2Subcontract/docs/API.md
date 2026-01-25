# API Reference

## Core Modules

### Subcontract.Core.Storable

Type-safe storage with phantom-typed references.

```idris
-- Interface for types that can be stored in EVM storage
interface Storable a where
  slotCount : Nat                        -- Number of 256-bit slots needed
  toSlots : a -> Vect slotCount Bits256  -- Serialize to slots
  fromSlots : Vect slotCount Bits256 -> a -- Deserialize from slots

-- Phantom-typed storage reference
data Ref : Type -> Type where
  MkRef : Bits256 -> Ref a

-- Storage operations
get : Storable a => Ref a -> IO a
set : Storable a => Ref a -> a -> IO ()
```

**Example:**
```idris
record Member where
  constructor MkMember
  addr : Bits256
  meta : Bits256

Storable Member where
  slotCount = 2
  toSlots m = [m.addr, m.meta]
  fromSlots [a, m] = MkMember a m
```

---

### Subcontract.Core.Schema

Schema definitions for storage layout documentation.

```idris
-- Field definition
record Field where
  constructor MkField
  fieldName : String
  fieldType : String
  fieldSlot : Nat

-- Schema definition
record Schema where
  constructor MkSchema
  schemaName : String
  schemaFields : List Field
```

---

### Subcontract.Core.Derived

Schema derivation and state machine proofs.

#### HasSchema Interface

```idris
interface Storable a => HasSchema a where
  schema : Schema
  fieldNames : Vect (slotCount {a}) String
```

#### State Machine Proofs

```idris
-- Proposal states
data PropState = PropDraft | PropVoting | PropApproved | PropRejected | PropExecuted

-- Valid transitions (only these compile!)
data PropTransition : PropState -> PropState -> Type where
  Submit  : PropTransition PropDraft PropVoting
  Approve : PropTransition PropVoting PropApproved
  Reject  : PropTransition PropVoting PropRejected
  Execute : PropTransition PropApproved PropExecuted

-- Type-indexed proposal
data StatefulProposal : PropState -> Type where
  MkStatefulProposal : (id : Bits256) -> (state : PropState) -> StatefulProposal state

-- Transition function
transitionProposal : PropTransition from to -> StatefulProposal from -> IO (StatefulProposal to)
```

#### Workflow Steps

```idris
-- Chain multiple transitions
data WorkflowStep : PropState -> PropState -> Type where
  SingleStep : PropTransition from to -> WorkflowStep from to
  ChainSteps : {mid : PropState} -> PropTransition from mid -> WorkflowStep mid to -> WorkflowStep from to

-- Pre-defined workflows
fullApprovalWorkflow : WorkflowStep PropDraft PropExecuted  -- Draft -> Voting -> Approved -> Executed
rejectionWorkflow : WorkflowStep PropDraft PropRejected     -- Draft -> Voting -> Rejected
```

---

### Subcontract.Core.Invariants

Compile-time invariants via Curry-Howard correspondence.

#### Bounded Values

```idris
-- Value proven to be <= cap at compile time
record Bounded (cap : Nat) where
  constructor MkBounded
  value : Nat
  {auto inBounds : LTE value cap}

mkBounded : (cap : Nat) -> (n : Nat) -> Maybe (Bounded cap)
boundedAdd : Bounded cap -> (delta : Nat) -> (fits : LTE (value + delta) cap) -> Bounded cap
boundedSub : Bounded cap -> (delta : Nat) -> (fits : LTE delta value) -> Bounded cap
```

#### NonZero

```idris
-- Non-zero natural number (safe division)
data NonZero : Type where
  MkNonZero : (n : Nat) -> {auto prf : IsSucc n} -> NonZero

mkNonZero : (n : Nat) -> Maybe NonZero
safeDiv : Nat -> NonZero -> Nat  -- Total! No runtime check needed
safeMod : Nat -> NonZero -> Nat
```

#### TokenBalance

```idris
-- Balance proven to be <= totalSupply
record TokenBalance (totalSupply : Nat) where
  constructor MkTokenBalance
  balance : Nat
  {auto balanceValid : LTE balance totalSupply}

-- Transfer with compile-time proofs
transfer : {supply : Nat}
        -> (amount : Nat)
        -> (sender : TokenBalance supply)
        -> (recipient : TokenBalance supply)
        -> (hasEnough : LTE amount (balance sender))
        -> (noOverflow : LTE (balance recipient + amount) supply)
        -> (TokenBalance supply, TokenBalance supply)
```

#### ValidAllowance

```idris
-- Allowance proven to be <= owner's balance
record ValidAllowance (ownerBalance : Nat) where
  constructor MkAllowance
  allowance : Nat
  {auto notExceedBalance : LTE allowance ownerBalance}

decreaseAllowance : (amount : Nat) -> ValidAllowance bal -> (hasAllowance : LTE amount allowance) -> ValidAllowance bal
```

#### Optimized Storage

```idris
-- Pack multiple values into one slot
record PackedAccount where
  constructor MkPackedAccount
  packedData : Bits256  -- [128 balance][64 nonce][64 flags]

packAccount : Bits256 -> Bits256 -> Bool -> PackedAccount
unpackBalance : PackedAccount -> Bits256
unpackNonce : PackedAccount -> Bits256
unpackFrozen : PackedAccount -> Bool
```

---

### Subcontract.Core.AccessControl

Type-safe access control with role proofs.

#### Roles

```idris
data Role = Owner | Admin | Member | Operator | Pauser | Minter
```

#### HasRole Proof

```idris
-- Proof that address has role (compile-time constraint)
data HasRole : Role -> Bits256 -> Type where
  MkHasRole : (role : Role) -> (addr : Bits256) -> HasRole role addr

-- Get proof at runtime
checkRole : RoleStorage -> (role : Role) -> (addr : Bits256) -> IO (Maybe (HasRole role addr))
```

#### Role Management

```idris
-- Requires Admin proof to grant/revoke
grantRole : HasRole Admin granter -> RoleStorage -> Role -> Bits256 -> IO ()
revokeRole : HasRole Admin revoker -> RoleStorage -> Role -> Bits256 -> IO ()
```

#### Owner Pattern

```idris
record OwnerStorage where
  constructor MkOwnerStorage
  ownerSlot : Bits256

checkOwner : OwnerStorage -> Bits256 -> IO (Maybe (HasRole Owner addr))
transferOwnership : HasRole Owner current -> OwnerStorage -> Bits256 -> IO ()
renounceOwnership : HasRole Owner current -> OwnerStorage -> IO ()
```

#### Pausable Pattern

```idris
record PauseStorage where
  constructor MkPauseStorage
  pauseSlot : Bits256

isPaused : PauseStorage -> IO Bool
pause : HasRole Pauser pauser -> PauseStorage -> IO ()
unpause : HasRole Pauser pauser -> PauseStorage -> IO ()
whenNotPaused : PauseStorage -> IO a -> IO (Maybe a)
```

---

### Subcontract.Core.Reentrancy

Linear-style reentrancy protection.

#### Lock States

```idris
data LockState = Unlocked | Locked

-- Type-indexed lock
data Lock : LockState -> Type where
  MkUnlocked : Bits256 -> Lock Unlocked
  MkLocked : Bits256 -> Lock Locked
```

#### State Transitions

```idris
-- CONSUMES Unlocked, PRODUCES Locked
acquireLock : Lock Unlocked -> IO (Lock Locked)

-- CONSUMES Locked, PRODUCES Unlocked
releaseLock : Lock Locked -> IO (Lock Unlocked)
```

#### Protected Execution

```idris
-- Execute with lock held
withLock : Lock Unlocked -> (Lock Locked -> IO a) -> IO (a, Lock Unlocked)
withLock_ : Lock Unlocked -> (Lock Locked -> IO a) -> IO a

-- Try to get unlocked state
tryUnlock : LockStorage -> IO (Maybe (Lock Unlocked))
```

#### Multi-Lock

```idris
data LockId = WithdrawLock | SwapLock | FlashLoanLock | CustomLock Bits256

record MultiLockStorage where
  constructor MkMultiLockStorage
  baseLockSlot : Bits256

getLock : MultiLockStorage -> LockId -> IO (Either (Lock Locked) (Lock Unlocked))
withResourceLock : MultiLockStorage -> LockId -> (Lock Locked -> IO a) -> IO (Maybe a)
```

#### Safe Withdraw Example

```idris
safeWithdraw : Lock Unlocked -> Bits256 -> Bits256 -> IO (Lock Unlocked, Bool)
safeBatchWithdraw : Lock Unlocked -> List (Bits256, Bits256) -> IO (Lock Unlocked, Nat)
```

---

### Subcontract.Core.Call

Type-safe external calls (CALL, DELEGATECALL, STATICCALL).

#### Call Result

```idris
-- Explicit success or revert
data CallResult : Type -> Type where
  CallSuccess : a -> CallResult a
  CallReverted : (returnSize : Bits256) -> CallResult a

isSuccess : CallResult a -> Bool
fromCallResult : a -> CallResult a -> a
```

#### Call Specification

```idris
record CallSpec where
  constructor MkCallSpec
  target : Bits256
  value : Bits256
  gasLimit : Bits256

callTo : (target : Bits256) -> (gas : Bits256) -> CallSpec
callWithValue : (target : Bits256) -> (value : Bits256) -> (gas : Bits256) -> CallSpec
```

#### Typed Calls

```idris
-- Return type is decoded automatically via Storable
typedCall : Storable a => CallSpec -> Calldata -> IO (CallResult a)
typedDelegatecall : Storable a => Bits256 -> Bits256 -> Calldata -> IO (CallResult a)
typedStaticcall : Storable a => Bits256 -> Bits256 -> Calldata -> IO (CallResult a)

-- Call or revert
callOrRevert : Storable a => CallSpec -> Calldata -> IO a
tryCall : Storable a => a -> CallSpec -> Calldata -> IO a
```

#### ERC20 Helpers

```idris
safeTransfer : Bits256 -> Bits256 -> Bits256 -> Bits256 -> IO Bool
safeTransferFrom : Bits256 -> Bits256 -> Bits256 -> Bits256 -> Bits256 -> IO Bool
```

---

### Subcontract.Core.Gas

Compile-time gas modeling via type-level naturals.

#### Gas Operations

```idris
-- Gas cost is a type-level Nat
data GasOp : Nat -> Type where
  ColdSLoad : GasOp 2100
  WarmSLoad : GasOp 100
  SStore : GasOp 20000
  SStoreReset : GasOp 2900
  ColdCall : GasOp 2600
  WarmCall : GasOp 100
  ValueCall : GasOp 11600
  MemOp : GasOp 3
  Pure : GasOp 0
  Custom : (cost : Nat) -> GasOp cost
```

#### Gas Sequences

```idris
-- Total gas is computed at compile time
data GasSeq : Nat -> Type where
  Done : GasSeq 0
  Op : GasOp g -> GasSeq g
  Then : GasSeq g1 -> GasSeq g2 -> GasSeq (g1 + g2)

totalGas : GasSeq g -> Nat
```

#### Gas Bounds

```idris
-- Proof that gas fits within limit
data GasFits : (limit : Nat) -> (cost : Nat) -> Type where
  MkGasFits : LTE cost limit -> GasFits limit cost

boundedGas : (limit : Nat) -> GasSeq cost -> {auto prf : LTE cost limit} -> GasFits limit cost
checkGasFits : (limit : Nat) -> (cost : Nat) -> Maybe (GasFits limit cost)
```

#### Bounded Transactions

```idris
-- Cost is type-level, limit is verified at compile time
record BoundedTx (limit : Nat) (cost : Nat) where
  constructor MkBoundedTx
  fitsProof : GasFits limit cost
  gasSeq : GasSeq cost
  computation : IO ()

boundedTx : (limit : Nat) -> IO () -> GasSeq cost -> {auto prf : LTE cost limit} -> BoundedTx limit cost
txGasCost : BoundedTx limit cost -> Nat
```

#### Common Patterns

```idris
readSlotCold : GasSeq 2100
writeSlot : GasSeq 20000
modifySlotPattern : GasSeq 5000  -- sload + sstore
externalCallCold : GasSeq 2600

-- N-slot operations
readSlots : (n : Nat) -> GasSeq (n * 2100)
writeSlots : (n : Nat) -> GasSeq (n * 20000)
```

#### Estimation Helpers

```idris
estimateStorageGas : (reads : Nat) -> (writes : Nat) -> Nat
estimateCallGas : (calls : Nat) -> (withValue : Nat) -> Nat
estimateTxGas : (base : Nat) -> (reads : Nat) -> (writes : Nat) -> (calls : Nat) -> Nat
```

---

### Subcontract.Core.Effects

Effect-typed entries with CEI (Checks-Effects-Interactions) safety proofs.

#### Effect Types

```idris
data Effect : Type where
  SLoad : SlotId -> Effect          -- Storage read
  SStore : SlotId -> Effect         -- Storage write
  Call : TargetId -> Effect         -- External call
  DelegateCall : TargetId -> Effect
  StaticCall : TargetId -> Effect
  Pure : Effect                     -- No effect
  MemEffect : Effect                -- Memory only
  Log : Nat -> Effect               -- Event emission
```

#### CEI Safety Proofs

```idris
-- Proof that no Write appears before Call
data CEISafe : EffectList -> Type

-- Proof that no Write follows
data NoWriteAfter : EffectList -> Type

-- Check at runtime
checkCEISafe : EffectList -> Maybe (CEISafe effs)
```

#### Effect-Typed Entry

```idris
record EffectEntry (effs : EffectList) where
  constructor MkEffectEntry
  sig : FnSig
  handler : IO ()

-- Create entry with CEI proof required
safeEntry : FnSig -> (effs : EffectList) -> {auto prf : CEISafe effs} -> IO () -> EffectEntry effs
```

#### Slot Collision Detection

```idris
slotCollision : EffectList -> EffectList -> Maybe SlotId
data NoCollision : EffectList -> EffectList -> Type
```

---

### Subcontract.Core.Epoch

Epoch-indexed state and upgrade safety.

#### Epoch Type

```idris
data Epoch : Type where
  Genesis : Epoch
  Next : Epoch -> Epoch

-- Aliases
E0 : Epoch  -- Genesis
E1 : Epoch  -- Next Genesis
E2 : Epoch  -- Next E1
```

#### Epoch-Indexed State

```idris
data EpochState : Epoch -> Type -> Type
data EpochRef : Epoch -> Type -> Type

epochGet : Storable a => EpochRef e a -> IO (EpochState e a)
epochSet : Storable a => EpochRef e a -> a -> IO ()
```

#### Epoch Transitions

```idris
-- Only sequential upgrades allowed
data EpochTransition : Epoch -> Epoch -> Type where
  Upgrade : EpochTransition e (Next e)

-- Migration function type
Migration : Epoch -> Epoch -> Type -> Type -> Type

-- Perform upgrade
upgrade : EpochTransition e1 e2 -> Migration e1 e2 a b -> EpochState e1 a -> EpochState e2 b
```

#### Epoch-Guarded Execution

```idris
epochGuardedEntry : (expected : Epoch) -> IO () -> IO ()
upgradeEntry : (from : Epoch) -> (to : Epoch) -> {auto trans : EpochTransition from to} -> IO () -> IO ()
```

---

### Subcontract.Core.LinearAsset

Linear-persistent assets with double-spend prevention.

#### Asset Identity

```idris
record AssetId where
  constructor MkAssetId
  id : Bits256
  assetType : Bits256
```

#### Spend Token (Linear)

```idris
-- Token that authorizes spending ONCE
data SpendToken : AssetId -> Type

-- Acquire if unspent and owner matches
acquireSpendToken : AssetStorage -> AssetId -> Bits256 -> IO (Maybe (SpendToken asset))
```

#### Core Operations

```idris
-- Consume token, mark spent
spend : AssetStorage -> SpendToken asset -> IO ()

-- Consume token, transfer ownership
transfer : AssetStorage -> SpendToken asset -> Bits256 -> IO ()

-- Consume token, burn permanently
burn : AssetStorage -> SpendToken asset -> IO ()
```

#### Lock/Unlock (Escrow)

```idris
data LockToken : AssetId -> Type

lockAsset : AssetStorage -> SpendToken asset -> IO (LockToken asset)
unlockAsset : AssetStorage -> LockToken asset -> IO (SpendToken asset)
```

#### Proofs

```idris
data IsUnspent : AssetId -> Type
data Owns : Bits256 -> AssetId -> Type

checkUnspent : AssetStorage -> AssetId -> IO (Maybe (IsUnspent asset))
checkOwns : AssetStorage -> Bits256 -> AssetId -> IO (Maybe (Owns caller asset))
```

---

## Standards Modules

### Subcontract.Standards.ERC7546

ERC-7546 Upgradeable Clone for Scalable contracts implementation.

- `Slots.idr` - DICTIONARY_SLOT constant
- `Forward.idr` - Proxy forwarding via DELEGATECALL
- `Proxy.idr` - Complete proxy contract
- `Dictionary.idr` - Selector to implementation mapping

---

## Standard Functions

### Subcontract.Std.Functions.FeatureToggle

```idris
shouldBeActive : Bits256 -> IO ()  -- Revert if feature disabled
```

### Subcontract.Std.Functions.Clone

EIP-1167 minimal proxy cloning.

### Subcontract.Std.Functions.Receive

ETH receive handling.
