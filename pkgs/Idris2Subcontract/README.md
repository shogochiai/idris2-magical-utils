# idris2-subcontract

**Subcontract framework for Idris2 - UCS patterns on idris2-yul**

Provides ERC-7546 UCS (Upgradeable Clone for Scalable contracts) implementation and standard functions for building modular smart contracts in Idris2.

## Overview

idris2-subcontract provides:

- **ERC-7546 Proxy**: DELEGATECALL-based proxy forwarding to dictionary
- **Dictionary Contract**: Function selector -> implementation mapping
- **Standard Functions**: FeatureToggle, Clone, Receive
- **Type-Safe API**: ABI signatures, decoders, and entry points
- **Storage Capability**: Controlled storage access via `StorageCap`
- **Type-Safe Storage**: Phantom-typed `Ref`, `Storable` interface, `Schema` derivation
- **Compile-Time Invariants**: Bounded values, NonZero divisors, TokenBalance proofs
- **Access Control as Types**: `HasRole` proofs instead of runtime `require()`
- **Reentrancy Protection**: Linear-style `Lock` types prevent reentrancy at compile time
- **State Machine Proofs**: `ValidTransition` ensures only valid state changes compile
- **Typed Failures**: `Outcome a = Ok a | Fail Conflict Evidence` - no untyped exceptions
- **Entry Context Policy**: `EntryCtx` restricts operations in receive/fallback/callback
- **CEI Indexed Monad**: `EvmM Phase` enforces Checks-Effects-Interactions at type level
- **Saga Pattern**: `Compensable` operations with automatic rollback on failure

## Installation

### Using pack

Add to your `pack.toml`:

```toml
[custom.all.idris2-subcontract]
type = "local"
path = "/path/to/idris2-subcontract"
ipkg = "idris2-subcontract.ipkg"
```

Then build:

```bash
pack build idris2-subcontract
```

### Dependencies

- [idris2-yul](https://github.com/shogochiai/idris2-yul) - Idris2 to EVM/Yul compiler

## Module Structure

```
Subcontract/
├── Standards/
│   └── ERC7546/              # ERC-7546 implementation
│       ├── Slots.idr         # Constants (DICTIONARY_SLOT, etc.)
│       ├── Forward.idr       # Proxy forwarding logic
│       ├── Proxy.idr         # Proxy exports
│       └── Dictionary.idr    # Dictionary contract
├── Core/
│   ├── Proxy.idr             # Re-exports Standards.ERC7546.Proxy
│   ├── Dictionary.idr        # Re-exports Standards.ERC7546.Dictionary
│   ├── Entry.idr             # Type-safe entry points
│   ├── StorageCap.idr        # Storage capability token
│   ├── Storable.idr          # Type → slot layout derivation
│   ├── Schema.idr            # Schema definitions for storage
│   ├── Derived.idr           # Schema derivation, state machines
│   ├── Invariants.idr        # Bounded, NonZero, TokenBalance
│   ├── AccessControl.idr     # HasRole proofs, RBAC
│   ├── Reentrancy.idr        # Lock types for reentrancy guard
│   ├── Call.idr              # Type-safe external calls
│   ├── Gas.idr               # Compile-time gas modeling
│   ├── Effects.idr           # Effect-typed entries, CEI safety
│   ├── Epoch.idr             # Epoch-indexed upgrades
│   ├── LinearAsset.idr       # Linear-persistent assets
│   ├── Conflict.idr          # Failure classification (finite sum)
│   ├── Evidence.idr          # Observable failure data
│   ├── Outcome.idr           # Result normal form (Ok/Fail)
│   ├── Resolve.idr           # Recovery procedures, compensation
│   ├── EntryCtx.idr          # Entry context (receive/fallback/callback)
│   ├── EvmM.idr              # Indexed monad with CEI phases
│   ├── Idempotent.idr        # Idempotence, compensable operations
│   └── ABI/
│       ├── Sig.idr           # Function signatures
│       └── Decoder.idr       # Calldata decoding
└── Std/
    └── Functions/
        ├── FeatureToggle.idr # Admin feature toggle
        ├── Clone.idr         # EIP-1167 proxy cloning
        └── Receive.idr       # ETH receive handling
```

## Quick Start

### Basic Proxy Contract

```idris
import Subcontract.Core.Proxy

main : IO ()
main = proxyMain
```

### Type-Safe Entry Points

```idris
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder

-- Define signature
addMemberSig : Sig
addMemberSig = MkSig "addMember(address,bytes32)"

addMemberSel : Sel addMemberSig
addMemberSel = MkSel 0x12345678

-- Create entry point
addMemberEntry : Entry addMemberSig
addMemberEntry = MkEntry addMemberSel $ do
  (addr, meta) <- runDecoder (decodeAddress <&> decodeBytes32)
  idx <- addMemberImpl (addrValue addr) (bytes32Value meta)
  returnUint idx

-- Dispatch
main : IO ()
main = dispatch [entry addMemberEntry]
```

### Storage Capability Pattern

```idris
import Subcontract.Core.StorageCap

-- Handler receives StorageCap from framework
myHandler : Handler Integer
myHandler cap = do
  val <- sloadCap cap SLOT_DATA
  sstoreCap cap SLOT_DATA (val + 1)
  pure val

-- Framework provides capability
main : IO ()
main = do
  result <- runHandler myHandler
  returnUint result
```

### Feature Toggle

```idris
import Subcontract.Std.Functions.FeatureToggle

myFunction : IO ()
myFunction = do
  shouldBeActive 0x12345678  -- Revert if disabled
  -- ... function logic
```

### Type-Safe Storage (Storable)

```idris
import Subcontract.Core.Storable

-- Define a record with automatic slot layout
record Member where
  constructor MkMember
  addr : Bits256
  meta : Bits256

Storable Member where
  slotCount = 2
  toSlots m = [m.addr, m.meta]
  fromSlots [a, m] = MkMember a m

-- Phantom-typed reference ensures type safety
getMember : Ref Member -> IO Member
getMember ref = get ref  -- Type guarantees correct slot count
```

### Compile-Time Invariants

```idris
import Subcontract.Core.Invariants

-- Bounded values: value <= cap proven at compile time
maxSupply : Bounded 1000000
maxSupply = MkBounded 500000  -- Compiles: 500000 <= 1000000

-- NonZero: safe division without runtime checks
safeDiv : Nat -> NonZero -> Nat  -- Total function!

-- TokenBalance: balance <= totalSupply by construction
transfer : {supply : Nat}
        -> (amount : Nat)
        -> (sender : TokenBalance supply)
        -> (recipient : TokenBalance supply)
        -> (hasEnough : LTE amount (balance sender))      -- Proof required!
        -> (noOverflow : LTE (balance recipient + amount) supply)
        -> (TokenBalance supply, TokenBalance supply)
```

### Access Control as Types

```idris
import Subcontract.Core.AccessControl

-- Functions require proof of role - not runtime check!
transferOwnership : HasRole Owner currentOwner  -- Must provide proof
                 -> OwnerStorage
                 -> (newOwner : Bits256)
                 -> IO ()

-- Obtain proof at runtime, use at compile-time
main : IO ()
main = do
  caller <- getCaller
  mproof <- checkRole roleStore Owner caller
  case mproof of
    Nothing => revert  -- No proof available
    Just prf => transferOwnership prf store newOwner  -- Proof provided
```

### Reentrancy Protection (Linear Types)

```idris
import Subcontract.Core.Reentrancy

-- Lock is CONSUMED when acquired - prevents reentrancy at compile time
withdraw : Lock Unlocked -> Amount -> IO (Lock Unlocked, Bool)
withdraw lock amount = do
  locked <- acquireLock lock     -- Lock Unlocked -> Lock Locked
  success <- call recipient amount
  unlocked <- releaseLock locked -- Lock Locked -> Lock Unlocked
  pure (unlocked, success)

-- This CANNOT compile - lock is already consumed:
-- badWithdraw lock = withLock_ lock $ \_ => badWithdraw lock
--                                           ^-- Error: lock consumed!
```

### Type-Safe External Calls

```idris
import Subcontract.Core.Call

-- Typed call with explicit return type
result <- typedCall @Bits256 (callTo target gas) calldata
case result of
  CallSuccess balance => use balance  -- Already typed!
  CallReverted _ => handleError

-- Safe ERC20 transfer
success <- safeTransfer token recipient amount gas
```

### Compile-Time Gas Modeling

```idris
import Subcontract.Core.Gas

-- Gas cost is part of the TYPE
erc20TransferSeq : GasSeq 44400  -- Compile-time constant
erc20TransferSeq = Then
  (Then (Op ColdSLoad) (Op ColdSLoad))  -- Read balances
  (Then (Op SStore) (Op SStore))        -- Write balances

-- Prove gas fits limit at compile time
erc20FitsLimit : GasFits TokenTransfer ERC20TransferGas
erc20FitsLimit = MkGasFits prf  -- LTE 44400 65000

-- Create bounded transaction - won't compile if exceeds limit!
tx : BoundedTx ComplexTx
tx = boundedTx ComplexTx computation gasSeq
```

### Effect-Typed Entries (CEI Safety)

```idris
import Subcontract.Core.Effects

-- Declare effects at type level
transferEffects : EffectList
transferEffects = [SLoad balanceSlot, SStore balanceSlot, Log 3]

-- CEI safety proof required - unsafe patterns won't compile
entry : {auto prf : CEISafe effs} -> EffectEntry effs

-- Write-before-Call detected at COMPILE TIME
unsafeEffects = [SStore slot, Call target]  -- Cannot get CEISafe proof!
```

### Epoch-Indexed Upgrades

```idris
import Subcontract.Core.Epoch

-- Functions tagged with epoch
myFunctionV1 : EpochFunction E0
myFunctionV2 : EpochFunction E1  -- Different type!

-- Migration with type-checked transformation
migrate : EpochState E0 OldData -> EpochState E1 NewData

-- Upgrade only allowed E0 -> E1 (no skipping)
upgrade : EpochTransition E0 E1 -> ...
```

### Linear-Persistent Assets

```idris
import Subcontract.Core.LinearAsset

-- SpendToken is LINEAR - use exactly once
spend : AssetStorage -> SpendToken asset -> IO ()  -- Token consumed

-- Double-spend impossible: token gone after first use
vote : SpendToken votingPower -> Choice -> IO ()
vote token choice = spend store token  -- Can't use token again!

-- Lock/unlock for escrow
lockAsset : SpendToken asset -> IO (LockToken asset)
unlockAsset : LockToken asset -> IO (SpendToken asset)
```

### Typed Failures (Outcome + Conflict)

```idris
import Subcontract.Core.Outcome
import Subcontract.Core.Conflict
import Subcontract.Core.Evidence

-- Every operation returns Outcome - never throws
transfer : Amount -> IO (Outcome Bool)
transfer amount = do
  balance <- getBalance caller
  if balance < amount
    then pure $ Fail InsufficientBalance (tagEvidence "transfer")
    else do
      updateBalances amount
      pure $ Ok True

-- Conflict is a FINITE sum - no "unknown" errors
data Conflict = Revert | Reentrancy | AuthViolation | ...

-- Evidence captures observable data for recovery
record Evidence where
  digest : Bits256
  tags : List String
  sloadSlots : List Bits256  -- Slots read
  sstoreSlots : List Bits256 -- Slots written
```

### Recovery Procedures (Resolve)

```idris
import Subcontract.Core.Resolve

-- Resolution after failure
data Resolution a = Recovered a | Escalate | Abort | Retry Nat

-- Conflict-specific resolution
resolveOutcome : Outcome a -> Resolution a
resolveOutcome (Ok x) = Recovered x
resolveOutcome (Fail GasExhausted _) = Retry 3  -- Idempotent: retry
resolveOutcome (Fail Reentrancy e) = Escalate e -- Security: escalate
resolveOutcome (Fail c e) = Abort c e           -- Otherwise: abort

-- Saga pattern with automatic rollback
runSaga : Saga a -> IO (Outcome ())
```

### Entry Context Policy (EntryCtx)

```idris
import Subcontract.Core.EntryCtx

-- Track HOW we got here
data EntryCtx = DirectCall | Receive | Fallback | ERC721Receive | ...

-- Policy restricts what's allowed
policyOf Receive = minimalPolicy     -- No storage, no calls
policyOf Fallback = conservativePolicy
policyOf DirectCall = fullPolicy

-- Guard operations by context
myReceive : EntryCtx -> IO (Outcome ())
myReceive ctx = do
  case checkStorage ctx of  -- Storage forbidden in Receive!
    Fail c e => pure (Fail c e)
    Ok () => doStorageOp
```

### Indexed Monad (EvmM) with CEI Phases

```idris
import Subcontract.Core.EvmM

-- Phase transitions are TYPE-CHECKED
data Phase = PreCheck | EffectsDone | ExternalDone | Final

-- EvmM tracks phase at type level
data EvmM : (pre : Phase) -> (post : Phase) -> Type -> Type

-- require only in PreCheck
require'' : Bool -> String -> EvmM PreCheck PreCheck ()

-- sstore transitions PreCheck -> EffectsDone
sstore' : Bits256 -> Bits256 -> EvmM PreCheck EffectsDone ()

-- call transitions EffectsDone -> ExternalDone
call' : Bits256 -> ... -> EvmM EffectsDone ExternalDone Bool

-- CEI-safe transaction enforced by TYPES
CEITransaction : Type -> Type
CEITransaction a = EvmM PreCheck Final a
```

### Idempotent and Compensable Operations

```idris
import Subcontract.Core.Idempotent

-- Mark operation as idempotent (safe to retry)
data IdempotentOp a = MkIdempotent Nat a  -- maxRetry, operation

-- Run with automatic retry on gas exhaustion
runIdempotent : IdempotentOp (IO (Outcome a)) -> IO (Outcome a)

-- Compensable: has inverse operation
record Compensable a where
  forward : IO (Outcome a)
  inverse : a -> IO (Outcome ())

-- Saga: sequence with automatic rollback
sequenceWithRollback : List (a ** Compensable a) -> IO (Outcome ())
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        idris2-subcontract                           │
├─────────────────────────────────────────────────────────────────────┤
│  Subcontract.Std.Functions.*     Application-level functions       │
│  Subcontract.Core.*              Framework core (Entry, StorageCap)│
│  Subcontract.Standards.ERC7546.* ERC-7546 implementation           │
└──────────────────────────────┬──────────────────────────────────────┘
                               │ imports
┌──────────────────────────────▼──────────────────────────────────────┐
│                          idris2-yul                                 │
├─────────────────────────────────────────────────────────────────────┤
│  EVM.Primitives              All EVM FFI definitions               │
│  EVM.Storage.Namespace       ERC-7201 slot calculations            │
│  EVM.ABI.*                   ABI encoding/decoding                 │
│  Compiler.EVM.*              Yul code generation                   │
└─────────────────────────────────────────────────────────────────────┘
```

### Layer Responsibilities

| Layer | Package | Responsibility |
|-------|---------|----------------|
| FFI | idris2-yul | `%foreign "evm:*"` primitives |
| Storage | idris2-yul | ERC-7201 slot calculations |
| Standards | idris2-subcontract | ERC-7546 proxy/dictionary |
| Framework | idris2-subcontract | Entry points, capabilities |
| Application | your-project | Business logic |

## ERC-7546 UCS Pattern

The Upgradeable Clone for Scalable contracts pattern:

```
┌─────────────┐     DELEGATECALL     ┌────────────────┐
│    Proxy    │ ──────────────────► │   Dictionary   │
│  (ERC-7546) │                      │ selector→impl  │
└─────────────┘                      └───────┬────────┘
                                             │
                    ┌────────────────────────┼────────────────────────┐
                    │                        │                        │
              ┌─────▼─────┐           ┌──────▼──────┐          ┌──────▼──────┐
              │FeatureToggle│         │    Clone    │          │   Receive   │
              │  function  │          │  function   │          │  function   │
              └────────────┘          └─────────────┘          └─────────────┘
```

## Solidity vs Idris2 Comparison

| Concern | Solidity (Runtime) | Idris2 (Compile-Time) |
|---------|-------------------|----------------------|
| Non-negative | `uint256` (implicit) | `Nat` / `Amount` |
| Bounded value | `require(x <= cap)` | `Bounded cap` type |
| Non-zero divisor | `require(d != 0)` | `NonZero` type |
| Balance invariant | hope + audit | `TokenBalance supply` |
| Access control | `require(hasRole)` | `HasRole role addr` proof |
| Reentrancy guard | `bool _locked` + modifier | `Lock Unlocked/Locked` |
| State machine | enum + require | `PropTransition from to` |
| External calls | `(bool, bytes)` + decode | `CallResult a` typed |
| Gas estimation | runtime / RPC estimate | `GasSeq n` compile-time |
| CEI pattern | hope + audit | `CEISafe effs` proof |
| Upgrades | proxy swap, pray | `EpochTransition` typed |
| Double-spend | trust + mutex | `SpendToken` linear |
| Error handling | try/catch, strings | `Outcome a = Ok a \| Fail Conflict Evidence` |
| Failure class | string/custom error | `Conflict` finite sum type |
| Recovery | revert (lose gas) | `Resolution`: Retry/Escalate/Compensate |
| Entry context | hope receive() is safe | `EntryCtx` policy enforces limits |
| CEI ordering | audit patterns | `EvmM Phase` indexed monad |
| Retry safety | manual idempotency | `IdempotentOp` with auto-retry |
| Rollback | manual undo | `Compensable` with Saga pattern |

**Key insight**: In Solidity, invariants are runtime `require()` checks that can fail.
In Idris2, invariants are types - violation is a compile error.

## Related Projects

- [idris2-yul](https://github.com/shogochiai/idris2-yul) - Idris2 to EVM/Yul compiler
- [idris2-textdao](https://github.com/ecdysisxyz/idris2-textdao) - Example application
- [EIP-7546](https://eips.ethereum.org/EIPS/eip-7546) - UCS Proxy Standard

## Documentation

- [API Reference](docs/API.md) - Module API documentation
- [FR Theory Paper](docs/FR_Theory.md) - Formal FR calculus foundations (monadic laws, seven implications)
- [Failure-Recovery Theory](docs/FR.md) - FR calculus implementation guide
- [Storage Guide](docs/STORAGE.md) - EVM storage layout guide
- [Architecture](docs/ARCHITECTURE.md) - Layer design and rationale
- [Troubleshooting](docs/TROUBLESHOOTING.md) - Common integration issues

## License

MIT
