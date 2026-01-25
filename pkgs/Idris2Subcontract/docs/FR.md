# Failure-Recovery (FR) Theory Implementation

This document describes the Failure-Recovery calculus implementation in idris2-subcontract, based on the formal FR theory for world-computer virtual machines.

## Overview

The FR framework transforms smart contract error handling from "fail and hope" to **"classify, observe, and recover"**. The core insight is:

> **Composability is the existence of recovery-preserving morphisms, not the abundance of success paths.**

In practical terms: every failure must be:
1. **Classified** (finite sum type, no "Unknown")
2. **Observable** (evidence always produced)
3. **Localized** (bounded by entry context)
4. **Recoverable** (resolution procedures exist)

## FR Theory → Idris2 Mapping

| FR Theory | Mathematical | Idris2 Type | Module |
|-----------|--------------|-------------|--------|
| Failure Surface | F | `Conflict` | `Core.Conflict` |
| Evidence | E | `Evidence` | `Core.Evidence` |
| Result | R = (V×E) ∪ (F×E) | `Outcome a` | `Core.Outcome` |
| Boundary | b ∈ B | `EntryCtx` | `Core.EntryCtx` |
| Handler | H_b | `Resolution` | `Core.Resolve` |
| Phase | execution phase | `Phase` | `Core.EvmM` |
| Recovery | compensation | `Compensable` | `Core.Idempotent` |
| EVM Bridge | serialization | `FR.*` | `Core.FR` |

## Module Architecture

```
Subcontract.Core.FR (EVM Bridge)
         │
         ├── Conflict      (F: Failure classification)
         ├── Evidence      (E: Observable failure data)
         ├── Outcome       (R: Result normal form)
         ├── Resolve       (H_b: Recovery procedures)
         ├── EntryCtx      (b: Boundary/context)
         ├── EvmM          (Indexed monad with phases)
         └── Idempotent    (Retry/compensation patterns)
```

## Core Types

### Conflict (Failure Surface)

```idris
-- Finite sum of ALL failure modes (no "Unknown")
data Conflict : Type where
  Revert              : Conflict  -- Explicit revert
  Reentrancy          : Conflict  -- Reentrancy detected
  AuthViolation       : Conflict  -- Access control failure
  CEIViolation        : Conflict  -- Checks-Effects-Interactions violation
  EpochMismatch       : Conflict  -- Upgrade safety violation
  InitAlready         : Conflict  -- Already initialized
  NotInitialized      : Conflict  -- Not yet initialized
  StorageAlias        : Conflict  -- Slot collision
  ExternalCallForbidden : Conflict  -- Call not allowed in context
  UnsafeEntryContext  : Conflict  -- Dangerous entry point
  GasExhausted        : Conflict  -- Out of gas
  DecodeError         : Conflict  -- ABI decode failure
  ArithmeticError     : Conflict  -- Overflow/underflow/div-by-zero
  UpgradeNotAllowed   : Conflict  -- Upgrade blocked
  RollbackNotAllowed  : Conflict  -- Rollback blocked
  AssetAlreadySpent   : Conflict  -- Double-spend attempt
  AssetNotOwned       : Conflict  -- Ownership violation
  InvariantViolation  : Conflict  -- Bounds/balance violation
  InvalidTransition   : Conflict  -- State machine violation
  ProofMissing        : Conflict  -- Required proof not provided
```

**Key principle**: No `Unknown` constructor. Every failure must be classifiable.

### Evidence (Observable Data)

```idris
record Evidence where
  constructor MkEvidence
  digest : Bits256           -- Hash of relevant data
  tags : List String         -- Human-readable context
  sloadSlots : List Bits256  -- Storage slots read
  sstoreSlots : List Bits256 -- Storage slots written
  calls : List (Bits256, Bits256)  -- External calls made
```

Evidence is **always produced**, even on failure. This enables:
- Post-mortem debugging
- Replay and verification
- Recovery procedure selection

### Outcome (Result Normal Form)

```idris
data Outcome : Type -> Type where
  Ok   : a -> Outcome a
  Fail : Conflict -> Evidence -> Outcome a
```

**Critical difference from Solidity**:
- Solidity: `try/catch` with untyped `bytes` error data
- Idris2: `Outcome a` with typed `Conflict` + structured `Evidence`

### EntryCtx (Boundary)

```idris
data EntryCtx : Type where
  DirectCall        : EntryCtx  -- Normal function call
  Receive           : EntryCtx  -- ETH receive (empty calldata)
  Fallback          : EntryCtx  -- Unknown selector
  ERC721Receive     : EntryCtx  -- onERC721Received callback
  ERC1155Receive    : EntryCtx  -- onERC1155Received callback
  ERC1155BatchReceive : EntryCtx
  ForcedEther       : EntryCtx  -- SELFDESTRUCT victim
  DelegateContext   : EntryCtx  -- Inside delegatecall
  StaticContext     : EntryCtx  -- Inside staticcall
```

Each context has a **policy** restricting allowed operations:

```idris
policyOf : EntryCtx -> CtxPolicy

policyOf DirectCall = fullPolicy      -- All operations allowed
policyOf Receive = minimalPolicy      -- No storage, no calls
policyOf Fallback = conservativePolicy
policyOf StaticContext = readOnlyPolicy
```

### Resolution (Recovery Procedures)

```idris
data Resolution : Type -> Type where
  Recovered : a -> Resolution a           -- Successfully recovered
  Escalate  : Conflict -> Evidence -> Resolution a  -- Needs human
  Abort     : Conflict -> Evidence -> Resolution a  -- Unrecoverable
  Retry     : Nat -> Resolution a         -- Idempotent, can retry
```

## EVM Integration (FR.idr)

The `FR` module bridges FR types to EVM semantics:

### Revert Data Encoding

```idris
-- Layout: [4 bytes selector][32 bytes conflict][32 bytes digest]
encodeFailureToMemory : Conflict -> Evidence -> IO (Bits256, Bits256)

-- FR-compliant revert (REPLACES `evmRevert 0 0`)
revertWithFailure : Conflict -> Evidence -> IO ()

-- Simplified version
revertConflict : Conflict -> IO ()
```

**Why revert data?** EVM logs are LOST on revert. Evidence must go in revert data to survive failure.

### FR-Aware Dispatch

```idris
-- Old (Entry.idr): Unknown selector loses all evidence
evmRevert 0 0

-- New (FR.idr): Unknown selector is classified with evidence
dispatchFR : Bits256 -> List (Bits256, IO ()) -> IO ()
dispatchFR sel handlers = go handlers
  where
    go [] = revertWithFailure DecodeError (selectorEvidence sel)
    go ((s, h) :: rest) = if s == sel then h else go rest
```

### FR-Aware Guards

```idris
-- Returns Outcome (composable)
requireFR : Bool -> Conflict -> String -> Outcome ()

-- Reverts immediately with evidence
requireFR' : Bool -> Conflict -> String -> IO ()

-- Context-aware guards
guardStorageFR : EntryCtx -> IO (Outcome ())
guardExternalFR : EntryCtx -> IO (Outcome ())
```

### FR Computation Runners

```idris
-- Run computation, handle outcome
runFR : IO (Outcome a) -> (a -> IO ()) -> IO ()

-- Run and return value (revert on failure)
runFRReturn : Storable a => IO (Outcome a) -> IO ()

-- Sequence FR computations
bindFR : IO (Outcome a) -> (a -> IO (Outcome b)) -> IO (Outcome b)
```

## Seven Implications

The FR theory yields seven implications, all implemented:

### 1. Composability = Morphism Existence

A protocol is composable iff its failure modes admit a handler-complete interface.

```idris
interface FRComposable (handler : Type) where
  failureSurface : List Conflict   -- All failure modes
  producesEvidence : Bool          -- Evidence always produced
```

### 2. Safety ⊥ Composability

Safety (no funds lost) and composability (recovery-preserving morphisms) are orthogonal:
- Unsafe + Composable: failures occur but are classified and recoverable
- Safe + Non-composable: correct in isolation, but exports "Unknown" gaps

### 3. Oracles as Morphism Existence

Oracle problems become: *does a recovery-preserving morphism exist for disagreement states?*

```idris
-- Oracle should return:
data OracleResult a
  = OracleValue a Evidence           -- Value with provenance
  | OracleDisagreement Conflict Evidence  -- Classified disagreement
```

### 4. Human Intervention as Extension

A **Gap** is non-existence of a required morphism. Human annotation = defining a new handler branch.

```idris
-- Resolution may require human input
Resolve_b : Conflict -> Evidence -> Resolution a

-- Some resolutions need human decision
case resolve Reentrancy evidence of
  Escalate c e => awaitHumanDecision c e  -- Gap filled by human
```

### 5. Good vs Bad Failure Growth

- **Recoverable growth**: expanding F while expanding H_b (recovery closure maintained)
- **Unrecoverable externality**: expanding F without handlers (failures leak)

### 6. Observability is Semantic

Every result carries evidence. Not debugging afterthought, but semantic requirement.

```idris
-- Evidence is ALWAYS produced
Outcome a = Ok a | Fail Conflict Evidence
              ^               ^
              |               └── Evidence even on failure
              └── Could add evidence to success too
```

### 7. EVM as Privileged Substrate

EVM provides:
- **Sequential semantics**: single trace, deterministic ordering
- **Synchronous calls**: pause/resume matches monadic bind
- **Revert atomicity**: built-in recovery primitive
- **Single regime**: no consensus bypass complexity

## Usage Examples

### Basic FR Handler

```idris
import Subcontract.Core.FR

transferFR : Bits256 -> Bits256 -> IO (Outcome Bool)
transferFR to amount = do
  balance <- getBalance caller
  if balance < amount
    then pure $ Fail InsufficientBalance
                     (tagsEvidence ["transfer", "insufficient"])
    else do
      updateBalances to amount
      pure $ Ok True

-- Entry point with FR semantics
main : IO ()
main = runFR transferFR (\success => returnBool success)
```

### Context-Guarded Operation

```idris
import Subcontract.Core.FR
import Subcontract.Core.EntryCtx

myReceive : EntryCtx -> IO (Outcome ())
myReceive ctx = do
  -- Guard storage access by context
  guardResult <- guardStorageFR ctx
  case guardResult of
    Fail c e => pure (Fail c e)  -- Propagate classified failure
    Ok () => do
      -- Storage allowed, proceed
      doStorageOperation
      pure (Ok ())
```

### FR-Aware Dispatch

```idris
import Subcontract.Core.FR

main : IO ()
main = do
  sel <- getSelector
  dispatchFR sel
    [ (0x12345678, handleTransfer)
    , (0x87654321, handleApprove)
    ]
  -- Unknown selector: reverts with DecodeError + selector evidence
```

### Saga Pattern (Compensable Operations)

```idris
import Subcontract.Core.Idempotent

transferSaga : Compensable Bits256
transferSaga = MkCompensable
  { forward = debit sender amount   -- Forward operation
  , inverse = \_ => credit sender amount  -- Compensation
  , description = "transfer debit"
  }

-- Sequence with automatic rollback
runSaga [transferSaga, recipientCredit] >>= \case
  Ok () => pure ()
  Fail c e => logFailure c e  -- All completed ops were rolled back
```

## Migration Guide

### From `evmRevert 0 0` to FR

**Before:**
```idris
myGuard : Bool -> IO ()
myGuard True = pure ()
myGuard False = evmRevert 0 0  -- No classification, no evidence
```

**After:**
```idris
myGuard : Bool -> IO ()
myGuard True = pure ()
myGuard False = revertWithFailure AuthViolation
                  (tagEvidence "myGuard failed")
```

### From `dispatch` to `dispatchFR`

**Before:**
```idris
dispatch : List (Bits256, IO ()) -> IO ()
dispatch handlers = do
  sel <- getSelector
  case lookup sel handlers of
    Just h => h
    Nothing => evmRevert 0 0  -- Unknown = lost evidence
```

**After:**
```idris
main : IO ()
main = do
  sel <- getSelector
  dispatchFR sel handlers  -- Unknown = DecodeError + evidence
```

## Conflict Encoding (ABI)

Conflicts are encoded as uint8 for EVM compatibility:

| Code | Conflict |
|------|----------|
| 0 | Revert |
| 1 | Reentrancy |
| 2 | AuthViolation |
| 3 | CEIViolation |
| 4 | EpochMismatch |
| 5 | InitAlready |
| 6 | NotInitialized |
| 7 | StorageAlias |
| 8 | ExternalCallForbidden |
| 9 | UnsafeEntryContext |
| 10 | GasExhausted |
| 11 | DecodeError |
| 12 | ArithmeticError |
| 13 | UpgradeNotAllowed |
| 14 | RollbackNotAllowed |
| 15 | AssetAlreadySpent |
| 16 | AssetNotOwned |
| 17 | InvariantViolation |
| 18 | InvalidTransition |
| 19 | ProofMissing |

Revert data layout:
```
[4 bytes: FR selector][32 bytes: conflict code][32 bytes: evidence digest]
```

## Theoretical Foundation

This implementation is based on the formal theory described in [FR_Theory.md](FR_Theory.md):

> **Failure–Recovery Calculus for World-Computer Virtual Machines: Monadic Laws, Seven Implications, and the EVM as a Privileged Experimental Substrate**

Key theoretical contributions:
- FR category with evidence-carrying morphisms
- Monad laws as recovery stability invariants
- Composability redefined as morphism existence
- EVM specialness explained via sequential semantics

See [FR_Theory.md](FR_Theory.md) for the complete mathematical formalization.

## Lifecycle Module (Deploy/Upgrade)

Deploy and Upgrade are meta-operations that change morphism existence. The `Core.Lifecycle` module provides FR-aware versions.

### Lifecycle States

```idris
data LifecycleState : Type where
  Undeployed  : LifecycleState  -- Not yet deployed
  Deployed    : LifecycleState  -- Deployed but not initialized
  Initialized : LifecycleState  -- Fully operational
  Upgraded    : LifecycleState  -- Upgrade pending commit/rollback
  Deprecated  : LifecycleState  -- No longer operational
  Paused      : LifecycleState  -- Temporarily non-operational
```

### Type-Level Transitions

```idris
data LifecycleTransition : LifecycleState -> LifecycleState -> Type where
  DoDeploy         : LifecycleTransition Undeployed Deployed
  DoInit           : LifecycleTransition Deployed Initialized
  DoUpgrade        : LifecycleTransition Initialized Upgraded
  DoCommitUpgrade  : LifecycleTransition Upgraded Initialized
  DoRollbackUpgrade: LifecycleTransition Upgraded Initialized
  DoDeprecate      : LifecycleTransition Initialized Deprecated
  DoPause          : LifecycleTransition Initialized Paused
  DoUnpause        : LifecycleTransition Paused Initialized
```

### Deploy Evidence

```idris
record DeployEvidence where
  bytecodeHash : Bits256   -- Hash of deployed code
  salt : Bits256           -- CREATE2 salt
  expectedAddr : Bits256   -- Expected address
  actualAddr : Bits256     -- Actual address (0 = failed)
  initParams : List Bits256
  deployer : Bits256
```

### Upgrade Evidence

```idris
record UpgradeEvidence where
  fromEpoch : Nat          -- Epoch before
  toEpoch : Nat            -- Epoch after
  oldImpl : Bits256        -- Old implementation
  newImpl : Bits256        -- New implementation
  selector : Bits256       -- Function being upgraded
  migrationHash : Bits256  -- Migration data hash
  upgrader : Bits256
```

### FR-Aware Operations

```idris
-- Deploy with full evidence
deployWithEvidence : Bits256 -> Bits256 -> Bits256 -> List Bits256
                   -> IO (Outcome (Bits256, DeployEvidence))

-- Upgrade with rollback capability
upgradeFR : Bits256 -> Bits256 -> IO (Outcome UpgradeEvidence)

-- Rollback using evidence
rollbackUpgrade : UpgradeEvidence -> IO (Outcome ())

-- Compensable upgrade (Saga pattern)
compensableUpgrade : Bits256 -> Bits256 -> IO (Outcome UpgradeCompensable)
compensateUpgrade : UpgradeCompensable -> IO (Outcome ())
```

### Lifecycle Guards

```idris
-- Require operational (initialized + not paused)
requireOperational : IO (Outcome ())

-- Guard any function with lifecycle check
withLifecycleGuard : IO (Outcome a) -> IO (Outcome a)
```

### Example: Safe Upgrade Flow

```idris
safeUpgrade : Bits256 -> Bits256 -> IO (Outcome ())
safeUpgrade selector newImpl = do
  -- 1. Create compensable upgrade
  result <- compensableUpgrade selector newImpl
  case result of
    Fail c e => pure $ Fail c e
    Ok uc => do
      -- 2. Run migrations
      migResult <- runMigrations
      case migResult of
        Fail c e => do
          -- 3a. Migration failed: rollback
          _ <- compensateUpgrade uc
          pure $ Fail c (addTag "migration failed, rolled back" e)
        Ok () => do
          -- 3b. Success: commit
          commitUpgrade
```

## Related Modules

| Module | Purpose |
|--------|---------|
| `Core.Lifecycle` | FR-aware Deploy/Upgrade with compensation |
| `Core.EvmM` | Indexed monad with CEI phase tracking |
| `Core.Effects` | Effect-typed entries with CEI safety proofs |
| `Core.Epoch` | Epoch-indexed upgrades |
| `Core.LinearAsset` | Linear-persistent assets (double-spend prevention) |
| `Core.AccessControl` | Role-based access with proof types |
| `Core.Reentrancy` | Lock types for reentrancy protection |

## License

MIT
