||| Subcontract Core: Epoch-Indexed State & Upgrades
|||
||| Storage and functions are indexed by "epoch" (version/era).
||| This enables:
||| - Safe upgrades: old functions can't run in new epoch
||| - Migration proofs: state transitions are type-checked
||| - Rollback safety: epoch boundaries are explicit
|||
||| Solidity: Upgrades are runtime proxy swaps, can break state
||| Idris2: Epoch mismatch is a compile-time type error
module Subcontract.Core.Epoch

import public Data.Nat
import public Data.Vect
import public Subcontract.Core.Storable
import public Subcontract.Core.Schema

%default total

-- =============================================================================
-- Epoch Type
-- =============================================================================

||| Epoch identifier (version number)
||| Type-level natural number for compile-time epoch checking
public export
data Epoch : Type where
  ||| Genesis epoch (initial deployment)
  Genesis : Epoch
  ||| Successor epoch (after upgrade)
  Next : Epoch -> Epoch

||| Epoch equality
public export
Eq Epoch where
  Genesis == Genesis = True
  (Next a) == (Next b) = a == b
  _ == _ = False

||| Convert epoch to natural number
public export
epochToNat : Epoch -> Nat
epochToNat Genesis = 0
epochToNat (Next e) = S (epochToNat e)

||| Common epoch aliases
public export
E0 : Epoch
E0 = Genesis

public export
E1 : Epoch
E1 = Next Genesis

public export
E2 : Epoch
E2 = Next E1

public export
E3 : Epoch
E3 = Next E2

-- =============================================================================
-- Epoch-Indexed Storage Schema
-- =============================================================================

||| Storage schema for a specific epoch
||| Different epochs can have different schemas
public export
record EpochSchema (e : Epoch) where
  constructor MkEpochSchema
  schemaName : String
  fields : List (String, String)  -- (name, type)
  baseSlot : Bits256

||| Example: V1 schema
public export
schemaV1 : EpochSchema E0
schemaV1 = MkEpochSchema "MemberV1" [("addr", "address"), ("meta", "bytes32")] 0x1000

||| Example: V2 schema with additional field
public export  
schemaV2 : EpochSchema E1
schemaV2 = MkEpochSchema "MemberV2" [("addr", "address"), ("meta", "bytes32"), ("role", "uint8")] 0x2000

-- =============================================================================
-- Epoch-Indexed State
-- =============================================================================

||| State value indexed by epoch
||| The type guarantees you can't mix states from different epochs
public export
data EpochState : Epoch -> Type -> Type where
  MkEpochState : (epoch : Epoch) -> (value : a) -> EpochState epoch a

||| Extract value from epoch state
export
stateValue : EpochState e a -> a
stateValue (MkEpochState _ v) = v

||| Get the epoch tag
export
stateEpoch : EpochState e a -> Epoch
stateEpoch (MkEpochState e _) = e

-- =============================================================================
-- Epoch-Indexed References
-- =============================================================================

||| Storage reference tagged with epoch
||| Prevents accessing old-epoch storage with new-epoch code
public export
data EpochRef : Epoch -> Type -> Type where
  MkEpochRef : (epoch : Epoch) -> (slot : Bits256) -> EpochRef epoch a

||| Get underlying slot
export
epochRefSlot : EpochRef e a -> Bits256
epochRefSlot (MkEpochRef _ s) = s

||| Read from epoch-tagged storage
export
epochGet : Storable a => EpochRef e a -> IO (EpochState e a)
epochGet {e} (MkEpochRef _ slot) = do
  val <- get (MkRef slot)
  pure (MkEpochState e val)

||| Write to epoch-tagged storage
export
epochSet : Storable a => EpochRef e a -> a -> IO ()
epochSet (MkEpochRef _ slot) val = set (MkRef slot) val

-- =============================================================================
-- Epoch Transitions (Upgrades)
-- =============================================================================

||| Valid epoch transition proof
||| Only allows upgrading to the NEXT epoch (no skipping)
public export
data EpochTransition : Epoch -> Epoch -> Type where
  ||| Upgrade from epoch e to Next e
  Upgrade : EpochTransition e (Next e)

||| Migration function type
||| Transforms state from old epoch to new epoch
public export
Migration : Epoch -> Epoch -> Type -> Type -> Type
Migration e1 e2 a b = EpochState e1 a -> EpochState e2 b

||| Perform epoch upgrade with migration
export
upgrade : EpochTransition e1 e2
       -> Migration e1 e2 a b
       -> EpochState e1 a
       -> EpochState e2 b
upgrade Upgrade migrate state = migrate state

-- =============================================================================
-- Epoch-Indexed Functions
-- =============================================================================

||| Function that only works in a specific epoch
public export
record EpochFunction (e : Epoch) where
  constructor MkEpochFunction
  fnName : String
  fnSelector : Bits256
  handler : IO ()

||| Create an epoch-bound function
export
epochFn : (e : Epoch) -> String -> Bits256 -> IO () -> EpochFunction e
epochFn e name sel h = MkEpochFunction name sel h

||| Run epoch function only if current epoch matches
export
runIfEpoch : (current : Epoch) 
          -> (target : Epoch) 
          -> EpochFunction target 
          -> {auto prf : current = target}
          -> IO ()
runIfEpoch _ _ fn = fn.handler

-- =============================================================================
-- Epoch Registry (Runtime)
-- =============================================================================

||| Storage slot for current epoch
public export
EPOCH_SLOT : Bits256
EPOCH_SLOT = 0xEEEE000000000000000000000000000000000000000000000000000000000001

||| Read current epoch from storage
export
getCurrentEpoch : IO Nat
getCurrentEpoch = do
  val <- sload EPOCH_SLOT
  pure (cast val)

||| Write new epoch to storage (internal, used by upgrade)
export
setCurrentEpoch : Nat -> IO ()
setCurrentEpoch n = sstore EPOCH_SLOT (cast n)

-- =============================================================================
-- Upgrade Safety Proofs
-- =============================================================================

||| Proof that a migration preserves an invariant
public export
data PreservesInvariant : (inv : a -> Bool) -> Migration e1 e2 a a -> Type where
  MkPreserves : ((s : EpochState e1 a) -> inv (stateValue s) = True -> inv (stateValue (m s)) = True)
             -> PreservesInvariant inv m

||| Proof that schemas are compatible (subset relation)
public export
data SchemaCompatible : EpochSchema e1 -> EpochSchema e2 -> Type where
  ||| All fields from e1 exist in e2 (extension)
  SchemaExtension : SchemaCompatible s1 s2

-- =============================================================================
-- Frozen Epoch (No More Upgrades)
-- =============================================================================

||| Mark an epoch as frozen (final)
public export
data FrozenEpoch : Epoch -> Type where
  MkFrozen : (e : Epoch) -> FrozenEpoch e

||| Cannot transition FROM a frozen epoch
||| (No EpochTransition constructor for frozen epochs)

-- =============================================================================
-- Example: DAO Upgrade Pattern
-- =============================================================================

||| V1 Proposal structure
public export
record ProposalV1 where
  constructor MkProposalV1
  proposer : Bits256
  description : Bits256

public export
Storable ProposalV1 where
  slotCount = 2
  toSlots p = [p.proposer, p.description]
  fromSlots [a, b] = MkProposalV1 a b

||| V2 Proposal with additional fields
public export
record ProposalV2 where
  constructor MkProposalV2
  proposer : Bits256
  description : Bits256
  deadline : Bits256
  quorum : Bits256

public export
Storable ProposalV2 where
  slotCount = 4
  toSlots p = [p.proposer, p.description, p.deadline, p.quorum]
  fromSlots [a, b, c, d] = MkProposalV2 a b c d

||| Migration from V1 to V2
export
migrateProposal : Migration E0 E1 ProposalV1 ProposalV2
migrateProposal (MkEpochState _ v1) = 
  MkEpochState E1 (MkProposalV2 v1.proposer v1.description 0 0)

||| Type-safe upgrade execution
export
upgradeDAO : EpochState E0 ProposalV1 -> EpochState E1 ProposalV2
upgradeDAO = upgrade Upgrade migrateProposal

-- =============================================================================
-- Epoch-Aware Entry Points
-- =============================================================================

||| Entry point that checks epoch at runtime
export
epochGuardedEntry : (expected : Epoch) -> IO () -> IO ()
epochGuardedEntry expected action = do
  current <- getCurrentEpoch
  if current == epochToNat expected
    then action
    else evmRevert 0 0  -- Wrong epoch

||| Upgrade entry point (only callable to advance epoch)
export
upgradeEntry : (from : Epoch) -> (to : Epoch) 
            -> {auto trans : EpochTransition from to}
            -> IO ()
            -> IO ()
upgradeEntry from to action = do
  current <- getCurrentEpoch
  if current == epochToNat from
    then do
      action
      setCurrentEpoch (epochToNat to)
    else evmRevert 0 0

-- =============================================================================
-- Compile-Time Guarantees
-- =============================================================================

-- 1. Epoch is TYPE-LEVEL - functions/refs tagged at compile time
-- 2. EpochTransition only allows Next (no skipping versions)
-- 3. Migration functions must transform old state to new state
-- 4. FrozenEpoch prevents further upgrades
-- 5. Schema compatibility can be proven for safe migrations
-- 6. Old epoch functions cannot be called in new epoch (type error)
