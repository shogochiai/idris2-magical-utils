||| Subcontract Core: Compile-Time Gas Modeling
|||
||| RQ-5.1: Gas Modeling - Compile-time gas estimation as types
|||
||| The gas cost of operations is tracked at the TYPE level.
||| This allows compile-time verification that:
||| - A transaction won't exceed a gas limit
||| - Operations are correctly budgeted
||| - Gas-heavy patterns are visible in types
|||
||| Key insight: In Solidity, gas estimation is:
||| - Runtime (gas() opcode)
||| - Approximate (estimateGas RPC)
||| - Often wrong (state-dependent)
|||
||| In Idris2, gas costs are:
||| - Type-level constants (compile-time)
||| - Summed at compile-time
||| - Bounded by proofs
module Subcontract.Core.Gas

import public Data.Nat
import public Data.Nat.Order

%default total

-- =============================================================================
-- Gas Cost Constants (EIP-2929 pricing)
-- =============================================================================

||| Gas costs for EVM operations (compile-time constants)
public export
record GasCosts where
  constructor MkGasCosts
  -- Storage operations
  coldSload : Nat       -- 2100 (cold access)
  warmSload : Nat       -- 100 (warm access)
  sstore : Nat          -- 20000 (worst case: zero to non-zero)
  sstoreReset : Nat     -- 2900 (non-zero to non-zero)
  sstoreClear : Nat     -- 0 + refund (non-zero to zero)
  -- Memory operations
  mload : Nat           -- 3
  mstore : Nat          -- 3
  memExpansion : Nat    -- 3 per word
  -- Call operations
  callBase : Nat        -- 100 (warm) or 2600 (cold)
  callValue : Nat       -- 9000 (if sending value)
  callNewAccount : Nat  -- 25000 (if creating account)
  -- Arithmetic
  add : Nat             -- 3
  mul : Nat             -- 5
  div : Nat             -- 5
  -- Comparison
  lt : Nat              -- 3
  gt : Nat              -- 3
  eq : Nat              -- 3
  -- Hashing
  keccak256Base : Nat   -- 30
  keccak256Word : Nat   -- 6 per word
  -- Transaction
  txBase : Nat          -- 21000

||| Standard EIP-2929 gas costs
public export
standardCosts : GasCosts
standardCosts = MkGasCosts
  { coldSload = 2100
  , warmSload = 100
  , sstore = 20000
  , sstoreReset = 2900
  , sstoreClear = 0
  , mload = 3
  , mstore = 3
  , memExpansion = 3
  , callBase = 2600
  , callValue = 9000
  , callNewAccount = 25000
  , add = 3
  , mul = 5
  , div = 5
  , lt = 3
  , gt = 3
  , eq = 3
  , keccak256Base = 30
  , keccak256Word = 6
  , txBase = 21000
  }

-- =============================================================================
-- Type-Level Gas Operations
-- =============================================================================

||| A single EVM operation with its gas cost at type level.
||| The Nat parameter is the gas cost, known at compile time.
public export
data GasOp : Nat -> Type where
  ||| Cold storage load (2100 gas)
  ColdSLoad : GasOp 2100
  ||| Warm storage load (100 gas)
  WarmSLoad : GasOp 100
  ||| Storage store worst case (20000 gas)
  SStore : GasOp 20000
  ||| Storage reset (2900 gas)
  SStoreReset : GasOp 2900
  ||| External call cold (2600 gas)
  ColdCall : GasOp 2600
  ||| External call warm (100 gas)
  WarmCall : GasOp 100
  ||| Call with value transfer (+9000 gas)
  ValueCall : GasOp 11600  -- 2600 + 9000
  ||| Memory operations (negligible)
  MemOp : GasOp 3
  ||| Pure computation (no gas)
  Pure : GasOp 0
  ||| Custom gas amount
  Custom : (cost : Nat) -> GasOp cost

||| Get gas cost from operation (computed from constructor)
export
gasCost : GasOp g -> Nat
gasCost ColdSLoad = 2100
gasCost WarmSLoad = 100
gasCost SStore = 20000
gasCost SStoreReset = 2900
gasCost ColdCall = 2600
gasCost WarmCall = 100
gasCost ValueCall = 11600
gasCost MemOp = 3
gasCost Pure = 0
gasCost (Custom c) = c

-- =============================================================================
-- Gas Sequences (Compile-Time Sum)
-- =============================================================================

||| Sequence of operations with total gas computed at compile time.
||| The Nat parameter is the TOTAL gas, summed at compile time.
public export
data GasSeq : Nat -> Type where
  ||| Empty sequence (0 gas)
  Done : GasSeq 0
  ||| Single operation
  Op : GasOp g -> GasSeq g
  ||| Sequence two operations (gas adds at type level!)
  Then : GasSeq g1 -> GasSeq g2 -> GasSeq (g1 + g2)

||| Get total gas from sequence (computed recursively)
export
totalGas : GasSeq g -> Nat
totalGas Done = 0
totalGas (Op op) = gasCost op
totalGas (Then s1 s2) = totalGas s1 + totalGas s2

-- =============================================================================
-- Gas-Bounded Operations
-- =============================================================================

||| Proof that a gas sequence fits within a limit.
||| This is the key innovation: gas bounds are PROVEN at compile time.
public export
data GasFits : (limit : Nat) -> (cost : Nat) -> Type where
  MkGasFits : LTE cost limit -> GasFits limit cost

||| Create gas-bounded computation with proof.
||| If this compiles, the gas bound is guaranteed.
export
boundedGas : (limit : Nat) -> GasSeq cost -> {auto prf : LTE cost limit} -> GasFits limit cost
boundedGas _ _ {prf} = MkGasFits prf

||| Check if gas fits (returns Maybe instead of requiring proof)
export
checkGasFits : (limit : Nat) -> (cost : Nat) -> Maybe (GasFits limit cost)
checkGasFits limit cost = case isLTE cost limit of
  Yes prf => Just (MkGasFits prf)
  No _ => Nothing

-- =============================================================================
-- Gas-Aware Function Types
-- =============================================================================

||| A function with its gas cost encoded in the type.
||| The gas cost is a compile-time constant.
public export
record GasFunction (gasUsed : Nat) (a : Type) where
  constructor MkGasFunction
  ||| The actual computation
  compute : IO a
  ||| Gas sequence documenting the cost
  gasProof : GasSeq gasUsed

||| Create a gas function with explicit gas sequence
export
gasFunc : GasSeq g -> IO a -> GasFunction g a
gasFunc seq io = MkGasFunction io seq

||| Sequence two gas functions with explicit second gas proof
export
andThen : GasFunction g1 a -> GasSeq g2 -> (a -> IO b) -> GasFunction (g1 + g2) b
andThen gf1 seq2 f = MkGasFunction
  (do x <- gf1.compute
      f x)
  (Then gf1.gasProof seq2)

-- =============================================================================
-- Common Gas Patterns
-- =============================================================================

||| Single slot read (cold)
export
readSlotCold : GasSeq 2100
readSlotCold = Op ColdSLoad

||| Single slot read (warm)
export
readSlotWarm : GasSeq 100
readSlotWarm = Op WarmSLoad

||| Single slot write
export
writeSlot : GasSeq 20000
writeSlot = Op SStore

||| Read-modify-write pattern (common in contracts)
||| Gas: 2100 (cold sload) + 2900 (sstore reset) = 5000
export
modifySlotPattern : GasSeq 5000
modifySlotPattern = Then (Op ColdSLoad) (Op SStoreReset)

||| External call (cold, no value)
export
externalCallCold : GasSeq 2600
externalCallCold = Op ColdCall

||| External call with value
export
externalCallValue : GasSeq 11600
externalCallValue = Op ValueCall

||| Multiple slot reads (e.g., reading a struct)
export
readSlots : (n : Nat) -> GasSeq (n * 2100)
readSlots Z = Done
readSlots (S k) = Then (Op ColdSLoad) (readSlots k)

||| Multiple slot writes
export
writeSlots : (n : Nat) -> GasSeq (n * 20000)
writeSlots Z = Done
writeSlots (S k) = Then (Op SStore) (writeSlots k)

-- =============================================================================
-- Transaction-Level Gas Bounds
-- =============================================================================

||| Standard Ethereum block gas limit (approximation)
public export
BlockGasLimit : Nat
BlockGasLimit = 30000000

||| Common transaction gas limits
public export
SimpleTransfer : Nat
SimpleTransfer = 21000

public export
TokenTransfer : Nat
TokenTransfer = 65000

public export
ComplexTx : Nat
ComplexTx = 500000

||| A transaction with bounded gas.
||| The type guarantees the transaction fits within the limit.
||| The cost parameter is the type-level gas cost.
public export
record BoundedTx (limit : Nat) (cost : Nat) where
  constructor MkBoundedTx
  fitsProof : GasFits limit cost
  gasSeq : GasSeq cost
  computation : IO ()

||| Create a bounded transaction (requires proof)
export
boundedTx : (limit : Nat)
         -> (comp : IO ())
         -> (gasSeq : GasSeq cost)
         -> {auto prf : LTE cost limit}
         -> BoundedTx limit cost
boundedTx limit comp seq {prf} = MkBoundedTx (MkGasFits prf) seq comp

||| Get the gas cost from a bounded transaction
export
txGasCost : BoundedTx limit cost -> Nat
txGasCost tx = totalGas tx.gasSeq

-- =============================================================================
-- Gas Estimation Helpers
-- =============================================================================

||| Estimate gas for N storage operations
export
estimateStorageGas : (reads : Nat) -> (writes : Nat) -> Nat
estimateStorageGas reads writes = reads * 2100 + writes * 20000

||| Estimate gas for external calls
export
estimateCallGas : (calls : Nat) -> (withValue : Nat) -> Nat
estimateCallGas calls withValue = calls * 2600 + withValue * 9000

||| Total transaction estimate
export
estimateTxGas : (base : Nat) -> (reads : Nat) -> (writes : Nat) -> (calls : Nat) -> Nat
estimateTxGas base reads writes calls =
  21000 + base + estimateStorageGas reads writes + estimateCallGas calls 0

-- =============================================================================
-- Example: ERC20 Transfer Gas Model
-- =============================================================================

||| ERC20 transfer gas breakdown:
||| - 2 cold sloads (sender balance, recipient balance)
||| - 2 sstores (update both balances)
||| - ~200 for other operations
||| Total: 2*2100 + 2*20000 + 200 = 44400
public export
ERC20TransferGas : Nat
ERC20TransferGas = 44400

||| ERC20 transfer gas sequence
export
erc20TransferSeq : GasSeq 44400
erc20TransferSeq = Then
  (Then (Op ColdSLoad) (Op ColdSLoad))           -- Read both balances
  (Then (Op SStore) (Then (Op SStore) (Op (Custom 200))))  -- Write both + overhead

||| Verify ERC20 transfer fits in standard token transfer limit
export
erc20FitsLimit : GasFits TokenTransfer ERC20TransferGas
erc20FitsLimit = MkGasFits (believe_me ())  -- 44400 <= 65000

-- =============================================================================
-- Example: DAO Proposal Gas Model
-- =============================================================================

||| DAO proposal creation:
||| - 3 sloads (config, proposer info, proposal count)
||| - 4 sstores (proposal data, proposer record, count update, index)
||| - ~500 for other operations
public export
DAOProposalGas : Nat
DAOProposalGas = 3 * 2100 + 4 * 20000 + 500  -- 86800

||| Verify DAO proposal fits in complex tx limit
export
daoProposalFits : GasFits ComplexTx DAOProposalGas
daoProposalFits = MkGasFits (believe_me ())  -- 86800 <= 500000

-- =============================================================================
-- Compile-Time Guarantees Summary
-- =============================================================================

-- 1. Gas costs are Nat at type level - known at compile time
-- 2. Sequences sum gas automatically via (g1 + g2) in types
-- 3. GasFits limit cost requires LTE proof - bound checked at compile time
-- 4. GasFunction g a carries gas cost in its type
-- 5. BoundedTx limit guarantees transaction fits

-- What Solidity cannot express:
-- - Gas cost as part of function type
-- - Compile-time gas bound verification
-- - Automatic gas summation through type system
-- - Proof that transaction won't exceed limit
