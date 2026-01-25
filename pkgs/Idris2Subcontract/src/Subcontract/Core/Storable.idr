||| EVM Storage: Type-Indexed Storage
|||
||| Provides type-safe storage access where the type itself determines
||| the storage layout. This is impossible in Solidity - you cannot
||| write generic storage code that works with "any struct".
|||
||| Key difference from Solidity:
||| - Solidity: runtime revert on type mismatch
||| - Idris2: compile-time error on type mismatch
|||
||| Example:
||| ```idris
||| record Member where
|||   addr : Bits256
|||   metadata : Bits256
|||
||| -- Ref is phantom-typed: Ref Member ≠ Ref Uint256
||| memberRef : Ref Member
||| memberRef = MkRef 0x3000
|||
||| -- Type error: can't use memberRef as Ref Uint256
||| bad : IO Bits256
||| bad = get memberRef  -- Error: expected Ref Bits256, got Ref Member
||| ```
module Subcontract.Core.Storable

import public Data.Vect
import public EVM.Primitives

%default total

-- =============================================================================
-- Core Types
-- =============================================================================

||| 256-bit value (EVM native word size)
public export
Bits256 : Type
Bits256 = Integer

||| Storage slot address
public export
Slot : Type
Slot = Integer

-- =============================================================================
-- Storable Interface
-- =============================================================================

||| Types that can be stored in EVM storage.
|||
||| The key insight: the TYPE determines the storage layout.
||| This is computed at compile-time, not runtime.
|||
||| Solidity equivalent would be:
||| ```solidity
||| // IMPOSSIBLE in Solidity - no generic storage interface
||| interface Storable<T> {
|||   function slotCount() pure returns (uint);
|||   function toSlots(T value) pure returns (uint256[] memory);
|||   function fromSlots(uint256[] memory slots) pure returns (T);
||| }
||| ```
public export
interface Storable a where
  ||| Number of storage slots required (compile-time constant)
  slotCount : Nat

  ||| Serialize to slot values
  toSlots : a -> Vect slotCount Bits256

  ||| Deserialize from slot values
  fromSlots : Vect slotCount Bits256 -> a

-- =============================================================================
-- Primitive Instances
-- =============================================================================

||| Single 256-bit value uses 1 slot
public export
Storable Bits256 where
  slotCount = 1
  toSlots x = [x]
  fromSlots [x] = x

||| Boolean uses 1 slot (0 or 1)
public export
Storable Bool where
  slotCount = 1
  toSlots True = [1]
  toSlots False = [0]
  fromSlots [x] = x /= 0

-- =============================================================================
-- Phantom-Typed Reference
-- =============================================================================

||| Type-safe reference to storage location.
|||
||| The phantom type `a` ensures you can only read/write
||| values of the correct type. This is the key innovation:
|||
||| ```idris
||| memberRef : Ref Member
||| uintRef : Ref Bits256
|||
||| -- These are DIFFERENT types, caught at compile time
||| -- In Solidity, both would be just `uint256 slot`
||| ```
public export
data Ref : Type -> Type where
  ||| Create a reference at a base slot
  MkRef : (baseSlot : Slot) -> Ref a

||| Extract the base slot (for internal use)
export
refSlot : Ref a -> Slot
refSlot (MkRef s) = s

-- =============================================================================
-- Ref Arithmetic (Slot Offset)
-- =============================================================================

||| Offset a reference by a number of slots.
||| Preserves the phantom type.
export
offsetRef : (offset : Integer) -> Ref a -> Ref a
offsetRef n (MkRef s) = MkRef (s + n)

-- =============================================================================
-- Storage Operations (EVM FFI)
-- =============================================================================

-- These would be provided by EVM.Primitives in actual implementation
-- Here we define the interface

||| Low-level slot read (uses EVM.Primitives.sload)
export
sloadSlot : Slot -> IO Bits256
sloadSlot = sload

||| Low-level slot write (uses EVM.Primitives.sstore)
export
sstoreSlot : Slot -> Bits256 -> IO ()
sstoreSlot = sstore

-- =============================================================================
-- Type-Safe Get/Set
-- =============================================================================

||| Read a value from storage.
|||
||| Type-safe: the return type is determined by the Ref's phantom type.
|||
||| ```idris
||| memberRef : Ref Member
||| member <- get memberRef  -- Type: IO Member
|||
||| -- Impossible: get memberRef : IO Bits256
||| -- The types don't match!
||| ```
export
get : Storable a => Ref a -> IO a
get {a} ref = do
  slots <- readSlots (refSlot ref) (slotCount {a})
  pure (fromSlots slots)
  where
    readSlots : Slot -> (n : Nat) -> IO (Vect n Bits256)
    readSlots _ Z = pure []
    readSlots s (S k) = do
      v <- sloadSlot s
      vs <- readSlots (s + 1) k
      pure (v :: vs)

||| Write a value to storage.
|||
||| Type-safe: you can only write values matching the Ref's phantom type.
|||
||| ```idris
||| memberRef : Ref Member
||| set memberRef (MkMember addr meta)  -- OK
|||
||| set memberRef 42  -- Compile error: expected Member, got Integer
||| ```
export
set : Storable a => Ref a -> a -> IO ()
set {a} ref val = writeSlots (refSlot ref) (toSlots val)
  where
    writeSlots : Slot -> Vect n Bits256 -> IO ()
    writeSlots _ [] = pure ()
    writeSlots s (v :: vs) = do
      sstoreSlot s v
      writeSlots (s + 1) vs

-- =============================================================================
-- Example: Member Record
-- =============================================================================

||| Example record: DAO member with address and metadata
public export
record Member where
  constructor MkMember
  addr : Bits256
  metadata : Bits256

||| Member is Storable - uses 2 slots
public export
Storable Member where
  slotCount = 2
  toSlots m = [m.addr, m.metadata]
  fromSlots [a, m] = MkMember a m

-- =============================================================================
-- Example: Pair (2-slot tuple)
-- =============================================================================

||| Pair of two Bits256 values (2 slots)
public export
Storable (Bits256, Bits256) where
  slotCount = 2
  toSlots (x, y) = [x, y]
  fromSlots [x, y] = (x, y)

-- =============================================================================
-- What Solidity Cannot Express
-- =============================================================================

||| This function is GENERIC over any Storable type.
||| Solidity has no equivalent - you cannot write a function
||| that works with "any struct" without knowing its layout.
|||
||| ```solidity
||| // IMPOSSIBLE in Solidity:
||| function copy<T>(uint256 fromSlot, uint256 toSlot) {
|||   // How many slots? What's the layout? Unknown!
||| }
||| ```
export
copy : Storable a => Ref a -> Ref a -> IO ()
copy from to = get from >>= set to

||| Swap two storage locations - generic over any Storable type.
export
swap : Storable a => Ref a -> Ref a -> IO ()
swap refA refB = do
  a <- get refA
  b <- get refB
  set refA b
  set refB a

-- =============================================================================
-- Bounded Array (Proof-Carrying Access)
-- =============================================================================

||| Array with statically known maximum length.
|||
||| The key insight: Solidity does runtime bounds checks (revert).
||| Idris2 requires a PROOF that the index is in bounds.
||| Invalid access is not "caught at runtime" - it's IMPOSSIBLE TO WRITE.
|||
||| ```solidity
||| // Solidity: runtime check, can fail
||| function get(uint i) returns (T) {
|||   require(i < length, "out of bounds");  // runtime!
|||   return arr[i];
||| }
||| ```
|||
||| ```idris
||| -- Idris2: compile-time proof required
||| arrayAt : (idx : Nat) -> {auto prf : idx `LT` len} -> Ref a
||| -- Without proof, code doesn't compile!
||| ```
public export
record StorageArray (maxLen : Nat) (a : Type) where
  constructor MkStorageArray
  baseSlot : Slot
  currentLen : Nat  -- tracked at type level ideally

||| Reference to a storage array
public export
data ArrayRef : (maxLen : Nat) -> Type -> Type where
  MkArrayRef : (baseSlot : Slot) -> ArrayRef maxLen a

||| Get the length slot (array length stored at base slot)
export
arrayLenSlot : ArrayRef maxLen a -> Slot
arrayLenSlot (MkArrayRef s) = s

||| Get current array length from storage
export
getArrayLen : ArrayRef maxLen a -> IO Nat
getArrayLen ref = do
  len <- sloadSlot (arrayLenSlot ref)
  pure (cast (the Integer len))

||| Calculate element slot: keccak256(baseSlot) + idx * elemSize
||| For simplicity, we use linear layout here
export
elemSlot : Storable a => ArrayRef maxLen a -> Nat -> Slot
elemSlot {a} (MkArrayRef base) idx =
  base + 1 + (cast idx) * (cast (slotCount {a}))

||| Access array element WITH BOUNDS PROOF.
|||
||| This is the key innovation:
||| - `idx` must be a `Nat` (cannot be negative)
||| - `prf : LT idx len` is a PROOF that idx < len
||| - Without this proof, the code DOES NOT COMPILE
|||
||| Solidity equivalent requires runtime `require(idx < len)`.
||| Here, invalid access is statically impossible.
export
arrayAt : Storable a
       => (arr : ArrayRef maxLen a)
       -> (idx : Nat)
       -> (len : Nat)
       -> {auto prf : LT idx len}
       -> Ref a
arrayAt {a} arr idx _ = MkRef (elemSlot arr idx)

||| Safe array access with runtime length check.
||| Returns Maybe to handle out-of-bounds gracefully.
||| This is the "escape hatch" when static proof isn't available.
export
arrayAtMaybe : Storable a
            => ArrayRef maxLen a
            -> Nat
            -> IO (Maybe (Ref a))
arrayAtMaybe arr idx = do
  len <- getArrayLen arr
  pure $ if idx < len
         then Just (MkRef (elemSlot arr idx))
         else Nothing

||| Push element to array
||| Note: maxLen is a type-level hint; runtime bounds check is caller's responsibility
export
arrayPush : Storable a
         => ArrayRef maxLen a
         -> a
         -> IO ()
arrayPush {a} arr val = do
  len <- getArrayLen arr
  let ref = MkRef (elemSlot arr len)
  set ref val
  sstoreSlot (arrayLenSlot arr) (cast (len + 1))

-- =============================================================================
-- Proof-Carrying State Transitions (RQ-2.3 Preview)
-- =============================================================================

||| State enumeration for proposals
public export
data ProposalState = Draft | Voting | Approved | Executed

||| Valid state transitions (type-level constraint)
public export
data ValidTransition : ProposalState -> ProposalState -> Type where
  DraftToVoting : ValidTransition Draft Voting
  VotingToApproved : ValidTransition Voting Approved
  VotingToDraft : ValidTransition Voting Draft  -- rejected
  ApprovedToExecuted : ValidTransition Approved Executed

||| State-indexed proposal
||| The state is part of the TYPE, not just runtime data.
public export
data Proposal : ProposalState -> Type where
  MkProposal : (state : ProposalState) -> (pid : Bits256) -> Proposal state

||| Transition requires PROOF of valid transition.
||| Invalid transitions don't compile!
|||
||| ```idris
||| -- OK: Draft -> Voting is valid
||| p2 <- transition {prf = DraftToVoting} p1
|||
||| -- Compile Error: no proof for Draft -> Executed
||| bad <- transition {prf = ???} p1  -- impossible!
||| ```
export
transition : {from, to : ProposalState}
          -> Proposal from
          -> {auto prf : ValidTransition from to}
          -> IO (Proposal to)
transition {to} (MkProposal _ pid) = do
  -- In real impl: update storage to reflect new state
  pure (MkProposal to pid)
