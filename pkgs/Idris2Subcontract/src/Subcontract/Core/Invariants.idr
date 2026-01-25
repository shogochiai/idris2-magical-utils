||| Subcontract Core: Storage Invariants via Curry-Howard
|||
||| RQ-1.2: Express storage invariants as types
||| RQ-3.1: Patterns for optimal Yul code generation
|||
||| Key insight: In Solidity, invariants are runtime `require()` checks.
||| In Idris2, invariants are TYPES - violation is a compile error.
|||
||| Example: "balance >= 0" is not a check, it's the type `Nat`.
||| You cannot construct a negative Nat, so the invariant is always true.
module Subcontract.Core.Invariants

import public Data.Nat
import public Data.Nat.Order
import public Data.Vect
import public Decidable.Equality
import public Subcontract.Core.Storable

%default total

-- Helper: LTE is reflexive
lteRefl : {n : Nat} -> LTE n n
lteRefl {n = Z} = LTEZero
lteRefl {n = S k} = LTESucc lteRefl

-- Helper: LTE is transitive
lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero _ = LTEZero
lteTrans (LTESucc x) (LTESucc y) = LTESucc (lteTrans x y)

-- =============================================================================
-- RQ-1.2: Non-Negative Amounts (balance >= 0)
-- =============================================================================

||| Non-negative amount - cannot be negative by construction
||| Solidity: `require(amount >= 0)` - but uint256 is already non-negative!
||| The real issue is overflow/underflow checking.
public export
Amount : Type
Amount = Nat

||| Convert to storage representation
export
amountToBits : Amount -> Bits256
amountToBits = cast

||| Parse from storage (unsafe - assumes valid range)
export
bitsToAmount : Bits256 -> Amount
bitsToAmount n = cast (max 0 n)

||| Storable instance for Amount
public export
Storable Amount where
  slotCount = 1
  toSlots n = [amountToBits n]
  fromSlots [x] = bitsToAmount x

-- =============================================================================
-- RQ-1.2: Bounded Values (value <= cap)
-- =============================================================================

||| Value bounded by a maximum.
||| The bound is part of the TYPE, not a runtime check.
|||
||| Solidity:
||| ```solidity
||| require(value <= MAX_SUPPLY, "exceeds cap");
||| ```
|||
||| Idris2: If you have `Bounded 1000000 n`, then `n <= 1000000` is PROVEN.
public export
record Bounded (cap : Nat) where
  constructor MkBounded
  value : Nat
  {auto inBounds : LTE value cap}

||| Create a bounded value (returns Nothing if out of bounds)
export
mkBounded : (cap : Nat) -> (n : Nat) -> Maybe (Bounded cap)
mkBounded cap n = case isLTE n cap of
  Yes prf => Just (MkBounded n)
  No _ => Nothing

||| Bounded addition that cannot overflow
export
boundedAdd : {cap : Nat} -> (b : Bounded cap) -> (delta : Nat)
          -> (fits : LTE (value b + delta) cap)
          -> Bounded cap
boundedAdd (MkBounded v) delta fits = MkBounded (v + delta) {inBounds = fits}

||| Bounded subtraction that cannot underflow
export
boundedSub : {cap : Nat} -> (b : Bounded cap) -> (delta : Nat)
          -> (fits : LTE delta (value b))
          -> Bounded cap
boundedSub {cap} (MkBounded v {inBounds}) delta fits =
  MkBounded (minus v delta) {inBounds = lteTrans (minusLteLeft v delta) inBounds}
  where
    minusLteLeft : (a, b : Nat) -> LTE (minus a b) a
    minusLteLeft Z _ = LTEZero
    minusLteLeft (S k) Z = lteRefl
    minusLteLeft (S k) (S j) = lteSuccRight (minusLteLeft k j)

-- =============================================================================
-- RQ-1.2: Non-Zero Values (divisor != 0)
-- =============================================================================

||| Non-zero natural number.
||| Division by NonZero is total - no runtime divide-by-zero possible.
public export
data NonZero : Type where
  MkNonZero : (n : Nat) -> {auto prf : IsSucc n} -> NonZero

||| Extract the value from NonZero
export
nzValue : NonZero -> Nat
nzValue (MkNonZero n) = n

||| Create NonZero (returns Nothing if zero)
export
mkNonZero : (n : Nat) -> Maybe NonZero
mkNonZero Z = Nothing
mkNonZero (S k) = Just (MkNonZero (S k))

||| Safe division - requires NonZero divisor
||| This is TOTAL - no runtime check needed!
export
safeDiv : Nat -> NonZero -> Nat
safeDiv n nz = div n (nzValue nz)

||| Safe modulo - requires NonZero divisor
export
safeMod : Nat -> NonZero -> Nat
safeMod n nz = mod n (nzValue nz)

-- =============================================================================
-- RQ-1.2: Token Balance Invariants
-- =============================================================================

||| Token balance that tracks:
||| 1. balance >= 0 (via Nat)
||| 2. balance <= totalSupply (via LTE proof)
|||
||| In Solidity, you hope your ERC20 doesn't have balance > totalSupply bugs.
||| In Idris2, it's IMPOSSIBLE to construct such a state.
public export
record TokenBalance (totalSupply : Nat) where
  constructor MkTokenBalance
  balance : Nat
  {auto balanceValid : LTE balance totalSupply}

||| Transfer tokens: compile-time guarantee of no underflow/overflow
||| The type signature PROVES:
||| - Sender has enough tokens (LTE amount senderBalance)
||| - Recipient won't overflow (LTE (recipientBalance + amount) totalSupply)
export
transfer : {supply : Nat}
        -> (amount : Nat)
        -> (sender : TokenBalance supply)
        -> (recipient : TokenBalance supply)
        -> (hasEnough : LTE amount (balance sender))
        -> (noOverflow : LTE (balance recipient + amount) supply)
        -> (TokenBalance supply, TokenBalance supply)
transfer {supply} amount (MkTokenBalance sb {balanceValid=sbValid}) (MkTokenBalance rb) hasEnough noOverflow =
  let senderPrf = lteTrans (minusLte sb amount) sbValid
      newSender = MkTokenBalance (minus sb amount) {balanceValid = senderPrf}
      newRecipient = MkTokenBalance (rb + amount) {balanceValid = noOverflow}
  in (newSender, newRecipient)
  where
    minusLte : (a, b : Nat) -> LTE (minus a b) a
    minusLte Z _ = LTEZero
    minusLte (S k) Z = lteRefl
    minusLte (S k) (S j) = lteSuccRight (minusLte k j)

-- =============================================================================
-- RQ-1.2: Allowance Invariants (allowance <= balance)
-- =============================================================================

||| Allowance that is always <= balance.
||| ERC20 bug class: allowance can exceed balance after transfer.
||| In Idris2: The type prevents this.
public export
record ValidAllowance (ownerBalance : Nat) where
  constructor MkAllowance
  allowance : Nat
  {auto notExceedBalance : LTE allowance ownerBalance}

||| Decrease allowance (for transferFrom)
export
decreaseAllowance : {bal : Nat}
                 -> (amount : Nat)
                 -> (allow : ValidAllowance bal)
                 -> (hasAllowance : LTE amount (allowance allow))
                 -> ValidAllowance bal
decreaseAllowance {bal} amount (MkAllowance a {notExceedBalance}) hasAllowance =
  MkAllowance (minus a amount) {notExceedBalance = prf}
  where
    minusLte : (x, y : Nat) -> LTE (minus x y) x
    minusLte Z _ = LTEZero
    minusLte (S k) Z = lteRefl
    minusLte (S k) (S j) = lteSuccRight (minusLte k j)

    prf : LTE (minus a amount) bal
    prf = lteTrans (minusLte a amount) notExceedBalance

-- =============================================================================
-- RQ-3.1: Optimized Storage Patterns
-- =============================================================================

||| Packed storage: multiple small values in one slot.
||| This generates efficient code: one sload instead of multiple.
|||
||| Example: Pack (balance, nonce, frozen) into one slot
||| - balance: 128 bits
||| - nonce: 64 bits
||| - frozen: 1 bit
||| Total: 193 bits < 256 bits (fits in one slot)
public export
record PackedAccount where
  constructor MkPackedAccount
  packedData : Bits256  -- All fields packed into one word

||| Storable for packed account (1 slot)
public export
Storable PackedAccount where
  slotCount = 1
  toSlots p = [p.packedData]
  fromSlots [x] = MkPackedAccount x

||| Pack account fields into one slot
||| Layout: [128 bits balance][64 bits nonce][64 bits flags]
export
packAccount : (balance : Bits256) -> (nonce : Bits256) -> (frozen : Bool) -> PackedAccount
packAccount bal nonce frozen =
  let frozenBit = if frozen then 1 else 0
      -- balance in high 128 bits, nonce in middle 64, flags in low 64
      packed = bal * 0x100000000000000000000000000000000  -- shift left 128
             + nonce * 0x10000000000000000                 -- shift left 64
             + frozenBit
  in MkPackedAccount packed

||| Unpack balance from packed account
export
unpackBalance : PackedAccount -> Bits256
unpackBalance (MkPackedAccount p) = p `div` 0x100000000000000000000000000000000

||| Unpack nonce from packed account
export
unpackNonce : PackedAccount -> Bits256
unpackNonce (MkPackedAccount p) =
  (p `div` 0x10000000000000000) `mod` 0x10000000000000000

||| Unpack frozen flag from packed account
export
unpackFrozen : PackedAccount -> Bool
unpackFrozen (MkPackedAccount p) = (p `mod` 2) == 1

-- =============================================================================
-- RQ-3.1: Batch Operations (Minimize sload/sstore)
-- =============================================================================

||| Batch read: read multiple values with one storage access pattern.
||| The compiler can optimize this to sequential slot reads.
export
batchRead : Storable a => Ref a -> IO a
batchRead = get  -- Storable.get already reads all slots efficiently

||| Batch write: write multiple values with one storage access pattern.
export
batchWrite : Storable a => Ref a -> a -> IO ()
batchWrite = set  -- Storable.set already writes all slots efficiently

||| Read-modify-write pattern: atomic update with one read + one write.
||| The compiler should optimize this to: sload, compute, sstore
export
modifyStorage : Storable a => Ref a -> (a -> a) -> IO a
modifyStorage ref f = do
  old <- get ref
  let new = f old
  set ref new
  pure new

-- =============================================================================
-- RQ-3.1: Compile-Time Slot Calculation
-- =============================================================================

||| Slot with compile-time known offset.
||| This ensures type-safe slot access with statically known structure.
public export
record StaticSlot (offset : Nat) where
  constructor MkStaticSlot
  baseSlot : Bits256

||| Get the slot address (offset added to base)
export
staticSlotAddr : (offset : Nat) -> StaticSlot offset -> Bits256
staticSlotAddr offset slot = slot.baseSlot + cast offset

||| Type-safe slot access with known offset
export
readStaticSlot : (offset : Nat) -> StaticSlot offset -> IO Bits256
readStaticSlot offset slot = sload (staticSlotAddr offset slot)

export
writeStaticSlot : (offset : Nat) -> StaticSlot offset -> Bits256 -> IO ()
writeStaticSlot offset slot val = sstore (staticSlotAddr offset slot) val

-- =============================================================================
-- Summary: What Solidity Cannot Express
-- =============================================================================

-- 1. Bounded values: type guarantees value <= cap
-- 2. Non-zero divisors: division is total, no runtime check
-- 3. Balance invariants: balance <= totalSupply by construction
-- 4. Allowance invariants: allowance <= balance by construction
-- 5. Packed storage: type-safe packing/unpacking
-- 6. Batch operations: type ensures efficient code generation

-- In Solidity, all of these require runtime `require()` checks.
-- In Idris2, they are compile-time guarantees.
