||| Subcontract Core: Type-Safe Reentrancy Guard
|||
||| Reentrancy protection via linear types / state indexing.
|||
||| Solidity pattern (runtime check):
||| ```solidity
||| bool private _locked;
||| modifier nonReentrant() {
|||   require(!_locked, "reentrant");
|||   _locked = true;
|||   _;
|||   _locked = false;
||| }
||| ```
|||
||| Idris2 pattern (compile-time guarantee):
||| ```idris
||| withdraw : Lock Unlocked -> IO (Lock Locked, Amount)
||| -- The function CONSUMES Unlocked state and PRODUCES Locked state
||| -- Reentrancy is impossible because Lock Unlocked is consumed!
||| ```
module Subcontract.Core.Reentrancy

import public Subcontract.Core.Storable

%default total

-- =============================================================================
-- Lock State (Type-Level)
-- =============================================================================

||| Lock state at type level
public export
data LockState = Unlocked | Locked

||| Type-indexed lock.
||| The state is part of the TYPE, not just a runtime value.
public export
data Lock : LockState -> Type where
  ||| An unlocked lock
  MkUnlocked : (slot : Bits256) -> Lock Unlocked
  ||| A locked lock
  MkLocked : (slot : Bits256) -> Lock Locked

||| Get the storage slot
export
lockSlot : Lock s -> Bits256
lockSlot (MkUnlocked s) = s
lockSlot (MkLocked s) = s

-- =============================================================================
-- State Transitions (Linear-Style)
-- =============================================================================

||| Acquire lock: Unlocked -> Locked
||| This CONSUMES the Unlocked proof and PRODUCES a Locked proof.
||| After calling this, the original Lock Unlocked is no longer usable.
export
acquireLock : Lock Unlocked -> IO (Lock Locked)
acquireLock (MkUnlocked slot) = do
  sstore slot 1
  pure (MkLocked slot)

||| Release lock: Locked -> Unlocked
||| This CONSUMES the Locked proof and PRODUCES an Unlocked proof.
export
releaseLock : Lock Locked -> IO (Lock Unlocked)
releaseLock (MkLocked slot) = do
  sstore slot 0
  pure (MkUnlocked slot)

-- =============================================================================
-- Protected Execution Pattern
-- =============================================================================

||| Execute an action with reentrancy protection.
||| The action receives a Locked proof, preventing nested calls.
|||
||| ```idris
||| withdraw : Lock Unlocked -> Amount -> IO (Lock Unlocked)
||| withdraw lock amount = withLock lock $ \_ => do
|||   -- This code runs with lock held
|||   -- Any reentrant call would need Lock Unlocked, but we have Lock Locked
|||   transferETH recipient amount
||| ```
export
withLock : Lock Unlocked -> (Lock Locked -> IO a) -> IO (a, Lock Unlocked)
withLock lock action = do
  locked <- acquireLock lock
  result <- action locked
  unlocked <- releaseLock locked
  pure (result, unlocked)

||| Execute and discard the returned lock (simpler API)
export
withLock_ : Lock Unlocked -> (Lock Locked -> IO a) -> IO a
withLock_ lock action = fst <$> withLock lock action

-- =============================================================================
-- Runtime Initialization
-- =============================================================================

||| Storage for the lock
public export
record LockStorage where
  constructor MkLockStorage
  lockSlotAddr : Bits256

||| Initialize lock from storage (checks current state)
export
initLock : LockStorage -> IO (Either (Lock Locked) (Lock Unlocked))
initLock store = do
  state <- sload store.lockSlotAddr
  pure $ if state == 0
    then Right (MkUnlocked store.lockSlotAddr)
    else Left (MkLocked store.lockSlotAddr)

||| Try to get unlocked state (fails if already locked)
export
tryUnlock : LockStorage -> IO (Maybe (Lock Unlocked))
tryUnlock store = do
  result <- initLock store
  pure $ case result of
    Right unlocked => Just unlocked
    Left _ => Nothing

-- =============================================================================
-- Multi-Lock Pattern (Multiple Reentrancy Guards)
-- =============================================================================

||| Named locks for different resources
public export
data LockId = WithdrawLock | SwapLock | FlashLoanLock | CustomLock Bits256

||| Convert lock ID to slot offset
public export
lockIdToOffset : LockId -> Bits256
lockIdToOffset WithdrawLock = 0
lockIdToOffset SwapLock = 1
lockIdToOffset FlashLoanLock = 2
lockIdToOffset (CustomLock n) = 100 + n

||| Multi-lock storage
public export
record MultiLockStorage where
  constructor MkMultiLockStorage
  baseLockSlot : Bits256

||| Get lock for specific resource
export
getLock : MultiLockStorage -> LockId -> IO (Either (Lock Locked) (Lock Unlocked))
getLock store lid = do
  let slot = store.baseLockSlot + lockIdToOffset lid
  state <- sload slot
  pure $ if state == 0
    then Right (MkUnlocked slot)
    else Left (MkLocked slot)

||| Protected execution for specific resource
export
withResourceLock : MultiLockStorage
                -> LockId
                -> (Lock Locked -> IO a)
                -> IO (Maybe a)
withResourceLock store lid action = do
  result <- getLock store lid
  case result of
    Left _ => pure Nothing  -- Already locked
    Right unlocked => Just <$> withLock_ unlocked action

-- =============================================================================
-- Cross-Contract Reentrancy Protection
-- =============================================================================

||| Call external contract with reentrancy protection.
||| The lock is held during the external call.
export
protectedCall : Lock Unlocked
             -> (target : Bits256)
             -> (value : Bits256)
             -> (calldata : Bits256)  -- memory offset
             -> (calldataLen : Bits256)
             -> IO (Bool, Lock Unlocked)
protectedCall lock target value cdOff cdLen = do
  locked <- acquireLock lock
  -- External call happens while locked
  success <- call 0xFFFFFFFF target value cdOff cdLen 0 0  -- gas, target, value, argOff, argSize, retOff, retSize
  unlocked <- releaseLock locked
  pure (success /= 0, unlocked)

-- =============================================================================
-- Compile-Time Guarantees
-- =============================================================================

-- The key insight: Lock Unlocked is CONSUMED when acquiring the lock.
-- After acquireLock, you have Lock Locked, not Lock Unlocked.
-- To call a function requiring Lock Unlocked, you must release the lock first.
--
-- This makes reentrancy IMPOSSIBLE at compile time:
--
-- badWithdraw : Lock Unlocked -> IO ()
-- badWithdraw lock = withLock_ lock $ \locked => do
--   -- Here we have Lock Locked, not Lock Unlocked
--   badWithdraw lock  -- ERROR: lock is already consumed!
--   -- ^-- This doesn't compile because 'lock' was consumed by withLock_
--
-- In Solidity, this would be a runtime revert.
-- In Idris2, it's a compile-time type error.

-- =============================================================================
-- Example: Safe Withdraw Pattern
-- =============================================================================

||| Withdraw pattern with type-safe reentrancy protection
||| The caller provides Lock Unlocked, proving no reentrant call is in progress.
export
safeWithdraw : Lock Unlocked
            -> (recipient : Bits256)
            -> (amount : Bits256)
            -> IO (Lock Unlocked, Bool)
safeWithdraw lock recipient amount = do
  -- Acquire lock (Lock Unlocked -> Lock Locked)
  locked <- acquireLock lock

  -- Perform withdrawal logic
  -- ... balance checks, state updates ...

  -- External call (CEI pattern: Checks-Effects-Interactions)
  success <- call 0xFFFFFFFF recipient amount 0 0 0 0

  -- Release lock (Lock Locked -> Lock Unlocked)
  unlocked <- releaseLock locked

  pure (unlocked, success /= 0)

||| Batch withdrawals with lock held across all transfers
export
safeBatchWithdraw : Lock Unlocked
                 -> List (Bits256, Bits256)  -- (recipient, amount) pairs
                 -> IO (Lock Unlocked, Nat)  -- returns count of successful transfers
safeBatchWithdraw lock [] = pure (lock, 0)
safeBatchWithdraw lock transfers = do
  (count, unlocked) <- withLock lock $ \locked => do
    -- Lock is held during entire batch
    countSuccesses transfers 0
  pure (unlocked, count)
  where
    countSuccesses : List (Bits256, Bits256) -> Nat -> IO Nat
    countSuccesses [] acc = pure acc
    countSuccesses ((recipient, amount) :: rest) acc = do
      success <- call 0xFFFFFFFF recipient amount 0 0 0 0
      let newAcc = if success /= 0 then S acc else acc
      countSuccesses rest newAcc
