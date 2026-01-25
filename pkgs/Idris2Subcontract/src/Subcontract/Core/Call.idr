||| Subcontract Core: Type-Safe External Calls
|||
||| RQ-3.3: Call Optimization - Cross-contract call patterns
|||
||| Type-safe wrappers for EVM external calls (CALL, DELEGATECALL, STATICCALL).
||| The type system ensures:
||| - Return types are correctly decoded
||| - Gas limits are explicitly specified
||| - Success/failure is handled at type level
|||
||| Solidity pattern:
||| ```solidity
||| (bool success, bytes memory data) = target.call{value: v, gas: g}(calldata);
||| require(success, "call failed");
||| // Decode data manually...
||| ```
|||
||| Idris2 pattern:
||| ```idris
||| result <- typedCall @MyReturnType target value gas calldata
||| case result of
|||   CallSuccess val => use val  -- val is already typed!
|||   CallReverted _ => handleError
||| ```
module Subcontract.Core.Call

import public Subcontract.Core.Storable
import public Data.Vect

%default total

-- =============================================================================
-- Call Result Types
-- =============================================================================

||| Result of an external call - explicit success or revert
public export
data CallResult : Type -> Type where
  ||| Call succeeded with decoded return value
  CallSuccess : a -> CallResult a
  ||| Call reverted (returndata contains revert reason)
  CallReverted : (returnSize : Bits256) -> CallResult a

||| Check if call succeeded
export
isSuccess : CallResult a -> Bool
isSuccess (CallSuccess _) = True
isSuccess (CallReverted _) = False

||| Extract value or default
export
fromCallResult : a -> CallResult a -> a
fromCallResult _ (CallSuccess x) = x
fromCallResult def (CallReverted _) = def

||| Functor instance for CallResult
public export
Functor CallResult where
  map f (CallSuccess x) = CallSuccess (f x)
  map _ (CallReverted s) = CallReverted s

-- =============================================================================
-- Call Specification
-- =============================================================================

||| External call specification with all parameters explicit
public export
record CallSpec where
  constructor MkCallSpec
  ||| Target contract address
  target : Bits256
  ||| ETH value to send (in wei)
  value : Bits256
  ||| Gas limit for the call
  gasLimit : Bits256

||| Create a call spec with no value transfer
export
callTo : (target : Bits256) -> (gas : Bits256) -> CallSpec
callTo t g = MkCallSpec t 0 g

||| Create a call spec with value transfer
export
callWithValue : (target : Bits256) -> (value : Bits256) -> (gas : Bits256) -> CallSpec
callWithValue = MkCallSpec

-- =============================================================================
-- Calldata Encoding
-- =============================================================================

||| Encoded calldata ready for external call
public export
record Calldata where
  constructor MkCalldata
  ||| Memory offset where calldata starts
  offset : Bits256
  ||| Length of calldata in bytes
  size : Bits256

||| Create empty calldata (for receive/fallback)
export
emptyCalldata : Calldata
emptyCalldata = MkCalldata 0 0

||| Encode a function selector (4 bytes)
export
encodeSelector : Bits256 -> IO Calldata
encodeSelector sel = do
  mstore 0 sel
  pure (MkCalldata 28 4)  -- selector at bytes 28-31 of first word

||| Encode selector with single argument
export
encodeSelectorArg : Bits256 -> Bits256 -> IO Calldata
encodeSelectorArg sel arg = do
  mstore 0 sel
  mstore 32 arg
  pure (MkCalldata 28 36)  -- 4 + 32

||| Encode selector with two arguments
export
encodeSelectorArg2 : Bits256 -> Bits256 -> Bits256 -> IO Calldata
encodeSelectorArg2 sel arg1 arg2 = do
  mstore 0 sel
  mstore 32 arg1
  mstore 64 arg2
  pure (MkCalldata 28 68)  -- 4 + 32 + 32

||| Encode selector with three arguments
export
encodeSelectorArg3 : Bits256 -> Bits256 -> Bits256 -> Bits256 -> IO Calldata
encodeSelectorArg3 sel arg1 arg2 arg3 = do
  mstore 0 sel
  mstore 32 arg1
  mstore 64 arg2
  mstore 96 arg3
  pure (MkCalldata 28 100)  -- 4 + 32 + 32 + 32

-- =============================================================================
-- Return Data Decoding
-- =============================================================================

||| Decode single word from return data
export
decodeReturnWord : IO Bits256
decodeReturnWord = do
  returndatacopy 0 0 32
  mload 0

||| Decode two words from return data
export
decodeReturnWords2 : IO (Bits256, Bits256)
decodeReturnWords2 = do
  returndatacopy 0 0 64
  a <- mload 0
  b <- mload 32
  pure (a, b)

||| Decode three words from return data
export
decodeReturnWords3 : IO (Bits256, Bits256, Bits256)
decodeReturnWords3 = do
  returndatacopy 0 0 96
  a <- mload 0
  b <- mload 32
  c <- mload 64
  pure (a, b, c)

-- =============================================================================
-- Type-Safe CALL
-- =============================================================================

||| Execute external CALL with typed return value.
|||
||| The return type is decoded using Storable interface.
||| Gas, value, and target are all explicit.
|||
||| ```idris
||| result <- typedCall @Bits256 spec calldata
||| case result of
|||   CallSuccess balance => ...
|||   CallReverted _ => revert
||| ```
export
typedCall : Storable a => CallSpec -> Calldata -> IO (CallResult a)
typedCall {a} spec cd = do
  let retOff = 256  -- Return data offset in memory
  let retSize = cast (slotCount {a}) * 32
  success <- call spec.gasLimit spec.target spec.value cd.offset cd.size retOff retSize
  if success /= 0
    then do
      -- Decode return data into slots
      slots <- readReturnSlots retOff (slotCount {a})
      pure (CallSuccess (fromSlots slots))
    else do
      rsize <- returndatasize
      pure (CallReverted rsize)
  where
    readReturnSlots : Bits256 -> (n : Nat) -> IO (Vect n Bits256)
    readReturnSlots _ Z = pure []
    readReturnSlots off (S k) = do
      v <- mload off
      vs <- readReturnSlots (off + 32) k
      pure (v :: vs)

||| Raw CALL that returns success/failure and return data size
export
rawCall : CallSpec -> Calldata -> IO (Bool, Bits256)
rawCall spec cd = do
  success <- call spec.gasLimit spec.target spec.value cd.offset cd.size 0 0
  if success /= 0
    then do
      rsize <- returndatasize
      pure (True, rsize)
    else do
      rsize <- returndatasize
      pure (False, rsize)

||| Simple value transfer (no calldata)
export
transferValue : (to : Bits256) -> (value : Bits256) -> (gas : Bits256) -> IO Bool
transferValue to val gas = do
  success <- call gas to val 0 0 0 0
  pure (success /= 0)

-- =============================================================================
-- Type-Safe DELEGATECALL
-- =============================================================================

||| Execute DELEGATECALL (preserves msg.sender and storage context).
|||
||| Used for proxy patterns and library calls.
||| Note: No value parameter (msg.value preserved from original call).
export
typedDelegatecall : Storable a => (target : Bits256) -> (gas : Bits256) -> Calldata -> IO (CallResult a)
typedDelegatecall {a} target gas cd = do
  let retOff = 256
  let retSize = cast (slotCount {a}) * 32
  success <- delegatecall gas target cd.offset cd.size retOff retSize
  if success /= 0
    then do
      slots <- readReturnSlots retOff (slotCount {a})
      pure (CallSuccess (fromSlots slots))
    else do
      rsize <- returndatasize
      pure (CallReverted rsize)
  where
    readReturnSlots : Bits256 -> (n : Nat) -> IO (Vect n Bits256)
    readReturnSlots _ Z = pure []
    readReturnSlots off (S k) = do
      v <- mload off
      vs <- readReturnSlots (off + 32) k
      pure (v :: vs)

||| Raw DELEGATECALL
export
rawDelegatecall : (target : Bits256) -> (gas : Bits256) -> Calldata -> IO Bool
rawDelegatecall target gas cd = do
  success <- delegatecall gas target cd.offset cd.size 0 0
  pure (success /= 0)

-- =============================================================================
-- Type-Safe STATICCALL
-- =============================================================================

||| Execute STATICCALL (read-only, reverts on state modification).
|||
||| Safe for calling view/pure functions.
||| The EVM guarantees no state changes occur.
export
typedStaticcall : Storable a => (target : Bits256) -> (gas : Bits256) -> Calldata -> IO (CallResult a)
typedStaticcall {a} target gas cd = do
  let retOff = 256
  let retSize = cast (slotCount {a}) * 32
  success <- staticcall gas target cd.offset cd.size retOff retSize
  if success /= 0
    then do
      slots <- readReturnSlots retOff (slotCount {a})
      pure (CallSuccess (fromSlots slots))
    else do
      rsize <- returndatasize
      pure (CallReverted rsize)
  where
    readReturnSlots : Bits256 -> (n : Nat) -> IO (Vect n Bits256)
    readReturnSlots _ Z = pure []
    readReturnSlots off (S k) = do
      v <- mload off
      vs <- readReturnSlots (off + 32) k
      pure (v :: vs)

||| Raw STATICCALL
export
rawStaticcall : (target : Bits256) -> (gas : Bits256) -> Calldata -> IO Bool
rawStaticcall target gas cd = do
  success <- staticcall gas target cd.offset cd.size 0 0
  pure (success /= 0)

-- =============================================================================
-- High-Level Call Patterns
-- =============================================================================

||| Call or revert pattern - commonly used in contracts
export
callOrRevert : Storable a => CallSpec -> Calldata -> IO a
callOrRevert spec cd = do
  result <- typedCall spec cd
  case result of
    CallSuccess val => pure val
    CallReverted size => do
      -- Forward revert data
      returndatacopy 0 0 size
      evmRevert 0 size
      pure (fromSlots (replicate _ 0))  -- Unreachable after revert

||| Try call with fallback value
export
tryCall : Storable a => a -> CallSpec -> Calldata -> IO a
tryCall fallback spec cd = do
  result <- typedCall spec cd
  pure (fromCallResult fallback result)

||| Multi-call: execute multiple calls, collect results
export
multiCall : Storable a => List (CallSpec, Calldata) -> IO (List (CallResult a))
multiCall [] = pure []
multiCall ((spec, cd) :: rest) = do
  r <- typedCall spec cd
  rs <- multiCall rest
  pure (r :: rs)

-- =============================================================================
-- Checked Transfer Patterns (ERC20-style)
-- =============================================================================

||| ERC20 transfer call with success check
export
safeTransfer : (token : Bits256)
            -> (to : Bits256)
            -> (amount : Bits256)
            -> (gas : Bits256)
            -> IO Bool
safeTransfer token to amount gas = do
  -- transfer(address,uint256) selector = 0xa9059cbb
  cd <- encodeSelectorArg2 0xa9059cbb to amount
  let spec = callTo token gas
  result <- typedCall {a=Bool} spec cd
  case result of
    CallSuccess True => pure True
    _ => pure False

||| ERC20 transferFrom call with success check
export
safeTransferFrom : (token : Bits256)
                -> (from : Bits256)
                -> (to : Bits256)
                -> (amount : Bits256)
                -> (gas : Bits256)
                -> IO Bool
safeTransferFrom token from to amount gas = do
  -- transferFrom(address,address,uint256) selector = 0x23b872dd
  cd <- encodeSelectorArg3 0x23b872dd from to amount
  let spec = callTo token gas
  result <- typedCall {a=Bool} spec cd
  case result of
    CallSuccess True => pure True
    _ => pure False

-- =============================================================================
-- Integration with Reentrancy Guard
-- =============================================================================

||| Execute call with lock held (see Reentrancy module).
||| This pattern ensures no reentrant calls can occur.
|||
||| Note: Actual Lock type comes from Reentrancy module.
||| This is a pattern demonstration.
export
protectedTypedCall : Storable a
                  => (preLock : IO ())
                  -> (postUnlock : IO ())
                  -> CallSpec
                  -> Calldata
                  -> IO (CallResult a)
protectedTypedCall lock unlock spec cd = do
  lock
  result <- typedCall spec cd
  unlock
  pure result

-- =============================================================================
-- Compile-Time Guarantees
-- =============================================================================

-- 1. Return type is explicit: CallResult a where a is Storable
-- 2. Gas limit must be specified (no implicit "all gas")
-- 3. Value transfer is explicit (0 or specified amount)
-- 4. Success/failure is type-level (CallSuccess vs CallReverted)
-- 5. Decoding errors are impossible (Storable handles it)
--
-- In Solidity:
-- - Return type is bytes, decoded manually
-- - Gas can be implicit (gasleft())
-- - Value is optional parameter
-- - Success is runtime bool check
-- - Decoding can fail at runtime
