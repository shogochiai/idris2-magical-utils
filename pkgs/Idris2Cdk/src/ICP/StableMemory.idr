||| ICP Stable Memory Abstraction Layer
|||
||| This module provides safe, typed abstractions over ICP's stable memory.
||| Stable memory persists across canister upgrades.
|||
||| Design follows nicp_cdk's pattern:
||| - StableValue: Single value storage
||| - StableSeq: Sequential list storage
|||
||| Reference: https://internetcomputer.org/docs/current/developer-docs/memory/stable-memory
module ICP.StableMemory

import ICP.IC0
import Data.List
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Size of a WebAssembly memory page (64 KiB)
public export
pageSize : Nat
pageSize = 65536

||| Maximum stable memory size (currently 400 GiB = 6,553,600 pages)
public export
maxPages : Nat
maxPages = 6553600

--------------------------------------------------------------------------------
-- Storable Type Class
--------------------------------------------------------------------------------

||| Type class for types that can be stored in stable memory
public export
interface Storable a where
  ||| Serialize to bytes
  toBytes : a -> List Bits8

  ||| Deserialize from bytes (may fail)
  fromBytes : List Bits8 -> Maybe a

  ||| Size in bytes (if fixed-size, otherwise Nothing)
  fixedSize : Maybe Nat
  fixedSize = Nothing

--------------------------------------------------------------------------------
-- Storable Instances for Primitive Types
--------------------------------------------------------------------------------

public export
Storable Bits8 where
  toBytes b = [b]
  fromBytes [b] = Just b
  fromBytes _ = Nothing
  fixedSize = Just 1

public export
Storable Bits16 where
  toBytes b = [cast (b `mod` 256), cast (b `div` 256)]
  fromBytes [lo, hi] = Just (cast lo + cast hi * 256)
  fromBytes _ = Nothing
  fixedSize = Just 2

public export
Storable Bits32 where
  toBytes b =
    let b0 = cast (b `mod` 256)
        b1 = cast ((b `div` 256) `mod` 256)
        b2 = cast ((b `div` 65536) `mod` 256)
        b3 = cast (b `div` 16777216)
    in [b0, b1, b2, b3]
  fromBytes [b0, b1, b2, b3] =
    Just (cast b0 + cast b1 * 256 + cast b2 * 65536 + cast b3 * 16777216)
  fromBytes _ = Nothing
  fixedSize = Just 4

public export
Storable Bits64 where
  toBytes b =
    let lo = cast {to=Bits32} (b `mod` 4294967296)
        hi = cast {to=Bits32} (b `div` 4294967296)
    in toBytes lo ++ toBytes hi
  fromBytes bs =
    case splitAt 4 bs of
      (loBytes, hiBytes) => do
        lo <- fromBytes {a=Bits32} loBytes
        hi <- fromBytes {a=Bits32} hiBytes
        Just (cast lo + cast hi * 4294967296)
  fixedSize = Just 8

public export
Storable Bool where
  toBytes True = [1]
  toBytes False = [0]
  fromBytes [0] = Just False
  fromBytes [1] = Just True
  fromBytes _ = Nothing
  fixedSize = Just 1

--------------------------------------------------------------------------------
-- Memory Region
--------------------------------------------------------------------------------

||| A region of stable memory
public export
record MemoryRegion where
  constructor MkRegion
  offset : Nat    -- Start offset in bytes
  length : Nat    -- Length in bytes

||| Create a region at given offset with given length
public export
region : Nat -> Nat -> MemoryRegion
region = MkRegion

--------------------------------------------------------------------------------
-- Stable Memory Operations (IO wrappers)
--------------------------------------------------------------------------------

||| Get current stable memory size in pages
export
stableSize : IO Nat
stableSize = do
  pages <- primIO prim__stableSize
  pure (cast pages)

||| Get current stable memory size in bytes
export
stableSizeBytes : IO Nat
stableSizeBytes = do
  pages <- stableSize
  pure (pages * pageSize)

||| Grow stable memory by n pages
||| Returns True on success, False on failure
export
stableGrow : Nat -> IO Bool
stableGrow pages = do
  result <- primIO (prim__stableGrow (cast pages))
  pure (result /= -1)

||| Safe division for Nat
safeDiv : Nat -> Nat -> Nat
safeDiv n Z = 0
safeDiv n (S k) = divNatNZ n (S k) SIsNonZero

||| Ensure at least n bytes are available
export
ensureCapacity : Nat -> IO Bool
ensureCapacity bytes = do
  current <- stableSizeBytes
  if current >= bytes
    then pure True
    else do
      let diff = ((bytes `minus` current) + pageSize) `minus` 1
      let needed = safeDiv diff pageSize
      stableGrow needed

||| Read bytes from stable memory
export
stableRead : MemoryRegion -> IO (List Bits8)
stableRead (MkRegion offset len) = do
  -- In real implementation, would allocate buffer, call prim__stableRead
  -- and copy bytes out
  pure []

||| Write bytes to stable memory
export
stableWrite : MemoryRegion -> List Bits8 -> IO Bool
stableWrite (MkRegion offset len) bytes = do
  -- In real implementation, would check capacity, copy bytes to linear
  -- memory, then call prim__stableWrite
  pure True

--------------------------------------------------------------------------------
-- StableValue: Single Value Storage
--------------------------------------------------------------------------------

||| Storage for a single value at a fixed offset
public export
record StableValue a where
  constructor MkStableValue
  offset : Nat

||| Create a stable value handle at given offset
public export
stableValue : Nat -> StableValue a
stableValue = MkStableValue

||| Read a value from stable storage
export
readValue : Storable a => StableValue a -> IO (Maybe a)
readValue (MkStableValue offset) = do
  -- For fixed-size types, read exactly that many bytes
  -- For variable-size, need length prefix
  bytes <- stableRead (MkRegion offset 1024)  -- TODO: proper sizing
  pure (fromBytes bytes)

||| Write a value to stable storage
export
writeValue : Storable a => StableValue a -> a -> IO Bool
writeValue (MkStableValue offset) val = do
  let bytes = toBytes val
  ensured <- ensureCapacity (offset + length bytes)
  if ensured
    then stableWrite (MkRegion offset (length bytes)) bytes
    else pure False

--------------------------------------------------------------------------------
-- StableSeq: Sequential List Storage
--------------------------------------------------------------------------------

||| Storage for a sequential list of values
||| Layout: [length: 8 bytes][item0][item1]...
public export
record StableSeq a where
  constructor MkStableSeq
  baseOffset : Nat
  itemSize   : Nat  -- Fixed item size (0 for variable)

||| Create a stable sequence handle
public export
stableSeq : Nat -> Nat -> StableSeq a
stableSeq = MkStableSeq

||| Get the number of items in the sequence
export
seqLength : StableSeq a -> IO Nat
seqLength (MkStableSeq offset _) = do
  bytes <- stableRead (MkRegion offset 8)
  case fromBytes {a=Bits64} bytes of
    Just n => pure (cast n)
    Nothing => pure 0

||| Read item at index
export
seqGet : Storable a => StableSeq a -> Nat -> IO (Maybe a)
seqGet seq idx = do
  len <- seqLength seq
  if idx >= len
    then pure Nothing
    else do
      let itemOffset = seq.baseOffset + 8 + idx * seq.itemSize
      bytes <- stableRead (MkRegion itemOffset seq.itemSize)
      pure (fromBytes bytes)

||| Append item to sequence
export
seqPush : Storable a => StableSeq a -> a -> IO Bool
seqPush seq val = do
  len <- seqLength seq
  let itemOffset = seq.baseOffset + 8 + len * seq.itemSize
  let bytes = toBytes val
  ensured <- ensureCapacity (itemOffset + length bytes)
  if ensured
    then do
      -- Write item
      _ <- stableWrite (MkRegion itemOffset (length bytes)) bytes
      -- Update length
      stableWrite (MkRegion seq.baseOffset 8) (toBytes (cast {to=Bits64} (len + 1)))
    else pure False
