||| Subcontract ABI: Type-Safe Calldata Decoder
|||
||| Eliminates manual offset calculations by encoding the current read
||| position in the type. Each decode operation advances the position
||| by the appropriate amount.
|||
||| Example usage:
|||   decodeAddMember : Decoder (Address, Bytes32)
|||   decodeAddMember = do
|||     addr <- decodeAddress
|||     meta <- decodeBytes32
|||     pure (addr, meta)
|||
|||   -- In entry point:
|||   (addr, meta) <- runDecoder decodeAddMember
|||   idx <- addMemberImpl addr meta
|||
module Subcontract.Core.ABI.Decoder

import Subcontract.Core.ABI.Sig

-- =============================================================================
-- EVM Primitives
-- =============================================================================

%foreign "evm:calldataload"
prim__calldataload : Integer -> PrimIO Integer

%foreign "evm:calldatasize"
prim__calldatasize : PrimIO Integer

export
calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

export
calldatasize : IO Integer
calldatasize = primIO prim__calldatasize

-- =============================================================================
-- Typed Value Wrappers
-- =============================================================================

||| Address type (20 bytes, stored as Integer with mask)
public export
record Address where
  constructor MkAddress
  addrValue : Integer

||| Bytes32 type (32 bytes)
public export
record Bytes32 where
  constructor MkBytes32
  bytes32Value : Integer

||| Uint256 type
public export
record Uint256 where
  constructor MkUint256
  uint256Value : Integer

-- =============================================================================
-- Decoder Monad (State = current offset)
-- =============================================================================

||| Decoder that reads from calldata at the current offset
||| The offset starts at 4 (after selector) and advances by 32 per slot
public export
record Decoder a where
  constructor MkDecoder
  runDec : Integer -> IO (a, Integer)

export
Functor Decoder where
  map f (MkDecoder run) = MkDecoder $ \off => do
    (a, off') <- run off
    pure (f a, off')

export
Applicative Decoder where
  pure x = MkDecoder $ \off => pure (x, off)
  (MkDecoder runF) <*> (MkDecoder runA) = MkDecoder $ \off => do
    (f, off') <- runF off
    (a, off'') <- runA off'
    pure (f a, off'')

export
Monad Decoder where
  (MkDecoder run) >>= f = MkDecoder $ \off => do
    (a, off') <- run off
    let (MkDecoder run') = f a
    run' off'

-- =============================================================================
-- Primitive Decoders
-- =============================================================================

||| Decode a raw 32-byte slot (no masking)
export
decodeSlot : Decoder Integer
decodeSlot = MkDecoder $ \off => do
  val <- calldataload off
  pure (val, off + 32)

||| Decode an Address (mask to 20 bytes)
export
decodeAddress : Decoder Address
decodeAddress = MkDecoder $ \off => do
  val <- calldataload off
  let masked = val `mod` 0x10000000000000000000000000000000000000000  -- 2^160
  pure (MkAddress masked, off + 32)

||| Decode a Bytes32
export
decodeBytes32 : Decoder Bytes32
decodeBytes32 = MkDecoder $ \off => do
  val <- calldataload off
  pure (MkBytes32 val, off + 32)

||| Decode a Uint256
export
decodeUint256 : Decoder Uint256
decodeUint256 = MkDecoder $ \off => do
  val <- calldataload off
  pure (MkUint256 val, off + 32)

||| Decode a Bool
export
decodeBool : Decoder Bool
decodeBool = MkDecoder $ \off => do
  val <- calldataload off
  pure (val /= 0, off + 32)

-- =============================================================================
-- Runner
-- =============================================================================

||| Run decoder starting after the 4-byte selector
export
runDecoder : Decoder a -> IO a
runDecoder (MkDecoder run) = do
  (a, _) <- run 4  -- Start at offset 4 (after selector)
  pure a

||| Get current offset (for debugging)
export
getOffset : Decoder Integer
getOffset = MkDecoder $ \off => pure (off, off)
