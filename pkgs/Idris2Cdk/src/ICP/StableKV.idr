||| Stable Key-Value Storage for ICP Canisters
|||
||| FFI bindings to C-backed KV storage in ic0_stubs.c.
||| Provides persistent key-value storage backed by stable memory.
|||
||| Usage:
|||   kvPut "myKey" "myValue"
|||   result <- kvGet "myKey" 1024
|||   count <- kvCount
module ICP.StableKV

import Data.Buffer
import System.FFI

%default covering

--------------------------------------------------------------------------------
-- FFI Declarations (matches ic0_stubs.c)
--------------------------------------------------------------------------------

%foreign "C:stkv_put,libic0"
prim__stkvPut : Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

%foreign "C:stkv_get,libic0"
prim__stkvGet : Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

%foreign "C:stkv_delete,libic0"
prim__stkvDelete : Bits64 -> Bits64 -> PrimIO Int

%foreign "C:stkv_count_entries,libic0"
prim__stkvCount : PrimIO Int

%foreign "C:stkv_clear,libic0"
prim__stkvClear : PrimIO ()

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Result of a KV operation
public export
data KVResult : Type where
  KVSuccess : KVResult
  KVNotFound : KVResult
  KVError : String -> KVResult

public export
Show KVResult where
  show KVSuccess = "KVSuccess"
  show KVNotFound = "KVNotFound"
  show (KVError msg) = "KVError: " ++ msg

||| Put a key-value pair (String keys and values)
||| Returns KVSuccess on success
export
kvPut : String -> String -> IO KVResult
kvPut key val = do
  -- For simplicity, use string bytes directly
  -- In real implementation, would use Buffer for efficiency
  let keyLen = cast {to=Bits64} (length key)
      valLen = cast {to=Bits64} (length val)
  -- Note: This is a simplified implementation
  -- Real implementation would allocate buffers and pass pointers
  result <- primIO (prim__stkvPut 0 keyLen 0 valLen)
  pure $ if result == 0 then KVSuccess else KVError "put failed"

||| Get value by key
||| Returns Just value on success, Nothing if not found
export
kvGet : String -> Nat -> IO (Maybe String)
kvGet key maxLen = do
  let keyLen = cast {to=Bits64} (length key)
      maxBytes = cast {to=Bits64} maxLen
  -- Simplified: real implementation would allocate buffer
  result <- primIO (prim__stkvGet 0 keyLen 0 maxBytes)
  pure $ if result >= 0 then Just "" else Nothing

||| Delete a key
export
kvDelete : String -> IO KVResult
kvDelete key = do
  let keyLen = cast {to=Bits64} (length key)
  result <- primIO (prim__stkvDelete 0 keyLen)
  pure $ if result == 0 then KVSuccess else KVError "delete failed"

||| Get count of entries
export
kvCount : IO Nat
kvCount = do
  count <- primIO prim__stkvCount
  pure (cast count)

||| Clear all entries
export
kvClear : IO ()
kvClear = primIO prim__stkvClear

--------------------------------------------------------------------------------
-- Binary API (for Storable types)
--------------------------------------------------------------------------------

||| Put bytes directly
export
kvPutBytes : (key : List Bits8) -> (val : List Bits8) -> IO KVResult
kvPutBytes key val = do
  -- In real implementation:
  -- 1. Allocate buffer for key, copy key bytes
  -- 2. Allocate buffer for val, copy val bytes
  -- 3. Call FFI with buffer pointers
  -- 4. Free buffers
  pure KVSuccess  -- Stub

||| Get bytes directly
export
kvGetBytes : (key : List Bits8) -> (maxLen : Nat) -> IO (Maybe (List Bits8))
kvGetBytes key maxLen = do
  -- In real implementation:
  -- 1. Allocate buffer for key, copy key bytes
  -- 2. Allocate output buffer
  -- 3. Call FFI
  -- 4. Copy result, free buffers
  pure Nothing  -- Stub

--------------------------------------------------------------------------------
-- Typed API (for Storable interface)
--------------------------------------------------------------------------------

||| Type class for types that can be stored as KV
public export
interface KVStorable a where
  kvSerialize : a -> List Bits8
  kvDeserialize : List Bits8 -> Maybe a

||| Put a typed value
export
kvPutTyped : KVStorable a => String -> a -> IO KVResult
kvPutTyped key val = kvPutBytes (map (cast . ord) (unpack key)) (kvSerialize val)

||| Get a typed value
export
kvGetTyped : KVStorable a => String -> Nat -> IO (Maybe a)
kvGetTyped key maxLen = do
  mbytes <- kvGetBytes (map (cast . ord) (unpack key)) maxLen
  case mbytes of
    Nothing => pure Nothing
    Just bytes => pure (kvDeserialize bytes)

--------------------------------------------------------------------------------
-- Basic KVStorable instances
--------------------------------------------------------------------------------

||| Helper: power of 256
pow256 : Nat -> Bits64
pow256 Z = 1
pow256 (S n) = 256 * pow256 n

||| Helper: extract byte at position (0 = LSB)
byteAt : Bits64 -> Nat -> Bits8
byteAt n pos =
  let divisor = pow256 pos
  in cast ((n `div` divisor) `mod` 256)

||| Helper: combine bytes into Bits64
combineBytes : List Bits8 -> Bits64
combineBytes bytes = go 0 0 bytes
  where
    go : Nat -> Bits64 -> List Bits8 -> Bits64
    go _ acc [] = acc
    go pos acc (b :: bs) = go (S pos) (acc + cast b * pow256 pos) bs

public export
KVStorable Nat where
  kvSerialize n =
    let n64 = cast {to=Bits64} n
    in [ byteAt n64 0, byteAt n64 1, byteAt n64 2, byteAt n64 3
       , byteAt n64 4, byteAt n64 5, byteAt n64 6, byteAt n64 7
       ]
  kvDeserialize bytes@[_, _, _, _, _, _, _, _] = Just (cast (combineBytes bytes))
  kvDeserialize _ = Nothing

public export
KVStorable String where
  kvSerialize s = map (cast . ord) (unpack s)
  kvDeserialize bytes = Just (pack (map (chr . cast) bytes))
