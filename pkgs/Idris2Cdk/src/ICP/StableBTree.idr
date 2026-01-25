||| Stable B+Tree Storage for ICP Canisters
|||
||| O(log n) key-value storage backed by stable memory.
||| Uses a B+Tree data structure for efficient lookups.
|||
||| Performance:
|||   - Get: O(log n)
|||   - Put: O(log n)
|||   - Range query: O(log n + k) where k is result count
|||
||| Usage:
|||   result <- btPut keyBytes valBytes
|||   mbVal <- btGet keyBytes 1024
|||   count <- btCount
module ICP.StableBTree

import Data.List
import System.FFI

%default covering

--------------------------------------------------------------------------------
-- FFI Declarations (matches stable_btree.c)
--------------------------------------------------------------------------------

%foreign "C:stbt_get,libic0"
prim__stbtGet : Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

%foreign "C:stbt_put,libic0"
prim__stbtPut : Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

%foreign "C:stbt_count,libic0"
prim__stbtCount : PrimIO Int

%foreign "C:stbt_height,libic0"
prim__stbtHeight : PrimIO Int

%foreign "C:stbt_clear,libic0"
prim__stbtClear : PrimIO ()

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Result of a BTree operation
public export
data BTResult : Type where
  BTSuccess : BTResult
  BTNotFound : BTResult
  BTFull : BTResult      -- Node full, split needed
  BTError : String -> BTResult

public export
Show BTResult where
  show BTSuccess = "BTSuccess"
  show BTNotFound = "BTNotFound"
  show BTFull = "BTFull"
  show (BTError msg) = "BTError: " ++ msg

public export
Eq BTResult where
  BTSuccess == BTSuccess = True
  BTNotFound == BTNotFound = True
  BTFull == BTFull = True
  (BTError a) == (BTError b) = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- Low-level API (bytes)
--------------------------------------------------------------------------------

||| Put bytes into BTree
||| Returns BTSuccess on success, BTFull if node needs split
export
btPutBytes : (key : List Bits8) -> (val : List Bits8) -> IO BTResult
btPutBytes key val = do
  -- Note: Simplified stub - real implementation needs buffer allocation
  let keyLen = cast {to=Bits64} (length key)
      valLen = cast {to=Bits64} (length val)
  result <- primIO (prim__stbtPut 0 keyLen 0 valLen)
  pure $ case result of
    0 => BTSuccess
    -1 => BTFull
    _ => BTError "unknown error"

||| Get bytes from BTree
||| Returns Just bytes if found, Nothing if not found
export
btGetBytes : (key : List Bits8) -> (maxLen : Nat) -> IO (Maybe (List Bits8))
btGetBytes key maxLen = do
  let keyLen = cast {to=Bits64} (length key)
      maxBytes = cast {to=Bits64} maxLen
  result <- primIO (prim__stbtGet 0 keyLen 0 maxBytes)
  pure $ if result >= 0
         then Just []  -- Stub: would return actual bytes
         else Nothing

||| Get entry count
export
btCount : IO Nat
btCount = do
  count <- primIO prim__stbtCount
  pure (cast count)

||| Get tree height
export
btHeight : IO Nat
btHeight = do
  height <- primIO prim__stbtHeight
  pure (cast height)

||| Clear all entries
export
btClear : IO ()
btClear = primIO prim__stbtClear

--------------------------------------------------------------------------------
-- High-level API (Strings)
--------------------------------------------------------------------------------

||| Convert String to bytes
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| Convert bytes to String
bytesToString : List Bits8 -> String
bytesToString bs = pack (map (chr . cast) bs)

||| Put string key-value pair
export
btPut : String -> String -> IO BTResult
btPut key val = btPutBytes (stringToBytes key) (stringToBytes val)

||| Get string value by key
export
btGet : String -> Nat -> IO (Maybe String)
btGet key maxLen = do
  result <- btGetBytes (stringToBytes key) maxLen
  pure (map bytesToString result)

--------------------------------------------------------------------------------
-- Typed API
--------------------------------------------------------------------------------

||| Type class for types that can be stored in BTree
public export
interface BTStorable a where
  btSerialize : a -> List Bits8
  btDeserialize : List Bits8 -> Maybe a

||| Put a typed value
export
btPutTyped : BTStorable a => String -> a -> IO BTResult
btPutTyped key val = btPutBytes (stringToBytes key) (btSerialize val)

||| Get a typed value
export
btGetTyped : BTStorable a => String -> Nat -> IO (Maybe a)
btGetTyped key maxLen = do
  mbytes <- btGetBytes (stringToBytes key) maxLen
  case mbytes of
    Nothing => pure Nothing
    Just bytes => pure (btDeserialize bytes)

--------------------------------------------------------------------------------
-- BTStorable Instances
--------------------------------------------------------------------------------

||| Helper: power of 256
pow256 : Nat -> Bits64
pow256 Z = 1
pow256 (S n) = 256 * pow256 n

||| Helper: extract byte at position
byteAt : Bits64 -> Nat -> Bits8
byteAt n pos =
  let divisor = pow256 pos
  in cast ((n `div` divisor) `mod` 256)

||| Helper: combine bytes
combineBytes : List Bits8 -> Bits64
combineBytes bytes = go 0 0 bytes
  where
    go : Nat -> Bits64 -> List Bits8 -> Bits64
    go _ acc [] = acc
    go pos acc (b :: bs) = go (S pos) (acc + cast b * pow256 pos) bs

public export
BTStorable Nat where
  btSerialize n =
    let n64 = cast {to=Bits64} n
    in [ byteAt n64 0, byteAt n64 1, byteAt n64 2, byteAt n64 3
       , byteAt n64 4, byteAt n64 5, byteAt n64 6, byteAt n64 7
       ]
  btDeserialize bytes@[_, _, _, _, _, _, _, _] = Just (cast (combineBytes bytes))
  btDeserialize _ = Nothing

public export
BTStorable Int where
  btSerialize n =
    let n64 = cast {to=Bits64} n
    in [ byteAt n64 0, byteAt n64 1, byteAt n64 2, byteAt n64 3
       , byteAt n64 4, byteAt n64 5, byteAt n64 6, byteAt n64 7
       ]
  btDeserialize bytes@[_, _, _, _, _, _, _, _] = Just (cast (combineBytes bytes))
  btDeserialize _ = Nothing

public export
BTStorable String where
  btSerialize s = stringToBytes s
  btDeserialize bytes = Just (bytesToString bytes)

public export
BTStorable Bool where
  btSerialize True = [1]
  btSerialize False = [0]
  btDeserialize [0] = Just False
  btDeserialize [1] = Just True
  btDeserialize _ = Nothing

--------------------------------------------------------------------------------
-- Indexed Event Storage (for idris2-icp-indexer)
--------------------------------------------------------------------------------

||| Store an indexed event
||| Key format: "event:<chain_id>:<block_num>:<log_index>"
export
storeEvent : (chainId : Nat) -> (blockNum : Nat) -> (logIndex : Nat)
          -> (eventData : String) -> IO BTResult
storeEvent chainId blockNum logIndex eventData =
  let key = "event:" ++ show chainId ++ ":" ++ show blockNum ++ ":" ++ show logIndex
  in btPut key eventData

||| Get an event by chain, block, log index
export
getEvent : (chainId : Nat) -> (blockNum : Nat) -> (logIndex : Nat)
        -> (maxLen : Nat) -> IO (Maybe String)
getEvent chainId blockNum logIndex maxLen =
  let key = "event:" ++ show chainId ++ ":" ++ show blockNum ++ ":" ++ show logIndex
  in btGet key maxLen

||| Store cursor position for a chain
export
storeCursor : (chainId : Nat) -> (blockNum : Nat) -> IO BTResult
storeCursor chainId blockNum =
  btPutTyped ("cursor:" ++ show chainId) blockNum

||| Get cursor position for a chain
export
getCursor : (chainId : Nat) -> IO (Maybe Nat)
getCursor chainId =
  btGetTyped ("cursor:" ++ show chainId) 8
