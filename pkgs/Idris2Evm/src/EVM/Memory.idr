||| EVM.Memory - EVM Memory model
|||
||| EVM memory is byte-addressable, expandable, and volatile.
||| Memory expansion costs gas (not modeled here).
module EVM.Memory

import EVM.Word256
import Data.List
import Data.Maybe
import Data.SortedMap

%default covering

||| EVM Memory - sparse byte array
||| Uses SortedMap for efficient sparse representation
public export
record Memory where
  constructor MkMemory
  bytes : SortedMap Integer Bits8
  size : Nat  -- Highest accessed address + 1 (for gas calculation)

||| Empty memory
public export
empty : Memory
empty = MkMemory empty 0

||| Read a single byte
public export
readByte : Integer -> Memory -> Bits8
readByte addr mem =
  fromMaybe 0 (lookup addr mem.bytes)

||| Write a single byte
public export
writeByte : Integer -> Bits8 -> Memory -> Memory
writeByte addr val mem =
  let newBytes = insert addr val mem.bytes
      newSize = max mem.size (cast addr + 1)
  in MkMemory newBytes newSize

||| Read a 256-bit word from memory (big-endian)
public export
readWord : Integer -> Memory -> Word256
readWord addr mem =
  let bytes = map (\i => readByte (addr + i) mem) [0..31]
      val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
  in fromInteger val

||| Write a 256-bit word to memory (big-endian) - helper
writeWordGo : Integer -> Integer -> Nat -> Memory -> Memory
writeWordGo _ _ 0 m = m
writeWordGo addr v n m =
  let byteVal = cast {to=Bits8} (v `mod` 256)
      offset = cast {to=Integer} (minus n 1)
  in writeWordGo addr (v `div` 256) (minus n 1) (writeByte (addr + offset) byteVal m)

||| Write a 256-bit word to memory (big-endian)
public export
writeWord : Integer -> Word256 -> Memory -> Memory
writeWord addr val mem = writeWordGo addr (toInteger val) 32 mem

||| Read n bytes from memory as a list
public export
readBytes : Integer -> Nat -> Memory -> List Bits8
readBytes addr 0 mem = []
readBytes addr (S n) mem = readByte addr mem :: readBytes (addr + 1) n mem

||| Write bytes to memory
public export
writeBytes : Integer -> List Bits8 -> Memory -> Memory
writeBytes addr [] mem = mem
writeBytes addr (b :: bs) mem =
  writeBytes (addr + 1) bs (writeByte addr b mem)

natDiv : Nat -> Nat -> Nat
natDiv n d = if d == 0 then 0 else cast ((cast {to=Integer} n) `div` (cast d))

||| Get current memory size (in 32-byte words, rounded up)
public export
memorySize : Memory -> Nat
memorySize mem = natDiv (mem.size + 31) 32

||| Expand memory to at least the given size
public export
expand : Nat -> Memory -> Memory
expand minSize mem =
  if minSize > mem.size
    then { size := minSize } mem
    else mem

||| MSTORE - store 32 bytes
public export
mstore : Word256 -> Word256 -> Memory -> Memory
mstore offset val mem =
  let addr = toInteger offset
      mem' = expand (cast addr + 32) mem
  in writeWord addr val mem'

||| MSTORE8 - store 1 byte
public export
mstore8 : Word256 -> Word256 -> Memory -> Memory
mstore8 offset val mem =
  let addr = toInteger offset
      byteVal = cast {to=Bits8} (toInteger val `mod` 256)
      mem' = expand (cast addr + 1) mem
  in writeByte addr byteVal mem'

||| MLOAD - load 32 bytes
public export
mload : Word256 -> Memory -> Word256
mload offset mem =
  let addr = toInteger offset
  in readWord addr mem

||| Copy memory region
public export
mcopy : (destOffset : Integer) ->
        (srcOffset : Integer) ->
        (len : Nat) ->
        Memory -> Memory
mcopy dest src 0 mem = mem
mcopy dest src len mem =
  let srcBytes = readBytes src len mem
  in writeBytes dest srcBytes mem

||| CALLDATACOPY helper - copy from calldata to memory
public export
calldatacopy : (memOffset : Integer) ->
               (dataOffset : Nat) ->
               (len : Nat) ->
               (calldata : List Bits8) ->
               Memory -> Memory
calldatacopy memOff dataOff 0 _ mem = mem
calldatacopy memOff dataOff len calldata mem =
  let dataBytes = take len (drop dataOff calldata ++ replicate len 0)
  in writeBytes memOff dataBytes (expand (cast memOff + len) mem)

||| Show instance for debugging
public export
Show Memory where
  show mem =
    "Memory(size=" ++ show mem.size ++ ", entries=" ++
    show (length $ SortedMap.toList mem.bytes) ++ ")"
