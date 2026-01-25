||| Subcontract ABI: Pure Keccak256 Implementation (Buffer-based)
|||
||| A pure Idris2 implementation of Keccak256 (Ethereum's hash function).
||| Uses IOArray internally to avoid Vect-based type explosion during compilation.
|||
||| Based on the Keccak specification: https://keccak.team/keccak_specs_summary.html
||| Keccak256 uses Keccak-f[1600] with rate=1088, capacity=512, output=256 bits.
|||
module Subcontract.Core.ABI.Keccak

import Data.Bits
import Data.List
import Data.String
import Data.IOArray.Prims

-- IO operations are inherently partial
%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| 64-bit word (lane in Keccak state)
public export
Word64 : Type
Word64 = Bits64

-- =============================================================================
-- Constants (as functions to avoid type-level computation)
-- =============================================================================

||| Round constants for Keccak-f[1600] (24 rounds)
roundConstant : Int -> Word64
roundConstant 0  = 0x0000000000000001
roundConstant 1  = 0x0000000000008082
roundConstant 2  = 0x800000000000808a
roundConstant 3  = 0x8000000080008000
roundConstant 4  = 0x000000000000808b
roundConstant 5  = 0x0000000080000001
roundConstant 6  = 0x8000000080008081
roundConstant 7  = 0x8000000000008009
roundConstant 8  = 0x000000000000008a
roundConstant 9  = 0x0000000000000088
roundConstant 10 = 0x0000000080008009
roundConstant 11 = 0x000000008000000a
roundConstant 12 = 0x000000008000808b
roundConstant 13 = 0x800000000000008b
roundConstant 14 = 0x8000000000008089
roundConstant 15 = 0x8000000000008003
roundConstant 16 = 0x8000000000008002
roundConstant 17 = 0x8000000000000080
roundConstant 18 = 0x000000000000800a
roundConstant 19 = 0x800000008000000a
roundConstant 20 = 0x8000000080008081
roundConstant 21 = 0x8000000000008080
roundConstant 22 = 0x0000000080000001
roundConstant 23 = 0x8000000080008008
roundConstant _  = 0

||| Rotation offsets r[x + 5*y]
rotationOffset : Int -> Int
rotationOffset 0  = 0
rotationOffset 1  = 1
rotationOffset 2  = 62
rotationOffset 3  = 28
rotationOffset 4  = 27
rotationOffset 5  = 36
rotationOffset 6  = 44
rotationOffset 7  = 6
rotationOffset 8  = 55
rotationOffset 9  = 20
rotationOffset 10 = 3
rotationOffset 11 = 10
rotationOffset 12 = 43
rotationOffset 13 = 25
rotationOffset 14 = 39
rotationOffset 15 = 41
rotationOffset 16 = 45
rotationOffset 17 = 15
rotationOffset 18 = 21
rotationOffset 19 = 8
rotationOffset 20 = 18
rotationOffset 21 = 2
rotationOffset 22 = 61
rotationOffset 23 = 56
rotationOffset 24 = 14
rotationOffset _  = 0

||| Pi permutation: piInv[i] gives source index for destination i
piIndex : Int -> Int
piIndex 0  = 0
piIndex 1  = 6
piIndex 2  = 12
piIndex 3  = 18
piIndex 4  = 24
piIndex 5  = 3
piIndex 6  = 9
piIndex 7  = 10
piIndex 8  = 16
piIndex 9  = 22
piIndex 10 = 1
piIndex 11 = 7
piIndex 12 = 13
piIndex 13 = 19
piIndex 14 = 20
piIndex 15 = 4
piIndex 16 = 5
piIndex 17 = 11
piIndex 18 = 17
piIndex 19 = 23
piIndex 20 = 2
piIndex 21 = 8
piIndex 22 = 14
piIndex 23 = 15
piIndex 24 = 21
piIndex _  = 0

-- =============================================================================
-- Bit Operations
-- =============================================================================

||| Rotate left for 64-bit word using bit operations
rotl64 : Word64 -> Int -> Word64
rotl64 x 0 = x
rotl64 x n =
  let n64 : Bits64 = cast (n `mod` 64)
      m64 : Bits64 = cast ((64 - n) `mod` 64)
      -- Manual shift using multiplication/division for left
      -- shiftL x n = x * 2^n, shiftR x n = x / 2^n
      left = x * (pow2 n64)
      right = x `div` (pow2 m64)
  in left .|. right
  where
    pow2 : Bits64 -> Bits64
    pow2 0 = 1
    pow2 1 = 2
    pow2 2 = 4
    pow2 3 = 8
    pow2 4 = 16
    pow2 5 = 32
    pow2 6 = 64
    pow2 7 = 128
    pow2 8 = 256
    pow2 9 = 512
    pow2 10 = 1024
    pow2 11 = 2048
    pow2 12 = 4096
    pow2 13 = 8192
    pow2 14 = 16384
    pow2 15 = 32768
    pow2 16 = 65536
    pow2 17 = 131072
    pow2 18 = 262144
    pow2 19 = 524288
    pow2 20 = 1048576
    pow2 21 = 2097152
    pow2 22 = 4194304
    pow2 23 = 8388608
    pow2 24 = 16777216
    pow2 25 = 33554432
    pow2 26 = 67108864
    pow2 27 = 134217728
    pow2 28 = 268435456
    pow2 29 = 536870912
    pow2 30 = 1073741824
    pow2 31 = 2147483648
    pow2 32 = 4294967296
    pow2 33 = 8589934592
    pow2 34 = 17179869184
    pow2 35 = 34359738368
    pow2 36 = 68719476736
    pow2 37 = 137438953472
    pow2 38 = 274877906944
    pow2 39 = 549755813888
    pow2 40 = 1099511627776
    pow2 41 = 2199023255552
    pow2 42 = 4398046511104
    pow2 43 = 8796093022208
    pow2 44 = 17592186044416
    pow2 45 = 35184372088832
    pow2 46 = 70368744177664
    pow2 47 = 140737488355328
    pow2 48 = 281474976710656
    pow2 49 = 562949953421312
    pow2 50 = 1125899906842624
    pow2 51 = 2251799813685248
    pow2 52 = 4503599627370496
    pow2 53 = 9007199254740992
    pow2 54 = 18014398509481984
    pow2 55 = 36028797018963968
    pow2 56 = 72057594037927936
    pow2 57 = 144115188075855872
    pow2 58 = 288230376151711744
    pow2 59 = 576460752303423488
    pow2 60 = 1152921504606846976
    pow2 61 = 2305843009213693952
    pow2 62 = 4611686018427387904
    pow2 63 = 9223372036854775808
    pow2 _  = 1

-- =============================================================================
-- State Operations (using raw arrays)
-- =============================================================================

||| State is an array of 25 Word64 values
State : Type
State = ArrayData Word64

||| Create new state (all zeros)
newState : IO State
newState = fromPrim $ prim__newArray 25 0

||| Read from state
readState : State -> Int -> IO Word64
readState st i = fromPrim $ prim__arrayGet st i

||| Write to state
writeState : State -> Int -> Word64 -> IO ()
writeState st i v = fromPrim $ prim__arraySet st i v

-- =============================================================================
-- Keccak-f[1600] Round Function
-- =============================================================================

||| Theta step: column parity mixing
theta : State -> IO ()
theta st = do
  -- Compute column parities C[x]
  a00 <- readState st 0
  a05 <- readState st 5
  a10 <- readState st 10
  a15 <- readState st 15
  a20 <- readState st 20
  let c0 = a00 `xor` a05 `xor` a10 `xor` a15 `xor` a20

  a01 <- readState st 1
  a06 <- readState st 6
  a11 <- readState st 11
  a16 <- readState st 16
  a21 <- readState st 21
  let c1 = a01 `xor` a06 `xor` a11 `xor` a16 `xor` a21

  a02 <- readState st 2
  a07 <- readState st 7
  a12 <- readState st 12
  a17 <- readState st 17
  a22 <- readState st 22
  let c2 = a02 `xor` a07 `xor` a12 `xor` a17 `xor` a22

  a03 <- readState st 3
  a08 <- readState st 8
  a13 <- readState st 13
  a18 <- readState st 18
  a23 <- readState st 23
  let c3 = a03 `xor` a08 `xor` a13 `xor` a18 `xor` a23

  a04 <- readState st 4
  a09 <- readState st 9
  a14 <- readState st 14
  a19 <- readState st 19
  a24 <- readState st 24
  let c4 = a04 `xor` a09 `xor` a14 `xor` a19 `xor` a24

  -- Compute D[x] = C[x-1] xor rot(C[x+1], 1)
  let d0 = c4 `xor` rotl64 c1 1
  let d1 = c0 `xor` rotl64 c2 1
  let d2 = c1 `xor` rotl64 c3 1
  let d3 = c2 `xor` rotl64 c4 1
  let d4 = c3 `xor` rotl64 c0 1

  -- Apply: A[x,y] = A[x,y] xor D[x]
  writeState st 0  (a00 `xor` d0)
  writeState st 1  (a01 `xor` d1)
  writeState st 2  (a02 `xor` d2)
  writeState st 3  (a03 `xor` d3)
  writeState st 4  (a04 `xor` d4)
  writeState st 5  (a05 `xor` d0)
  writeState st 6  (a06 `xor` d1)
  writeState st 7  (a07 `xor` d2)
  writeState st 8  (a08 `xor` d3)
  writeState st 9  (a09 `xor` d4)
  writeState st 10 (a10 `xor` d0)
  writeState st 11 (a11 `xor` d1)
  writeState st 12 (a12 `xor` d2)
  writeState st 13 (a13 `xor` d3)
  writeState st 14 (a14 `xor` d4)
  writeState st 15 (a15 `xor` d0)
  writeState st 16 (a16 `xor` d1)
  writeState st 17 (a17 `xor` d2)
  writeState st 18 (a18 `xor` d3)
  writeState st 19 (a19 `xor` d4)
  writeState st 20 (a20 `xor` d0)
  writeState st 21 (a21 `xor` d1)
  writeState st 22 (a22 `xor` d2)
  writeState st 23 (a23 `xor` d3)
  writeState st 24 (a24 `xor` d4)

||| Rho step: lane rotation (unrolled)
rho : State -> IO ()
rho st = do
  v0 <- readState st 0
  writeState st 0 (rotl64 v0 (rotationOffset 0))
  v1 <- readState st 1
  writeState st 1 (rotl64 v1 (rotationOffset 1))
  v2 <- readState st 2
  writeState st 2 (rotl64 v2 (rotationOffset 2))
  v3 <- readState st 3
  writeState st 3 (rotl64 v3 (rotationOffset 3))
  v4 <- readState st 4
  writeState st 4 (rotl64 v4 (rotationOffset 4))
  v5 <- readState st 5
  writeState st 5 (rotl64 v5 (rotationOffset 5))
  v6 <- readState st 6
  writeState st 6 (rotl64 v6 (rotationOffset 6))
  v7 <- readState st 7
  writeState st 7 (rotl64 v7 (rotationOffset 7))
  v8 <- readState st 8
  writeState st 8 (rotl64 v8 (rotationOffset 8))
  v9 <- readState st 9
  writeState st 9 (rotl64 v9 (rotationOffset 9))
  v10 <- readState st 10
  writeState st 10 (rotl64 v10 (rotationOffset 10))
  v11 <- readState st 11
  writeState st 11 (rotl64 v11 (rotationOffset 11))
  v12 <- readState st 12
  writeState st 12 (rotl64 v12 (rotationOffset 12))
  v13 <- readState st 13
  writeState st 13 (rotl64 v13 (rotationOffset 13))
  v14 <- readState st 14
  writeState st 14 (rotl64 v14 (rotationOffset 14))
  v15 <- readState st 15
  writeState st 15 (rotl64 v15 (rotationOffset 15))
  v16 <- readState st 16
  writeState st 16 (rotl64 v16 (rotationOffset 16))
  v17 <- readState st 17
  writeState st 17 (rotl64 v17 (rotationOffset 17))
  v18 <- readState st 18
  writeState st 18 (rotl64 v18 (rotationOffset 18))
  v19 <- readState st 19
  writeState st 19 (rotl64 v19 (rotationOffset 19))
  v20 <- readState st 20
  writeState st 20 (rotl64 v20 (rotationOffset 20))
  v21 <- readState st 21
  writeState st 21 (rotl64 v21 (rotationOffset 21))
  v22 <- readState st 22
  writeState st 22 (rotl64 v22 (rotationOffset 22))
  v23 <- readState st 23
  writeState st 23 (rotl64 v23 (rotationOffset 23))
  v24 <- readState st 24
  writeState st 24 (rotl64 v24 (rotationOffset 24))

||| Pi step: lane permutation
pi : State -> IO State
pi st = do
  dst <- newState
  v0 <- readState st (piIndex 0)
  writeState dst 0 v0
  v1 <- readState st (piIndex 1)
  writeState dst 1 v1
  v2 <- readState st (piIndex 2)
  writeState dst 2 v2
  v3 <- readState st (piIndex 3)
  writeState dst 3 v3
  v4 <- readState st (piIndex 4)
  writeState dst 4 v4
  v5 <- readState st (piIndex 5)
  writeState dst 5 v5
  v6 <- readState st (piIndex 6)
  writeState dst 6 v6
  v7 <- readState st (piIndex 7)
  writeState dst 7 v7
  v8 <- readState st (piIndex 8)
  writeState dst 8 v8
  v9 <- readState st (piIndex 9)
  writeState dst 9 v9
  v10 <- readState st (piIndex 10)
  writeState dst 10 v10
  v11 <- readState st (piIndex 11)
  writeState dst 11 v11
  v12 <- readState st (piIndex 12)
  writeState dst 12 v12
  v13 <- readState st (piIndex 13)
  writeState dst 13 v13
  v14 <- readState st (piIndex 14)
  writeState dst 14 v14
  v15 <- readState st (piIndex 15)
  writeState dst 15 v15
  v16 <- readState st (piIndex 16)
  writeState dst 16 v16
  v17 <- readState st (piIndex 17)
  writeState dst 17 v17
  v18 <- readState st (piIndex 18)
  writeState dst 18 v18
  v19 <- readState st (piIndex 19)
  writeState dst 19 v19
  v20 <- readState st (piIndex 20)
  writeState dst 20 v20
  v21 <- readState st (piIndex 21)
  writeState dst 21 v21
  v22 <- readState st (piIndex 22)
  writeState dst 22 v22
  v23 <- readState st (piIndex 23)
  writeState dst 23 v23
  v24 <- readState st (piIndex 24)
  writeState dst 24 v24
  pure dst

||| Chi step: nonlinear mixing
chi : State -> IO ()
chi st = do
  -- Row 0
  a0 <- readState st 0
  a1 <- readState st 1
  a2 <- readState st 2
  a3 <- readState st 3
  a4 <- readState st 4
  writeState st 0 (a0 `xor` (complement a1 .&. a2))
  writeState st 1 (a1 `xor` (complement a2 .&. a3))
  writeState st 2 (a2 `xor` (complement a3 .&. a4))
  writeState st 3 (a3 `xor` (complement a4 .&. a0))
  writeState st 4 (a4 `xor` (complement a0 .&. a1))
  -- Row 1
  b0 <- readState st 5
  b1 <- readState st 6
  b2 <- readState st 7
  b3 <- readState st 8
  b4 <- readState st 9
  writeState st 5 (b0 `xor` (complement b1 .&. b2))
  writeState st 6 (b1 `xor` (complement b2 .&. b3))
  writeState st 7 (b2 `xor` (complement b3 .&. b4))
  writeState st 8 (b3 `xor` (complement b4 .&. b0))
  writeState st 9 (b4 `xor` (complement b0 .&. b1))
  -- Row 2
  c0 <- readState st 10
  c1 <- readState st 11
  c2 <- readState st 12
  c3 <- readState st 13
  c4 <- readState st 14
  writeState st 10 (c0 `xor` (complement c1 .&. c2))
  writeState st 11 (c1 `xor` (complement c2 .&. c3))
  writeState st 12 (c2 `xor` (complement c3 .&. c4))
  writeState st 13 (c3 `xor` (complement c4 .&. c0))
  writeState st 14 (c4 `xor` (complement c0 .&. c1))
  -- Row 3
  d0 <- readState st 15
  d1 <- readState st 16
  d2 <- readState st 17
  d3 <- readState st 18
  d4 <- readState st 19
  writeState st 15 (d0 `xor` (complement d1 .&. d2))
  writeState st 16 (d1 `xor` (complement d2 .&. d3))
  writeState st 17 (d2 `xor` (complement d3 .&. d4))
  writeState st 18 (d3 `xor` (complement d4 .&. d0))
  writeState st 19 (d4 `xor` (complement d0 .&. d1))
  -- Row 4
  e0 <- readState st 20
  e1 <- readState st 21
  e2 <- readState st 22
  e3 <- readState st 23
  e4 <- readState st 24
  writeState st 20 (e0 `xor` (complement e1 .&. e2))
  writeState st 21 (e1 `xor` (complement e2 .&. e3))
  writeState st 22 (e2 `xor` (complement e3 .&. e4))
  writeState st 23 (e3 `xor` (complement e4 .&. e0))
  writeState st 24 (e4 `xor` (complement e0 .&. e1))

||| Iota step: round constant addition
iota : State -> Int -> IO ()
iota st round = do
  v <- readState st 0
  writeState st 0 (v `xor` roundConstant round)

-- =============================================================================
-- Keccak-f[1600] Permutation
-- =============================================================================

||| Single round of Keccak-f[1600]
keccakRound : State -> Int -> IO State
keccakRound st round = do
  theta st
  rho st
  st' <- pi st
  chi st'
  iota st' round
  pure st'

||| Apply all 24 rounds (unrolled for totality)
keccakF : State -> IO State
keccakF st = do
  st0 <- keccakRound st 0
  st1 <- keccakRound st0 1
  st2 <- keccakRound st1 2
  st3 <- keccakRound st2 3
  st4 <- keccakRound st3 4
  st5 <- keccakRound st4 5
  st6 <- keccakRound st5 6
  st7 <- keccakRound st6 7
  st8 <- keccakRound st7 8
  st9 <- keccakRound st8 9
  st10 <- keccakRound st9 10
  st11 <- keccakRound st10 11
  st12 <- keccakRound st11 12
  st13 <- keccakRound st12 13
  st14 <- keccakRound st13 14
  st15 <- keccakRound st14 15
  st16 <- keccakRound st15 16
  st17 <- keccakRound st16 17
  st18 <- keccakRound st17 18
  st19 <- keccakRound st18 19
  st20 <- keccakRound st19 20
  st21 <- keccakRound st20 21
  st22 <- keccakRound st21 22
  keccakRound st22 23

-- =============================================================================
-- Padding and Absorption
-- =============================================================================

||| Get byte at index from list, defaulting to 0
getByteAt : Nat -> List Bits8 -> Bits8
getByteAt _ [] = 0
getByteAt Z (x :: _) = x
getByteAt (S k) (_ :: xs) = getByteAt k xs

||| Pack 8 bytes into a Word64 (little-endian)
packWord64 : List Bits8 -> Word64
packWord64 bs =
  let b0 = getByteAt 0 bs
      b1 = getByteAt 1 bs
      b2 = getByteAt 2 bs
      b3 = getByteAt 3 bs
      b4 = getByteAt 4 bs
      b5 = getByteAt 5 bs
      b6 = getByteAt 6 bs
      b7 = getByteAt 7 bs
  in cast b0
     .|. (cast b1 `shiftL` 8)
     .|. (cast b2 `shiftL` 16)
     .|. (cast b3 `shiftL` 24)
     .|. (cast b4 `shiftL` 32)
     .|. (cast b5 `shiftL` 40)
     .|. (cast b6 `shiftL` 48)
     .|. (cast b7 `shiftL` 56)

||| Convert string to bytes (ASCII)
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| Pad message with Keccak padding (pad10*1)
padMessage : List Bits8 -> List Bits8
padMessage msg =
  let rate : Nat = 136
      msgLen = length msg
      remainder = msgLen `mod` rate
      padLen = if remainder == 0 then rate else rate `minus` remainder
      zeros : List Bits8 = replicate (minus padLen 2) 0
      padding : List Bits8 = if padLen == 1
                              then [0x81]
                              else the Bits8 0x01 :: zeros ++ [the Bits8 0x80]
  in msg ++ padding

||| XOR word i of a block into state (unrolled)
xorBlockWord : State -> Int -> List Bits8 -> IO ()
xorBlockWord st i bytes = do
  let chunk = take 8 (drop (cast i * 8) bytes)
  let word = packWord64 chunk
  v <- readState st i
  writeState st i (v `xor` word)

||| XOR a block (136 bytes = 17 words) into state
xorBlock : State -> List Bits8 -> IO ()
xorBlock st bytes = do
  xorBlockWord st 0 bytes
  xorBlockWord st 1 bytes
  xorBlockWord st 2 bytes
  xorBlockWord st 3 bytes
  xorBlockWord st 4 bytes
  xorBlockWord st 5 bytes
  xorBlockWord st 6 bytes
  xorBlockWord st 7 bytes
  xorBlockWord st 8 bytes
  xorBlockWord st 9 bytes
  xorBlockWord st 10 bytes
  xorBlockWord st 11 bytes
  xorBlockWord st 12 bytes
  xorBlockWord st 13 bytes
  xorBlockWord st 14 bytes
  xorBlockWord st 15 bytes
  xorBlockWord st 16 bytes

||| Absorb all blocks (recursive but bounded by input size)
absorb : State -> List Bits8 -> IO State
absorb st [] = pure st
absorb st bytes = do
  let block = take 136 bytes
  let rest = drop 136 bytes
  xorBlock st block
  st' <- keccakF st
  assert_total $ absorb st' rest

-- =============================================================================
-- Squeezing (Output)
-- =============================================================================

||| Extract selector (first 4 bytes) as Integer
squeeze4Bytes : State -> IO Integer
squeeze4Bytes st = do
  w0 <- readState st 0
  let b0 = cast {to=Integer} ((w0 `shiftR` 0) .&. 0xff)
  let b1 = cast {to=Integer} ((w0 `shiftR` 8) .&. 0xff)
  let b2 = cast {to=Integer} ((w0 `shiftR` 16) .&. 0xff)
  let b3 = cast {to=Integer} ((w0 `shiftR` 24) .&. 0xff)
  pure ((b0 `prim__shl_Integer` 24) + (b1 `prim__shl_Integer` 16) + (b2 `prim__shl_Integer` 8) + b3)

-- =============================================================================
-- Public API
-- =============================================================================

||| Compute the 4-byte function selector from signature string
||| This is what Ethereum uses for function dispatch
export
selector : String -> IO Integer
selector input = do
  let bytes = stringToBytes input
  let padded = padMessage bytes
  st <- newState
  st' <- absorb st padded
  squeeze4Bytes st'

||| Compute selector (unsafe IO extraction for use in pure context)
||| WARNING: Uses unsafePerformIO - only safe because keccak256 is deterministic
export
selectorPure : String -> Integer
selectorPure input = unsafePerformIO (selector input)
