||| EVM.Word256 - 256-bit unsigned integer for EVM
|||
||| EVM operates on 256-bit words. We represent this as
||| a pair of 128-bit Integers for basic arithmetic.
module EVM.Word256

import Data.Bits
import Data.String

%default covering

||| 256-bit unsigned word
||| Stored as (high128, low128) pair
public export
record Word256 where
  constructor MkWord256
  high : Integer
  low : Integer

||| Mask for 128 bits
mask128 : Integer
mask128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

||| Mask for 256 bits (for overflow handling)
mask256 : Integer
mask256 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

||| Zero word
public export
zero : Word256
zero = MkWord256 0 0

||| One word
public export
one : Word256
one = MkWord256 0 1

||| Maximum value (2^256 - 1)
public export
maxWord : Word256
maxWord = MkWord256 mask128 mask128

||| Create from Integer (modulo 2^256)
public export
fromInteger : Integer -> Word256
fromInteger n =
  let masked = n `mod` (mask256 + 1)
      lo = masked `mod` (mask128 + 1)
      hi = masked `div` (mask128 + 1)
  in MkWord256 hi lo

||| Convert to Integer
public export
toInteger : Word256 -> Integer
toInteger w = w.high * (mask128 + 1) + w.low

||| Equality
public export
Eq Word256 where
  w1 == w2 = w1.high == w2.high && w1.low == w2.low

||| Ordering
public export
Ord Word256 where
  compare w1 w2 =
    case compare w1.high w2.high of
      EQ => compare w1.low w2.low
      other => other

hexDigit : Integer -> Char
hexDigit d = if d < 10
             then cast (cast {to=Int} d + ord '0')
             else cast (cast {to=Int} (d - 10) + ord 'a')

partial
toHexGo : Integer -> String -> String
toHexGo 0 acc = acc
toHexGo x acc = toHexGo (x `div` 16) (strCons (hexDigit (x `mod` 16)) acc)

||| Convert integer to hex string
toHexStr : Integer -> String
toHexStr n = if n == 0 then "0" else assert_total $ toHexGo n ""

||| Show instance
public export
Show Word256 where
  show w = "0x" ++ toHexStr (toInteger w)

||| Addition (wrapping)
public export
add : Word256 -> Word256 -> Word256
add a b = fromInteger (toInteger a + toInteger b)

||| Subtraction (wrapping)
public export
sub : Word256 -> Word256 -> Word256
sub a b = fromInteger (toInteger a - toInteger b + mask256 + 1)

||| Multiplication (wrapping)
public export
mul : Word256 -> Word256 -> Word256
mul a b = fromInteger (toInteger a * toInteger b)

||| Division (returns 0 for division by 0 per EVM spec)
public export
div : Word256 -> Word256 -> Word256
div a b = if b == zero then zero else fromInteger (toInteger a `div` toInteger b)

||| Modulo (returns 0 for mod by 0 per EVM spec)
public export
mod : Word256 -> Word256 -> Word256
mod a b = if b == zero then zero else fromInteger (toInteger a `mod` toInteger b)

||| Signed division (two's complement)
public export
sdiv : Word256 -> Word256 -> Word256
sdiv a b = if b == zero then zero else
  let ai = toSigned (toInteger a)
      bi = toSigned (toInteger b)
      result = ai `div` bi
  in fromInteger result
  where
    toSigned : Integer -> Integer
    toSigned n = if n >= (mask256 + 1) `div` 2
                 then n - (mask256 + 1)
                 else n

||| Less than
public export
lt : Word256 -> Word256 -> Word256
lt a b = if a < b then one else zero

||| Greater than
public export
gt : Word256 -> Word256 -> Word256
gt a b = if a > b then one else zero

||| Equal
public export
eq : Word256 -> Word256 -> Word256
eq a b = if a == b then one else zero

||| Is zero
public export
iszero : Word256 -> Word256
iszero a = if a == zero then one else zero

||| Bitwise AND
public export
and : Word256 -> Word256 -> Word256
and a b = MkWord256 (a.high `prim__and_Integer` b.high)
                    (a.low `prim__and_Integer` b.low)

||| Bitwise OR
public export
or : Word256 -> Word256 -> Word256
or a b = MkWord256 (a.high `prim__or_Integer` b.high)
                   (a.low `prim__or_Integer` b.low)

||| Bitwise XOR
public export
xor : Word256 -> Word256 -> Word256
xor a b = MkWord256 (a.high `prim__xor_Integer` b.high)
                    (a.low `prim__xor_Integer` b.low)

||| Bitwise NOT
public export
not : Word256 -> Word256
not a = MkWord256 (mask128 - a.high) (mask128 - a.low)

||| Power function for Integer
intPow : Integer -> Nat -> Integer
intPow _ 0 = 1
intPow b (S n) = b * intPow b n

||| Get byte at position (0 = most significant)
public export
byte : Word256 -> Word256 -> Word256
byte i val =
  let idx = toInteger i
  in if idx >= 32 then zero
     else let bytePos = 31 - idx
              fullVal = toInteger val
              shifted = fullVal `div` intPow 256 (cast bytePos)
          in fromInteger (shifted `mod` 256)

||| Shift left
public export
shl : Word256 -> Word256 -> Word256
shl shift val =
  let s = toInteger shift
  in if s >= 256 then zero
     else fromInteger ((toInteger val) * intPow 2 (cast s))

||| Shift right (logical)
public export
shr : Word256 -> Word256 -> Word256
shr shift val =
  let s = toInteger shift
  in if s >= 256 then zero
     else fromInteger ((toInteger val) `div` intPow 2 (cast s))

||| Parse from hex string (with or without 0x prefix)
public export
fromHex : String -> Word256
fromHex s =
  let s' = if isPrefixOf "0x" s then substr 2 (length s) s else s
  in fromInteger (parseHexInt s')
  where
    hexVal : Char -> Integer
    hexVal c = if c >= '0' && c <= '9' then cast (ord c - ord '0')
               else if c >= 'a' && c <= 'f' then cast (ord c - ord 'a' + 10)
               else if c >= 'A' && c <= 'F' then cast (ord c - ord 'A' + 10)
               else 0

    parseHexInt : String -> Integer
    parseHexInt hex = foldl (\acc, c => acc * 16 + hexVal c) 0 (unpack hex)
