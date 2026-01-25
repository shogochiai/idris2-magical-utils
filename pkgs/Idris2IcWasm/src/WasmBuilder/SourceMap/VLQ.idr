||| Base64 VLQ Encoder/Decoder for Source Maps
|||
||| VLQ (Variable Length Quantity) encoding is used in Source Map V3
||| mappings field to efficiently encode position deltas.
|||
||| Reference: https://sourcemaps.info/spec.html
module WasmBuilder.SourceMap.VLQ

import Data.List
import Data.String
import Data.Nat

%default covering

-- =============================================================================
-- Base64 Encoding Table
-- =============================================================================

||| Base64 character set for VLQ
||| ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
base64Chars : String
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

||| Base64 character list (for indexing)
base64List : List Char
base64List = unpack base64Chars

||| Get character at index from list
indexList : List a -> Nat -> Maybe a
indexList [] _ = Nothing
indexList (x :: _) Z = Just x
indexList (_ :: xs) (S k) = indexList xs k

||| Get character at index from base64 table
public export
base64Encode : Nat -> Char
base64Encode n =
  let idx = min n 63
  in case indexList base64List idx of
       Just c => c
       Nothing => 'A'

||| Find index of character in list
findCharIndex : Char -> List Char -> Nat -> Maybe Nat
findCharIndex _ [] _ = Nothing
findCharIndex target (c :: rest) idx =
  if c == target then Just idx else findCharIndex target rest (S idx)

||| Decode base64 character to value (0-63)
public export
base64Decode : Char -> Maybe Nat
base64Decode c = findCharIndex c base64List 0

-- =============================================================================
-- VLQ Encoding
-- =============================================================================

||| VLQ continuation bit (bit 5)
vlqContinuationBit : Nat
vlqContinuationBit = 32

||| Convert signed integer to VLQ-ready value
||| Negative numbers use sign bit in LSB
public export
toVLQSigned : Int -> Nat
toVLQSigned n =
  if n < 0
    then cast $ (abs n) * 2 + 1
    else cast $ n * 2

||| Convert VLQ value back to signed integer
public export
fromVLQSigned : Nat -> Int
fromVLQSigned n =
  let isNeg = modNatNZ n 2 ItIsSucc == 1
      val = divNatNZ n 2 ItIsSucc
  in if isNeg then negate (cast val) else cast val

||| Power function for Nat (local to avoid collision)
natPower : Nat -> Nat -> Nat
natPower _ Z = 1
natPower base (S k) = base * natPower base k

||| Encode a single integer as VLQ Base64
||| Returns the encoded characters
public export
encodeVLQ : Int -> String
encodeVLQ n = pack $ encodeVLQ' (toVLQSigned n) []
  where
    encodeVLQ' : Nat -> List Char -> List Char
    encodeVLQ' 0 [] = [base64Encode 0]
    encodeVLQ' 0 acc = reverse acc
    encodeVLQ' val acc =
      let digit = modNatNZ val 64 ItIsSucc
          remaining = divNatNZ val 64 ItIsSucc
          -- Add continuation bit if more digits follow
          finalDigit = if remaining > 0
                       then digit + vlqContinuationBit
                       else digit
      in encodeVLQ' remaining (base64Encode finalDigit :: acc)

||| Decode VLQ from string, return (value, remaining string)
public export
decodeVLQ : String -> Maybe (Int, String)
decodeVLQ s = decodeVLQ' (unpack s) 0 0
  where
    decodeVLQ' : List Char -> Nat -> Nat -> Maybe (Int, String)
    decodeVLQ' [] _ _ = Nothing
    decodeVLQ' (c :: rest) acc shift =
      case base64Decode c of
        Nothing => Nothing
        Just digit =>
          let value = modNatNZ digit vlqContinuationBit ItIsSucc
              continuation = digit >= vlqContinuationBit
              newAcc = acc + (value * (natPower 64 shift))
          in if continuation
             then decodeVLQ' rest newAcc (S shift)
             else Just (fromVLQSigned newAcc, pack rest)

-- =============================================================================
-- Mappings Encoding/Decoding
-- =============================================================================

||| A single mapping segment
||| (generatedColumn, sourceIndex, sourceLine, sourceColumn, nameIndex)
||| nameIndex is optional (-1 for none)
public export
record Segment where
  constructor MkSegment
  genColumn : Nat      -- Generated column (0-indexed)
  sourceIdx : Nat      -- Source file index
  sourceLine : Nat     -- Source line (0-indexed)
  sourceCol : Nat      -- Source column (0-indexed)
  nameIdx : Int        -- Name index (-1 for none)

public export
Show Segment where
  show seg = "Seg(" ++ show seg.genColumn ++ "," ++
             show seg.sourceIdx ++ "," ++
             show seg.sourceLine ++ "," ++
             show seg.sourceCol ++ ")"

||| Encode a list of segments for one generated line
||| Segments are relative to previous values
public export
encodeSegments : (prevGenCol, prevSrcIdx, prevSrcLine, prevSrcCol : Nat)
              -> List Segment
              -> (String, Nat, Nat, Nat, Nat)
encodeSegments pGC pSI pSL pSC [] = ("", pGC, pSI, pSL, pSC)
encodeSegments pGC pSI pSL pSC (seg :: rest) =
  let deltaGC = cast seg.genColumn - cast pGC
      deltaSI = cast seg.sourceIdx - cast pSI
      deltaSL = cast seg.sourceLine - cast pSL
      deltaSC = cast seg.sourceCol - cast pSC
      encoded = encodeVLQ deltaGC ++
                encodeVLQ deltaSI ++
                encodeVLQ deltaSL ++
                encodeVLQ deltaSC
      (restEncoded, nGC, nSI, nSL, nSC) =
        encodeSegments seg.genColumn seg.sourceIdx seg.sourceLine seg.sourceCol rest
      separator = if null restEncoded then "" else ","
  in (encoded ++ separator ++ restEncoded, nGC, nSI, nSL, nSC)

||| State for encoding mappings
record EncodeState where
  constructor MkEncodeState
  prevSrcIdx : Nat
  prevSrcLine : Nat
  prevSrcCol : Nat
  lineNum : Nat
  result : String

||| Encode full mappings string from list of lines (each line has segments)
public export
encodeMappings : List (List Segment) -> String
encodeMappings lines =
  let initial = MkEncodeState 0 0 0 0 ""
      final = foldl encodeLine initial lines
  in final.result
  where
    encodeLine : EncodeState -> List Segment -> EncodeState
    encodeLine st segs =
      -- Reset genColumn to 0 for each new line
      let (encoded, _, nSI, nSL, nSC) = encodeSegments 0 st.prevSrcIdx st.prevSrcLine st.prevSrcCol segs
          separator = if st.lineNum == 0 then "" else ";"
      in MkEncodeState nSI nSL nSC (st.lineNum + 1) (st.result ++ separator ++ encoded)
