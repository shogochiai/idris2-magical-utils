||| EVM.Bytecode - Bytecode parsing and representation
|||
||| Parses hex bytecode into a list of bytes for interpretation.
module EVM.Bytecode

import EVM.Opcodes
import Data.List
import Data.Maybe
import Data.String

%default covering

||| Bytecode is a list of bytes
public export
Bytecode : Type
Bytecode = List Bits8

||| Parse hex character to nibble
hexNibble : Char -> Maybe Bits8
hexNibble c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

||| Parse two hex characters to a byte
parseHexByte : Char -> Char -> Maybe Bits8
parseHexByte h l = do
  hi <- hexNibble h
  lo <- hexNibble l
  pure (hi * 16 + lo)

||| Check if string starts with prefix
startsWith : String -> String -> Bool
startsWith pre s =
  let plen = length pre
  in plen <= length s && substr 0 plen s == pre

||| Parse hex string to bytecode
||| Strips optional 0x prefix
export
fromHex : String -> Maybe Bytecode
fromHex s =
  let s' = if startsWith "0x" s then substr 2 (length s) s else s
      chars = unpack s'
  in parseBytes chars
  where
    parseBytes : List Char -> Maybe Bytecode
    parseBytes [] = Just []
    parseBytes [_] = Nothing  -- Odd number of characters
    parseBytes (h :: l :: rest) = do
      b <- parseHexByte h l
      bs <- parseBytes rest
      pure (b :: bs)

nibbleToHex : Bits8 -> Char
nibbleToHex n =
  if n < 10 then cast (cast {to=Int} n + ord '0')
  else cast (cast {to=Int} (n - 10) + ord 'a')

byteToHex : Bits8 -> String
byteToHex b =
  let hi = b `div` 16
      lo = b `mod` 16
  in strCons (nibbleToHex hi) (strCons (nibbleToHex lo) "")

||| Convert bytecode to hex string
export
toHex : Bytecode -> String
toHex bytes = "0x" ++ concat (map byteToHex bytes)

||| Get element at index (0-based)
indexAt : Nat -> List a -> Maybe a
indexAt _ [] = Nothing
indexAt 0 (x :: _) = Just x
indexAt (S n) (_ :: xs) = indexAt n xs

||| Read byte at position (returns 0 for out of bounds, per EVM spec)
export
readAt : Nat -> Bytecode -> Bits8
readAt n bytes = fromMaybe 0 (indexAt n bytes)

||| Read n bytes starting at position
export
readBytes : Nat -> Nat -> Bytecode -> List Bits8
readBytes start len bytes =
  take len (drop start bytes ++ replicate len 0)

||| Get bytecode length
export
codeSize : Bytecode -> Nat
codeSize = length

||| Find all JUMPDEST positions in bytecode
export
findJumpDests : Bytecode -> List Nat
findJumpDests code = go 0 code
  where
    go : Nat -> Bytecode -> List Nat
    go _ [] = []
    go pc (b :: rest) =
      let op = fromByte b
          size = pushSize op
          dests = if b == 0x5b  -- JUMPDEST
                    then [pc]
                    else []
      in dests ++ go (pc + 1 + size) (drop size rest)

||| Check if position is a valid jump destination
export
isValidJumpDest : Nat -> Bytecode -> Bool
isValidJumpDest pc code =
  case indexAt pc code of
    Just 0x5b => True  -- JUMPDEST opcode
    _ => False

||| Disassemble bytecode to list of (pc, opcode, data)
export
disassemble : Bytecode -> List (Nat, Opcode, List Bits8)
disassemble code = go 0 code
  where
    go : Nat -> Bytecode -> List (Nat, Opcode, List Bits8)
    go _ [] = []
    go pc (b :: rest) =
      let op = fromByte b
          size = pushSize op
          (pushData, remaining) = splitAt size rest
      in (pc, op, pushData) :: go (pc + 1 + size) remaining

padL : Nat -> Char -> String -> String
padL n c s =
  if length s >= n then s
  else pack (replicate (minus n (length s)) c) ++ s

showHexByte : Bits8 -> String
showHexByte b = strCons (nibbleToHex (b `div` 16)) (strCons (nibbleToHex (b `mod` 16)) "")

||| Pretty print disassembly
export
showDisassembly : Bytecode -> String
showDisassembly code =
  unlines $ map showInstr (disassemble code)
  where
    showInstr : (Nat, Opcode, List Bits8) -> String
    showInstr (pc, op, []) = padL 4 '0' (show pc) ++ ": " ++ show op
    showInstr (pc, op, data_) =
      padL 4 '0' (show pc) ++ ": " ++ show op ++ " 0x" ++
      concat (map showHexByte data_)
