||| EVM.Storage - EVM Storage model
|||
||| EVM storage is a persistent key-value store.
||| Both keys and values are 256-bit words.
module EVM.Storage.Model

import EVM.Word256
import Data.Maybe
import Data.List
import Data.List1
import Data.String
import Data.SortedMap

%default covering

||| Storage key type (256-bit)
public export
StorageKey : Type
StorageKey = Word256

||| Storage value type (256-bit)
public export
StorageValue : Type
StorageValue = Word256

||| EVM Storage - persistent key-value mapping
public export
record Storage where
  constructor MkStorage
  slots : SortedMap Integer Word256  -- key as Integer for SortedMap

||| Empty storage
public export
empty : Storage
empty = MkStorage empty

||| SLOAD - load from storage
public export
sload : StorageKey -> Storage -> StorageValue
sload key store =
  let k = toInteger key
  in fromMaybe Word256.zero (lookup k store.slots)

||| SSTORE - store to storage
public export
sstore : StorageKey -> StorageValue -> Storage -> Storage
sstore key val store =
  let k = toInteger key
  in MkStorage (insert k val store.slots)

||| Check if slot has been accessed (for gas calculation)
public export
isWarm : StorageKey -> Storage -> Bool
isWarm key store =
  isJust (lookup (toInteger key) store.slots)

||| Get all storage slots (for debugging/introspection)
public export
toList : Storage -> List (Word256, Word256)
toList store =
  map (\(k, v) => (fromInteger k, v)) (SortedMap.toList store.slots)

||| Get number of non-zero slots
public export
nonZeroCount : Storage -> Nat
nonZeroCount store =
  length $ filter (\(_, v) => v /= Word256.zero) (SortedMap.toList store.slots)

||| Show instance for debugging
public export
Show Storage where
  show store =
    let slots = SortedMap.toList store.slots
        showSlot : (Integer, Word256) -> String
        showSlot (k, v) = "  " ++ show (Word256.fromInteger k) ++ " => " ++ show v
    in "Storage{\n" ++ unlines (map showSlot slots) ++ "}"

-- =============================================================================
-- JSON Serialization for Storage Persistence
-- =============================================================================

||| Convert Integer to hex string
intToHex : Integer -> String
intToHex n = "0x" ++ go n
  where
    hexDigit : Integer -> Char
    hexDigit i = if i < 10 then chr (ord '0' + cast i) else chr (ord 'a' + cast i - 10)
    go : Integer -> String
    go 0 = "0"
    go i = goAcc i ""
      where
        goAcc : Integer -> String -> String
        goAcc 0 acc = if acc == "" then "0" else acc
        goAcc x acc = goAcc (x `div` 16) (singleton (hexDigit (x `mod` 16)) ++ acc)

||| Parse hex string to Integer
hexToInt : String -> Maybe Integer
hexToInt s =
  let chars = unpack s
      chars' = if isPrefixOf "0x" s then drop 2 chars else chars
  in parseHexDigits chars' 0
  where
    hexVal : Char -> Maybe Integer
    hexVal c = if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
               else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
               else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
               else Nothing
    parseHexDigits : List Char -> Integer -> Maybe Integer
    parseHexDigits [] acc = Just acc
    parseHexDigits (c :: cs) acc = case hexVal c of
      Nothing => Nothing
      Just v => parseHexDigits cs (acc * 16 + v)

||| Serialize storage to JSON string
public export
toJSON : Storage -> String
toJSON store =
  let slots = SortedMap.toList store.slots
      entries = map (\(k, v) => "\"" ++ intToHex k ++ "\":\"" ++ intToHex (toInteger v) ++ "\"") slots
  in "{" ++ concat (intersperse "," entries) ++ "}"

||| Parse a simple JSON object into key-value pairs
parseJSONPairs : String -> List (String, String)
parseJSONPairs s =
  let trimmed = trim s
      chars = unpack trimmed
      inner = if isPrefixOf "{" trimmed && isSuffixOf "}" trimmed
              then pack $ drop 1 $ take (length chars `minus` 1) chars
              else trimmed
      pairs = forget $ split (== ',') inner
  in mapMaybe parsePair pairs
  where
    splitOnColon : List Char -> (List Char, List Char)
    splitOnColon cs = go [] cs
      where
        go : List Char -> List Char -> (List Char, List Char)
        go acc [] = (reverse acc, [])
        go acc (':' :: rest) = (reverse acc, rest)
        go acc (c :: rest) = go (c :: acc) rest

    parsePair : String -> Maybe (String, String)
    parsePair p =
      let chars = unpack (trim p)
      in case splitOnColon chars of
        (k, []) => Nothing
        (k, rest) =>
          let key = trim $ pack $ filter (/= '"') k
              val = trim $ pack $ filter (/= '"') rest
          in Just (key, val)

||| Deserialize storage from JSON string
public export
fromJSON : String -> Maybe Storage
fromJSON s =
  let pairs = parseJSONPairs s
      parsed = traverse parsePairToSlot pairs
  in map (\slots => MkStorage (SortedMap.fromList slots)) parsed
  where
    parsePairToSlot : (String, String) -> Maybe (Integer, Word256)
    parsePairToSlot (k, v) = do
      key <- hexToInt k
      val <- hexToInt v
      pure (key, fromInteger val)
