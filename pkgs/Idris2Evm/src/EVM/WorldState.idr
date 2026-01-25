||| EVM.WorldState - Multi-contract world state
|||
||| Manages multiple contract accounts, each with their own code and storage.
||| Supports CALL, DELEGATECALL, and STATICCALL semantics.
module EVM.WorldState

import EVM.Word256
import EVM.Storage
import EVM.Bytecode
import Data.Maybe
import Data.List
import Data.List1
import Data.String
import Data.SortedMap

%default covering

-- =============================================================================
-- Account Model
-- =============================================================================

||| EVM Account - represents a contract or EOA
public export
record Account where
  constructor MkAccount
  code : Bytecode
  storage : Storage
  balance : Word256
  nonce : Integer

||| Empty account (no code, no storage)
public export
emptyAccount : Account
emptyAccount = MkAccount [] Storage.empty Word256.zero 0

||| Account with just code (fresh deployment)
public export
accountWithCode : Bytecode -> Account
accountWithCode code = MkAccount code Storage.empty Word256.zero 0

-- =============================================================================
-- World State
-- =============================================================================

||| World State - maps addresses to accounts
public export
record WorldState where
  constructor MkWorldState
  accounts : SortedMap Integer Account

||| Empty world state
public export
empty : WorldState
empty = MkWorldState empty

||| Get account at address (returns empty account if not exists)
public export
getAccount : Word256 -> WorldState -> Account
getAccount addr world =
  fromMaybe emptyAccount (lookup (toInteger addr) world.accounts)

||| Set account at address
public export
setAccount : Word256 -> Account -> WorldState -> WorldState
setAccount addr acc world =
  MkWorldState (insert (toInteger addr) acc world.accounts)

||| Check if account exists
public export
accountExists : Word256 -> WorldState -> Bool
accountExists addr world =
  isJust (lookup (toInteger addr) world.accounts)

||| Get code at address
public export
getCode : Word256 -> WorldState -> Bytecode
getCode addr world = (getAccount addr world).code

||| Get storage value at address and slot
public export
getStorageAt : Word256 -> Word256 -> WorldState -> Word256
getStorageAt addr slot world =
  sload slot (getAccount addr world).storage

||| Set storage value at address and slot
public export
setStorageAt : Word256 -> Word256 -> Word256 -> WorldState -> WorldState
setStorageAt addr slot val world =
  let acc = getAccount addr world
      newStorage = sstore slot val acc.storage
      newAcc = { storage := newStorage } acc
  in setAccount addr newAcc world

||| Get balance at address
public export
getBalance : Word256 -> WorldState -> Word256
getBalance addr world = (getAccount addr world).balance

||| Set balance at address
public export
setBalance : Word256 -> Word256 -> WorldState -> WorldState
setBalance addr bal world =
  let acc = getAccount addr world
      newAcc = { balance := bal } acc
  in setAccount addr newAcc world

||| Deploy contract at address
public export
deployContract : Word256 -> Bytecode -> WorldState -> WorldState
deployContract addr code world =
  setAccount addr (accountWithCode code) world

||| Get all addresses in world state
public export
addresses : WorldState -> List Word256
addresses world = map fromInteger (keys world.accounts)

-- =============================================================================
-- Storage Operations (for specific address context)
-- =============================================================================

||| Update storage for a specific address and return new world state
public export
updateStorage : Word256 -> Storage -> WorldState -> WorldState
updateStorage addr newStorage world =
  let acc = getAccount addr world
      newAcc = { storage := newStorage } acc
  in setAccount addr newAcc world

||| Get storage for a specific address
public export
getStorage : Word256 -> WorldState -> Storage
getStorage addr world = (getAccount addr world).storage

-- =============================================================================
-- JSON Serialization
-- =============================================================================

||| Convert Integer to hex string (reuse from Storage)
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

||| Serialize account to JSON
accountToJSON : Account -> String
accountToJSON acc =
  "{\"code\":\"" ++ Bytecode.toHex acc.code ++
  "\",\"storage\":" ++ Storage.toJSON acc.storage ++
  ",\"balance\":\"" ++ intToHex (toInteger acc.balance) ++
  "\",\"nonce\":" ++ show acc.nonce ++ "}"

||| Serialize world state to JSON
public export
toJSON : WorldState -> String
toJSON world =
  let accs = SortedMap.toList world.accounts
      entries = map (\(addr, acc) => "\"" ++ intToHex addr ++ "\":" ++ accountToJSON acc) accs
  in "{\"accounts\":{" ++ concat (intersperse "," entries) ++ "}}"

-- =============================================================================
-- JSON Parsing Helpers
-- =============================================================================

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

-- Note: Full JSON parsing for WorldState is complex.
-- For now, we provide a simpler format for loading contracts.

||| Load world state from simple format:
||| Each line: address:bytecode_hex:storage_json
||| Example: 0x1000:608060...:{"0x0":"0x1"}
||| Note: Uses first two colons as delimiters, rest is storage JSON
public export
fromSimpleFormat : String -> Maybe WorldState
fromSimpleFormat content =
  let linesList = forget $ split (== '\n') content
      nonEmpty = filter (\l => length (trim l) > 0) linesList
  in foldlM parseLine empty nonEmpty
  where
    -- Split on first colon only
    splitFirst : Char -> String -> Maybe (String, String)
    splitFirst c s =
      let chars = unpack s
      in case break (== c) chars of
        (before, []) => Nothing
        (before, _ :: after) => Just (pack before, pack after)

    parseLine : WorldState -> String -> Maybe WorldState
    parseLine world line = do
      -- Split: address : rest
      (addrStr, rest1) <- splitFirst ':' line
      addr <- hexToInt (trim addrStr)
      -- Split: bytecode : storage (or just bytecode)
      case splitFirst ':' rest1 of
        Nothing => do
          -- Only bytecode, no storage
          code <- Bytecode.fromHex (trim rest1)
          pure $ deployContract (fromInteger addr) code world
        Just (codeStr, storageStr) => do
          code <- Bytecode.fromHex (trim codeStr)
          storage <- Storage.fromJSON (trim storageStr)
          let acc = MkAccount code storage Word256.zero 0
          pure $ setAccount (fromInteger addr) acc world

||| Export world state to simple format
public export
toSimpleFormat : WorldState -> String
toSimpleFormat world =
  let accs = SortedMap.toList world.accounts
      lines = map formatAccount accs
  in unlines lines
  where
    formatAccount : (Integer, Account) -> String
    formatAccount (addr, acc) =
      intToHex addr ++ ":" ++ Bytecode.toHex acc.code ++ ":" ++ Storage.toJSON acc.storage

-- =============================================================================
-- Show Instance
-- =============================================================================

public export
Show Account where
  show acc = "Account{code=" ++ show (length acc.code) ++ " bytes, " ++
             "storage=" ++ show (Storage.nonZeroCount acc.storage) ++ " slots, " ++
             "balance=" ++ show acc.balance ++ "}"

public export
covering
Show WorldState where
  show world =
    let accs = SortedMap.toList world.accounts
        showAcc : (Integer, Account) -> String
        showAcc (addr, acc) = "  " ++ assert_total (intToHex addr) ++ " => " ++ show acc
    in "WorldState{\n" ++ unlines (map showAcc accs) ++ "}"
