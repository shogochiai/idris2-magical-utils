||| icp:public name Section Parser
|||
||| Parses the `icp:public name` custom section from ic-wasm instrumented WASMs.
||| This section contains a Candid-encoded mapping of function indices to names.
|||
||| Format: Candid vec { nat; text } where each entry is (func_id, func_name)
module DfxCoverage.IcWasm.IcpPublicNameParser

import Data.List
import Data.Maybe
import Data.String
import Data.SortedMap
import System
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Function name mapping from icp:public name section
public export
record IcpFuncNames where
  constructor MkIcpFuncNames
  mapping : SortedMap Nat String  -- func_id -> func_name
  count : Nat

public export
Show IcpFuncNames where
  show fn = "IcpFuncNames: " ++ show fn.count ++ " functions"

||| Empty function names
export
emptyIcpFuncNames : IcpFuncNames
emptyIcpFuncNames = MkIcpFuncNames empty 0

-- =============================================================================
-- Parsing Helpers
-- =============================================================================

||| Parse hex string to bytes (pairs of hex chars)
hexToBytes : String -> List Nat
hexToBytes s = go (unpack s)
  where
    hexDigit : Char -> Maybe Nat
    hexDigit c =
      if isDigit c then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

    go : List Char -> List Nat
    go [] = []
    go [_] = []
    go (h1 :: h2 :: rest) =
      case (hexDigit h1, hexDigit h2) of
        (Just d1, Just d2) => (d1 * 16 + d2) :: go rest
        _ => go rest

||| Extract hex data from wasm-objdump output line
||| Format: "000a8c9: 4449 444c 026d 016c 0200 7a01 7101 0050  DIDL.m.l..z.q..P"
parseObjdumpLine : String -> String
parseObjdumpLine line =
  case break (== ':') (unpack line) of
    (_, []) => ""
    (_, _ :: rest) =>
      let afterColon = pack rest
          parts = words afterColon
          hexParts = takeWhile (\w => length w == 4 && all isHexDigit (unpack w)) parts
      in concat hexParts

-- =============================================================================
-- Main Parser
-- =============================================================================

||| Parse the raw bytes from icp:public name section
||| Format: section_name_len + "icp:public name" + DIDL header + vec data
||| DIDL header for vec {nat; text}: DIDL 02 6d 01 6c 02 00 7a 01 71 01 00 (15 bytes)
||| vec data: length (1 byte if < 128) + entries
||| entry: func_id (2 bytes LE) + str_len (1 byte) + string
parseIcpPublicNameBytes : List Nat -> IcpFuncNames
parseIcpPublicNameBytes bytes =
  case findDIDL bytes 0 of
       Nothing => emptyIcpFuncNames
       Just didlPos =>
         let dataStart = didlPos + 15  -- Skip DIDL header
         in case getAt dataStart bytes of
              Nothing => emptyIcpFuncNames
              Just vecLen =>
                let entries = parseEntries bytes (dataStart + 1) (cast vecLen)
                    entryMap = Data.SortedMap.fromList entries
                in MkIcpFuncNames entryMap (length entries)
  where
    getAt : Nat -> List Nat -> Maybe Nat
    getAt Z (x :: _) = Just x
    getAt (S n) (_ :: xs) = getAt n xs
    getAt _ [] = Nothing

    getAt2LE : Nat -> List Nat -> Maybe Nat
    getAt2LE idx arr =
      case (getAt idx arr, getAt (S idx) arr) of
        (Just lo, Just hi) => Just (lo + hi * 256)
        _ => Nothing

    natRange : Nat -> Nat -> List Nat
    natRange _ Z = []
    natRange start (S n) = start :: natRange (S start) n

    getString : Nat -> Nat -> List Nat -> Maybe String
    getString start len arr =
      let indices = natRange start len
          chars = mapMaybe (\i => map (chr . cast) (getAt i arr)) indices
      in if length chars == len then Just (pack chars) else Nothing

    findDIDL : List Nat -> Nat -> Maybe Nat
    findDIDL arr pos =
      case (getAt pos arr, getAt (S pos) arr, getAt (S (S pos)) arr, getAt (S (S (S pos))) arr) of
        (Just 0x44, Just 0x49, Just 0x44, Just 0x4C) => Just pos  -- "DIDL"
        (Just _, _, _, _) => findDIDL arr (S pos)
        _ => Nothing

    parseEntries : List Nat -> Nat -> Nat -> List (Nat, String)
    parseEntries arr pos 0 = []
    parseEntries arr pos remaining =
      case getAt2LE pos arr of
        Nothing => []
        Just funcId =>
          case getAt (pos + 2) arr of
            Nothing => []
            Just strLen =>
              case getString (pos + 3) strLen arr of
                Nothing => []
                Just name =>
                  let nextPos = pos + 3 + strLen
                  in (funcId, name) :: parseEntries arr nextPos (minus remaining 1)

-- =============================================================================
-- CLI Integration
-- =============================================================================

||| Parse wasm-objdump output for icp:public name section
parseObjdumpOutput : String -> List Nat
parseObjdumpOutput output =
  let outputLines = lines output
      hexLines = map parseObjdumpLine outputLines
      allHex = concat hexLines
  in hexToBytes allHex

||| Extract function names from instrumented WASM using wasm-objdump
export
extractIcpFuncNames : String -> IO (Either String IcpFuncNames)
extractIcpFuncNames wasmPath = do
  let tmpFile = "/tmp/icp_public_name_" ++ show !time ++ ".txt"
  let cmd = "wasm-objdump -s -j 'icp:public name' " ++ wasmPath ++ " > " ++ tmpFile ++ " 2>&1"
  exitCode <- system cmd
  if exitCode /= 0
    then do
      _ <- system $ "rm -f " ++ tmpFile
      pure $ Left "wasm-objdump failed or section not found (is this an ic-wasm instrumented WASM?)"
    else do
      Right content <- readFile tmpFile
        | Left err => do
            _ <- system $ "rm -f " ++ tmpFile
            pure $ Left $ "Failed to read output: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      let bytes = parseObjdumpOutput content
      if null bytes
        then pure $ Left "No data in icp:public name section"
        else pure $ Right $ parseIcpPublicNameBytes bytes

-- =============================================================================
-- Lookup Functions
-- =============================================================================

||| Look up function name by ID
export
lookupFuncName : Nat -> IcpFuncNames -> Maybe String
lookupFuncName funcId names = lookup funcId names.mapping

||| Resolve list of func IDs to names
export
resolveFuncIds : List Nat -> IcpFuncNames -> List (Nat, String)
resolveFuncIds ids names =
  mapMaybe (\id => map (\n => (id, n)) (lookupFuncName id names)) ids

||| Get all Idris function names (those starting with known prefixes)
export
getIdrisFuncNames : IcpFuncNames -> List String
getIdrisFuncNames names =
  let allNames = map snd (toList names.mapping)
      idrisPrefixes = ["Main_", "PrimIO_", "Prelude_", "Data_", "Control_"]
  in filter (\n => any (\p => isPrefixOf p n) idrisPrefixes) allNames

||| Filter to only Idris functions from resolved list
export
filterIdrisFuncs : List (Nat, String) -> List (Nat, String)
filterIdrisFuncs resolved =
  let idrisPrefixes = ["Main_", "PrimIO_", "Prelude_", "Data_", "Control_"]
  in filter (\(_, n) => any (\p => isPrefixOf p n) idrisPrefixes) resolved
