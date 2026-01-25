||| EVM Assembly JSON Parser
|||
||| Parses the asm.json output from solc/idris2-evm to extract:
||| - EVM instruction sequence with Yul source offsets (begin/end)
||| - Builds PC to Yul offset mapping for coverage correlation
|||
||| The asm.json file format (after text header):
|||   {".code": [...], ".data": {"0": {".code": [...]}}}
|||
||| Each instruction has:
|||   {"begin": 88, "end": 107, "name": "PUSH", "source": -1, "value": "..."}
|||
module EvmCoverage.AsmJsonParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| An EVM instruction with source location info
public export
record AsmInstr where
  constructor MkAsmInstr
  pc : Nat              -- Program counter (bytecode offset)
  beginOff : Int        -- Yul source begin offset
  endOff : Int          -- Yul source end offset
  opName : String       -- Opcode name (PUSH, JUMP, etc.)
  opValue : Maybe String -- Optional value for PUSH

public export
Show AsmInstr where
  show i = "PC=" ++ show i.pc ++ " [" ++ show i.beginOff ++ ".." ++
           show i.endOff ++ "] " ++ i.opName

public export
Eq AsmInstr where
  a == b = a.pc == b.pc && a.beginOff == b.beginOff && a.opName == b.opName

||| Mapping from PC to Yul source range
public export
record PcToYul where
  constructor MkPcToYul
  pc : Nat
  yulBegin : Int
  yulEnd : Int

public export
Show PcToYul where
  show m = "PC " ++ show m.pc ++ " -> Yul[" ++ show m.yulBegin ++ ".." ++ show m.yulEnd ++ "]"

-- =============================================================================
-- Simple JSON-like Parsing (minimal, focused on asm.json structure)
-- =============================================================================

||| Skip whitespace
skipWs : List Char -> List Char
skipWs [] = []
skipWs (c :: rest) =
  if c == ' ' || c == '\n' || c == '\t' || c == '\r'
    then skipWs rest
    else c :: rest

||| Parse a quoted string, return (string content, remaining chars)
parseQuotedStr : List Char -> Maybe (String, List Char)
parseQuotedStr ('"' :: rest) = parseStrContent rest []
  where
    parseStrContent : List Char -> List Char -> Maybe (String, List Char)
    parseStrContent [] _ = Nothing
    parseStrContent ('"' :: rest) acc = Just (pack (reverse acc), rest)
    parseStrContent ('\\' :: c :: rest) acc = parseStrContent rest (c :: acc)
    parseStrContent (c :: rest) acc = parseStrContent rest (c :: acc)
parseQuotedStr _ = Nothing

||| Parse consecutive digits
parseDigits : List Char -> List Char -> Maybe (List Char, List Char)
parseDigits [] acc = if null acc then Nothing else Just (reverse acc, [])
parseDigits (c :: rest) acc =
  if c >= '0' && c <= '9'
    then parseDigits rest (c :: acc)
    else if null acc then Nothing else Just (reverse acc, c :: rest)

||| Parse an integer (possibly negative)
parseInt : List Char -> Maybe (Int, List Char)
parseInt [] = Nothing
parseInt ('-' :: rest) =
  case parseDigits rest [] of
    Just (digits, rem) =>
      case parseInteger {a=Int} (pack digits) of
        Just n => Just (negate n, rem)
        Nothing => Nothing
    Nothing => Nothing
parseInt cs =
  case parseDigits cs [] of
    Just (digits, rem) =>
      case parseInteger {a=Int} (pack digits) of
        Just n => Just (n, rem)
        Nothing => Nothing
    Nothing => Nothing

||| Find substring in list of chars
findStrInList : List Char -> List Char -> Maybe (List Char)
findStrInList _ [] = Nothing
findStrInList needle haystack@(h :: rest) =
  if isPrefixOfList needle haystack
    then Just (drop (length needle) haystack)
    else findStrInList needle rest
  where
    isPrefixOfList : List Char -> List Char -> Bool
    isPrefixOfList [] _ = True
    isPrefixOfList _ [] = False
    isPrefixOfList (x :: xs) (y :: ys) = x == y && isPrefixOfList xs ys

-- =============================================================================
-- ASM JSON Specific Parsing
-- =============================================================================

||| Extract a field value from within an object (simplified)
||| Looks for "fieldName": and extracts the value
extractIntField : String -> String -> Maybe Int
extractIntField fieldName jsonStr =
  let pattern = "\"" ++ fieldName ++ "\":"
      chars = unpack jsonStr
  in case findStrInList (unpack pattern) chars of
       Nothing => Nothing
       Just afterField =>
         case parseInt (skipWs afterField) of
           Just (n, _) => Just n
           Nothing => Nothing

||| Extract string field value
extractStrField : String -> String -> Maybe String
extractStrField fieldName jsonStr =
  let pattern = "\"" ++ fieldName ++ "\":"
      chars = unpack jsonStr
  in case findStrInList (unpack pattern) chars of
       Nothing => Nothing
       Just afterField =>
         case parseQuotedStr (skipWs afterField) of
           Just (s, _) => Just s
           Nothing => Nothing

||| Parse a single instruction object {...}
||| Returns (instruction, remaining string after })
parseOneInstr : Nat -> String -> Maybe (AsmInstr, String)
parseOneInstr pc objStr =
  let beginM = extractIntField "begin" objStr
      endM = extractIntField "end" objStr
      nameM = extractStrField "name" objStr
      valueM = extractStrField "value" objStr
  in case (beginM, endM, nameM) of
       (Just b, Just e, Just n) =>
         Just (MkAsmInstr pc b e n valueM, "")
       _ => Nothing

||| Split on instruction boundaries
||| Find all {...} blocks within [...]
splitInstrObjects : String -> List String
splitInstrObjects content = go (unpack content) 0 [] []
  where
    go : List Char -> Nat -> List Char -> List String -> List String
    go [] _ acc result =
      if null acc then reverse result else reverse (pack (reverse acc) :: result)
    go ('{' :: rest) 0 acc result = go rest 1 ['{'] result
    go ('{' :: rest) depth acc result = go rest (S depth) ('{' :: acc) result
    go ('}' :: rest) 1 acc result =
      let obj = pack (reverse ('}' :: acc))
      in go rest 0 [] (obj :: result)
    go ('}' :: rest) depth acc result =
      case depth of
        Z => go rest 0 acc result
        S d => go rest d ('}' :: acc) result
    go (c :: rest) depth acc result =
      if depth > 0
        then go rest depth (c :: acc) result
        else go rest depth acc result

||| Calculate PC offset based on opcode
||| PUSH1 = 2 bytes, PUSH2 = 3 bytes, ... PUSH32 = 33 bytes
||| Most opcodes = 1 byte
opcodeSize : String -> Nat
opcodeSize name =
  if isPrefixOf "PUSH" name
    then case name of
           "PUSH" => 33       -- Default PUSH with 32-byte value
           "PUSH [tag]" => 3  -- Jump target (2-byte offset typically)
           "PUSH #[$]" => 33  -- Code size
           "PUSH [$]" => 33   -- Code offset
           _ => 1             -- Fallback
    else if name == "tag" || name == "JUMPDEST"
           then 1
           else 1

||| Parse instructions from a .code array content
parseCodeArray : String -> List AsmInstr
parseCodeArray content =
  let objects = splitInstrObjects content
      -- Start with PC = 0, increment based on opcode size
  in fst $ foldl processObj ([], 0) objects
  where
    processObj : (List AsmInstr, Nat) -> String -> (List AsmInstr, Nat)
    processObj (acc, pc) objStr =
      case parseOneInstr pc objStr of
        Just (instr, _) =>
          let nextPc = pc + opcodeSize instr.opName
          in (instr :: acc, nextPc)
        Nothing => (acc, pc)

-- =============================================================================
-- File Parsing
-- =============================================================================

||| Find the JSON content (skip text header lines)
extractJson : String -> String
extractJson content =
  let allLines = lines content
      jsonLines = dropWhile (\ln => not (isPrefixOf "{" (trim ln))) allLines
  in unlines jsonLines

||| Extract .code array content from JSON
||| Looks for ".code":[...] and extracts the array content
extractCodeSection : String -> Maybe String
extractCodeSection json =
  let pattern = "\".code\":["
      chars = unpack json
  in case findStrInList (unpack pattern) chars of
       Nothing => Nothing
       Just afterCode => Just $ extractArray afterCode
  where
    extractArray : List Char -> String
    extractArray cs = pack $ takeWhileLevel cs 0
      where
        takeWhileLevel : List Char -> Nat -> List Char
        takeWhileLevel [] _ = []
        takeWhileLevel (']' :: _) 0 = []
        takeWhileLevel (']' :: rest) (S n) = ']' :: takeWhileLevel rest n
        takeWhileLevel ('[' :: rest) n = '[' :: takeWhileLevel rest (S n)
        takeWhileLevel (c :: rest) n = c :: takeWhileLevel rest n

||| Extract runtime code section (.data.0.code)
extractRuntimeCode : String -> Maybe String
extractRuntimeCode json =
  -- Find .data section first
  let dataPattern = "\".data\":{\"0\":{"
      chars = unpack json
  in case findStrInList (unpack dataPattern) chars of
       Nothing => Nothing
       Just afterData => extractCodeSection (pack afterData)

||| Parse asm.json file
export
parseAsmJson : String -> List AsmInstr
parseAsmJson content =
  let json = extractJson content
  in case extractRuntimeCode json of
       Just codeContent => reverse $ parseCodeArray codeContent
       Nothing =>
         -- Try top-level .code if no .data section
         case extractCodeSection json of
           Just codeContent => reverse $ parseCodeArray codeContent
           Nothing => []

-- =============================================================================
-- PC Mapping Functions
-- =============================================================================

||| Build PC to Yul offset mapping
export
buildPcMap : List AsmInstr -> SortedMap Nat PcToYul
buildPcMap instrs = foldl addInstr empty instrs
  where
    addInstr : SortedMap Nat PcToYul -> AsmInstr -> SortedMap Nat PcToYul
    addInstr m i = insert i.pc (MkPcToYul i.pc i.beginOff i.endOff) m

||| Lookup Yul source range by PC
export
lookupByPc : SortedMap Nat PcToYul -> Nat -> Maybe PcToYul
lookupByPc m pc = lookup pc m

||| Get all PCs that map to a given Yul offset range
export
findPcsByYulRange : List AsmInstr -> Int -> Int -> List Nat
findPcsByYulRange instrs yulBegin yulEnd =
  map pc $ filter inRange instrs
  where
    inRange : AsmInstr -> Bool
    inRange i = i.beginOff >= yulBegin && i.endOff <= yulEnd

||| Filter instructions by Yul offset range
export
filterByYulRange : Int -> Int -> List AsmInstr -> List AsmInstr
filterByYulRange yulBegin yulEnd instrs =
  filter (\i => i.beginOff >= yulBegin && i.beginOff < yulEnd) instrs

-- =============================================================================
-- File I/O
-- =============================================================================

||| Read and parse asm.json file
export
readAsmJson : String -> IO (Either String (List AsmInstr))
readAsmJson path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read asm.json: " ++ show err
  let instrs = parseAsmJson content
  if null instrs
    then pure $ Left "No instructions parsed from asm.json"
    else pure $ Right instrs

||| Read asm.json and build PC map
export
readAndBuildPcMap : String -> IO (Either String (SortedMap Nat PcToYul))
readAndBuildPcMap path = do
  Right instrs <- readAsmJson path
    | Left err => pure $ Left err
  pure $ Right $ buildPcMap instrs

-- =============================================================================
-- Debug/Stats
-- =============================================================================

||| Get statistics about parsed instructions
export
asmStats : List AsmInstr -> String
asmStats instrs =
  let instrCount = length instrs
      maxPc = maybe 0 pc (head' $ reverse instrs)
      uniqueRanges = length $ nub $ map (\i => (i.beginOff, i.endOff)) instrs
  in "Instructions: " ++ show instrCount ++
     ", Max PC: " ++ show maxPc ++
     ", Unique Yul ranges: " ++ show uniqueRanges
