||| WASM Name Section Parser
|||
||| Parses the WASM name section to extract function names.
|||
||| The name section is a custom section in WASM that contains:
||| - Module name
||| - Function names (index → name mapping)
||| - Local variable names
|||
||| We primarily use function names to map WASM functions back to Idris2 source.
module DfxCoverage.WasmMapper.NameSection

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File

import DfxCoverage.WasmMapper.WasmFunc

%default covering

-- =============================================================================
-- Name Entry Types
-- =============================================================================

||| A single name entry from the name section
public export
record NameEntry where
  constructor MkNameEntry
  index : Nat
  name : String

public export
Show NameEntry where
  show ne = show ne.index ++ " -> " ++ ne.name

public export
Eq NameEntry where
  a == b = a.index == b.index && a.name == b.name

||| All names from a WASM module
public export
record WasmNames where
  constructor MkWasmNames
  moduleName : Maybe String
  funcNames : List NameEntry
  importCount : Nat

public export
Show WasmNames where
  show wn = "WasmNames: " ++
            maybe "<anon>" id wn.moduleName ++
            " (" ++ show (length wn.funcNames) ++ " functions, " ++
            show wn.importCount ++ " imports)"

-- =============================================================================
-- Helper Functions (module level)
-- =============================================================================

||| Find index of character in string
findCharIdx : Char -> String -> Maybe Nat
findCharIdx c s = go 0 (unpack s)
  where
    go : Nat -> List Char -> Maybe Nat
    go _ [] = Nothing
    go idx (x :: rest) = if x == c then Just idx else go (S idx) rest

||| Find substring in string
findSubstr : String -> String -> Maybe Nat
findSubstr needle haystack = go 0
  where
    go : Nat -> Maybe Nat
    go idx =
      if idx + length needle > length haystack
        then Nothing
        else if substr idx (length needle) haystack == needle
          then Just idx
          else go (S idx)

||| Extract function name from $funcName pattern
extractFuncName : String -> Maybe String
extractFuncName line =
  case findCharIdx '$' line of
    Nothing => Nothing
    Just idx =>
      let afterDollar = substr (idx + 1) (length line) line
          nameChars = takeWhile (\c => isAlphaNum c || c == '_') (unpack afterDollar)
      in if null nameChars then Nothing else Just (pack nameChars)

-- =============================================================================
-- Parsing from wasm-tools output
-- =============================================================================

||| Parse import lines: (import "ic0" "msg_reply" (func $ic0_msg_reply ...))
parseWasmImports : List String -> List NameEntry
parseWasmImports = go 0
  where
    go : Nat -> List String -> List NameEntry
    go _ [] = []
    go idx (l :: rest) =
      if isInfixOf "(import" l && isInfixOf "(func $" l
        then case extractFuncName l of
          Just name => MkNameEntry idx name :: go (S idx) rest
          Nothing => go idx rest
        else go idx rest

||| Parse function definitions: (func $funcName ...)
parseWasmFuncs : Nat -> List String -> List NameEntry
parseWasmFuncs startIdx = go startIdx
  where
    go : Nat -> List String -> List NameEntry
    go _ [] = []
    go idx (l :: rest) =
      if isInfixOf "(func $" l && not (isInfixOf "(import" l)
        then case extractFuncName l of
          Just name => MkNameEntry idx name :: go (S idx) rest
          Nothing => go idx rest
        else go idx rest

||| Parse `wasm-tools print` output to extract function names
export
parseWasmToolsOutput : String -> WasmNames
parseWasmToolsOutput content =
  let contentLines = lines content
      imports = parseWasmImports contentLines
      funcs = parseWasmFuncs (length imports) contentLines
  in MkWasmNames Nothing (imports ++ funcs) (length imports)

-- =============================================================================
-- Parsing from wasm-objdump output
-- =============================================================================

||| Extract function index from "func[N]" pattern
extractFuncIndex : String -> Maybe Nat
extractFuncIndex line =
  case findSubstr "func[" line of
    Nothing => Nothing
    Just start =>
      let afterBracket = substr (start + 5) 20 line
          digits = takeWhile isDigit (unpack afterBracket)
      in if null digits then Nothing else parsePositive (pack digits)

||| Extract name from angle brackets: <name>
extractAngleName : String -> Maybe String
extractAngleName line =
  case findCharIdx '<' line of
    Nothing => Nothing
    Just start =>
      let afterAngle = substr (start + 1) (length line) line
          nameChars = takeWhile (\c => c /= '>') (unpack afterAngle)
      in if null nameChars then Nothing else Just (pack nameChars)

||| Parse `wasm-objdump -x` output for name section
export
parseObjdumpOutput : String -> WasmNames
parseObjdumpOutput content =
  let contentLines = lines content
      funcLines = filter (isInfixOf "func[") contentLines
      entries = mapMaybe parseFuncLine funcLines
  in MkWasmNames Nothing entries 0
  where
    parseFuncLine : String -> Maybe NameEntry
    parseFuncLine line =
      case (extractFuncIndex line, extractAngleName line) of
        (Just idx, Just name) => Just $ MkNameEntry idx name
        _ => Nothing

-- =============================================================================
-- Using wasm-tools CLI
-- =============================================================================

||| Extract names using wasm-tools print command
export
extractNamesWithWasmTools : String -> IO (Either String WasmNames)
extractNamesWithWasmTools wasmPath = do
  exitCode <- system $ "wasm-tools print " ++ wasmPath ++ " > /tmp/wasm-print-output.txt 2>&1"
  if exitCode /= 0
    then pure $ Left "wasm-tools print failed (is wasm-tools installed?)"
    else do
      Right content <- readFile "/tmp/wasm-print-output.txt"
        | Left err => pure $ Left $ "Failed to read output: " ++ show err
      pure $ Right $ parseWasmToolsOutput content

||| Extract names using wasm-objdump command
export
extractNamesWithObjdump : String -> IO (Either String WasmNames)
extractNamesWithObjdump wasmPath = do
  exitCode <- system $ "wasm-objdump -x " ++ wasmPath ++ " > /tmp/wasm-objdump-output.txt 2>&1"
  if exitCode /= 0
    then pure $ Left "wasm-objdump failed (is wabt installed?)"
    else do
      Right content <- readFile "/tmp/wasm-objdump-output.txt"
        | Left err => pure $ Left $ "Failed to read output: " ++ show err
      pure $ Right $ parseObjdumpOutput content

-- =============================================================================
-- Auto-detection
-- =============================================================================

||| Try to extract names using available tools
export
extractNames : String -> IO (Either String WasmNames)
extractNames wasmPath = do
  Right names <- extractNamesWithWasmTools wasmPath
    | Left _ => do
        Right names <- extractNamesWithObjdump wasmPath
          | Left err => pure $ Left $ "No WASM tools available: " ++ err
        pure $ Right names
  pure $ Right names

-- =============================================================================
-- Conversion to WasmFunc
-- =============================================================================

||| Convert NameEntry to WasmFunc
export
nameEntryToWasmFunc : NameEntry -> WasmFunc
nameEntryToWasmFunc ne =
  let kind = if isInfixOf "ic0" ne.name
               then ImportedFunc "ic0"
               else if isInfixOf "wasi" (toLower ne.name)
                 then ImportedFunc "wasi"
                 else DefinedFunc
  in MkWasmFunc ne.index ne.name kind Nothing

||| Convert WasmNames to list of WasmFunc
export
wasmNamesToFuncs : WasmNames -> List WasmFunc
wasmNamesToFuncs wn = map nameEntryToWasmFunc wn.funcNames

||| Build full mapping table from WASM file
export
buildMappingTableFromWasm : String -> IO (Either String FuncMappingTable)
buildMappingTableFromWasm wasmPath = do
  Right names <- extractNames wasmPath
    | Left err => pure $ Left err
  let funcs = wasmNamesToFuncs names
  pure $ Right $ buildMappingTable funcs
