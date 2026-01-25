||| Solc Integration via FFI
|||
||| Calls solc via standard-json to compile Yul and generate source maps.
||| Source maps enable PC-to-source-offset mapping for coverage analysis.
|||
module Compiler.EVM.Solc

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Solc compilation output with source maps
public export
record SolcOutput where
  constructor MkSolcOutput
  bytecode : String
  runtimeBytecode : String
  sourceMap : String
  runtimeSourceMap : String

public export
Show SolcOutput where
  show o = "SolcOutput { bytecode: " ++ show (length o.bytecode) ++ " chars, " ++
           "sourceMap: " ++ show (length o.sourceMap) ++ " chars }"

||| Parsed source map entry: (startOffset, length, fileIndex)
public export
record SourceMapEntry where
  constructor MkSourceMapEntry
  startOffset : Nat
  len : Nat
  fileIndex : Nat
  jumpType : String

public export
Show SourceMapEntry where
  show e = show e.startOffset ++ ":" ++ show e.len ++ ":" ++ show e.fileIndex

-- =============================================================================
-- Standard JSON Generation
-- =============================================================================

||| Generate solc standard-json input for Yul compilation
export
generateStandardJson : (yulFileName : String) -> (yulContent : String) -> (evmVersion : String) -> String
generateStandardJson fileName content evmVersion =
  let escapedContent = escapeJsonString content
  in """
{
  "language": "Yul",
  "sources": {
    "\{fileName}": {
      "content": "\{escapedContent}"
    }
  },
  "settings": {
    "outputSelection": {
      "*": {
        "*": ["evm.bytecode.object", "evm.bytecode.sourceMap", "evm.deployedBytecode.object", "evm.deployedBytecode.sourceMap"]
      }
    },
    "evmVersion": "\{evmVersion}"
  }
}
"""
  where
    escapeJsonString : String -> String
    escapeJsonString s = concatMap escapeChar (unpack s)
      where
        escapeChar : Char -> String
        escapeChar '"' = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '\n' = "\\n"
        escapeChar '\r' = "\\r"
        escapeChar '\t' = "\\t"
        escapeChar c = singleton c

-- =============================================================================
-- JSON Parsing (minimal, focused on solc output)
-- =============================================================================

||| Find value for a key in JSON (simplified)
findJsonValue : String -> String -> Maybe String
findJsonValue key json =
  let pattern = "\"" ++ key ++ "\":"
  in case findSubstr pattern json of
       Nothing => Nothing
       Just idx =>
         let afterKey = substr (idx + length pattern) (length json) json
             trimmed = ltrim afterKey
         in extractValue trimmed
  where
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

    extractValue : String -> Maybe String
    extractValue s =
      case strUncons s of
        Nothing => Nothing
        Just ('"', rest) => extractString rest []
        Just (c, rest) => extractNonString s
      where
        extractString : String -> List Char -> Maybe String
        extractString s acc =
          case strUncons s of
            Nothing => Nothing
            Just ('"', _) => Just (pack (reverse acc))
            Just ('\\', rest) =>
              case strUncons rest of
                Just (c, rest') => extractString rest' (c :: acc)
                Nothing => Nothing
            Just (c, rest) => extractString rest (c :: acc)

        extractNonString : String -> Maybe String
        extractNonString s =
          let chars = takeWhile (\c => c /= ',' && c /= '}' && c /= ']') (unpack s)
          in Just (pack chars)

-- =============================================================================
-- Solc Execution
-- =============================================================================

||| Call solc with standard-json and extract results using jq
||| This is more reliable than parsing complex JSON in Idris
export
callSolcWithJq : (inputPath : String) -> (fileName : String) -> IO (Either String SolcOutput)
callSolcWithJq inputPath fileName = do
  -- Use jq to extract the nested values
  let jqExpr = ".contracts[\"" ++ fileName ++ "\"].Contract.evm"
  let bytecodeCmd = "solc --standard-json < " ++ inputPath ++ " | jq -r '" ++ jqExpr ++ ".bytecode.object'"
  let srcmapCmd = "solc --standard-json < " ++ inputPath ++ " | jq -r '" ++ jqExpr ++ ".bytecode.sourceMap'"
  let runtimeCmd = "solc --standard-json < " ++ inputPath ++ " | jq -r '" ++ jqExpr ++ ".deployedBytecode.object'"
  let runtimeSrcmapCmd = "solc --standard-json < " ++ inputPath ++ " | jq -r '" ++ jqExpr ++ ".deployedBytecode.sourceMap'"

  -- Execute each command and capture output
  bytecode <- readProcess bytecodeCmd
  srcmap <- readProcess srcmapCmd
  runtime <- readProcess runtimeCmd
  runtimeSrcmap <- readProcess runtimeSrcmapCmd

  case (bytecode, srcmap, runtime, runtimeSrcmap) of
    (Right bc, Right sm, Right rt, Right rsm) =>
      if bc == "null" || sm == "null"
        then pure $ Left "solc compilation failed - check Yul syntax"
        else pure $ Right $ MkSolcOutput (trim bc) (trim rt) (trim sm) (trim rsm)
    _ => pure $ Left "Failed to extract solc output"
  where
    readProcess : String -> IO (Either String String)
    readProcess cmd = do
      let outPath = "/tmp/solc-cmd-out.txt"
      exitCode <- system $ cmd ++ " > " ++ outPath ++ " 2>&1"
      if exitCode /= 0
        then pure $ Left $ "Command failed: " ++ cmd
        else do
          Right content <- readFile outPath
            | Left err => pure $ Left $ show err
          pure $ Right content

||| Compile Yul file and return bytecode with source maps
export
compileYulWithSourceMap : (yulPath : String) -> (evmVersion : String) -> IO (Either String SolcOutput)
compileYulWithSourceMap yulPath evmVersion = do
  Right content <- readFile yulPath
    | Left err => pure $ Left $ "Failed to read Yul file: " ++ show err
  let fileName = getFileName yulPath
  let stdJson = generateStandardJson fileName content evmVersion
  let inputPath = "/tmp/solc-input.json"
  Right _ <- writeFile inputPath stdJson
    | Left err => pure $ Left $ "Failed to write solc input: " ++ show err
  callSolcWithJq inputPath fileName
  where
    getFileName : String -> String
    getFileName path =
      let parts = forget $ split (== '/') path
      in fromMaybe path (last' parts)

-- =============================================================================
-- Source Map Parsing
-- =============================================================================

parseFieldNat : String -> Nat -> Nat
parseFieldNat s def =
  if null s then def
  else fromMaybe def (parsePositive s)

updateFromParts : SourceMapEntry -> List String -> SourceMapEntry
updateFromParts prev [] = prev
updateFromParts prev (s :: rest) =
  let start = parseFieldNat s prev.startOffset
      len = case rest of
              (l :: _) => parseFieldNat l prev.len
              [] => prev.len
      fIdx = case rest of
               (_ :: f :: _) => parseFieldNat f prev.fileIndex
               _ => prev.fileIndex
      jmp = case rest of
              (_ :: _ :: j :: _) => if null j then prev.jumpType else j
              _ => prev.jumpType
  in MkSourceMapEntry start len fIdx jmp

parseOneEntry : (SourceMapEntry, List SourceMapEntry) -> String -> (SourceMapEntry, List SourceMapEntry)
parseOneEntry (prev, acc) entry =
  let parts = forget $ split (== ':') entry
      newEntry = updateFromParts prev parts
  in (newEntry, newEntry :: acc)

||| Parse source map string into entries
||| Format: "start:length:fileIndex:jumpType;..."
||| Entries can omit fields, inheriting from previous
export
parseSourceMap : String -> List SourceMapEntry
parseSourceMap srcMap =
  let entries = forget $ split (== ';') srcMap
  in reverse $ snd $ foldl parseOneEntry (MkSourceMapEntry 0 0 0 "", []) entries

||| Get source offset for a given PC index
||| Returns the entry at the given instruction index
export
pcToSourceOffset : Nat -> List SourceMapEntry -> Maybe SourceMapEntry
pcToSourceOffset n [] = Nothing
pcToSourceOffset Z (x :: _) = Just x
pcToSourceOffset (S k) (_ :: xs) = pcToSourceOffset k xs

-- =============================================================================
-- Convenience API
-- =============================================================================

||| Compile Yul and save all outputs (bytecode, source map, etc.)
export
compileAndSave : (yulPath : String) -> (outputDir : String) -> (evmVersion : String) -> IO (Either String ())
compileAndSave yulPath outputDir evmVersion = do
  Right output <- compileYulWithSourceMap yulPath evmVersion
    | Left err => pure $ Left err
  let baseName = getBaseName yulPath
  Right _ <- writeFile (outputDir ++ "/" ++ baseName ++ ".bin") output.bytecode
    | Left err => pure $ Left $ "Failed to write bytecode: " ++ show err
  Right _ <- writeFile (outputDir ++ "/" ++ baseName ++ ".bin-runtime") output.runtimeBytecode
    | Left err => pure $ Left $ "Failed to write runtime bytecode: " ++ show err
  Right _ <- writeFile (outputDir ++ "/" ++ baseName ++ ".srcmap") output.sourceMap
    | Left err => pure $ Left $ "Failed to write source map: " ++ show err
  Right _ <- writeFile (outputDir ++ "/" ++ baseName ++ ".srcmap-runtime") output.runtimeSourceMap
    | Left err => pure $ Left $ "Failed to write runtime source map: " ++ show err
  pure $ Right ()
  where
    getBaseName : String -> String
    getBaseName path =
      let fileName = fromMaybe path (last' $ forget $ split (== '/') path)
      in fromMaybe fileName (head' $ forget $ split (== '.') fileName)
