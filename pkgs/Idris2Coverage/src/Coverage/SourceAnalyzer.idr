||| Idris2 source file analyzer
||| REQ_COV_SRC_001 - REQ_COV_SRC_004
module Coverage.SourceAnalyzer

import Coverage.Types
import Data.List
import Data.Maybe
import Data.String
import System.File

%default total

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Zip a list with indices starting from 1
zipWithIndex : List a -> List (Nat, a)
zipWithIndex = go 1
  where
    go : Nat -> List a -> List (Nat, a)
    go _ [] = []
    go n (x :: xs) = (n, x) :: go (S n) xs

||| Check if string is blank (whitespace only)
isBlank : String -> Bool
isBlank s = all isSpace (unpack s)

-- =============================================================================
-- Function Definition Record
-- =============================================================================

||| Parsed function definition from Idris source
public export
record FunctionDef where
  constructor MkFunctionDef
  name      : String
  signature : Maybe String
  lineStart : Nat
  lineEnd   : Nat
  exported  : Bool

public export
Show FunctionDef where
  show f = "FunctionDef(\{f.name}, \{show f.lineStart}-\{show f.lineEnd}, exported=\{show f.exported})"

-- =============================================================================
-- Export Declaration Parsing
-- =============================================================================

||| Check if a line is an export declaration
isExportLine : String -> Bool
isExportLine line =
  let trimmed = trim line
  in isPrefixOf "export" trimmed || isPrefixOf "public export" trimmed

||| Extract function name from signature line
||| e.g., "runAudit : AuditOptions -> IO ()" -> "runAudit"
extractFuncName : String -> Maybe String
extractFuncName line =
  let trimmed = trim line
      parts = break (== ':') (unpack trimmed)
  in if null (snd parts)
        then Nothing
        else let funcName = trim $ pack $ fst parts
             in if funcName /= "" && not (isPrefixOf "--" funcName)
                   then Just funcName
                   else Nothing

||| Extract signature from line
||| e.g., "runAudit : AuditOptions -> IO ()" -> "AuditOptions -> IO ()"
extractSignature : String -> Maybe String
extractSignature line =
  let parts = break (== ':') (unpack line)
  in if null (snd parts)
        then Nothing
        else Just $ trim $ pack $ drop 1 $ snd parts

-- =============================================================================
-- Function Definition Extraction
-- =============================================================================

||| REQ_COV_SRC_001: Extract export declarations from .idr files
||| REQ_COV_SRC_002: Determine line_start and line_end for each function
||| REQ_COV_SRC_003: Extract function signatures
export
covering
analyzeSource : String -> List FunctionDef
analyzeSource content =
  let ls = zipWithIndex (lines content)
  in extractFunctions ls False Nothing
  where
    extractFunctions : List (Nat, String) -> Bool -> Maybe (String, Nat, Maybe String) -> List FunctionDef
    extractFunctions [] _ Nothing = []
    extractFunctions [] _ (Just (funcName, startLine, sig)) =
      -- End of file, close pending function
      [MkFunctionDef funcName sig startLine startLine True]
    extractFunctions ((lineNum, line) :: rest) wasExport pending =
      let trimmed = trim line
          isExport = isExportLine trimmed
          -- Check if this is a type signature (name : Type)
          mName = extractFuncName trimmed
          mSig = extractSignature trimmed
      in case pending of
           Nothing =>
             -- Not currently in a function
             if isExport
                then extractFunctions rest True Nothing
                else case mName of
                       Nothing => extractFunctions rest False Nothing
                       Just funcName =>
                         if wasExport
                            then extractFunctions rest False (Just (funcName, lineNum, mSig))
                            else extractFunctions rest False Nothing
           Just (funcName, startLine, sig) =>
             -- Currently tracking a function
             if isBlank trimmed || isPrefixOf "--" trimmed
                then extractFunctions rest wasExport pending
                else if isExport || (isJust mName && trim line == line)
                        -- New function starts
                        then MkFunctionDef funcName sig startLine (lineNum `minus` 1) True
                             :: extractFunctions ((lineNum, line) :: rest) False Nothing
                        else extractFunctions rest wasExport pending

-- =============================================================================
-- Module Name Extraction
-- =============================================================================

||| Extract module name from source content
export
extractModuleName : String -> Maybe String
extractModuleName content =
  let ls = lines content
  in findModule ls
  where
    findModule : List String -> Maybe String
    findModule [] = Nothing
    findModule (line :: rest) =
      let trimmed = trim line
      in if isPrefixOf "module " trimmed
            then Just $ trim $ pack $ drop 7 $ unpack trimmed
            else findModule rest

-- =============================================================================
-- File Analysis
-- =============================================================================

||| REQ_COV_SRC_004: Handle multi-line function definitions
||| Analyze an Idris source file
export
covering
analyzeFile : String -> IO (Either String (String, List FunctionDef))
analyzeFile path = do
  Right content <- readFile path
    | Left err => pure $ Left "Failed to read \{path}: \{show err}"
  let moduleName = fromMaybe "Unknown" $ extractModuleName content
  let funcs = analyzeSource content
  pure $ Right (moduleName, funcs)

-- =============================================================================
-- Exported Function List
-- =============================================================================

||| Get list of exported function names from source
export
covering
getExportedFunctions : String -> List String
getExportedFunctions content =
  map (.name) $ analyzeSource content
