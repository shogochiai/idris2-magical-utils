||| Yul Function Mapper
|||
||| Parses Yul source code to extract function definitions with byte offsets.
||| Maps Yul function names back to Idris2 module.function names.
|||
||| Yul function naming convention:
|||   TextDAO_Module_SubModule_u_funcName  -> TextDAO.Module.SubModule.funcName
|||   TextDAO_Module_m_funcName_N          -> TextDAO.Module.funcName (pattern match branch N)
|||   TextDAO_Module_n_NNNN_MMMM_u_local   -> TextDAO.Module.local (nested function)
module EvmCoverage.YulMapper

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import System.File

%default covering

-- =============================================================================
-- Yul Function Types
-- =============================================================================

||| A Yul function definition with source location
public export
record YulFunc where
  constructor MkYulFunc
  name : String
  startOffset : Nat
  endOffset : Nat

public export
Show YulFunc where
  show f = f.name ++ " [" ++ show f.startOffset ++ ".." ++ show f.endOffset ++ "]"

public export
Eq YulFunc where
  a == b = a.name == b.name && a.startOffset == b.startOffset

||| Idris2 module.function representation
public export
record IdrisFunc where
  constructor MkIdrisFunc
  modulePath : String
  funcName : String
  variant : Maybe String

public export
Show IdrisFunc where
  show f = f.modulePath ++ "." ++ f.funcName ++
           maybe "" (\v => "#" ++ v) f.variant

-- =============================================================================
-- Helpers
-- =============================================================================

||| Find index of needle in haystack
findSubstrGo : Nat -> Nat -> String -> String -> Maybe Nat
findSubstrGo idx needleLen needle haystack =
  if idx + needleLen > length haystack
    then Nothing
    else if substr idx needleLen haystack == needle
           then Just idx
           else findSubstrGo (S idx) needleLen needle haystack

findSubstr : String -> String -> Maybe Nat
findSubstr needle haystack = findSubstrGo 0 (length needle) needle haystack

-- =============================================================================
-- Yul Name to Idris Name Mapping
-- =============================================================================

||| Skip numeric parts in list
skipNumeric : List String -> (List String, List String)
skipNumeric [] = ([], [])
skipNumeric (x :: rest) =
  case parsePositive {a=Nat} x of
    Just _ => let (nums, rem) = skipNumeric rest in (x :: nums, rem)
    Nothing => ([], x :: rest)

||| Helper to find function marker
findMarkerGo : List String -> List String -> Maybe (List String, String, List String)
findMarkerGo _ [] = Nothing
findMarkerGo acc ("u" :: rest) = Just (reverse acc, "u", rest)
findMarkerGo acc ("m" :: rest) = Just (reverse acc, "m", rest)
findMarkerGo acc ("n" :: rest) =
  case skipNumeric rest of
    (_, remaining) =>
      if null remaining then Nothing
      else Just (reverse acc, "n", remaining)
findMarkerGo acc (x :: rest) = findMarkerGo (x :: acc) rest

findFuncMarker : List String -> Maybe (List String, String, List String)
findFuncMarker parts = findMarkerGo [] parts

||| Extract function name and variant from parts
extractFunc : String -> List String -> (String, Maybe String)
extractFunc "m" parts =
  case reverse parts of
    [] => ("", Nothing)
    [single] => (single, Nothing)
    (num :: rest) =>
      case parsePositive {a=Nat} num of
        Just _ => (joinBy "_" (reverse rest), Just num)
        Nothing => (joinBy "_" parts, Nothing)
extractFunc _ parts = (joinBy "_" parts, Nothing)

||| Parse Yul function name to Idris module.function
export
parseYulFuncName : String -> Maybe IdrisFunc
parseYulFuncName name =
  let parts = forget $ split (== '_') name
  in case findFuncMarker parts of
       Just (modParts, marker, funcParts) =>
         let modulePath = joinBy "." modParts
             (funcNm, variant) = extractFunc marker funcParts
         in Just $ MkIdrisFunc modulePath funcNm variant
       Nothing => Nothing

-- =============================================================================
-- Yul Source Parsing
-- =============================================================================

||| Extract function name from line
extractFuncName : String -> Maybe String
extractFuncName line =
  case findSubstr "function " line of
    Nothing => Nothing
    Just idx =>
      let afterKeyword = substr (idx + 9) (length line) line
          -- Take characters until ( or space
          funcChars = takeWhile (\c => c /= '(' && c /= ' ') (unpack afterKeyword)
          funcName = pack funcChars
      in if null funcName then Nothing else Just funcName

||| Zip list with indices
zipWithIdx : Nat -> List a -> List (Nat, a)
zipWithIdx _ [] = []
zipWithIdx n (x :: xs) = (n, x) :: zipWithIdx (S n) xs

||| Parse single line for function
parseYulLine : (Nat, String) -> Maybe YulFunc
parseYulLine (lineNum, line) =
  case extractFuncName (trim line) of
    Nothing => Nothing
    Just funcName =>
      let startOff = lineNum * 80
          endOff = startOff + 1000
      in Just $ MkYulFunc funcName startOff endOff

||| Parse Yul source and extract function definitions
export
parseYulFunctions : String -> List YulFunc
parseYulFunctions content =
  let contentLines = lines content
      indexed = zipWithIdx 0 contentLines
  in mapMaybe parseYulLine indexed

-- =============================================================================
-- Lookup Functions
-- =============================================================================

||| Find function containing a byte offset
export
lookupFuncByOffset : List YulFunc -> Nat -> Maybe YulFunc
lookupFuncByOffset funcs offset =
  find (\f => offset >= f.startOffset && offset < f.endOffset) funcs

||| Get Idris function name for a Yul function
export
yulToIdris : YulFunc -> Maybe IdrisFunc
yulToIdris yf = parseYulFuncName yf.name

||| Filter functions by prefix
export
filterByPrefix : String -> List YulFunc -> List YulFunc
filterByPrefix pfx xs = filter (\f => isPrefixOf pfx (name f)) xs

-- =============================================================================
-- File I/O
-- =============================================================================

||| Read and parse Yul file
export
readYulFile : String -> IO (Either String (List YulFunc))
readYulFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read Yul file: " ++ show err
  pure $ Right $ parseYulFunctions content

||| Read Yul file and filter by prefix
export
readYulFileFiltered : String -> String -> IO (Either String (List YulFunc))
readYulFileFiltered path pfx = do
  Right funcs <- readYulFile path
    | Left err => pure $ Left err
  pure $ Right $ filterByPrefix pfx funcs
