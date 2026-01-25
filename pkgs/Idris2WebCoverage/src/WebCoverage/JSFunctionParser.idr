||| JS Function Definition Parser
|||
||| Parses Idris2-generated JavaScript to extract function definitions
||| with their byte offsets for V8 coverage matching.
|||
||| Idris2 JS codegen produces:
|||   /* Module.Name.funcName : Type */
|||   const Module_Name_funcName = __lazy(function () { ... });
|||
||| This module extracts function names and byte ranges.
module WebCoverage.JSFunctionParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Nat

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| A parsed JS function definition
public export
record JSFunctionDef where
  constructor MkJSFunctionDef
  idrisName : String      -- Original Idris name (e.g., "OucDashboard.Update.update")
  jsName : String         -- JS name (e.g., "OucDashboard_Update_update")
  startOffset : Nat       -- Byte offset where function starts
  endOffset : Nat         -- Byte offset where function ends (approximate)
  lineNumber : Nat        -- Line number in JS file (1-indexed)

public export
Show JSFunctionDef where
  show f = f.idrisName ++ " @ " ++ show f.startOffset ++ "-" ++ show f.endOffset

-- =============================================================================
-- Line Offset Calculation
-- =============================================================================

||| Calculate cumulative byte offsets for each line
||| Returns: List (lineNumber, startByteOffset)
export
calculateLineOffsets : String -> List (Nat, Nat)
calculateLineOffsets content =
  let ls = lines content
  in go 1 0 ls
  where
    go : Nat -> Nat -> List String -> List (Nat, Nat)
    go _ _ [] = []
    go lineNum offset (l :: rest) =
      let lineLen = length l + 1  -- +1 for newline
      in (lineNum, offset) :: go (S lineNum) (offset + lineLen) rest

||| Convert byte offset to line number
export
offsetToLine : List (Nat, Nat) -> Nat -> Nat
offsetToLine [] _ = 0
offsetToLine ((lineNum, startOff) :: rest) offset =
  case rest of
    [] => lineNum
    ((nextLine, nextOff) :: _) =>
      if offset >= startOff && offset < nextOff
        then lineNum
        else offsetToLine rest offset

-- =============================================================================
-- JS Comment Parsing
-- =============================================================================

||| Extract Idris function name from JS comment
||| Input: "/* OucDashboard.Tests.AllTests.test_REQ : IO Bool */"
||| Output: Just "OucDashboard.Tests.AllTests.test_REQ"
parseIdrisComment : String -> Maybe String
parseIdrisComment s =
  let trimmed = trim s
  in if isPrefixOf "/*" trimmed && isSuffixOf "*/" trimmed
       then let content = substr 2 (length trimmed `minus` 4) trimmed
                -- Find the colon separator (before type signature)
                parts = break (== ':') content
            in case fst parts of
                 "" => Nothing
                 name => Just (trim name)
       else Nothing

||| Check if line defines a const or function
isDefinitionLine : String -> Bool
isDefinitionLine s =
  let trimmed = trim s
  in isPrefixOf "const " trimmed || isPrefixOf "function " trimmed

||| Extract JS function name from definition line
||| Input: "const OucDashboard_Update_update = __lazy(function () {"
||| Output: Just "OucDashboard_Update_update"
parseJSFuncName : String -> Maybe String
parseJSFuncName s =
  let trimmed = trim s
  in if isPrefixOf "const " trimmed
       then let rest = substr 6 (length trimmed) trimmed
                name = takeWhile (\c => c /= ' ' && c /= '=') (unpack rest)
            in Just (pack name)
       else if isPrefixOf "function " trimmed
            then let rest = substr 9 (length trimmed) trimmed
                     name = takeWhile (\c => c /= '(' && c /= ' ') (unpack rest)
                 in Just (pack name)
            else Nothing

-- =============================================================================
-- Main Parser
-- =============================================================================

||| Parse JS file content and extract function definitions
|||
||| Strategy:
||| 1. Find lines starting with "/*" that contain Idris function names
||| 2. The next line should be a const/function definition
||| 3. Record the byte offset of the definition line
export
parseJSFunctions : String -> List JSFunctionDef
parseJSFunctions content =
  let ls = lines content
      lineOffsets = calculateLineOffsets content
      indexed = zip [1..length ls] ls
  in go indexed lineOffsets
  where
    findLineOffset : Nat -> List (Nat, Nat) -> Nat
    findLineOffset lineNum offsets =
      case find (\(ln, _) => ln == lineNum) offsets of
        Nothing => 0
        Just (_, off) => off

    -- Estimate function end offset (next function start or file end)
    estimateEndOffset : Nat -> List (Nat, Nat) -> Nat -> Nat
    estimateEndOffset startLine offsets fileLen =
      case find (\(ln, _) => ln > startLine + 10) offsets of
        Nothing => fileLen
        Just (_, off) => off

    go : List (Nat, String) -> List (Nat, Nat) -> List JSFunctionDef
    go [] _ = []
    go ((_, _) :: []) _ = []
    go ((lineNum, line) :: (nextLineNum, nextLine) :: rest) offsets =
      case parseIdrisComment line of
        Nothing => go ((nextLineNum, nextLine) :: rest) offsets
        Just idrisName =>
          if isDefinitionLine nextLine
            then case parseJSFuncName nextLine of
                   Nothing => go ((nextLineNum, nextLine) :: rest) offsets
                   Just jsName =>
                     let startOff = findLineOffset nextLineNum offsets
                         endOff = estimateEndOffset nextLineNum offsets (length content)
                         def = MkJSFunctionDef idrisName jsName startOff endOff nextLineNum
                     in def :: go ((nextLineNum, nextLine) :: rest) offsets
            else go ((nextLineNum, nextLine) :: rest) offsets

-- =============================================================================
-- Coverage Matching
-- =============================================================================

||| Find which function contains a given byte offset
export
findFunctionAtOffset : List JSFunctionDef -> Nat -> Maybe JSFunctionDef
findFunctionAtOffset [] _ = Nothing
findFunctionAtOffset (f :: fs) offset =
  if offset >= f.startOffset && offset < f.endOffset
    then Just f
    else findFunctionAtOffset fs offset

||| Result of range matching: function name, executed count, total count
public export
record FuncRangeMatch where
  constructor MkFuncRangeMatch
  funcName : String
  executedRanges : Nat
  totalRanges : Nat

||| Match V8 ranges to JS functions and count coverage per function
export
matchRangesToFunctions : List JSFunctionDef
                       -> List (Nat, Nat, Nat)  -- (startOffset, endOffset, count)
                       -> List FuncRangeMatch
matchRangesToFunctions funcs ranges =
  filter (\m => m.totalRanges > 0) (map countFunc funcs)
  where
    rangeInFunc : JSFunctionDef -> Nat -> Bool
    rangeInFunc f rangeStart = rangeStart >= f.startOffset && rangeStart < f.endOffset

    getStart : (Nat, Nat, Nat) -> Nat
    getStart (s, _, _) = s

    getCount : (Nat, Nat, Nat) -> Nat
    getCount (_, _, c) = c

    countFunc : JSFunctionDef -> FuncRangeMatch
    countFunc f =
      let funcRanges = filter (\r => rangeInFunc f (getStart r)) ranges
          execCount = length $ filter (\r => getCount r > 0) funcRanges
          totCount = length funcRanges
      in MkFuncRangeMatch f.idrisName execCount totCount
