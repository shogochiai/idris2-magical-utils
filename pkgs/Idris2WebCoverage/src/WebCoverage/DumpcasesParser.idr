||| Dumpcases Parser for Node.js Backend
|||
||| Parses Idris2's `--dumpcases` output for Node.js code generator.
||| Reuses types from idris2-coverage-core.
module WebCoverage.DumpcasesParser

import Data.List
import Data.String
import System
import System.File

import public Coverage.Core.Types
import public Coverage.Core.DumpcasesRunner

%default covering

-- =============================================================================
-- Node.js Backend Support
-- =============================================================================

||| Node.js backend type for DumpcasesRunner
public export
NodeBackend : Backend
NodeBackend = CustomBackend "node"

||| Run dumpcases with Node.js backend
|||
||| @projectDir - Directory containing the .ipkg file
||| @ipkgName   - Name of the .ipkg file
export
runDumpcasesNode : String -> String -> IO (Either String String)
runDumpcasesNode = runDumpcasesDefault NodeBackend

-- =============================================================================
-- Parsing (reuse core logic)
-- =============================================================================

||| Check if character is identifier char
isIdentChar : Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '.' || c == '\'' || c == '-'

||| Extract function name from definition line
extractFuncName : String -> Maybe String
extractFuncName line =
  let trimmed = trim line
      chars = unpack trimmed
  in case break (== ' ') chars of
       (nameChars, ' ' :: '=' :: _) =>
         if null nameChars then Nothing else Just (pack nameChars)
       _ => Nothing

||| Check if line starts a function definition
isFuncDefLine : String -> Bool
isFuncDefLine line =
  let trimmed = trim line
  in case extractFuncName trimmed of
       Just _ => isInfixOf "= [" trimmed || isInfixOf "]: " trimmed
       Nothing => False

||| Count occurrences of substring
countOccurrences : String -> String -> Nat
countOccurrences needle haystack = go 0 (unpack haystack)
  where
    needleLen : Nat
    needleLen = length needle

    needleChars : List Char
    needleChars = unpack needle

    go : Nat -> List Char -> Nat
    go acc [] = acc
    go acc cs@(_ :: rest) =
      if take needleLen cs == needleChars
        then go (S acc) rest
        else go acc rest

||| Count case expressions in text
countCaseExprs : String -> Nat
countCaseExprs = countOccurrences "(%case"

||| Count concase alternatives
countConCases : String -> Nat
countConCases s = countOccurrences "(%concase" s + countOccurrences "%concase" s

||| Count constcase alternatives
countConstCases : String -> Nat
countConstCases s = countOccurrences "(%constcase" s + countOccurrences "%constcase" s

||| Check if case has default
hasDefaultCase : String -> Bool
hasDefaultCase s =
  let justCount = countOccurrences "] Just" s
      nothingCount = countOccurrences "] Nothing" s
  in justCount > nothingCount

||| Parse case expressions from function body
parseCaseExprs : String -> List CaseExpr
parseCaseExprs body =
  let numCases = countCaseExprs body
      conCount = countConCases body
      constCount = countConstCases body
      hasDefault = hasDefaultCase body
  in if numCases == 0
       then []
       else
         let alts = replicate conCount (MkCaseAlt ConCase Nothing Nothing) ++
                    replicate constCount (MkCaseAlt ConstCase Nothing Nothing)
         in [MkCaseExpr alts hasDefault]

||| Collect lines belonging to a function definition
collectFuncBody : List String -> (String, List String)
collectFuncBody [] = ("", [])
collectFuncBody (line :: rest) =
  case rest of
    [] => (line, [])
    (next :: _) =>
      if isFuncDefLine (trim next)
        then (line, rest)
        else let (body, remaining) = collectFuncBody rest
             in (line ++ "\n" ++ body, remaining)

||| Parse a single function definition
parseFunc : List String -> Maybe (FuncCases, List String)
parseFunc [] = Nothing
parseFunc (line :: rest) =
  case extractFuncName (trim line) of
    Nothing => Nothing
    Just name =>
      let (body, remaining) = collectFuncBody (line :: rest)
          cases = parseCaseExprs body
          totalBranches = calcTotalBranches cases
      in Just (MkFuncCases name cases totalBranches, remaining)

||| Parse all functions from lines
parseFuncs : List String -> List FuncCases
parseFuncs [] = []
parseFuncs (line :: rest) =
  if isFuncDefLine (trim line)
    then case parseFunc (line :: rest) of
           Just (fc, remaining) => fc :: parseFuncs remaining
           Nothing => parseFuncs rest
    else parseFuncs rest

||| Parse entire dumpcases file content
export
parseDumpcases : String -> List FuncCases
parseDumpcases content =
  let ls = lines content
      nonEmpty = filter (not . null . trim) ls
  in parseFuncs nonEmpty

||| Load and parse dumpcases from file path
export
loadDumpcases : String -> IO (Either String (List FuncCases))
loadDumpcases path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumpcases: " ++ show err
  let result = parseDumpcases content
  pure $ Right result

-- =============================================================================
-- Node.js Specific Execution
-- =============================================================================

||| Run dumpcases for Node.js backend and parse results
|||
||| @projectDir - Directory containing the .ipkg file
||| @ipkgName   - Name of the .ipkg file
export
runAndParseDumpcases : (projectDir : String)
                     -> (ipkgName : String)
                     -> IO (Either String (List FuncCases))
runAndParseDumpcases projectDir ipkgName = do
  result <- runDumpcasesNode projectDir ipkgName
  case result of
    Left err => pure $ Left err
    Right content =>
      let parsed = parseDumpcases content
      in if null parsed
           then pure $ Left "No function definitions found"
           else pure $ Right parsed

-- =============================================================================
-- Statistics
-- =============================================================================

||| Compute statistics from parsed dumpcases
export
computeStats : List FuncCases -> (Nat, Nat, Nat)  -- (funcs, cases, branches)
computeStats funcs =
  let totalFuncs = length funcs
      totalCases = foldl (\acc, f => acc + length f.cases) 0 funcs
      totalBranches = foldl (\acc, f => acc + f.totalBranches) 0 funcs
  in (totalFuncs, totalCases, totalBranches)

||| Get high-impact functions sorted by branch count
export
getHighImpact : Nat -> List FuncCases -> List FuncCases
getHighImpact n funcs =
  take n $ sortByBranches $ filter (\f => f.totalBranches > 0) funcs
  where
    sortByBranches : List FuncCases -> List FuncCases
    sortByBranches [] = []
    sortByBranches (x :: xs) = insert x (sortByBranches xs)
      where
        insert : FuncCases -> List FuncCases -> List FuncCases
        insert e [] = [e]
        insert e (y :: ys) =
          if e.totalBranches >= y.totalBranches
            then e :: y :: ys
            else y :: insert e ys
