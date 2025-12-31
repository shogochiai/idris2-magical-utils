||| Parser for Idris2 --dumpcases output
||| Extracts semantic coverage information (canonical vs impossible cases)
module EvmCoverage.DumpcasesParser

import EvmCoverage.Types
import Data.List
import Data.List1
import Data.String
import Data.Maybe
import System
import System.File

%default covering

-- =============================================================================
-- Helper Functions (module level)
-- =============================================================================

||| Find substring in list of chars
findSubstr : List Char -> List Char -> Nat -> Maybe Nat
findSubstr needle [] _ = Nothing
findSubstr needle haystack@(_ :: rest) idx =
  if isPrefixOf needle haystack
    then Just idx
    else findSubstr needle rest (S idx)

||| Extract text between two delimiters
extractBetween : String -> String -> String -> Maybe String
extractBetween start end str =
  let chars = unpack str
      startChars = unpack start
      endChars = unpack end
  in do startIdx <- findSubstr startChars chars 0
        let afterStart = drop (startIdx + length startChars) chars
        endIdx <- findSubstr endChars afterStart 0
        pure $ pack $ take endIdx afterStart

||| Count occurrences of substring
countOccurrences : String -> String -> Nat
countOccurrences needle haystack = go 0 (unpack haystack)
  where
    needleChars : List Char
    needleChars = unpack needle

    go : Nat -> List Char -> Nat
    go acc [] = acc
    go acc rest@(_ :: xs) =
      if isPrefixOf needleChars rest
        then go (S acc) (drop (length needleChars) rest)
        else go acc xs

-- =============================================================================
-- CRASH Reason Classification
-- =============================================================================

||| Parse CRASH reason from dumpcases output
||| Examples:
|||   CRASH "No clauses in AbsurdTest.absurdFunc"
|||   CRASH "Unhandled input for Main.partial at ..."
|||   CRASH "Nat case not covered for ..."
export
parseCrashReason : String -> BranchClass
parseCrashReason msg =
  if isInfixOf "No clauses" msg then BCExcludedNoClauses
  else if isInfixOf "Unhandled input" msg then BCBugUnhandledInput
  else if isInfixOf "Nat case" msg then BCOptimizerNat
  else BCUnknownCrash msg

-- =============================================================================
-- Module Filtering (EVM-specific)
-- =============================================================================

||| Check if function belongs to project (not runtime/stdlib)
||| Based on idris2-coverage/exclusions/base.txt patterns
isProjectFunction : String -> Bool
isProjectFunction name =
  let n = case strUncons name of
            Just ('.', rest) => rest
            _ => name
  in not (isExcluded n)
  where
    -- Compiler-generated MN names
    isCompilerGenerated : String -> Bool
    isCompilerGenerated s =
      isInfixOf "{csegen:" s ||
      isInfixOf "{eta:" s ||
      isInfixOf "{arg:" s ||
      isInfixOf "{lazyArg:" s ||
      isInfixOf "{conArg:" s ||
      isInfixOf "{case" s

    -- Standard library modules
    isStdLib : String -> Bool
    isStdLib s =
      isPrefixOf "Prelude." s ||
      isPrefixOf "Data." s ||
      isPrefixOf "System." s ||
      isPrefixOf "Control." s ||
      isPrefixOf "Decidable." s ||
      isPrefixOf "Language." s ||
      isPrefixOf "Debug." s ||
      isPrefixOf "PrimIO." s ||
      isPrefixOf "_builtin." s ||
      isPrefixOf "Builtin." s

    -- Builtins and primitives
    isPrimitive : String -> Bool
    isPrimitive s = isPrefixOf "prim__" s

    -- Test modules (not deployed to EVM)
    isTestModule : String -> Bool
    isTestModule s = isInfixOf ".Tests." s || isInfixOf "Test." s

    -- Schema/Storages modules (pure storage layout, not business logic)
    isSchemaModule : String -> Bool
    isSchemaModule s = isInfixOf ".Storages." s || isInfixOf ".Schema" s

    isExcluded : String -> Bool
    isExcluded s =
      isCompilerGenerated s ||
      isStdLib s ||
      isPrimitive s ||
      isTestModule s ||
      isSchemaModule s

-- =============================================================================
-- Line Parsing
-- =============================================================================

||| Count arguments from the right side of a function definition
countArgs : String -> Nat
countArgs s =
  case extractBetween "[" "]" s of
    Nothing => 0
    Just argsStr => length $ forget $ split (== ',') argsStr

||| Split list into init and last
splitInitLast : List a -> Maybe (List a, a)
splitInitLast [] = Nothing
splitInitLast [x] = Just ([], x)
splitInitLast (x :: xs) = do
  (rest, lastItem) <- splitInitLast xs
  Just (x :: rest, lastItem)

||| Parse function definition line
||| Format: "Module.funcName = [arg0, arg1]: body"
parseFunctionDef : String -> Maybe (String, String, Nat)
parseFunctionDef line =
  case break (== '=') line of
    (name, rest) =>
      if null rest then Nothing
      else
        let trimmedName = trim name
            parts = forget $ split (== '.') trimmedName
        in case parts of
             [] => Nothing
             [single] => Just ("", single, countArgs rest)
             _ => case splitInitLast parts of
                    Just (modParts, fName) =>
                      Just (concat $ intersperse "." modParts, fName, countArgs rest)
                    Nothing => Nothing

||| Check if line contains a case expression
||| Format: "(%case expr [(pattern body), ...] default)"
isCaseLine : String -> Bool
isCaseLine line = isInfixOf "%case" line || isInfixOf "%concase" line

||| Check if line contains CRASH
||| Format: "CRASH \"message\""
isCrashLine : String -> Bool
isCrashLine line = isInfixOf "CRASH" line || isInfixOf "blodwen-error-quit" line

||| Extract CRASH message from line
extractCrashMsg : String -> String
extractCrashMsg line =
  case extractBetween "\"" "\"" line of
    Just msg => msg
    Nothing => "Unknown CRASH"

-- =============================================================================
-- Counting Branches in Case Expression
-- =============================================================================

||| Count branches in a %case expression
||| Looks for %concase and %constcase patterns
countCaseBranches : String -> Nat
countCaseBranches line =
  let conCases = countOccurrences "%concase" line
      constCases = countOccurrences "%constcase" line
  in max 1 (conCases + constCases)  -- At least 1 branch

-- =============================================================================
-- Main Parser
-- =============================================================================

||| Make a canonical branch
makeBranch : String -> Nat -> Nat -> ClassifiedBranch
makeBranch fName baseIdx i =
  let bid = MkBranchId "" fName (baseIdx + i)
  in MkClassifiedBranch bid BCCanonical ("case branch " ++ show i)

||| Parse branches from function body lines
parseBranches : String -> List String -> Nat -> List ClassifiedBranch
parseBranches _ [] _ = []
parseBranches fName (l :: ls) idx =
  if isCrashLine l
    then let crashClass = parseCrashReason (extractCrashMsg l)
             bid = MkBranchId "" fName idx
             branch = MkClassifiedBranch bid crashClass l
         in branch :: parseBranches fName ls (S idx)
    else if isCaseLine l
      then let numBranches = countCaseBranches l
               indices = take numBranches [0..]
               newBranches = map (makeBranch fName idx) indices
           in newBranches ++ parseBranches fName ls (idx + numBranches)
      else parseBranches fName ls idx

||| Parse a single function from dumpcases output
||| Note: In dumpcases output, each function definition is a single line
parseFunction : String -> List String -> (String, List ClassifiedBranch)
parseFunction funcLine bodyLines =
  case parseFunctionDef funcLine of
    Nothing => ("", [])
    Just (modName, fName, arity) =>
      let fullName = if null modName then fName else modName ++ "." ++ fName
          -- Count case branches in the function line itself (not bodyLines)
          numBranches = countCaseBranches funcLine
          indices = take numBranches [0..]
          branches = map (makeBranch fullName 0) indices
      in (fullName, branches)

||| Parse function list helper
parseFunctions : List String -> List ClassifiedBranch -> List ClassifiedBranch
parseFunctions [] acc = acc
parseFunctions (l :: rest) acc =
  if isInfixOf " = [" l && not (isPrefixOf "{" (trim l))
    then let (funcName, branches) = parseFunction l rest
         in parseFunctions rest (acc ++ branches)
    else parseFunctions rest acc

||| Parse entire dumpcases file content
export
parseDumpcases : String -> List ClassifiedBranch
parseDumpcases content =
  let allBranches = parseFunctions (lines content) []
  in filter (\b => isProjectFunction b.branchId.funcName) allBranches

||| Run idris2-yul --dumpcases and parse output
||| EVM coverage requires idris2-yul compiler for EVM FFI support
export
runDumpcasesAndParse : String -> String -> IO (Either String (List ClassifiedBranch))
runDumpcasesAndParse ipkgPath outputPath = do
  let cmd = "idris2-yul --dumpcases " ++ outputPath ++ " --build " ++ ipkgPath
  exitCode <- system cmd
  if exitCode /= 0
    then pure $ Left $ "Failed to run: " ++ cmd
    else do
      Right content <- readFile outputPath
        | Left err => pure $ Left $ "Failed to read dumpcases: " ++ show err
      pure $ Right $ parseDumpcases content

-- =============================================================================
-- Static Analysis
-- =============================================================================

||| Check if branch class is unknown
isUnknownClass : ClassifiedBranch -> Bool
isUnknownClass cb = case cb.branchClass of
                      BCUnknownCrash _ => True
                      _ => False

||| Analyze dumpcases output and classify all branches
export
analyzeStatic : List ClassifiedBranch -> StaticBranchAnalysis
analyzeStatic branches =
  let canonical = filter (\b => isCanonical b.branchClass) branches
      excluded  = filter (\b => b.branchClass == BCExcludedNoClauses) branches
      bugs      = filter (\b => b.branchClass == BCBugUnhandledInput) branches
      optimizer = filter (\b => b.branchClass == BCOptimizerNat) branches
      unknown   = filter isUnknownClass branches
      compiler  = filter (\b => b.branchClass == BCCompilerGenerated) branches
  in MkStaticBranchAnalysis
       branches
       (length canonical)
       (length excluded)
       (length bugs)
       (length optimizer)
       (length unknown)
       (length compiler)

-- =============================================================================
-- Top-K Targets with Severity
-- =============================================================================

||| Group branches by function name
groupByFunc : List ClassifiedBranch -> List (String, List ClassifiedBranch)
groupByFunc branches = go branches []
  where
    insertOrUpdate : String -> ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    insertOrUpdate fn b [] = [(fn, [b])]
    insertOrUpdate fn b ((k, vs) :: rest) =
      if k == fn then (k, b :: vs) :: rest
      else (k, vs) :: insertOrUpdate fn b rest

    go : List ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    go [] acc = acc
    go (b :: bs) acc = go bs (insertOrUpdate b.branchId.funcName b acc)

||| Convert function branches to HighImpactTarget
funcToTarget : (String, List ClassifiedBranch) -> HighImpactTarget
funcToTarget (fn, branches) =
  let canonical = filter (\b => isCanonical b.branchClass) branches
      canonicalCount = length canonical
      -- For static-only: executed = 0, severity = Inf
      severity = 1.0e309  -- Infinity - no runtime data
      sevLevel = if canonicalCount >= 5 then Error
                 else if canonicalCount >= 2 then Warning
                 else Info
  in MkHighImpactTarget fn canonicalCount 0 severity sevLevel
       ("Function has " ++ show canonicalCount ++ " untested branches")

||| Get top K high-impact targets from branches, sorted by severity
||| @k - Maximum number of targets to return
||| @branches - All classified branches
export
topKTargets : Nat -> List ClassifiedBranch -> List HighImpactTarget
topKTargets k branches =
  let grouped = groupByFunc branches
      targets = map funcToTarget grouped
      -- Filter out functions with 0 canonical branches
      nonEmpty = filter (\t => t.branchCount > 0) targets
      sorted = sortBy compareSeverity nonEmpty
  in take k sorted
