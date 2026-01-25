||| Dumpcases Parser for ICP/DFX
|||
||| Parses Idris2's `--dumpcases` output to extract type-level case trees.
||| This enables semantic branch coverage analysis independent of backend.
|||
||| Uses shared types from idris2-coverage-core for cross-backend compatibility.
|||
||| Dumpcases format example:
|||   Prelude.Types.null = [{arg:1}]: (%case !{arg:1} [
|||     (%concase [nil] _builtin.NIL Just 0 [] 1),
|||     (%concase [cons] _builtin.CONS Just 1 [{e:2}, {e:3}] 0)
|||   ] Nothing)
module DfxCoverage.DumpcasesParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import System

-- Import shared types from coverage-core
import public Coverage.Core.Types
import public Coverage.Core.Exclusions
import public Coverage.Core.Stats
import public Coverage.Core.RuntimeHit
import public Coverage.Core.HighImpact
import public Coverage.Classification.CrashReason
import public Coverage.Classification.BranchClass
import public Coverage.Core.DumpcasesRunner

%default covering

-- =============================================================================
-- Parsing Helpers
-- =============================================================================

||| Check if character is identifier char
isIdentChar : Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '.' || c == '\'' || c == '-'

||| Extract function name from definition line
||| Format: "FuncName = [args]: body"
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

-- =============================================================================
-- Case Expression Parsing (Heuristic)
-- =============================================================================

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

||| Check if case has default (Just expr vs Nothing)
hasDefaultCase : String -> Bool
hasDefaultCase s =
  let justCount = countOccurrences "] Just" s
      nothingCount = countOccurrences "] Nothing" s
  in justCount > nothingCount

||| Extract CRASH message if present
extractCrashMsg : String -> Maybe String
extractCrashMsg s =
  if isInfixOf "(CRASH \"" s
  then case findSubstring "(CRASH \"" s of
         Nothing => Nothing
         Just idx =>
           let afterCrash = substr (idx + 8) (length s) s
           in case break (== '"') (unpack afterCrash) of
                (msg, _) => Just (pack msg)
  else Nothing
  where
    findSubstring : String -> String -> Maybe Nat
    findSubstring needle haystack = go 0 (unpack haystack) (unpack needle)
      where
        go : Nat -> List Char -> List Char -> Maybe Nat
        go _ [] _ = Nothing
        go idx hs ns =
          if isPrefixOf ns hs
          then Just idx
          else go (S idx) (drop 1 hs) ns

||| Parse case expressions from function body
||| Returns list of case expressions found
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

-- =============================================================================
-- Function Parsing
-- =============================================================================

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

-- =============================================================================
-- Main Parser API
-- =============================================================================

||| Parse entire dumpcases file content
|||
||| @content Raw content from dumpcases file
||| @returns List of function case information
export
parseDumpcases : String -> List FuncCases
parseDumpcases content =
  let ls = lines content
      nonEmpty = filter (not . null . trim) ls
  in parseFuncs nonEmpty

||| Load and parse dumpcases from file path
|||
||| @path Path to dumpcases file
export
loadDumpcases : String -> IO (Either String (List FuncCases))
loadDumpcases path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumpcases file: " ++ show err
  let result = parseDumpcases content
  pure $ Right result

-- =============================================================================
-- High Impact Targets
-- =============================================================================

||| Sort functions by branch count descending
sortByBranches : List FuncCases -> List FuncCases
sortByBranches [] = []
sortByBranches (x :: xs) = insert x (sortByBranches xs)
  where
    insert : FuncCases -> List FuncCases -> List FuncCases
    insert e [] = [e]
    insert e (y :: ys) = if e.totalBranches >= y.totalBranches
                           then e :: y :: ys
                           else y :: insert e ys

||| Get high impact targets by type-level branch count
|||
||| @n Number of top targets to return
||| @funcs List of function case information
export
getHighImpactByTypeBranches : Nat -> List FuncCases -> List FuncCases
getHighImpactByTypeBranches n funcs =
  take n $ sortByBranches $ filter (\f => f.totalBranches > 0) funcs

||| Get functions with at least N branches
export
getFunctionsWithMinBranches : Nat -> List FuncCases -> List FuncCases
getFunctionsWithMinBranches minBranches funcs =
  sortByBranches $ filter (\f => f.totalBranches >= minBranches) funcs

-- =============================================================================
-- Conversion to Shared HighImpactTarget (from coverage-core)
-- =============================================================================

||| Convert FuncCases to HighImpactTarget
||| This enables shared tooling between Chez Scheme and DFX/WASM coverage
|||
||| @fc Function case information from dumpcases
||| @executed Number of branches executed (0 if unknown)
export
funcCasesToHighImpactTarget : FuncCases -> Nat -> HighImpactTarget
funcCasesToHighImpactTarget fc executed =
  mkUntestedTarget fc.funcName (getModuleName fc.funcName) fc.totalBranches executed

||| Convert list of FuncCases to HighImpactTargets
||| Assumes 0 executed branches (static analysis only)
export
funcCasesToHighImpactTargets : List FuncCases -> List HighImpactTarget
funcCasesToHighImpactTargets = map (\fc => funcCasesToHighImpactTarget fc 0)

||| Get top K high impact targets as shared HighImpactTarget type
export
getHighImpactTargets : Nat -> List FuncCases -> List HighImpactTarget
getHighImpactTargets n funcs =
  let sorted = getHighImpactByTypeBranches n funcs
  in funcCasesToHighImpactTargets sorted

-- =============================================================================
-- Exclusion Filtering (using core types)
-- =============================================================================

||| Check if function should be excluded
isExcludedFunc : List ExclPattern -> FuncCases -> Bool
isExcludedFunc patterns fc = isJust (isMethodExcluded patterns fc.funcName)

||| Filter function cases by exclusion patterns
export
filterFuncCases : List ExclPattern -> List FuncCases -> List FuncCases
filterFuncCases patterns funcs =
  filter (not . isExcludedFunc patterns) funcs

||| Check which exclusion pattern matches a function
findMatchingPattern : String -> List ExclPattern -> Maybe String
findMatchingPattern _ [] = Nothing
findMatchingPattern name (p :: ps) =
  case isMethodExcluded [p] name of
    Just reason => Just reason
    Nothing => findMatchingPattern name ps

||| Annotate function with exclusion reason
annotateFunc : List ExclPattern -> FuncCases -> AnnotatedFuncCases
annotateFunc patterns fc =
  let reason = determineExclusionReason patterns fc.funcName
  in MkAnnotatedFuncCases fc reason

||| Annotate function cases with exclusion status
export
annotateFuncCases : List ExclPattern -> List FuncCases -> List AnnotatedFuncCases
annotateFuncCases patterns = map (annotateFunc patterns)

||| Sort annotated functions by branch count descending
sortByAnnotatedBranches : List AnnotatedFuncCases -> List AnnotatedFuncCases
sortByAnnotatedBranches [] = []
sortByAnnotatedBranches (x :: xs) = insert x (sortByAnnotatedBranches xs)
  where
    insert : AnnotatedFuncCases -> List AnnotatedFuncCases -> List AnnotatedFuncCases
    insert e [] = [e]
    insert e (y :: ys) = if e.funcCases.totalBranches >= y.funcCases.totalBranches
                           then e :: y :: ys
                           else y :: insert e ys

||| Get filtered high impact targets with annotations
export
getFilteredHighImpact : Nat -> List ExclPattern -> List FuncCases -> List AnnotatedFuncCases
getFilteredHighImpact n patterns funcs =
  let annotated = annotateFuncCases patterns funcs
      filtered = filter (not . isAnnotatedExcluded) annotated
      sorted = sortByAnnotatedBranches filtered
  in take n sorted

||| Get only included (non-excluded) annotated functions
export
getIncludedOnly : List AnnotatedFuncCases -> List AnnotatedFuncCases
getIncludedOnly = filter (not . isAnnotatedExcluded)

-- =============================================================================
-- Statistics (using core types)
-- =============================================================================

||| Compute basic statistics from parsed dumpcases
export
computeStats : List FuncCases -> DumpcasesStats
computeStats funcs =
  let totalFuncs = length funcs
      totalCases = foldl (\acc, f => acc + length f.cases) 0 funcs
      totalBranches = foldl (\acc, f => acc + f.totalBranches) 0 funcs
  in MkDumpcasesStats totalFuncs totalCases totalBranches 0 totalFuncs

||| Get detailed statistics with exclusion counts
export
getStats : List ExclPattern -> List FuncCases -> DumpcasesStats
getStats patterns funcs =
  let annotated = annotateFuncCases patterns funcs
      excluded = filter isAnnotatedExcluded annotated
      included = filter (not . isAnnotatedExcluded) annotated
      totalCases = foldl (\acc, f => acc + length f.cases) 0 funcs
      totalBranches = foldl (\acc, f => acc + f.totalBranches) 0 funcs
  in MkDumpcasesStats (length funcs) totalCases totalBranches
                       (length excluded) (length included)

-- =============================================================================
-- Module-Level Filtering
-- =============================================================================

||| Filter functions by module prefix
export
filterByModule : String -> List FuncCases -> List FuncCases
filterByModule modulePrefix funcs =
  filter (\f => isPrefixOf modulePrefix f.funcName) funcs

||| Exclude functions by module prefix
export
excludeModule : String -> List FuncCases -> List FuncCases
excludeModule modulePrefix funcs =
  filter (\f => not $ isPrefixOf modulePrefix f.funcName) funcs

||| Apply default module exclusions
export
excludeDefaultModules : List FuncCases -> List FuncCases
excludeDefaultModules funcs =
  foldl (\acc, modPrefix => excludeModule modPrefix acc) funcs defaultModuleExclusions

-- =============================================================================
-- CRASH Classification (using core types)
-- =============================================================================

||| Analyze function for CRASH messages and classify
export
analyzeFunction : String -> Maybe (FuncCases, Maybe CrashReason)
analyzeFunction line =
  case extractFuncName (trim line) of
    Nothing => Nothing
    Just name =>
      let cases = parseCaseExprs line
          totalBranches = calcTotalBranches cases
          crashReason = map classifyCrashMessage (extractCrashMsg line)
      in Just (MkFuncCases name cases totalBranches, crashReason)

||| Convert FuncCases to StaticFunctionAnalysis
export
toStaticAnalysis : FuncCases -> StaticFunctionAnalysis
toStaticAnalysis fc =
  let (modName, funcName) = parseFullName fc.funcName
      branches = zipWithIndex 0 fc.cases >>= \(idx, ce) =>
        map (\(altIdx, _) =>
          let bid = MkBranchId modName funcName (cast idx) altIdx
              cls = if shouldExcludeFunctionName fc.funcName
                      then BCCompilerGenerated
                      else BCCanonical
          in MkClassifiedBranch bid cls "case_alt") (zipWithIndex 0 ce.alternatives)
  in MkStaticFunctionAnalysis fc.funcName branches
  where
    zipWithIndex : Nat -> List a -> List (Nat, a)
    zipWithIndex _ [] = []
    zipWithIndex n (x :: xs) = (n, x) :: zipWithIndex (S n) xs

||| Convert list of FuncCases to StaticBranchAnalysis
export
toStaticBranchAnalysis : List FuncCases -> StaticBranchAnalysis
toStaticBranchAnalysis funcs =
  let analyses = map toStaticAnalysis funcs
      allBranches = concatMap (.branches) analyses
      canonCount = countByClass BCCanonical allBranches
  in MkStaticBranchAnalysis analyses allBranches canonCount

-- =============================================================================
-- DFX-Specific Dumpcases Execution (RefC backend)
-- =============================================================================

||| Run dumpcases with RefC backend for DFX/ICP projects
|||
||| @projectDir - Directory containing the .ipkg file
||| @ipkgName   - Name of the .ipkg file (e.g., "mycanister.ipkg")
||| @returns    - Either error message or parsed FuncCases
export
runAndParseDumpcases : (projectDir : String)
                    -> (ipkgName : String)
                    -> IO (Either String (List FuncCases))
runAndParseDumpcases projectDir ipkgName = do
  result <- runDumpcasesRefc projectDir ipkgName
  case result of
    Left err => pure $ Left err
    Right content =>
      let parsed = parseDumpcases content
      in if null parsed
           then pure $ Left "No function definitions found in dumpcases output"
           else pure $ Right parsed

||| Run dumpcases and return StaticBranchAnalysis
export
runAndAnalyzeDumpcases : (projectDir : String)
                      -> (ipkgName : String)
                      -> IO (Either String StaticBranchAnalysis)
runAndAnalyzeDumpcases projectDir ipkgName = do
  result <- runAndParseDumpcases projectDir ipkgName
  pure $ map toStaticBranchAnalysis result

||| Run dumpcases with custom output path
export
runAndParseDumpcasesAt : (projectDir : String)
                      -> (ipkgName : String)
                      -> (outputPath : String)
                      -> IO (Either String (List FuncCases))
runAndParseDumpcasesAt projectDir ipkgName outputPath = do
  result <- runDumpcases RefcBackend projectDir ipkgName outputPath
  case result of
    Left err => pure $ Left err
    Right content =>
      let parsed = parseDumpcases content
      in if null parsed
           then pure $ Left "No function definitions found in dumpcases output"
           else pure $ Right parsed
