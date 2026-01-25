||| Profile HTML Parser for Chez Scheme profiler output
||| Parses profile.html and *.ss.html files to extract coverage data
module Coverage.ProfileParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

%default total

-- =============================================================================
-- Branch Coverage Types
-- =============================================================================

||| A single expression hit from the profiler
||| Extracted from <span class=pcN title="line L char C count N">...</span>
public export
record ExprHit where
  constructor MkExprHit
  line      : Nat
  char      : Nat
  count     : Nat
  content   : String   -- The expression text

public export
Show ExprHit where
  show h = "ExprHit(line=\{show h.line}, char=\{show h.char}, count=\{show h.count})"

public export
Eq ExprHit where
  h1 == h2 = h1.line == h2.line && h1.char == h2.char && h1.count == h2.count

||| Function definition with its expression hits
public export
record FunctionHits where
  constructor MkFunctionHits
  schemeFunc : String         -- e.g., "Main-factorial"
  idrisModule : Maybe String  -- e.g., "Main"
  idrisFunc : Maybe String    -- e.g., "factorial"
  totalHits : Nat             -- Total call count
  exprHits : List ExprHit     -- Individual expression hits
  uncoveredExprs : Nat        -- Expressions with count=0
  totalExprs : Nat            -- Total expressions in function

public export
Show FunctionHits where
  show f = "\{f.schemeFunc}: \{show f.totalHits} hits, \{show f.uncoveredExprs}/\{show f.totalExprs} uncovered"

||| Branch coverage for a cond/case expression
public export
record BranchCoverage where
  constructor MkBranchCoverage
  branchType : String         -- "cond", "case", "if"
  totalBranches : Nat
  coveredBranches : Nat
  branchHits : List (String, Nat)  -- (branch label, hit count)

public export
Show BranchCoverage where
  show b = "\{b.branchType}: \{show b.coveredBranches}/\{show b.totalBranches} branches covered"

-- =============================================================================
-- Profile Summary
-- =============================================================================

||| Complete profile analysis result
public export
record ProfileResult where
  constructor MkProfileResult
  sourceFile : String
  functions : List FunctionHits
  totalFunctions : Nat
  coveredFunctions : Nat      -- Functions with totalHits > 0
  totalExpressions : Nat
  coveredExpressions : Nat    -- Expressions with count > 0
  functionCoverage : Double   -- coveredFunctions / totalFunctions
  expressionCoverage : Double -- coveredExpressions / totalExpressions
  branches : List BranchCoverage

public export
Show ProfileResult where
  show p = "ProfileResult(\{p.sourceFile}): \{show p.coveredFunctions}/\{show p.totalFunctions} functions, \{show p.expressionCoverage}% expr coverage"

-- =============================================================================
-- HTML Parsing Utilities
-- =============================================================================

||| Extract text between two delimiters
export
extractBetween : String -> String -> String -> Maybe String
extractBetween start end str =
  case strIndex str start of
    Nothing => Nothing
    Just startIdx =>
      let afterStart = strSubstr (cast startIdx + cast (length start)) (cast $ length str) str
      in case strIndex afterStart end of
           Nothing => Nothing
           Just endIdx => Just $ strSubstr 0 (cast endIdx) afterStart

  where
    findIndex : Nat -> List Char -> List Char -> Nat -> Maybe Nat
    findIndex _ [] _ _ = Nothing
    findIndex idx rest@(_ :: xs) needle nLen =
      if take nLen rest == needle
        then Just idx
        else findIndex (S idx) xs needle nLen

    strIndex : String -> String -> Maybe Nat
    strIndex haystack needle =
      let h = unpack haystack
          n = unpack needle
          nLen = length n
      in findIndex 0 h n nLen

||| Parse "count N" from title attribute
||| Title format: "line L char C count N"
export
parseCount : String -> Nat
parseCount title =
  let parts = words title
      -- Find "count" and take the next word
  in findCount parts
  where
    findCount : List String -> Nat
    findCount [] = 0
    findCount [_] = 0
    findCount ("count" :: n :: _) =
      case parsePositive {a=Nat} n of
        Nothing => 0
        Just num => num
    findCount (_ :: rest) = findCount rest

||| Parse "line L" from title attribute
export
parseLine : String -> Nat
parseLine title =
  case extractBetween "line " " " title of
    Nothing => 0
    Just lineStr =>
      case parsePositive {a=Nat} lineStr of
        Nothing => 0
        Just n => n

||| Parse "char C" from title attribute
export
parseChar : String -> Nat
parseChar title =
  case extractBetween "char " " " title of
    Nothing => 0
    Just charStr =>
      case parsePositive {a=Nat} charStr of
        Nothing => 0
        Just n => n

-- =============================================================================
-- Span Parsing
-- =============================================================================

||| Parse a single <span> element into an ExprHit
export
parseSpan : String -> Maybe ExprHit
parseSpan spanStr =
  case extractBetween "title=\"" "\"" spanStr of
    Nothing => Nothing
    Just title =>
      let line' = parseLine title
          char' = parseChar title
          count' = parseCount title
          content' = case extractBetween ">" "<" spanStr of
                       Nothing => ""
                       Just c => c
      in Just $ MkExprHit line' char' count' content'

||| Extract all spans from HTML content
export
covering
extractSpans : String -> List ExprHit
extractSpans html = extractSpansHelper html []
  where
    covering
    extractSpansHelper : String -> List ExprHit -> List ExprHit
    extractSpansHelper remaining acc =
      case extractBetween "<span class=pc" "</span>" remaining of
        Nothing => reverse acc
        Just spanContent =>
          let fullSpan = "<span class=pc" ++ spanContent ++ "</span>"
              hit = parseSpan fullSpan
              newAcc = case hit of
                         Nothing => acc
                         Just h => h :: acc
              -- Skip past this span for next iteration
              afterSpan = case strIndex remaining "</span>" of
                            Nothing => ""
                            Just idx => strSubstr (cast idx + 7) (cast $ length remaining) remaining
          in extractSpansHelper afterSpan newAcc

      where
        findIndex : Nat -> List Char -> List Char -> Nat -> Maybe Nat
        findIndex _ [] _ _ = Nothing
        findIndex idx rest@(_ :: xs) needle nLen =
          if take nLen rest == needle
            then Just idx
            else findIndex (S idx) xs needle nLen

        strIndex : String -> String -> Maybe Nat
        strIndex haystack needle =
          let h = unpack haystack
              n = unpack needle
              nLen = length n
          in findIndex 0 h n nLen

-- =============================================================================
-- Function Definition Extraction
-- =============================================================================

||| Check if expression is a function definition
export
isFunctionDef : ExprHit -> Bool
isFunctionDef hit = isPrefixOf "(define " hit.content

||| Extract function name from "(define FuncName ..."
export
extractFuncName : String -> Maybe String
extractFuncName content =
  if isPrefixOf "(define " content
    then let rest = strSubstr 8 (cast $ length content) content
             name = takeWhile (\c => c /= ' ' && c /= '(') (unpack rest)
         in if null name then Nothing else Just (pack name)
    else Nothing

||| Parse Scheme function name into Idris module and function
||| "Main-factorial" -> (Just "Main", Just "factorial")
||| "PreludeC-45Show-u--show_Show_Nat" -> (Just "Prelude.Show", Just "show")
export
parseSchemeToIdris : String -> (Maybe String, Maybe String)
parseSchemeToIdris schemeFunc =
  let parts = forget $ split (== '-') schemeFunc
  in case parts of
       [] => (Nothing, Nothing)
       [single] => (Nothing, Just single)
       (modPart :: rest) =>
         let funcName = fastConcat $ intersperse "-" rest
             -- Convert "C-45" back to "."
             moduleName = convertModule modPart
         in (Just moduleName, Just funcName)
  where
    convertModule : String -> String
    convertModule s = s  -- TODO: "AuditC-45Orchestrator" -> "Audit.Orchestrator"

-- =============================================================================
-- Main Parsing Functions
-- =============================================================================

||| Group expression hits by function
export
groupByFunction : List ExprHit -> List FunctionHits
groupByFunction hits =
  let funcDefs = filter isFunctionDef hits
      -- For each function def, collect hits until next function def
  in mapMaybe buildFunctionHits funcDefs
  where
    buildFunctionHits : ExprHit -> Maybe FunctionHits
    buildFunctionHits defHit =
      case extractFuncName defHit.content of
        Nothing => Nothing
        Just name =>
          let schemeInfo = parseSchemeToIdris name
              modName = fst schemeInfo
              funcName = snd schemeInfo
              funcHits = filter (\h => h.line == defHit.line) hits
              uncovered = length (filter (\h => h.count == 0) funcHits)
              total' = length funcHits
          in Just (MkFunctionHits name modName funcName defHit.count funcHits uncovered total')

||| Parse a profile HTML file and extract coverage data
export
covering
parseProfileHtml : String -> ProfileResult
parseProfileHtml html =
  let spans = extractSpans html
      functions = groupByFunction spans
      totalFuncs = length functions
      coveredFuncs = length $ filter (\f => f.totalHits > 0) functions
      totalExprs = length spans
      coveredExprs = length $ filter (\h => h.count > 0) spans
      funcCov = if totalFuncs == 0 then 0.0
                else cast coveredFuncs / cast totalFuncs * 100.0
      exprCov = if totalExprs == 0 then 0.0
                else cast coveredExprs / cast totalExprs * 100.0
  in MkProfileResult "" functions totalFuncs coveredFuncs totalExprs coveredExprs funcCov exprCov []

||| Read and parse a profile HTML file
export
covering
readProfileHtml : String -> IO (Either String ProfileResult)
readProfileHtml path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read \{path}: " ++ show err
  let result = parseProfileHtml content
  pure $ Right $ { sourceFile := path } result

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Convert FunctionHits to JSON string
functionHitsToJson : FunctionHits -> String
functionHitsToJson f =
  "{\"name\": \"" ++ f.schemeFunc ++ "\", \"hits\": " ++ show f.totalHits
  ++ ", \"uncoveredExprs\": " ++ show f.uncoveredExprs
  ++ ", \"totalExprs\": " ++ show f.totalExprs ++ "}"

||| Convert ProfileResult to JSON string
export
profileToJson : ProfileResult -> String
profileToJson p =
  "{\n"
  ++ "  \"sourceFile\": \"" ++ p.sourceFile ++ "\",\n"
  ++ "  \"totalFunctions\": " ++ show p.totalFunctions ++ ",\n"
  ++ "  \"coveredFunctions\": " ++ show p.coveredFunctions ++ ",\n"
  ++ "  \"functionCoverage\": " ++ show p.functionCoverage ++ ",\n"
  ++ "  \"totalExpressions\": " ++ show p.totalExpressions ++ ",\n"
  ++ "  \"coveredExpressions\": " ++ show p.coveredExpressions ++ ",\n"
  ++ "  \"expressionCoverage\": " ++ show p.expressionCoverage ++ ",\n"
  ++ "  \"functions\": [\n    "
  ++ fastConcat (intersperse ",\n    " (map functionHitsToJson p.functions))
  ++ "\n  ]\n}"
