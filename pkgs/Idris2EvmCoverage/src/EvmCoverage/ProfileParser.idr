||| Profile HTML Parser for Chez Scheme profiler output
||| Parses profile.html and *.ss.html files to extract coverage data
module EvmCoverage.ProfileParser

import EvmCoverage.Types
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

%default covering

-- =============================================================================
-- Expression Hit from HTML
-- =============================================================================

||| A single expression hit from the profiler
||| Extracted from <span class=pcN title="line L char C count N">...</span>
public export
record ExprHit where
  constructor MkExprHit
  line    : Nat
  char    : Nat
  count   : Nat
  content : String

public export
Show ExprHit where
  show h = "ExprHit(line=\{show h.line}, count=\{show h.count})"

-- =============================================================================
-- HTML Parsing Utilities
-- =============================================================================

||| Find substring index
findSubstr : List Char -> List Char -> Nat -> Maybe Nat
findSubstr [] _ idx = Just idx
findSubstr _ [] _ = Nothing
findSubstr needle haystack@(_ :: rest) idx =
  if isPrefixOf needle haystack
    then Just idx
    else findSubstr needle rest (S idx)

||| Extract text between two delimiters
export
extractBetween : String -> String -> String -> Maybe String
extractBetween start end str =
  let chars = unpack str
      startChars = unpack start
      endChars = unpack end
  in do startIdx <- findSubstr startChars chars 0
        let afterStart = drop (startIdx + length startChars) chars
        endIdx <- findSubstr endChars afterStart 0
        pure $ pack $ take endIdx afterStart

||| Parse "count N" from title attribute
export
parseCount : String -> Nat
parseCount title =
  case extractBetween "count " " " (title ++ " ") of
    Nothing => case extractBetween "count " "\"" title of
                 Nothing => 0
                 Just n => parseNat n
    Just n => parseNat n
  where
    parseNat : String -> Nat
    parseNat s = fromMaybe 0 (parsePositive {a=Nat} (trim s))

||| Parse "line L" from title attribute
export
parseLine : String -> Nat
parseLine title =
  case extractBetween "line " " " title of
    Nothing => 0
    Just n => fromMaybe 0 (parsePositive {a=Nat} (trim n))

||| Parse "char C" from title attribute
export
parseChar : String -> Nat
parseChar title =
  case extractBetween "char " " " title of
    Nothing => 0
    Just n => fromMaybe 0 (parsePositive {a=Nat} (trim n))

-- =============================================================================
-- Hot Spot Parsing (from profile.html main file)
-- =============================================================================

||| Hot spot entry from profile.html
public export
record HotSpot where
  constructor MkHotSpot
  fileName : String
  line     : Nat
  count    : Nat

public export
Show HotSpot where
  show h = "HotSpot(\{h.fileName}:\{show h.line}, \{show h.count})"

||| Parse hot spot line from profile.html
||| Format: <a href="...file.ss.html#lineN" ...>file.ss line N (count)</a>
parseHotSpot : String -> Maybe HotSpot
parseHotSpot line =
  if not (isInfixOf "href=" line) then Nothing
  else do
    -- Extract filename from href
    href <- extractBetween "href=\"" "\"" line
    let fileName = case extractBetween "" ".html" href of
                     Just f => f ++ ".html"
                     Nothing => href
    -- Extract line number from "line N"
    lineStr <- extractBetween " line " " " line
    lineNum <- parsePositive {a=Nat} (trim lineStr)
    -- Extract count from "(N)"
    countStr <- extractBetween "(" ")" line
    count <- parsePositive {a=Nat} (trim countStr)
    pure $ MkHotSpot fileName lineNum count

||| Parse all hot spots from profile.html content
export
parseHotSpots : String -> List HotSpot
parseHotSpots content =
  let ls = lines content
  in mapMaybe parseHotSpot ls

-- =============================================================================
-- Span Parsing (from *.ss.html files)
-- =============================================================================

||| Parse a single <span> element into an ExprHit
parseSpan : String -> Maybe ExprHit
parseSpan spanStr =
  case extractBetween "title=\"" "\"" spanStr of
    Nothing => Nothing
    Just title =>
      let lineNum = parseLine title
          charNum = parseChar title
          countNum = parseCount title
          content = fromMaybe "" $ extractBetween ">" "<" spanStr
      in Just $ MkExprHit lineNum charNum countNum content

||| Find substring index (string version)
findSubstrIdx : String -> String -> Maybe Nat
findSubstrIdx needle haystack =
  findSubstr (unpack needle) (unpack haystack) 0

||| Extract all spans from HTML content
export
extractSpans : String -> List ExprHit
extractSpans html = go html []
  where
    go : String -> List ExprHit -> List ExprHit
    go remaining acc =
      case extractBetween "<span class=pc" "</span>" remaining of
        Nothing => reverse acc
        Just spanContent =>
          let fullSpan = "<span class=pc" ++ spanContent ++ "</span>"
              hit = parseSpan fullSpan
              newAcc = case hit of
                         Nothing => acc
                         Just h => h :: acc
              -- Skip past this span
              afterIdx = fromMaybe 0 $ findSubstrIdx "</span>" remaining
              afterSpan = substr (afterIdx + 7) (length remaining) remaining
          in go afterSpan newAcc

-- =============================================================================
-- Function Definition Detection
-- =============================================================================

||| Check if line defines a Scheme function
||| Format: (define (FuncName ...) ...)  or  (define FuncName ...)
isFunctionDef : String -> Bool
isFunctionDef line = isInfixOf "(define " line

||| Extract function name from definition
||| Format: (define (FuncName args) body) or (define FuncName (lambda ...))
extractFuncName : String -> Maybe String
extractFuncName line =
  if isInfixOf "(define (" line
    then do
      afterDefine <- extractBetween "(define (" " " line
      pure $ trim afterDefine
    else if isInfixOf "(define " line
      then do
        afterDefine <- extractBetween "(define " " " line
        pure $ trim afterDefine
      else Nothing

-- =============================================================================
-- Profile Result Aggregation
-- =============================================================================

||| Function with its expression hits
public export
record FunctionProfile where
  constructor MkFunctionProfile
  schemeFunc     : String
  totalHits      : Nat
  exprHits       : List ExprHit
  uncoveredExprs : Nat
  totalExprs     : Nat

public export
Show FunctionProfile where
  show f = "\{f.schemeFunc}: \{show f.totalHits} hits"

||| Complete profile result
public export
record ProfileResult where
  constructor MkProfileResult
  sourceFile : String
  functions  : List FunctionProfile
  hotSpots   : List HotSpot
  totalExpressions : Nat
  coveredExpressions : Nat

public export
Show ProfileResult where
  show p = "Profile(\{p.sourceFile}): \{show $ length p.functions} functions"

-- =============================================================================
-- Main Parsing Functions
-- =============================================================================

||| Parse profile HTML file (*.ss.html)
export
parseProfileHtml : String -> String -> ProfileResult
parseProfileHtml fileName content =
  let spans = extractSpans content
      covered = filter (\s => s.count > 0) spans
      totalExprs = length spans
      coveredExprs = length covered
  in MkProfileResult fileName [] [] totalExprs coveredExprs

||| Parse main profile.html and associated ss.html files
export
parseProfileDir : String -> IO (Either String ProfileResult)
parseProfileDir profileHtmlPath = do
  Right mainContent <- readFile profileHtmlPath
    | Left err => pure $ Left $ "Failed to read profile.html: " ++ show err
  let hotSpots = parseHotSpots mainContent
  -- For now, just return hot spots info
  pure $ Right $ MkProfileResult profileHtmlPath [] hotSpots 0 0

-- =============================================================================
-- Convert to ProfileHit for Coverage.Types
-- =============================================================================

||| Convert ExprHit to ProfileHit
export
exprHitToProfileHit : String -> String -> ExprHit -> ProfileHit
exprHitToProfileHit schemeFunc filePath expr =
  MkProfileHit schemeFunc expr.count filePath expr.line

||| Convert HotSpot to ProfileHit
export
hotSpotToProfileHit : HotSpot -> ProfileHit
hotSpotToProfileHit hs =
  MkProfileHit "" hs.count hs.fileName hs.line

||| Extract all ProfileHits from a profile HTML file
export
extractProfileHits : String -> String -> List ProfileHit
extractProfileHits filePath content =
  let spans = extractSpans content
  in map (exprHitToProfileHit "" filePath) spans
