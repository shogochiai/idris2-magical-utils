||| Source Map Parser and Mapper
|||
||| Parses Source Map v3 format and maps V8 coverage positions
||| back to original Idris2 source locations.
module WebCoverage.SourceMapper

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File
import WebCoverage.Types

%default covering

-- =============================================================================
-- VLQ Decoding
-- =============================================================================

||| Base64 alphabet
base64Chars : String
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

||| Base64 character to value (0-63)
base64Value : Char -> Maybe Nat
base64Value c = go 0 (unpack base64Chars)
  where
    go : Nat -> List Char -> Maybe Nat
    go _ [] = Nothing
    go n (x :: xs) = if x == c then Just n else go (S n) xs

||| Power of 2 for VLQ decoding
pow2 : Nat -> Int
pow2 Z = 1
pow2 (S n) = 2 * pow2 n

||| Decode a single VLQ value from chars, return (value, remaining)
decodeVLQ : List Char -> Maybe (Int, List Char)
decodeVLQ [] = Nothing
decodeVLQ chars = go 0 0 chars
  where
    go : Int -> Nat -> List Char -> Maybe (Int, List Char)
    go acc shift [] = Nothing
    go acc shift (c :: rest) =
      case base64Value c of
        Nothing => Nothing
        Just val =>
          let valInt = cast {to=Int} val
              digit = valInt `mod` 32
              shifted = digit * pow2 shift
              newAcc = acc + shifted
              continuation = val >= 32
          in if continuation
               then go newAcc (shift + 5) rest
               else
                 -- Apply sign bit
                 let signed = if (newAcc `mod` 2) == 1
                                then negate (div newAcc 2 + 1)
                                else div newAcc 2
                 in Just (signed, rest)

-- =============================================================================
-- Source Map JSON Parsing
-- =============================================================================

||| Parsed source map data
public export
record SourceMap where
  constructor MkSourceMap
  version : Nat
  file : String
  sources : List String
  mappings : List SourceMapping

||| Find substring index (returns first occurrence)
findSubstrIndex : String -> String -> Maybe Nat
findSubstrIndex needle haystack = go 0 (unpack haystack)
  where
    needleChars : List Char
    needleChars = unpack needle
    needleLen : Nat
    needleLen = length needleChars

    go : Nat -> List Char -> Maybe Nat
    go _ [] = Nothing
    go n cs@(_ :: rest) =
      if take needleLen cs == needleChars
        then Just n
        else go (S n) rest

||| Take a quoted string from char list (helper for extractQuotedStrings)
takeQuotedStr : List Char -> List Char -> (String, List Char)
takeQuotedStr acc [] = (pack (reverse acc), [])
takeQuotedStr acc ('"' :: rest) = (pack (reverse acc), rest)
takeQuotedStr acc ('\\' :: '"' :: rest) = takeQuotedStr ('"' :: acc) rest
takeQuotedStr acc (c :: rest) = takeQuotedStr (c :: acc) rest

||| Extract quoted strings from JSON array
extractQuotedStrings : String -> List String
extractQuotedStrings s = go [] (unpack s)
  where
    go : List String -> List Char -> List String
    go acc [] = reverse acc
    go acc ('"' :: rest) =
      let (str, remaining) = takeQuotedStr [] rest
      in go (str :: acc) remaining
    go acc (_ :: rest) = go acc rest

||| Extract sources array from JSON
extractSources : String -> List String
extractSources json =
  case findSubstrIndex "\"sources\"" json of
    Nothing => []
    Just idx =>
      let afterSources = substr idx (length json) json
      in case findSubstrIndex "[" afterSources of
           Nothing => []
           Just bracketIdx =>
             let afterBracket = substr bracketIdx (length afterSources) afterSources
             in case findSubstrIndex "]" afterBracket of
                  Nothing => []
                  Just endIdx =>
                    let content = substr 0 (endIdx + 1) afterBracket
                    in extractQuotedStrings content

||| Extract mappings string from JSON
extractMappingsStr : String -> String
extractMappingsStr json =
  case findSubstrIndex "\"mappings\"" json of
    Nothing => ""
    Just idx =>
      let afterMappings = substr idx (length json) json
      in case findSubstrIndex "\"" (substr 11 (length afterMappings) afterMappings) of
           Nothing => ""
           Just startIdx =>
             let afterQuote = substr (startIdx + 12) (length afterMappings) afterMappings
             in case findSubstrIndex "\"" afterQuote of
                  Nothing => ""
                  Just endIdx => substr 0 endIdx afterQuote

||| Get source file from index
getSourceAt : List String -> Int -> String
getSourceAt sources idx =
  if idx < 0 then ""
  else fromMaybe "" (getAt (cast idx) sources)
  where
    getAt : Nat -> List a -> Maybe a
    getAt _ [] = Nothing
    getAt Z (x :: _) = Just x
    getAt (S n) (_ :: xs) = getAt n xs

||| Decode a single VLQ segment
decodeSegment : List String -> Int -> Int -> Int -> Int -> List Char
              -> Maybe (SourceMapping, Int, Int, Int, Int)
decodeSegment sources col srcIdx srcLine srcCol chars = do
  (colDelta, r1) <- decodeVLQ chars
  (srcIdxDelta, r2) <- decodeVLQ r1
  (srcLineDelta, r3) <- decodeVLQ r2
  (srcColDelta, _) <- decodeVLQ r3

  let newCol = col + colDelta
  let newSrcIdx = srcIdx + srcIdxDelta
  let newSrcLine = srcLine + srcLineDelta
  let newSrcCol = srcCol + srcColDelta

  let srcFile = getSourceAt sources newSrcIdx
  let mapping = MkSourceMapping 0 (cast newCol) srcFile
                  (cast (newSrcLine + 1)) (cast newSrcCol)
  Just (mapping, newCol, newSrcIdx, newSrcLine, newSrcCol)

||| Parse segments within a line
parseSegs : List String -> Nat -> Int -> Int -> Int -> Int -> List String
          -> (List SourceMapping, Int, Int, Int, Int)
parseSegs _ _ col srcIdx srcLine srcCol [] = ([], col, srcIdx, srcLine, srcCol)
parseSegs sources genLine col srcIdx srcLine srcCol (seg :: segs) =
  if null seg then parseSegs sources genLine col srcIdx srcLine srcCol segs
  else case decodeSegment sources col srcIdx srcLine srcCol (unpack seg) of
         Nothing => parseSegs sources genLine col srcIdx srcLine srcCol segs
         Just (m, newCol, newSrcIdx, newSrcLine, newSrcCol) =>
           let mapping = { genLine := genLine } m
               (rest, finalCol, finalSrcIdx, finalSrcLine, finalSrcCol) =
                 parseSegs sources genLine newCol newSrcIdx newSrcLine newSrcCol segs
           in (mapping :: rest, finalCol, finalSrcIdx, finalSrcLine, finalSrcCol)

||| Parse lines of mappings
parseLines : List String -> Nat -> Int -> Int -> Int -> Int -> List String -> List SourceMapping
parseLines _ _ _ _ _ _ [] = []
parseLines sources genLine genCol srcIdx srcLine srcCol (lineStr :: rest) =
  let segments = forget $ split (== ',') lineStr
      (mappings, newCol, newSrcIdx, newSrcLine, newSrcCol) =
        parseSegs sources genLine genCol srcIdx srcLine srcCol segments
  in mappings ++ parseLines sources (S genLine) 0 newSrcIdx newSrcLine newSrcCol rest

||| Parse VLQ-encoded mappings string
parseMappings : List String -> String -> List SourceMapping
parseMappings sources mappingsStr =
  let lineGroups = forget $ split (== ';') mappingsStr
  in parseLines sources 1 0 0 0 0 lineGroups

||| Parse source map from JSON content
export
parseSourceMap : String -> Maybe SourceMap
parseSourceMap json =
  let sources = extractSources json
      mappingsStr = extractMappingsStr json
      mappings = parseMappings sources mappingsStr
  in if null sources then Nothing
     else Just $ MkSourceMap 3 "" sources mappings

||| Load and parse source map from file
export
loadSourceMap : String -> IO (Either String SourceMap)
loadSourceMap path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read source map: " ++ show err
  case parseSourceMap content of
    Nothing => pure $ Left "Failed to parse source map"
    Just sm => pure $ Right sm

-- =============================================================================
-- Coverage Mapping
-- =============================================================================

||| Find original position for a generated JS line
export
findOriginalPosition : SourceMap -> Nat -> Maybe SourceMapping
findOriginalPosition sm genLine =
  let candidates = filter (\m => m.genLine <= genLine) sm.mappings
  in case reverse candidates of
       [] => Nothing
       (m :: _) => Just m

||| Map V8 byte offset to JS line number
offsetToLine : String -> Nat -> Nat
offsetToLine jsCode offset = go 1 0 (unpack jsCode)
  where
    go : Nat -> Nat -> List Char -> Nat
    go line pos [] = line
    go line pos (c :: rest) =
      if pos >= offset then line
      else if c == '\n' then go (S line) (S pos) rest
      else go line (S pos) rest

||| Map V8 coverage to Idris2 source files
export
mapCoverageToSource : SourceMap -> String -> V8ScriptCoverage
                    -> List FileCoverageSummary
mapCoverageToSource sm jsCode v8Cov =
  let coveredJsLines = collectCoveredLines v8Cov jsCode
      sourceMappings = mapMaybe (findOriginalPosition sm) coveredJsLines
      byFile = groupByFile sourceMappings
  in map toSummary byFile
  where
    collectCoveredLines : V8ScriptCoverage -> String -> List Nat
    collectCoveredLines cov js =
      let allRanges = concatMap (.ranges) cov.functions
          coveredRanges = filter (\r => r.count > 0) allRanges
      in map (\r => offsetToLine js r.startOffset) coveredRanges

    groupByFile : List SourceMapping -> List (String, List Nat)
    groupByFile [] = []
    groupByFile mappings =
      let files = nub $ map (.srcFile) mappings
      in map (\f => (f, map (.srcLine) $ filter (\m => m.srcFile == f) mappings)) files

    toSummary : (String, List Nat) -> FileCoverageSummary
    toSummary (file, covLines) =
      let uniqueLines = nub covLines
          totalEst = fromMaybe 1 (foldr (\x, acc => Just $ max x (fromMaybe 0 acc)) Nothing uniqueLines)
          pct = if totalEst == 0 then 100.0
                else cast (length uniqueLines) / cast totalEst * 100.0
      in MkFileCoverageSummary file uniqueLines [] totalEst pct
