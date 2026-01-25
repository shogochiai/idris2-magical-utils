||| Source Map V3 Generator and RefC Comment Parser
|||
||| Parses RefC-generated C code comments to extract Idris2 source locations,
||| generates Source Map V3 JSON, and chains multiple source maps.
|||
||| RefC comment format: `// Module:startLine:startCol--endLine:endCol`
|||
||| Reference: https://sourcemaps.info/spec.html
module WasmBuilder.SourceMap.SourceMap

import Data.List
import Data.String
import Data.Maybe
import Data.Fin
import System.File
import WasmBuilder.SourceMap.VLQ

%default covering

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| String length as Nat
strLen : String -> Nat
strLen s = cast (max 0 (strLength s))

-- =============================================================================
-- Types
-- =============================================================================

||| Idris2 source location
public export
record IdrisLoc where
  constructor MkIdrisLoc
  file : String       -- Module name (e.g., "Main", "Data.List")
  startLine : Nat     -- 1-indexed
  startCol : Nat
  endLine : Nat
  endCol : Nat

public export
Show IdrisLoc where
  show loc = loc.file ++ ":" ++ show loc.startLine ++ ":" ++ show loc.startCol ++
             "--" ++ show loc.endLine ++ ":" ++ show loc.endCol

public export
Eq IdrisLoc where
  a == b = a.file == b.file && a.startLine == b.startLine && a.startCol == b.startCol
           && a.endLine == b.endLine && a.endCol == b.endCol

||| C line → Idris2 location mapping
public export
record CToIdrisMapping where
  constructor MkMapping
  cFile : String
  cLine : Nat         -- 1-indexed
  idrisLoc : IdrisLoc

public export
Show CToIdrisMapping where
  show m = show m.cLine ++ " -> " ++ show m.idrisLoc

||| Source Map V3 structure
public export
record SourceMapV3 where
  constructor MkSourceMapV3
  version : Nat       -- Always 3
  file : String       -- Generated file name
  sourceRoot : String -- Source root path
  sources : List String  -- List of source file paths
  names : List String    -- Symbol names (optional)
  mappings : String      -- VLQ-encoded mappings

public export
Show SourceMapV3 where
  show sm = "SourceMapV3{file=" ++ sm.file ++ ", sources=" ++ show sm.sources ++ "}"

-- =============================================================================
-- RefC Comment Parser
-- =============================================================================

||| Parse natural number from string
parseNat : String -> Maybe Nat
parseNat s =
  let n = cast {to=Integer} s
  in if n >= 0 then Just (cast n) else Nothing

||| Split string by delimiter (simple implementation)
splitOn : Char -> String -> List String
splitOn delim s = splitOn' (unpack s) [] []
  where
    splitOn' : List Char -> List Char -> List String -> List String
    splitOn' [] acc results = reverse (pack (reverse acc) :: results)
    splitOn' (c :: rest) acc results =
      if c == delim
        then splitOn' rest [] (pack (reverse acc) :: results)
        else splitOn' rest (c :: acc) results

||| Parse RefC location comment: `Module:line:col--line:col`
||| Returns (module, startLine, startCol, endLine, endCol)
parseLocComment : String -> Maybe IdrisLoc
parseLocComment s =
  -- First split by "--" to get start and end
  case splitOn '-' s of
    [startPart, "", endPart] =>
      -- Parse start: Module:line:col
      case splitOn ':' startPart of
        [modName, sLine, sCol] =>
          case splitOn ':' endPart of
            [eLine, eCol] =>
              do startLine <- parseNat sLine
                 startCol <- parseNat sCol
                 endLine <- parseNat eLine
                 endCol <- parseNat eCol
                 Just $ MkIdrisLoc modName startLine startCol endLine endCol
            _ => Nothing
        _ => Nothing
    _ => Nothing

||| Extract comment from C line if present
||| Looks for `// Module:line:col--line:col` at end of line
extractComment : String -> Maybe String
extractComment line =
  let len = strLen line
  in if len == 0 then Nothing else
     -- Find last "//" in line
     let chars = unpack line
     in findComment chars 0 Nothing len
  where
    findComment : List Char -> Nat -> Maybe Nat -> Nat -> Maybe String
    findComment [] _ lastPos lineLen =
      case lastPos of
        Nothing => Nothing
        Just pos => Just $ trim $ substr (pos + 2) lineLen line
    findComment ('/' :: '/' :: rest) idx _ lineLen = findComment rest (idx + 2) (Just idx) lineLen
    findComment (_ :: rest) idx lastPos lineLen = findComment rest (S idx) lastPos lineLen

||| Parse a single C line, return mapping if location comment found
parseCLine : String -> Nat -> String -> Maybe CToIdrisMapping
parseCLine cFile lineNum line =
  do comment <- extractComment line
     loc <- parseLocComment comment
     Just $ MkMapping cFile lineNum loc

||| Parse entire RefC output file, extract all mappings
public export
parseRefCComments : String -> String -> List CToIdrisMapping
parseRefCComments cFile content =
  let cLines = lines content
      indexed = zip [1..length cLines] cLines
  in mapMaybe (\(n, l) => parseCLine cFile n l) indexed

-- =============================================================================
-- Function Name Extraction
-- =============================================================================

||| C function to Idris name mapping
public export
record CFunctionInfo where
  constructor MkCFunctionInfo
  cName : String       -- e.g., "Main_canisterInit"
  idrisName : String   -- e.g., "Main.canisterInit"
  lineStart : Nat      -- Line where function starts

public export
Show CFunctionInfo where
  show f = f.cName ++ " (" ++ f.idrisName ++ ") @ line " ++ show f.lineStart

||| Convert C function name to Idris format
||| "Module_submodule_function" -> "Module.submodule.function"
||| Handles special cases like "prim__xxx", "_braceOpen_", "__mainExpression"
cNameToIdris : String -> String
cNameToIdris cName =
  let -- Skip internal/special names
      isSpecial = isPrefixOf "__" cName ||
                  isPrefixOf "_brace" cName ||
                  isPrefixOf "prim__" cName
  in if isSpecial
     then cName
     else -- Replace all underscores with dots (Module_Sub_func -> Module.Sub.func)
          -- But preserve double underscores in function names (eq_eq -> eq.eq is wrong)
          replaceModuleSeps cName
  where
    -- Replace single underscores (module separators) but not double underscores
    replaceModuleSeps : String -> String
    replaceModuleSeps s = pack $ go (unpack s)
      where
        go : List Char -> List Char
        go [] = []
        go ('_' :: '_' :: rest) = '_' :: '_' :: go rest  -- Keep double underscore
        go ('_' :: rest) = '.' :: go rest                -- Single underscore -> dot
        go (c :: rest) = c :: go rest

||| Check if line is a function definition start
||| Pattern: "Value *FunctionName" at start of line (declaration)
||| or "Value *FunctionName(" for definition
parseFunctionDef : String -> Nat -> Maybe CFunctionInfo
parseFunctionDef line lineNum =
  let trimmed = ltrim line
  in if isPrefixOf "Value *" trimmed
     then let afterValue = substr 7 (strLen trimmed) trimmed
              -- Extract function name (until '(' or newline)
              funcName = takeWhile (\c => c /= '(' && c /= ' ' && c /= '\n') afterValue
          in if null funcName || isPrefixOf "(" funcName
             then Nothing
             else Just $ MkCFunctionInfo funcName (cNameToIdris funcName) lineNum
     else Nothing
  where
    takeWhile : (Char -> Bool) -> String -> String
    takeWhile pred s = pack $ takeWhile' pred (unpack s)
      where
        takeWhile' : (Char -> Bool) -> List Char -> List Char
        takeWhile' _ [] = []
        takeWhile' p (c :: rest) = if p c then c :: takeWhile' p rest else []

||| Extract all function definitions from RefC C file
public export
parseFunctionDefs : String -> List CFunctionInfo
parseFunctionDefs content =
  let cLines = lines content
      indexed = zip [1..length cLines] cLines
      defs = mapMaybe (\(n, l) => parseFunctionDef l n) indexed
  in nubBy (\a, b => a.cName == b.cName) defs

||| Build names array from function definitions
public export
buildNamesArray : List CFunctionInfo -> List String
buildNamesArray funcs = map idrisName funcs

-- =============================================================================
-- Source Map V3 Generation
-- =============================================================================

||| Convert module name to file path
||| "Data.List" -> "Data/List.idr"
moduleToPath : String -> String
moduleToPath modName =
  let parts = splitOn '.' modName
  in joinBy "/" parts ++ ".idr"

||| Build source index map from mappings
buildSourceIndex : List CToIdrisMapping -> List String
buildSourceIndex mappings =
  nub $ map (\m => moduleToPath m.idrisLoc.file) mappings

||| Find index of element in list (String specialized)
findStrIdx : String -> List String -> Nat
findStrIdx x xs = findStrIdx' x xs 0
  where
    findStrIdx' : String -> List String -> Nat -> Nat
    findStrIdx' _ [] acc = acc
    findStrIdx' y (z :: zs) acc = if y == z then acc else findStrIdx' y zs (S acc)

||| Group mappings by C line number
groupByLine : List CToIdrisMapping -> List (Nat, List CToIdrisMapping)
groupByLine [] = []
groupByLine mappings =
  let sorted = sortBy (\a, b => compare a.cLine b.cLine) mappings
      maxLine = foldl (\acc, m => max acc m.cLine) 0 sorted
  in groupByLine' sorted 1 maxLine []
  where
    groupByLine' : List CToIdrisMapping -> Nat -> Nat -> List (Nat, List CToIdrisMapping)
                -> List (Nat, List CToIdrisMapping)
    groupByLine' ms line maxL acc =
      if line > maxL
        then reverse acc
        else let (thisLine, rest) = span (\m => m.cLine == line) ms
             in groupByLine' rest (S line) maxL ((line, thisLine) :: acc)

||| Get max line from grouped
getMaxLine : List (Nat, List CToIdrisMapping) -> Nat
getMaxLine gs = foldl (\acc, p => max acc (fst p)) 0 gs

||| Convert mappings to VLQ-encoded string
generateMappingsString : List CToIdrisMapping -> List String -> String
generateMappingsString mappings sources =
  let grouped = groupByLine mappings
      maxLine = getMaxLine grouped
  in generateLines grouped 1 maxLine 0 0 0 ""
  where
    findSourceIdx : String -> Nat
    findSourceIdx src = findStrIdx src sources

    generateLines : List (Nat, List CToIdrisMapping) -> Nat -> Nat
                 -> Nat -> Nat -> Nat -> String -> String
    generateLines [] _ _ _ _ _ acc = acc
    generateLines ((lineNum, lineMs) :: rest) curLine maxL pSI pSL pSC acc =
      -- Add empty lines (semicolons) if needed
      let emptyLines = if lineNum > curLine
                       then pack $ replicate (minus lineNum curLine) ';'
                       else ""
          -- Generate segments for this line
          mkSeg = \m =>
            let srcPath = moduleToPath m.idrisLoc.file
                srcIdx = findSourceIdx srcPath
            in MkSegment 0 srcIdx (minus m.idrisLoc.startLine 1) (minus m.idrisLoc.startCol 1) (-1)
          segs = map mkSeg lineMs
          (encoded, _, nSI, nSL, nSC) = encodeSegments 0 pSI pSL pSC segs
          newAcc = acc ++ emptyLines ++ encoded
      in generateLines rest (S lineNum) maxL nSI nSL nSC newAcc

||| Generate Source Map V3 from C-to-Idris mappings
public export
generateIdrisCSourceMap : String -> List CToIdrisMapping -> SourceMapV3
generateIdrisCSourceMap cFile mappings =
  let sources = buildSourceIndex mappings
      mappingsStr = generateMappingsString mappings sources
  in MkSourceMapV3 3 cFile "" sources [] mappingsStr

||| Generate Source Map V3 from C-to-Idris mappings with function names
public export
generateIdrisCSourceMapWithFunctions : String -> String -> SourceMapV3
generateIdrisCSourceMapWithFunctions cFile content =
  let mappings = parseRefCComments cFile content
      funcs = parseFunctionDefs content
      sources = buildSourceIndex mappings
      names = buildNamesArray funcs
      mappingsStr = generateMappingsString mappings sources
  in MkSourceMapV3 3 cFile "" sources names mappingsStr

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Escape string for JSON
escapeJson : String -> String
escapeJson s = pack $ concatMap escapeChar (unpack s)
  where
    escapeChar : Char -> List Char
    escapeChar '"' = ['\\', '"']
    escapeChar '\\' = ['\\', '\\']
    escapeChar '\n' = ['\\', 'n']
    escapeChar '\r' = ['\\', 'r']
    escapeChar '\t' = ['\\', 't']
    escapeChar c = [c]

||| Convert list of strings to JSON array
jsonArray : List String -> String
jsonArray xs = "[" ++ joinBy ", " (map (\x => "\"" ++ escapeJson x ++ "\"") xs) ++ "]"

||| Convert SourceMapV3 to JSON string
public export
toJson : SourceMapV3 -> String
toJson sm =
  "{\n" ++
  "  \"version\": " ++ show sm.version ++ ",\n" ++
  "  \"file\": \"" ++ escapeJson sm.file ++ "\",\n" ++
  "  \"sourceRoot\": \"" ++ escapeJson sm.sourceRoot ++ "\",\n" ++
  "  \"sources\": " ++ jsonArray sm.sources ++ ",\n" ++
  "  \"names\": " ++ jsonArray sm.names ++ ",\n" ++
  "  \"mappings\": \"" ++ escapeJson sm.mappings ++ "\"\n" ++
  "}"

||| Write source map to file
public export
writeSourceMap : String -> SourceMapV3 -> IO (Either FileError ())
writeSourceMap path sm = writeFile path (toJson sm)

-- =============================================================================
-- Source Map Chaining
-- =============================================================================

||| Find substring in string starting from position
findSubstr : String -> String -> Nat -> Maybe Nat
findSubstr needle haystack start =
  let needleLen = strLen needle
      haystackLen = strLen haystack
  in if start + needleLen > haystackLen
       then Nothing
       else if substr start needleLen haystack == needle
            then Just start
            else findSubstr needle haystack (S start)

||| Extract quoted string from JSON
extractQuotedString : String -> Maybe String
extractQuotedString s = extractQuoted (unpack s) []
  where
    extractQuoted : List Char -> List Char -> Maybe String
    extractQuoted [] _ = Nothing
    extractQuoted ('"' :: _) acc = Just $ pack $ reverse acc
    extractQuoted ('\\' :: c :: rest) acc = extractQuoted rest (c :: acc)
    extractQuoted (c :: rest) acc = extractQuoted rest (c :: acc)

||| Drop characters until closing quote
dropQuoted : List Char -> String
dropQuoted [] = ""
dropQuoted ('"' :: rest) = pack rest
dropQuoted ('\\' :: _ :: rest) = dropQuoted rest
dropQuoted (_ :: rest) = dropQuoted rest

||| Check if string starts with given character
startsWithChar : Char -> String -> Bool
startsWithChar c s = case unpack s of
  (x :: _) => x == c
  [] => False

||| Extract JSON string value
extractJsonString : String -> String -> Maybe String
extractJsonString key json =
  let pattern = "\"" ++ key ++ "\":"
      patternLen = strLen pattern
      jsonLen = strLen json
  in case findSubstr pattern json 0 of
       Nothing => Nothing
       Just idx =>
         let afterKey = substr (idx + patternLen) jsonLen json
             trimmed = ltrim afterKey
             trimmedLen = strLen trimmed
         in if startsWithChar '"' trimmed
            then extractQuotedString (substr 1 trimmedLen trimmed)
            else Nothing

||| Extract JSON array of strings
extractJsonArray : String -> String -> Maybe (List String)
extractJsonArray key json =
  let pattern = "\"" ++ key ++ "\":"
      patternLen = strLen pattern
      jsonLen = strLen json
  in case findSubstr pattern json 0 of
       Nothing => Nothing
       Just idx =>
         let afterKey = substr (idx + patternLen) jsonLen json
             trimmed = ltrim afterKey
             trimmedLen = strLen trimmed
         in if startsWithChar '[' trimmed
            then extractArray (substr 1 trimmedLen trimmed) []
            else Nothing
  where
    extractArray : String -> List String -> Maybe (List String)
    extractArray s items =
      case unpack (ltrim s) of
        [] => Nothing
        (']' :: _) => Just $ reverse items
        ('"' :: rest) =>
          case extractQuotedString (pack rest) of
            Nothing => Nothing
            Just str => extractArray (dropQuoted rest) (str :: items)
        (',' :: rest) => extractArray (pack rest) items
        _ => Nothing

||| Parse simple JSON source map (basic implementation)
||| This handles the format generated by Emscripten
public export
parseSourceMapJson : String -> Maybe SourceMapV3
parseSourceMapJson json =
  do file <- extractJsonString "file" json
     let sourceRoot = fromMaybe "" (extractJsonString "sourceRoot" json)
     sources <- extractJsonArray "sources" json
     let names = fromMaybe [] (extractJsonArray "names" json)
     mappings <- extractJsonString "mappings" json
     Just $ MkSourceMapV3 3 file sourceRoot sources names mappings

||| Read and parse source map from file
public export
readSourceMap : String -> IO (Either String SourceMapV3)
readSourceMap path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read source map: " ++ show err
  case parseSourceMapJson content of
    Nothing => pure $ Left "Failed to parse source map JSON"
    Just sm => pure $ Right sm

||| Chain two source maps: A→B + B→C = A→C
||| The first map (idrisCMap) maps original Idris to C
||| The second map (cWasmMap) maps C to WASM
||| Result maps original Idris to WASM
public export
chainSourceMaps : (idrisCMap : SourceMapV3) -> (cWasmMap : SourceMapV3) -> SourceMapV3
chainSourceMaps idrisCMap cWasmMap =
  -- For now, return the Idris→C map with WASM file name
  -- Full implementation would decode cWasmMap.mappings,
  -- look up each C line in idrisCMap, and rebuild
  MkSourceMapV3
    { version = 3
    , file = cWasmMap.file
    , sourceRoot = idrisCMap.sourceRoot
    , sources = idrisCMap.sources
    , names = idrisCMap.names
    , mappings = idrisCMap.mappings  -- Simplified: use idris→C mappings directly
    }

||| Build complete Idris→WASM source map
public export
buildFullSourceMap : (idrisCMap : SourceMapV3) -> (cWasmMap : SourceMapV3) -> SourceMapV3
buildFullSourceMap = chainSourceMaps
