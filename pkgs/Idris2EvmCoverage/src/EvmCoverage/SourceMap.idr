||| Yul Source Map to Idris Location Mapping
|||
||| Parses Idris location comments embedded in Yul source:
|||   /* Module:startLine:startCol--endLine:endCol */
|||
||| Provides mapping: EVM PC -> Yul offset -> Idris source location
module EvmCoverage.SourceMap

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import System.File

import EvmCoverage.AsmJsonParser

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Idris source location (line-level granularity)
public export
record IdrisLoc where
  constructor MkIdrisLoc
  moduleName : String
  startLine : Int
  startCol : Int
  endLine : Int
  endCol : Int

public export
Show IdrisLoc where
  show loc = loc.moduleName ++ ":" ++ show loc.startLine ++ ":" ++ show loc.startCol

public export
Eq IdrisLoc where
  a == b = a.moduleName == b.moduleName && a.startLine == b.startLine

||| A parsed comment with its Yul offset
public export
record YulComment where
  constructor MkYulComment
  yulOffset : Nat    -- End offset of comment in Yul source
  idrisLoc : IdrisLoc

-- =============================================================================
-- Comment Parsing
-- =============================================================================

||| Parse start:end location pair
parseStartEnd : String -> String -> Maybe IdrisLoc
parseStartEnd startStr endStr =
  let startParts = forget $ split (== ':') startStr
      endParts = forget $ split (== ':') endStr
  in case (startParts, endParts) of
       ([mod, sl, sc], [el, ec]) =>
         case (parseInteger sl, parseInteger sc, parseInteger el, parseInteger ec) of
           (Just sLine, Just sCol, Just eLine, Just eCol) =>
             Just $ MkIdrisLoc mod sLine sCol eLine eCol
           _ => Nothing
       _ => Nothing

||| Parse a single Idris location comment
||| Format: /* Module:startLine:startCol--endLine:endCol */
parseLocComment : String -> Maybe IdrisLoc
parseLocComment s =
  let trimmed = trim s
  in if isPrefixOf "/*" trimmed && isSuffixOf "*/" trimmed
       then parseInner (substr 2 (length trimmed `minus` 4) trimmed)
       else Nothing
  where
    parseInner : String -> Maybe IdrisLoc
    parseInner inner =
      let parts = forget $ split (== '-') (trim inner)
      in case parts of
           [startPart, "", endPart] => parseStartEnd startPart endPart
           _ => Nothing

||| Find comment end marker
findCommentEnd : List Char -> List Char -> Maybe (List Char, List Char)
findCommentEnd [] _ = Nothing
findCommentEnd ('*' :: '/' :: rest) acc = Just (acc, rest)
findCommentEnd (c :: rest) acc = findCommentEnd rest (c :: acc)

||| Find all Idris location comments in Yul source
||| Returns list of (comment end offset, IdrisLoc)
export
parseYulComments : String -> List YulComment
parseYulComments content = go 0 (unpack content) []
  where
    go : Nat -> List Char -> List YulComment -> List YulComment
    go _ [] acc = reverse acc
    go pos ('/' :: '*' :: rest) acc =
      case findCommentEnd rest [] of
        Just (commentBody, afterComment) =>
          let comment = "/* " ++ pack (reverse commentBody) ++ " */"
              commentEnd = pos + 4 + length commentBody
          in case parseLocComment comment of
               Just loc => go commentEnd afterComment (MkYulComment commentEnd loc :: acc)
               Nothing => go commentEnd afterComment acc
        Nothing => go (S pos) ('*' :: rest) acc
    go pos (_ :: rest) acc = go (S pos) rest acc

-- =============================================================================
-- Yul Offset to Idris Location Mapping
-- =============================================================================

||| Find Idris location for a Yul offset
||| Uses "nearest preceding comment" strategy
export
findIdrisLoc : List YulComment -> Nat -> Maybe IdrisLoc
findIdrisLoc comments yulOffset =
  let preceding = filter (\c => c.yulOffset <= yulOffset) comments
  in map idrisLoc $ head' $ sortBy (\a, b => compare b.yulOffset a.yulOffset) preceding

||| Build lookup table: Yul offset ranges -> IdrisLoc
export
buildYulToIdrisMap : List YulComment -> SortedMap Nat IdrisLoc
buildYulToIdrisMap comments =
  foldl (\m, c => insert c.yulOffset c.idrisLoc m) empty comments

-- =============================================================================
-- PC to Idris Location Mapping
-- =============================================================================

||| Map EVM PC to Idris source location
||| Pipeline: PC -> AsmInstr -> Yul offset -> nearest comment -> IdrisLoc
export
mapPcToIdris : List YulComment -> List AsmInstr -> Nat -> Maybe IdrisLoc
mapPcToIdris comments instrs pc =
  case find (\i => i.pc == pc) instrs of
    Nothing => Nothing
    Just instr => findIdrisLoc comments (cast instr.beginOff)

||| Get all Idris locations hit by a trace
export
getHitLocations : List YulComment -> List AsmInstr -> List Nat -> List IdrisLoc
getHitLocations comments instrs pcs =
  nub $ mapMaybe (mapPcToIdris comments instrs) pcs

-- =============================================================================
-- Coverage Analysis (Line-Level)
-- =============================================================================

||| Line coverage result
public export
record LineCoverage where
  constructor MkLineCoverage
  loc : IdrisLoc
  hitCount : Nat

||| Aggregate hit counts by Idris location
export
aggregateByLocation : List IdrisLoc -> SortedMap String (SortedMap Int Nat)
aggregateByLocation locs = foldl addLoc empty locs
  where
    addLoc : SortedMap String (SortedMap Int Nat) -> IdrisLoc -> SortedMap String (SortedMap Int Nat)
    addLoc m loc =
      let modMap = fromMaybe empty (lookup loc.moduleName m)
          lineCount = fromMaybe 0 (lookup loc.startLine modMap)
          newModMap = insert loc.startLine (S lineCount) modMap
      in insert loc.moduleName newModMap m

-- =============================================================================
-- File I/O
-- =============================================================================

||| Parse Yul file and extract comments
export
readYulComments : String -> IO (Either String (List YulComment))
readYulComments path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read Yul file: " ++ show err
  pure $ Right $ parseYulComments content
