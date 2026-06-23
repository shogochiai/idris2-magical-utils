||| Recover path hits for EVM from runtime labels and dumppaths exports.
module EvmCoverage.PathRuntime

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import EvmCoverage.Runtime
import Coverage.Core.DumppathsJson
import Coverage.Core.PathCoverage
import Coverage.Core.Backend   -- coveredByKey/hitsByKey (the shared polymorphic join)
import Coverage.Core.Types
import public Coverage.Core.RuntimeHit

%default covering

-- ============================================================================
-- Canonical path-id LOG-topic recovery (--dumppathshits, source-level evm)
-- ============================================================================
-- The fork-built idris2-yul lowers prim__recordPathHit "<fn>#pN" to
-- log1(0,0,FNV1a64(path-id)). revm records that LOG; we scan the trace for the
-- topic [0] hex values, and a dumppaths path is covered iff FNV1a64(path_id) is
-- among the fired topics. pathIdTopic MUST be byte-identical to the Yul backend's
-- Compiler.EVM.Codegen.pathIdTopic.

||| FNV-1a 64-bit of the path-id — identical to idris2-yul Codegen.pathIdTopic.
export
pathIdTopic : String -> Integer
pathIdTopic s = foldl step 14695981039346656037 (map (cast . ord) (unpack s))
  where
    mask64 : Integer
    mask64 = 18446744073709551615
    step : Integer -> Integer -> Integer
    step h b =
      let x = prim__and_Integer (prim__xor_Integer h b) mask64
      in prim__and_Integer (x * 1099511628211) mask64

hexDigitVal : Char -> Maybe Integer
hexDigitVal c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

parseHexInteger : String -> Maybe Integer
parseHexInteger raw =
  let s = if isPrefixOf "0x" raw then substr 2 (length raw) raw else raw
      ds = unpack (trim s)
  in if null ds then Nothing
     else foldl (\acc, c => do a <- acc; d <- hexDigitVal c; pure (a * 16 + d)) (Just 0) ds

||| Extract every topic [0] hex value from a revm trace as integers. Trace shape:
|||   Topics: N
|||     [0] 0x<64hex>
||| We take the first topic of each log (the path-hit topic / ProfileFlush topic).
firedTopicIntegers : String -> List Integer
firedTopicIntegers content =
  mapMaybe topicOf (filter isTopic0Line (lines content))
  where
    isTopic0Line : String -> Bool
    isTopic0Line l = let t = ltrim l in isPrefixOf "[0] 0x" t
    topicOf : String -> Maybe Integer
    topicOf l =
      let t = ltrim l
          -- drop the "[0] " prefix, keep the 0x... token
          afterTag = substr 4 (length t) t
          tok = case words afterTag of (w :: _) => w; [] => ""
      in parseHexInteger tok

||| HASH join: a dumppaths path is covered iff FNV(path_id) was a fired topic. This
||| is the shared polymorphic `coveredByKey` with k = Integer and the key function
||| `pathIdTopic` (the non-invertible FNV hash) — the SAME join Core uses for web/dfx
||| with k = String and key `(.pathId)`. One join, two key types, no tagged union.
export
analyzePathHitsFromPathIdTopics : String -> String -> Either String (List PathRuntimeHit)
analyzePathHitsFromPathIdTopics dumppathsContent traceContent = do
  paths <- parseDumppathsJson dumppathsContent
  let fired = firedTopicIntegers traceContent
  -- key function PathObligation -> Integer = FNV hash of the path id
  pure (hitsByKey (\p => pathIdTopic p.pathId) fired paths)

legacyTerminalBranchId : PathObligation -> PathStep -> Maybe String
legacyTerminalBranchId path step =
  case step.caseIndex of
    Nothing => Nothing
    Just caseIdx =>
      let legacyId = show (MkBranchId path.moduleName (getSimpleFuncName path.functionName) caseIdx step.branchIndex)
      in Just legacyId

export
terminalBranchId : PathObligation -> Maybe String
terminalBranchId path =
  case last' path.steps of
    Nothing => Nothing
    Just step => case (trim step.nodeId) /= "" of
                   True => Just step.nodeId
                   False => legacyTerminalBranchId path step

stripPrefixIfPresent : String -> String -> String
stripPrefixIfPresent needle value =
  if isPrefixOf needle value
     then strSubstr (cast $ length needle) (cast $ length value) value
     else value

lastSegmentAfter : Char -> String -> String
lastSegmentAfter sep value =
  case reverse (forget (split (== sep) value)) of
    x :: _ => x
    [] => value

stripRepeatedCaseBlock : String -> String
stripRepeatedCaseBlock value =
  let stripped = stripPrefixIfPresent "case block in " value in
  if stripped == value
     then value
     else stripRepeatedCaseBlock stripped

nodeIdSuffixKey : String -> Maybe String
nodeIdSuffixKey nodeId =
  case break (== '#') (unpack nodeId) of
    (nameChars, hashAndRest) =>
      case hashAndRest of
        [] => Nothing
        _ =>
          let name = pack nameChars
              suffix = pack hashAndRest
              localName = lastSegmentAfter '.' name
              normalized = lastSegmentAfter ',' (stripRepeatedCaseBlock localName)
          in if trim normalized == ""
                then Nothing
                else Just (normalized ++ suffix)

export
branchMatchesNodeId : String -> String -> Bool
branchMatchesNodeId covered nodeId =
  covered == nodeId ||
  case nodeIdSuffixKey nodeId of
    Nothing => False
    Just key => isSuffixOf ("." ++ key) covered

export
pathCoveredByBranchIds : List String -> PathObligation -> Bool
pathCoveredByBranchIds coveredBranchIds path =
  case terminalBranchId path of
    Nothing => False
    Just nodeId => any (\covered => branchMatchesNodeId covered nodeId) coveredBranchIds

export
pathHitsFromCoveredBranchIds : List String -> List PathObligation -> List PathRuntimeHit
pathHitsFromCoveredBranchIds coveredBranchIds paths =
  mapMaybe toHit paths
  where
    toHit : PathObligation -> Maybe PathRuntimeHit
    toHit path =
      if pathCoveredByBranchIds coveredBranchIds path
         then Just (MkPathRuntimeHit path.pathId 1)
         else Nothing

export
pathHitsFromCoveredBranchIdsInContent : List String -> String -> Either String (List PathRuntimeHit)
pathHitsFromCoveredBranchIdsInContent coveredBranchIds dumppathsContent = do
  paths <- parseDumppathsJson dumppathsContent
  pure $ pathHitsFromCoveredBranchIds coveredBranchIds paths

export
analyzePathHitsFromTraceAndLabelsContent : String -> String -> String -> IO (Either String (List PathRuntimeHit))
analyzePathHitsFromTraceAndLabelsContent dumppathsContent tracePath labelPath = do
  -- PREFER source-level canonical path-id topics (fork idris2-yul emits
  -- log1(0,0,FNV(path-id))). If the trace carries such topics they identity-join
  -- directly to dumppaths path_ids — no label/observability heuristic, and
  -- inlined branches are observable too. Fall back to the legacy label-based
  -- path/branch recovery when no path-id topics are present (released idris2-yul).
  Right traceContent <- readFile tracePath
    | Left err => pure $ Left $ "Failed to read trace: " ++ show err
  case analyzePathHitsFromPathIdTopics dumppathsContent traceContent of
    Right topicHits@(_ :: _) => pure $ Right topicHits
    _ => do
      direct <- analyzePathHitsFromFile tracePath labelPath
      case direct of
        Left err => pure $ Left err
        Right hits =>
          if null hits
             then do
               Right branchResult <- analyzeBranchCoverageFromFile tracePath labelPath []
                 | Left err => pure $ Left err
               pure $ pathHitsFromCoveredBranchIdsInContent branchResult.coveredBranchIds dumppathsContent
             else pure $ Right hits

export
analyzePathHitsFromTraceAndLabelsFile : String -> String -> String -> IO (Either String (List PathRuntimeHit))
analyzePathHitsFromTraceAndLabelsFile dumppathsPath tracePath labelPath = do
  Right content <- readFile dumppathsPath
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  analyzePathHitsFromTraceAndLabelsContent content tracePath labelPath
