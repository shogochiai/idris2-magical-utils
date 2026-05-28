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
import Coverage.Core.Types
import public Coverage.Core.RuntimeHit

%default covering

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
