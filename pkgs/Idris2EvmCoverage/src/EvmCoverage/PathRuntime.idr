||| Recover path hits for EVM from runtime labels and dumppaths exports.
module EvmCoverage.PathRuntime

import Data.List
import Data.Maybe
import System.File

import EvmCoverage.Runtime
import Coverage.Core.DumppathsJson
import Coverage.Core.PathCoverage
import public Coverage.Core.RuntimeHit

%default covering

terminalNodeId : PathObligation -> Maybe String
terminalNodeId path = map (.nodeId) (last' path.steps)

export
pathHitsFromCoveredBranchIds : List String -> List PathObligation -> List PathRuntimeHit
pathHitsFromCoveredBranchIds coveredBranchIds paths =
  mapMaybe toHit paths
  where
    toHit : PathObligation -> Maybe PathRuntimeHit
    toHit path =
      case terminalNodeId path of
        Just nodeId =>
          if elem nodeId coveredBranchIds
             then Just (MkPathRuntimeHit path.pathId 1)
             else Nothing
        Nothing => Nothing

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
