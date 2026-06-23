||| Path-level coverage API for DFX/ICP projects backed by --dumppaths-json.
module DfxCoverage.PathCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import DfxCoverage.Exclusions
import Coverage.Core.DumppathsJson
import public Coverage.Core.PathCoverage
import public Coverage.Core.RuntimeHit
import Coverage.Core.Backend   -- shared isGeneratedRecordProjectionPath + reclassifyArtifacts

%default covering

export
defaultPathExclusions : List ExclPattern
defaultPathExclusions = icpFullExclusions

||| Deprecated delete-filter (kept for callers that want the pruned list). Uses the
||| BARE-only projection predicate (dfx historical form), now shared from Backend.
export
filterPathObligations : List ExclPattern -> List PathObligation -> List PathObligation
filterPathObligations patterns =
  filter (\path => not (isJust (isMethodExcluded patterns path.functionName)
                        || isBareRecordProjectionPath path))

||| Build the dfx denominator: reclassify (NOT delete) test-harness + generated
||| projections to CompilerInsertedArtifact so the shared countsAsDenominator drops
||| them — SAME numbers as the old delete-filter (reclassifyArtifactsBare uses the
||| BARE-only projection predicate dfx historically matched, not the web dotted
||| superset), honest (excluded paths survive reclassified, not vanished).
export
parseProjectDumppathsJson : List ExclPattern -> String -> Either String (List PathObligation)
parseProjectDumppathsJson patterns content = do
  paths <- parseDumppathsJson content
  pure $ reclassifyArtifactsBare patterns paths

export
loadProjectDumppathsJson : String -> List ExclPattern -> IO (Either String (List PathObligation))
loadProjectDumppathsJson path patterns = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ parseProjectDumppathsJson patterns content

export
analyzePathCoverageFromContent : List ExclPattern
                              -> String
                              -> List PathRuntimeHit
                              -> Either String PathCoverageResult
analyzePathCoverageFromContent patterns content hits = do
  paths <- parseProjectDumppathsJson patterns content
  pure $ buildPathCoverageResultFromHits paths hits

export
analyzePathCoverageFromFile : String
                           -> List ExclPattern
                           -> List PathRuntimeHit
                           -> IO (Either String PathCoverageResult)
analyzePathCoverageFromFile path patterns hits = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ analyzePathCoverageFromContent patterns content hits

export
untestedPathsFromContent : List ExclPattern
                        -> String
                        -> List PathRuntimeHit
                        -> Either String (List PathObligation)
untestedPathsFromContent patterns content hits =
  map missingPaths (analyzePathCoverageFromContent patterns content hits)

export
untestedPathsFromFile : String
                     -> List ExclPattern
                     -> List PathRuntimeHit
                     -> IO (Either String (List PathObligation))
untestedPathsFromFile path patterns hits = do
  result <- analyzePathCoverageFromFile path patterns hits
  pure $ map missingPaths result
