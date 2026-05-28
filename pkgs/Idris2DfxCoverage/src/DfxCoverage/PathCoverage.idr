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

%default covering

isAsciiUpper : Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower : Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

startsWith : (Char -> Bool) -> String -> Bool
startsWith predicate s =
  case unpack s of
    [] => False
    c :: _ => predicate c

isGeneratedRecordProjectionName : String -> Bool
isGeneratedRecordProjectionName name =
  case reverse (forget $ split (== '.') name) of
    field :: recordName :: _ =>
         startsWith isAsciiUpper recordName
      && startsWith isAsciiLower field
      && not (isInfixOf ":" field)
      && not (isInfixOf "case block" name)
    _ => False

isGeneratedRecordProjectionPath : PathObligation -> Bool
isGeneratedRecordProjectionPath path =
     path.terminalKind == "reached_clause"
  && path.steps == []
  && isNothing path.sourceSpanUnion
  && isGeneratedRecordProjectionName path.functionName

shouldExcludePath : List ExclPattern -> PathObligation -> Bool
shouldExcludePath patterns path =
  isJust (isMethodExcluded patterns path.functionName) ||
  isGeneratedRecordProjectionPath path

export
defaultPathExclusions : List ExclPattern
defaultPathExclusions = icpFullExclusions

export
filterPathObligations : List ExclPattern -> List PathObligation -> List PathObligation
filterPathObligations patterns =
  filter (\path => not (shouldExcludePath patterns path))

export
parseProjectDumppathsJson : List ExclPattern -> String -> Either String (List PathObligation)
parseProjectDumppathsJson patterns content = do
  paths <- parseDumppathsJson content
  pure $ filterPathObligations patterns paths

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
