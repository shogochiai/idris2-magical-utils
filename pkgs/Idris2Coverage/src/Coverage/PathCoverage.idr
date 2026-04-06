||| Path-level coverage API backed by Idris2 --dumppaths-json.
module Coverage.PathCoverage

import Data.List
import Data.Maybe
import Data.String
import System.File

import Coverage.Exclusions
import Coverage.DumpcasesParser
import Coverage.Core.DumppathsJson
import public Coverage.Core.PathCoverage
import public Coverage.Core.RuntimeHit

%default covering

matchesConfig : ExclusionConfig -> String -> Bool
matchesConfig config name =
     any (\p => isPrefixOf p name) config.modulePrefixes
  || any (\pkg => isPrefixOf (capitalizeFirst pkg ++ ".") name) config.packageNames
  || any (\fn => fn == name) config.functionNames
  where
    capitalizeFirst : String -> String
    capitalizeFirst s = case strM s of
      StrNil => ""
      StrCons c rest => singleton (toUpper c) ++ rest

shouldExcludePath : LoadedExclusions -> ExclusionConfig -> PathObligation -> Bool
shouldExcludePath excl config path =
     shouldExclude excl path.functionName
  || matchesConfig config path.functionName

export
defaultPathExclusions : LoadedExclusions
defaultPathExclusions = MkLoadedExclusions idris2FullExclusions "builtin"

export
filterPathObligations : LoadedExclusions -> ExclusionConfig -> List PathObligation -> List PathObligation
filterPathObligations excl config =
  filter (\path => not (shouldExcludePath excl config path))

export
parseProjectDumppathsJson : LoadedExclusions -> ExclusionConfig -> String -> Either String (List PathObligation)
parseProjectDumppathsJson excl config content = do
  paths <- parseDumppathsJson content
  pure $ filterPathObligations excl config paths

export
loadProjectDumppathsJson : String -> LoadedExclusions -> ExclusionConfig -> IO (Either String (List PathObligation))
loadProjectDumppathsJson path excl config = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ parseProjectDumppathsJson excl config content

export
analyzePathCoverageFromContent : LoadedExclusions
                              -> ExclusionConfig
                              -> String
                              -> List PathRuntimeHit
                              -> Either String PathCoverageResult
analyzePathCoverageFromContent excl config content hits = do
  paths <- parseProjectDumppathsJson excl config content
  pure $ buildPathCoverageResultFromHits paths hits

export
analyzePathCoverageFromFile : String
                           -> LoadedExclusions
                           -> ExclusionConfig
                           -> List PathRuntimeHit
                           -> IO (Either String PathCoverageResult)
analyzePathCoverageFromFile path excl config hits = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ analyzePathCoverageFromContent excl config content hits

export
untestedPathsFromContent : LoadedExclusions
                        -> ExclusionConfig
                        -> String
                        -> List PathRuntimeHit
                        -> Either String (List PathObligation)
untestedPathsFromContent excl config content hits =
  map missingPaths (analyzePathCoverageFromContent excl config content hits)

export
untestedPathsFromFile : String
                     -> LoadedExclusions
                     -> ExclusionConfig
                     -> List PathRuntimeHit
                     -> IO (Either String (List PathObligation))
untestedPathsFromFile path excl config hits = do
  result <- analyzePathCoverageFromFile path excl config hits
  pure $ map missingPaths result
