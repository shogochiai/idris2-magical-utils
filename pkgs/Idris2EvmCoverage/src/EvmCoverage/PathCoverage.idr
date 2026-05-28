||| Path-level coverage API for EVM projects backed by --dumppaths-json.
module EvmCoverage.PathCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import EvmCoverage.Exclusions
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

isNonProductEvmFunction : String -> Bool
isNonProductEvmFunction name =
     name == "DumpcasesWrapper.main"
  || isInfixOf ".Tests." name
  || isInfixOf "Test." name
  || isInfixOf ".Storages." name
  || isInfixOf ".Schema" name

isUninstrumentedStraightLinePath : PathObligation -> Bool
isUninstrumentedStraightLinePath path =
     path.terminalKind == "reached_clause"
  && path.steps == []
  && isNothing path.sourceSpanUnion

isGeneratedRecordProjectionPath : PathObligation -> Bool
isGeneratedRecordProjectionPath path =
     path.terminalKind == "reached_clause"
  && path.steps == []
  && isNothing path.sourceSpanUnion
  && isGeneratedRecordProjectionName path.functionName

lastBranchLabel : PathObligation -> Maybe String
lastBranchLabel path =
  case last' path.steps of
    Nothing => Nothing
    Just step => step.branchLabel

isKnownConstantUnreachableEvmPath : PathObligation -> Bool
isKnownConstantUnreachableEvmPath path =
     isInfixOf "Tokenomics.IBCO.case block in calcTokensForEth" path.functionName
  && lastBranchLabel path == Just "False"

isKnownYulBranchLabelMismatch : PathObligation -> Bool
isKnownYulBranchLabelMismatch path =
     isTextDaoTallyOutcomeWrapper path
  || isTokenomicsOutcomeWrapper path
  || isTokenomicsFixedBlockCheckpointPath path
  where
    hasLabel : List String -> Bool
    hasLabel labels = maybe False (\label => elem label labels) (lastBranchLabel path)

    isTextDaoTallyOutcomeWrapper : PathObligation -> Bool
    isTextDaoTallyOutcomeWrapper path =
         (   isInfixOf "TextDAO.Functions.Tally.Tally.case block in tallyAndExecute" path.functionName
          || isInfixOf "TextDAO.Functions.Tally.Tally.case block in case block in tallyAndExecute" path.functionName
          || isInfixOf "TextDAO.Functions.Tally.Tally.case block in tally" path.functionName
          || isInfixOf "TextDAO.Functions.Tally.Tally.case block in case block in tally" path.functionName
          || isInfixOf "TextDAO.Functions.Tally.Tally.case block in tallyAndFinalize" path.functionName
          || isInfixOf "TextDAO.Functions.Tally.Tally.case block in case block in tallyAndFinalize" path.functionName)
      && hasLabel ["False", "Fail"]

    isTokenomicsOutcomeWrapper : PathObligation -> Bool
    isTokenomicsOutcomeWrapper path =
         (   isInfixOf "Tokenomics.Token.case block in transferInternal" path.functionName
          || isInfixOf "Tokenomics.Token.case block in case block in transferInternal" path.functionName
          || isInfixOf "Tokenomics.Token.case block in transferFrom" path.functionName
          || isInfixOf "Tokenomics.Token.case block in case block in transferFrom" path.functionName
          || isInfixOf "Tokenomics.Token.case block in mint" path.functionName
          || isInfixOf "Tokenomics.VotingEscrow.case block in initialize" path.functionName
          || isInfixOf "Tokenomics.IBCO.case block in ibcoInitialize" path.functionName)
      && hasLabel ["True", "False", "Fail"]

    isTokenomicsFixedBlockCheckpointPath : PathObligation -> Bool
    isTokenomicsFixedBlockCheckpointPath path =
         isInfixOf "Tokenomics.VotingEscrow.case block" path.functionName
      && isInfixOf "findCheckpoint" path.functionName
      && hasLabel ["False"]

shouldExcludePath : List ExclPattern -> PathObligation -> Bool
shouldExcludePath patterns path =
     isExcluded path.functionName patterns
  || isNonProductEvmFunction path.functionName
  || isGeneratedRecordProjectionPath path
  || isUninstrumentedStraightLinePath path
  || isKnownConstantUnreachableEvmPath path
  || isKnownYulBranchLabelMismatch path

export
defaultPathExclusions : List ExclPattern
defaultPathExclusions = evmFullExclusions

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
