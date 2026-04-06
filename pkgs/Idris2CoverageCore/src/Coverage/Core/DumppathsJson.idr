||| Parser for Idris2 --dumppaths-json structured export.
module Coverage.Core.DumppathsJson

import Data.List
import Data.Maybe
import Data.String
import Language.JSON
import System.File

import Coverage.Core.PathCoverage
import Coverage.Standardization.Types

%default covering

maybeToEither : String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

getField : String -> JSON -> Maybe JSON
getField key (JObject fields) = lookup key fields
getField _ _ = Nothing

getString : JSON -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getArray : JSON -> Maybe (List JSON)
getArray (JArray xs) = Just xs
getArray _ = Nothing

getNat : JSON -> Maybe Nat
getNat (JNumber n) = if n < 0.0 then Nothing else Just (cast n)
getNat (JString s) =
  parseInteger {a = Integer} s >>= \i =>
    if i < 0 then Nothing else Just (cast i)
getNat _ = Nothing

getStringField : String -> JSON -> Maybe String
getStringField key json = getField key json >>= getString

getNatField : String -> JSON -> Maybe Nat
getNatField key json = getField key json >>= getNat

parseClassification : String -> ObligationClass
parseClassification "ReachableObligation" = ReachableObligation
parseClassification "LogicallyUnreachable" = LogicallyUnreachable
parseClassification "UserAdmittedPartialGap" = UserAdmittedPartialGap
parseClassification "CompilerInsertedArtifact" = CompilerInsertedArtifact
parseClassification _ = UnknownClassification

parseStep : JSON -> Either String PathStep
parseStep json = do
  nodeId <- maybeToEither "path step is missing node_id" (getStringField "node_id" json)
  branchIndex <- maybeToEither "path step is missing branch_index" (getNatField "branch_index" json)
  origin <- maybeToEither "path step is missing origin" (getStringField "origin" json)
  pure $ MkPathStep
    nodeId
    branchIndex
    origin
    (getNatField "case_index" json)
    (getStringField "branch_label" json)
    (getStringField "source_span" json)

parseSteps : List JSON -> Either String (List PathStep)
parseSteps [] = Right []
parseSteps (step :: rest) = do
  here <- parseStep step
  there <- parseSteps rest
  pure (here :: there)

parsePath : String -> JSON -> Either String PathObligation
parsePath functionName json = do
  pathId <- maybeToEither "path object is missing path_id" (getStringField "path_id" json)
  classificationRaw <- maybeToEither "path object is missing classification" (getStringField "classification" json)
  terminalKind <- maybeToEither "path object is missing terminal_kind" (getStringField "terminal_kind" json)
  stepsJson <- maybeToEither "path object is missing steps" (getField "steps" json >>= getArray)
  let (moduleName, _) = parseQualifiedFunction functionName
  steps <- parseSteps stepsJson
  pure $ MkPathObligation
    pathId
    functionName
    moduleName
    (parseClassification classificationRaw)
    terminalKind
    (getNatField "terminal_clause_id" json)
    steps
    (getStringField "source_span_union" json)
    (fromMaybe (length steps) (getNatField "path_length" json))

parsePaths : String -> List JSON -> Either String (List PathObligation)
parsePaths _ [] = Right []
parsePaths functionName (path :: rest) = do
  here <- parsePath functionName path
  there <- parsePaths functionName rest
  pure (here :: there)

parseFunctionObject : JSON -> Either String (List PathObligation)
parseFunctionObject json = do
  functionName <- maybeToEither "function object is missing function_name" (getStringField "function_name" json)
  pathsJson <- maybeToEither "function object is missing paths" (getField "paths" json >>= getArray)
  parsePaths functionName pathsJson

parseFunctionObjects : List JSON -> Either String (List PathObligation)
parseFunctionObjects [] = Right []
parseFunctionObjects (fn :: rest) = do
  here <- parseFunctionObject fn
  there <- parseFunctionObjects rest
  pure (here ++ there)

export
looksLikeDumppathsJson : String -> Bool
looksLikeDumppathsJson content =
  case parse content of
    Just json => case (getField "export_kind" json >>= getString, getField "functions" json >>= getArray) of
                   (Just "canonical_intrafunction_paths", Just _) => True
                   _ => False
    Nothing => False

export
parseDumppathsJson : String -> Either String (List PathObligation)
parseDumppathsJson content =
  case parse content of
    Nothing => Left "Failed to parse dumppaths JSON"
    Just json => do
      functions <- maybeToEither "dumppaths export is missing functions array"
                    (getField "functions" json >>= getArray)
      parseFunctionObjects functions

export
loadDumppathsJson : String -> IO (Either String (List PathObligation))
loadDumppathsJson path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ parseDumppathsJson content
