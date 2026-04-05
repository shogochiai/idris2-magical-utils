||| Parser for a future Idris2 structured case-tree export.
||| This lets EVM coverage consume compiler-provided branch provenance
||| without changing downstream reporting or LazyEvm APIs.
module EvmCoverage.StructuredExport

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import EvmCoverage.Types
import Language.JSON

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
getNat (JNumber n) =
  if n < 0.0 then Nothing else Just (cast n)
getNat (JString s) = parseInteger {a = Integer} s >>= natFromInteger
  where
    natFromInteger : Integer -> Maybe Nat
    natFromInteger i = if i < 0 then Nothing else Just (cast i)
getNat _ = Nothing

getStringField : String -> JSON -> Maybe String
getStringField key json = getField key json >>= getString

getNatField : String -> JSON -> Maybe Nat
getNatField key json = getField key json >>= getNat

splitQualifiedName : String -> (String, String)
splitQualifiedName functionName =
  let parts = forget $ split (== '.') functionName
  in case reverse parts of
       [] => ("", functionName)
       (fn :: []) => ("", fn)
       (fn :: revMods) => (fastConcat $ intersperse "." $ reverse revMods, fn)

isProjectFunction : String -> Bool
isProjectFunction name =
  let n = case strUncons name of
            Just ('.', rest) => rest
            _ => name
  in not (isExcluded n)
  where
    isCompilerGenerated : String -> Bool
    isCompilerGenerated s =
      isInfixOf "{csegen:" s ||
      isInfixOf "{eta:" s ||
      isInfixOf "{arg:" s ||
      isInfixOf "{lazyArg:" s ||
      isInfixOf "{conArg:" s ||
      isInfixOf "{case" s

    isStdLib : String -> Bool
    isStdLib s =
      isPrefixOf "Prelude." s ||
      isPrefixOf "Data." s ||
      isPrefixOf "System." s ||
      isPrefixOf "Control." s ||
      isPrefixOf "Decidable." s ||
      isPrefixOf "Language." s ||
      isPrefixOf "Debug." s ||
      isPrefixOf "PrimIO." s ||
      isPrefixOf "_builtin." s ||
      isPrefixOf "Builtin." s

    isPrimitive : String -> Bool
    isPrimitive s = isPrefixOf "prim__" s

    isTestModule : String -> Bool
    isTestModule s = isInfixOf ".Tests." s || isInfixOf "Test." s

    isSchemaModule : String -> Bool
    isSchemaModule s = isInfixOf ".Storages." s || isInfixOf ".Schema" s

    isExcluded : String -> Bool
    isExcluded s =
      isCompilerGenerated s ||
      isStdLib s ||
      isPrimitive s ||
      isTestModule s ||
      isSchemaModule s

isNonEmptyStatus : Maybe String -> Bool
isNonEmptyStatus Nothing = False
isNonEmptyStatus (Just s) =
  let trimmed = trim s
  in trimmed /= "" && trimmed /= "none" && trimmed /= "reachable" && trimmed /= "complete"

classifyStructuredNode : JSON -> BranchClass
classifyStructuredNode json =
  case getStringField "origin" json of
    Just "user_clause" => BCCanonical
    Just "impossible_clause" => BCExcludedNoClauses
    Just "compiler_partial_completion" => BCBugUnhandledInput
    Just "no_clause_body" => BCExcludedNoClauses
    Just "optimizer_artifact" => BCOptimizerNat
    Just "compiler_generated_helper" => BCCompilerGenerated
    Just "unknown" => BCUnknownCrash "structured:unknown"
    Just other => BCUnknownCrash ("structured:unknown-origin:" ++ other)
    Nothing =>
      let impossibleStatus = getStringField "impossible_status" json
          partialStatus = getStringField "partial_status" json
          artifactStatus = getStringField "backend_artifact_status" json
      in if isNonEmptyStatus artifactStatus then BCOptimizerNat
         else if isNonEmptyStatus partialStatus then BCBugUnhandledInput
         else if isNonEmptyStatus impossibleStatus then BCExcludedNoClauses
         else BCUnknownCrash "structured:missing-origin"

describeStructuredNode : JSON -> String
describeStructuredNode json =
  let label = fromMaybe "branch" (getStringField "branch_label" json)
      span = fromMaybe "" (getStringField "source_span" json)
      origin = fromMaybe "unknown" (getStringField "origin" json)
      suffix = if span == "" then "" else " @ " ++ span
  in label ++ suffix ++ " [" ++ origin ++ "]"

parseNode : String -> Nat -> JSON -> Either String ClassifiedBranch
parseNode functionName fallbackIdx json = do
  nodeId <- maybeToEither "structured export node is missing node_id"
            (getStringField "node_id" json)
  let branchIdx = fromMaybe fallbackIdx (getNatField "branch_index" json)
  let (moduleName, funcName) = splitQualifiedName functionName
  let branchId = MkBranchId (Just nodeId) moduleName funcName branchIdx
  pure $ MkClassifiedBranch branchId (classifyStructuredNode json) (describeStructuredNode json)

parseNodes : String -> Nat -> List JSON -> Either String (List ClassifiedBranch)
parseNodes _ _ [] = Right []
parseNodes functionName idx (node :: rest) = do
  parsed <- parseNode functionName idx node
  others <- parseNodes functionName (S idx) rest
  pure (parsed :: others)

parseFunctionObject : JSON -> Either String (List ClassifiedBranch)
parseFunctionObject json = do
  functionName <- maybeToEither "structured export function is missing function_name"
                  (getStringField "function_name" json)
  nodesJson <- maybeToEither "structured export function is missing nodes"
               (getField "nodes" json >>= getArray)
  if isProjectFunction functionName
    then parseNodes functionName 0 nodesJson
    else Right []

parseFunctionObjects : List JSON -> Either String (List ClassifiedBranch)
parseFunctionObjects [] = Right []
parseFunctionObjects (fn :: rest) = do
  here <- parseFunctionObject fn
  there <- parseFunctionObjects rest
  pure (here ++ there)

export
looksLikeStructuredExport : String -> Bool
looksLikeStructuredExport content =
  case parse content of
    Just json => isJust (getField "functions" json >>= getArray)
    Nothing => False

export
parseStructuredExport : String -> Either String (List ClassifiedBranch)
parseStructuredExport content =
  case parse content of
    Nothing => Left "Failed to parse structured case-tree JSON"
    Just json => do
      functions <- maybeToEither "structured export is missing functions array"
                    (getField "functions" json >>= getArray)
      parseFunctionObjects functions
