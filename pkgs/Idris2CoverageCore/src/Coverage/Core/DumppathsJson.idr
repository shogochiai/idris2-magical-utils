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
parseClassification "ExternalEffectBoundary" = ExternalEffectBoundary
parseClassification "StubbedReach" = StubbedReach
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

||| Parse the compiler-emitted effect_boundary tag. Unknown/absent → PureComputation
||| (the safe default: a path with no known boundary stays in the denominator).
parseEffectBoundary : Maybe String -> EffectBoundary
parseEffectBoundary (Just "ProcessSpawn")   = ProcessSpawn
parseEffectBoundary (Just "NetworkOutcall") = NetworkOutcall
parseEffectBoundary (Just "CanisterCall")   = CanisterCall
parseEffectBoundary (Just "FileSystemIO")   = FileSystemIO
parseEffectBoundary (Just s) =
  -- "UnclassifiedForeign(<cc>)" — an FFI hole the compiler could not classify to a
  -- precise primitive. Captured (excludable, but Unknown-classified = visible) so
  -- an unrecognised external call is never mistaken for pure code.
  if isPrefixOf "UnclassifiedForeign(" s
     then UnclassifiedForeign (substr 20 (minus (length s) 21) s)  -- strip prefix + trailing ')'
     else PureComputation
parseEffectBoundary Nothing = PureComputation

||| Per-family refinement of a compiler-emitted boundary. The compiler names the
||| hole (`UnclassifiedForeign(<cc>)`) but cannot know which harness will drive
||| the paths; whether that cc is a hole AT ALL is the family's fact
||| (Coverage.Boundary.Canonical: an EVM opcode prim is executed by revm on every
||| run, while the same shape of cc on Chez is a real hole). A matching
||| excludable=True row resolves to the row's recognised boundary (non-blocking
||| exclusion); an excludable=False row resolves to PureComputation — the
||| family's runner executes the prim, so the path stays a ReachableObligation.
||| A cc no row matches stays UnclassifiedForeign (claim-blocking Unknown),
||| preserving the soundness guarantee for genuinely new holes.
export
refineBoundaryWithSpecs : List EffectBoundarySpec -> EffectBoundary -> EffectBoundary
refineBoundaryWithSpecs specs (UnclassifiedForeign cc) =
  case find (\s => any (\sub => isInfixOf sub cc) s.ccSubstrings) specs of
    Nothing  => UnclassifiedForeign cc
    Just row => if row.excludable then row.boundary else PureComputation
refineBoundaryWithSpecs _ boundary = boundary

||| Fact-grounded reclassification: a path whose function transitively reaches an
||| unexecutable FFI hole (compiler-computed effect_boundary) is reclassified to
||| boundaryClass (UnknownClassification — visible, claim-affecting) instead of
||| being counted as a reachable obligation the harness could verify. Only a
||| ReachableObligation is overridden (idempotent; never touches Unknown/artifact).
||| The boundary is a COMPILER FACT, never an observer judgment.
applyBoundary : EffectBoundary -> ObligationClass -> ObligationClass
applyBoundary boundary cls =
  case (cls, boundaryExcludable boundary) of
    (ReachableObligation, True) => boundaryClass boundary
    _                           => cls

parsePath : String -> EffectBoundary -> JSON -> Either String PathObligation
parsePath functionName boundary json = do
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
    (applyBoundary boundary (parseClassification classificationRaw))
    terminalKind
    (getNatField "terminal_clause_id" json)
    steps
    (getStringField "source_span_union" json)
    (fromMaybe (length steps) (getNatField "path_length" json))

parsePaths : String -> EffectBoundary -> List JSON -> Either String (List PathObligation)
parsePaths _ _ [] = Right []
parsePaths functionName boundary (path :: rest) = do
  here <- parsePath functionName boundary path
  there <- parsePaths functionName boundary rest
  pure (here :: there)

parseFunctionObject : List EffectBoundarySpec -> JSON -> Either String (List PathObligation)
parseFunctionObject specs json = do
  functionName <- maybeToEither "function object is missing function_name" (getStringField "function_name" json)
  let boundary = refineBoundaryWithSpecs specs
                   (parseEffectBoundary (getStringField "effect_boundary" json))
  pathsJson <- maybeToEither "function object is missing paths" (getField "paths" json >>= getArray)
  parsePaths functionName boundary pathsJson

parseFunctionObjects : List EffectBoundarySpec -> List JSON -> Either String (List PathObligation)
parseFunctionObjects _ [] = Right []
parseFunctionObjects specs (fn :: rest) = do
  here <- parseFunctionObject specs fn
  there <- parseFunctionObjects specs rest
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
parseDumppathsJsonWithSpecs : List EffectBoundarySpec -> String -> Either String (List PathObligation)
parseDumppathsJsonWithSpecs specs content =
  case parse content of
    Nothing => Left "Failed to parse dumppaths JSON"
    Just json => do
      functions <- maybeToEither "dumppaths export is missing functions array"
                    (getField "functions" json >>= getArray)
      parseFunctionObjects specs functions

||| Family-agnostic parse: no boundary specs, so every UnclassifiedForeign hole
||| stays claim-blocking. Consumers that know their harness pass
||| `boundarySpecsFor <family>` to parseDumppathsJsonWithSpecs instead.
export
parseDumppathsJson : String -> Either String (List PathObligation)
parseDumppathsJson = parseDumppathsJsonWithSpecs []

export
loadDumppathsJson : String -> IO (Either String (List PathObligation))
loadDumppathsJson path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ parseDumppathsJson content
