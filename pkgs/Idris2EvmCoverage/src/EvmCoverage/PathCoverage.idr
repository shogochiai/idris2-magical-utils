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
import Coverage.Core.Backend   -- shared isBareRecordProjectionPath (was duplicated here)
import Coverage.Standardization.Types

%default covering

isAsciiUpper : Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

startsWith : (Char -> Bool) -> String -> Bool
startsWith predicate s =
  case unpack s of
    [] => False
    c :: _ => predicate c

isNonProductEvmFunction : String -> Bool
isNonProductEvmFunction name =
     name == "DumpcasesWrapper.main"
  || isInfixOf ".Tests." name
  || isInfixOf "Test." name
  || isInfixOf ".Storages." name
  || isInfixOf ".Schema" name

||| Standard-library / prelude functions are outside the product coverage model
||| (same policy as core's isStandardLibraryName). Excluding them is legitimate,
||| not silent truncation — they are reported as excluded-with-reason.
isStandardLibraryEvmFunction : String -> Bool
isStandardLibraryEvmFunction name =
  any (\p => isPrefixOf p name)
    [ "Prelude.", "Builtin.", "PrimIO.", "Data.", "System."
    , "Control.", "Decidable.", "Language.", "Debug." ]

isUninstrumentedStraightLinePath : PathObligation -> Bool
isUninstrumentedStraightLinePath path =
     path.terminalKind == "reached_clause"
  && path.steps == []
  && isNothing path.sourceSpanUnion

-- evm's historical generated-projection predicate is the BARE-only form (no
-- dotted), now shared from Coverage.Core.Backend.isBareRecordProjectionPath.
-- Call sites below reference it directly (no local alias, to avoid the
-- ambiguity with Backend.isGeneratedRecordProjectionPath, the web superset).

lastBranchLabel : PathObligation -> Maybe String
lastBranchLabel path =
  case last' path.steps of
    Nothing => Nothing
    Just step => step.branchLabel

||| A terminal whose label is a SINGLE-CONSTRUCTOR product/record destructure is
||| irrefutable: the matched type has exactly one constructor, so the `%case` has
||| exactly one `%concase` arm and emits NO EVM control-flow branch. Such a path
||| is therefore provably a non-branch (CompilerInsertedArtifact), NOT an
||| unobservable obligation. Proof basis (dumpcases): e.g.
|||   accumulateVote = (%case x [(%concase [cons] _builtin.CONS ...)])   -- 1 arm
|||   storeVote      = (%case x [(%concase [cons] _builtin.CONS ...)])   -- 1 arm
||| The detectable labels are the canonical product constructors:
|||   - tuple destructure: "MkPair"
|||   - single-constructor records: "Mk<TypeName>" (the Idris record convention)
|||   - curried-lambda artifact: "->" (a PI step, never a runtime branch)
||| This is conservative: only labels that are structurally single-constructor by
||| Idris's own naming are matched; genuine multi-way sums (Just/Nothing, Ok/Fail,
||| True/False, Left/Right, Nil/(::)) are NOT matched and stay product obligations.
isSingleConstructorDestructureLabel : String -> Bool
isSingleConstructorDestructureLabel raw =
  let label = trim raw in
       label == "MkPair"     -- tuple (always single-constructor)
    || label == "->"          -- curried PI/lambda artifact, not a branch
    || ( startsWith isAsciiUpper label
         && isPrefixOf "Mk" label
         && not (elem label sumTypeConstructors) )
  where
    -- Idris/Prelude multi-constructor data whose ctors happen to start with "Mk"
    -- must NOT be treated as single-constructor. (Currently none of the product
    -- record ctors we exclude collide with these, but list defensively.)
    sumTypeConstructors : List String
    sumTypeConstructors = []

isSingleConstructorDestructurePath : PathObligation -> Bool
isSingleConstructorDestructurePath path =
     path.terminalKind == "reached_clause"
  && maybe False isSingleConstructorDestructureLabel (lastBranchLabel path)

shouldExcludePath : List ExclPattern -> PathObligation -> Bool
shouldExcludePath patterns path =
     isExcluded path.functionName patterns
  || isNonProductEvmFunction path.functionName
  || isBareRecordProjectionPath path
  || isUninstrumentedStraightLinePath path

||| Classify an EVM exclusion into the CANONICAL `ExclusionReason` taxonomy,
||| instead of silently deleting the path from the denominator OR inventing an
||| ad-hoc `(ObligationClass, String)`. This is the anti-"hallucinated perfection"
||| rule: every source path stays in the set with an honest classification (so
||| denominator + excluded + unknown == total source paths). The legitimate
||| categories are enumerated ONCE in Coverage.Standardization.Types.ExclusionReason,
||| so this classifier CANNOT express "exclude an observable product branch" — the
||| type has no constructor for it.
|||   - config/dependency exclusion, non-product (Tests/Storages/Schema/wrapper)
|||       → NonProductModule
|||   - standard library / prelude                  → StandardLibrary
|||   - generated record projection (bare form)     → GeneratedProjection
|||   - single-constructor destructure (irrefutable) → SingleCtorDestructure
|||   - straight-line clause, no branch obligation  → StraightLineClause
||| Returns the reason, or Nothing if the path is a normal product obligation.
|||
||| REMOVED (the narrow-denominator dishonesty the user caught): the former
||| `isKnownYulBranchLabelMismatch` and `isKnownConstantUnreachableEvmPath`
||| exclusions. Both were RELEASED-YUL-ERA observability proxies keyed on a
||| function-name + branch-label heuristic, classifying REAL product branches
||| (Tally/Vote/Members True/False/Fail, Tokenomics transfer/mint outcomes) as
||| compiler artifacts so they vanished from the denominator. Under fork-yul
||| source-level `log1` markers EVERY such branch carries its own marker and IS
||| observable, so those paths return to the denominator as Reachable-or-Missing
||| (honest: covered if driven, Missing if not yet). There is no honest
||| "product branch we cannot observe" category — hence no ctor for it.
export
classifyEvmExclusion : List ExclPattern -> PathObligation -> Maybe Types.ExclusionReason
classifyEvmExclusion patterns path =
  if isExcluded path.functionName patterns
    then Just NonProductModule
  else if isNonProductEvmFunction path.functionName
    then Just NonProductModule
  else if isStandardLibraryEvmFunction path.functionName
    then Just StandardLibrary
  else if isBareRecordProjectionPath path
    then Just GeneratedProjection
  else if isSingleConstructorDestructurePath path
    then Just SingleCtorDestructure
  else if isUninstrumentedStraightLinePath path
    then Just StraightLineClause
  else Nothing

export
defaultPathExclusions : List ExclPattern
defaultPathExclusions = evmFullExclusions

||| Reclassify (NOT delete) excluded paths. Each path stays in the list; an
||| excluded path's `classification` is overridden with the target class so the
||| shared coverage measurement counts it as excluded-with-reason instead of
||| dropping it. IDEMPOTENCY GUARD: only override when the incoming class is
||| ReachableObligation, so a re-parse of already-classified content (Main.idr's
||| observability classifier round-trip) never resets a non-Reachable class back
||| to Reachable.
||| Reclassify excluded paths via the shared `reclassifyByClassifier`, applying the
||| canonical `reasonClass` to the evm classifier's `ExclusionReason`. The `Bool`
||| (formerly `honestObservable`, toggling the dishonest released-yul exclusions) is
||| now a NO-OP: those exclusions were deleted, so honest and narrow modes coincide.
||| The parameter is retained only so the Main.idr CLI surface is unchanged.
export
reclassifyPathObligationsMode : Bool -> List ExclPattern -> List PathObligation -> List PathObligation
reclassifyPathObligationsMode _ patterns =
  reclassifyByClassifier (classifyEvmExclusion patterns)

export
reclassifyPathObligations : List ExclPattern -> List PathObligation -> List PathObligation
reclassifyPathObligations = reclassifyPathObligationsMode False

||| Deprecated delete-filter; kept for callers that explicitly want the pruned
||| list. The coverage path uses reclassifyPathObligations (no silent deletion).
export
filterPathObligations : List ExclPattern -> List PathObligation -> List PathObligation
filterPathObligations patterns =
  filter (\path => not (shouldExcludePath patterns path))

export
parseProjectDumppathsJsonMode : Bool -> List ExclPattern -> String -> Either String (List PathObligation)
parseProjectDumppathsJsonMode honestObservable patterns content = do
  paths <- parseDumppathsJson content
  pure $ reclassifyPathObligationsMode honestObservable patterns paths

export
parseProjectDumppathsJson : List ExclPattern -> String -> Either String (List PathObligation)
parseProjectDumppathsJson = parseProjectDumppathsJsonMode False

export
loadProjectDumppathsJson : String -> List ExclPattern -> IO (Either String (List PathObligation))
loadProjectDumppathsJson path patterns = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
  pure $ parseProjectDumppathsJson patterns content

export
analyzePathCoverageFromContentMode : Bool
                                  -> List ExclPattern
                                  -> String
                                  -> List PathRuntimeHit
                                  -> Either String PathCoverageResult
analyzePathCoverageFromContentMode honestObservable patterns content hits = do
  paths <- parseProjectDumppathsJsonMode honestObservable patterns content
  pure $ buildPathCoverageResultFromHits paths hits

export
analyzePathCoverageFromContent : List ExclPattern
                              -> String
                              -> List PathRuntimeHit
                              -> Either String PathCoverageResult
analyzePathCoverageFromContent = analyzePathCoverageFromContentMode False

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
