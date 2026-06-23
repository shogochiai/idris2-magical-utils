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
  || isBareRecordProjectionPath path
  || isUninstrumentedStraightLinePath path
  || isKnownConstantUnreachableEvmPath path
  || isKnownYulBranchLabelMismatch path

||| Classify an EVM exclusion into the shared ObligationClass taxonomy WITH a
||| recorded reason, instead of silently deleting the path from the denominator.
||| This is the anti-"hallucinated perfection" rule: every source path stays in
||| the set with an honest classification (so denominator + excluded + unknown
||| == total source paths), rather than vanishing and inflating coverage %.
|||   - non-product (test/schema/storage), generated record projection,
|||     uninstrumented straight-line, known Yul branch-label collapse
|||       → CompilerInsertedArtifact (genuinely outside the product coverage model)
|||   - known constant-false guard → LogicallyUnreachable
||| Returns the target class + a human reason, or Nothing if the path is a
||| normal product obligation (left as-is).
|||
||| `honestObservable`: when True, the source is being measured with the FORK-yul
||| source-level instrumentation (EVM_PATHCOV_YUL), where EVERY source CaseTree
||| leaf carries its own `log1(0,0,FNV(path-id))` marker and is therefore directly
||| observable from the revm trace. In that mode the two exclusions that exist
||| ONLY to hide branches the released-yul bytecode could not observe are INVALID
||| and must be suppressed, otherwise they shrink the denominator dishonestly
||| (the narrow-denominator artifact the user caught):
|||   - `isKnownYulBranchLabelMismatch` — Tally/Tokenomics True/False/Fail product
|||     branches; with markers present these ARE observable, so they stay product
|||     obligations to be driven by calldata.
|||   - `isKnownConstantUnreachableEvmPath` — a constant-false guard claim; under
|||     honest measurement a "constant-false" branch must be PROVEN dead by a
|||     written exclusion, not assumed; keep it in the denominator.
||| The genuinely-non-product / genuinely-non-branch exclusions (Tests/Storages/
||| Schema/stdlib, generated record projection, single-constructor destructure,
||| straight-line no-branch) remain in BOTH modes — they are honest.
export
classifyEvmExclusionMode : Bool -> List ExclPattern -> PathObligation -> Maybe (ObligationClass, String)
classifyEvmExclusionMode honestObservable patterns path =
  if isExcluded path.functionName patterns
    then Just (CompilerInsertedArtifact, "config/dependency exclusion")
  else if isNonProductEvmFunction path.functionName
    then Just (CompilerInsertedArtifact, "non-product function (test/schema/storage/wrapper)")
  else if isStandardLibraryEvmFunction path.functionName
    then Just (CompilerInsertedArtifact, "standard library / prelude")
  else if not honestObservable && isKnownConstantUnreachableEvmPath path
    then Just (LogicallyUnreachable, "constant-false guard (logically unreachable)")
  else if isBareRecordProjectionPath path
    then Just (CompilerInsertedArtifact, "generated record projection")
  else if not honestObservable && isKnownYulBranchLabelMismatch path
    then Just (CompilerInsertedArtifact, "Yul branch-label collapse (compiler artifact)")
  else if isSingleConstructorDestructurePath path
    then Just (CompilerInsertedArtifact, "single-constructor destructure (irrefutable, not a branch)")
  else if isUninstrumentedStraightLinePath path
    then Just (CompilerInsertedArtifact, "straight-line clause, no branch obligation")
  else Nothing

||| Backward-compatible alias: the narrow (released-yul) classification, which
||| keeps the bytecode-unobservable exclusions. Callers that measure with the
||| fork-yul instrumentation should use `classifyEvmExclusionMode True`.
export
classifyEvmExclusion : List ExclPattern -> PathObligation -> Maybe (ObligationClass, String)
classifyEvmExclusion = classifyEvmExclusionMode False

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
export
reclassifyPathObligationsMode : Bool -> List ExclPattern -> List PathObligation -> List PathObligation
reclassifyPathObligationsMode honestObservable patterns = map reclassify
  where
    reclassify : PathObligation -> PathObligation
    reclassify path =
      case path.classification of
        ReachableObligation =>
          case classifyEvmExclusionMode honestObservable patterns path of
            Just (cls, _) => { classification := cls } path
            Nothing       => path
        _ => path

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
