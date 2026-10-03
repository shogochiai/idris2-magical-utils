||| Path-level coverage API for DFX/ICP projects backed by --dumppaths-json.
module DfxCoverage.PathCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import Data.SortedSet
import Coverage.Core.OrdNub
import Coverage.Standardization.Types
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

||| Why one path is out of the denominator, from the dumppaths obligation as the
||| compiler classified it (BEFORE reclassification). A path the compiler already
||| classified as non-reachable keeps the compiler's class name. Otherwise the
||| reason is what reclassifyArtifactsBare applied, in its order: a bare generated
||| record projection first, then the first matching exclusion pattern's reason.
||| Nothing for a path that stays in the denominator.
export
exclusionReasonOf : List ExclPattern -> PathObligation -> Maybe String
exclusionReasonOf patterns p =
  case p.classification of
    ReachableObligation =>
      if isBareRecordProjectionPath p
        then Just "generated record projection (bare)"
        else isMethodExcluded patterns p.functionName
    LogicallyUnreachable => Just "LogicallyUnreachable (compiler)"
    CompilerInsertedArtifact => Just "CompilerInsertedArtifact (compiler)"
    ExternalEffectBoundary => Just "ExternalEffectBoundary (compiler)"
    _ => Nothing

||| The excluded paths split by reason: (reason, paths, of which observed), most
||| paths first, ties by reason. `observed` counts the excluded paths whose id is
||| in the run's hits, so the split shows which reasons hold the
||| observed_outside_denominator paths that raise the rate.
export
excludedByReason : List ExclPattern -> List PathObligation -> List String
                -> List (String, Nat, Nat)
excludedByReason patterns rawPaths hitIds =
  let hitSet = SortedSet.fromList hitIds
      reasons = mapMaybe (\p => map (\r => (r, SortedSet.contains p.pathId hitSet))
                                    (exclusionReasonOf patterns p))
                         (nubOrdOn (.pathId) rawPaths)
      bump : SortedMap String (Nat, Nat) -> (String, Bool) -> SortedMap String (Nat, Nat)
      bump m (r, hit) =
        let (c, o) = fromMaybe (0, 0) (SortedMap.lookup r m)
        in SortedMap.insert r (S c, if hit then S o else o) m
      rows = map (\(n, (c, o)) => (n, c, o)) (SortedMap.toList (foldl bump SortedMap.empty reasons))
  in sortBy (\(n1, c1, _), (n2, c2, _) => case compare c2 c1 of
                                             EQ => compare n1 n2
                                             o  => o) rows

||| The split as text lines, placed next to the raw buckets. The lines start with
||| "excluded_by_reason", never with "paths_", so no bucket parser reads them.
export
renderExcludedByReason : List (String, Nat, Nat) -> String
renderExcludedByReason rows =
  unlines $
    ("excluded_by_reason: " ++ show (length rows) ++ " reason(s), "
       ++ show (sum (map (\(_, c, _) => c) rows)) ++ " path(s)")
    :: map (\(n, c, o) => "  " ++ show c ++ " (observed " ++ show o ++ ")  " ++ n) rows

||| excludedByReason over the dumppaths content as the compiler wrote it.
export
excludedByReasonFromContent : List ExclPattern -> String -> List PathRuntimeHit
                           -> Either String (List (String, Nat, Nat))
excludedByReasonFromContent patterns content hits = do
  raw <- parseDumppathsJson content
  pure $ excludedByReason patterns raw (map (.pathId) hits)

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
