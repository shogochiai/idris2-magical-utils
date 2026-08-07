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

||| The compiler names a case block inside function `f` as
||| `<Module>.case block in [case block in …] f`. Excluding `f` should also exclude
||| those nested-case obligations, otherwise listing an IO handler in `functions`
||| catches only its top-level path and leaves every `case block in <handler>`
||| obligation in the denominator. Strip the leading "case block in " segments from
||| the local part so the reconstructed `<Module>.<f>` can be compared to the
||| excluded name. (A bare function with no case block is unchanged.)
stripCaseBlockPrefixes : String -> String
stripCaseBlockPrefixes name =
  case break (== '.') (reverse name) of
    (revLocal, revModDot) =>
      let local = reverse revLocal
          modPart = reverse revModDot   -- includes the trailing '.'
          marker = "case block in "
          go : String -> String
          go s = if isPrefixOf marker s then go (substr (length marker) (length s) s) else s
      in modPart ++ go local

matchesConfig : ExclusionConfig -> String -> Bool
matchesConfig config name =
     any (\p => isPrefixOf p name) config.modulePrefixes
  || any (\pkg => isPrefixOf (capitalizeFirst pkg ++ ".") name) config.packageNames
  || any (\fn => fn == name) config.functionNames
  -- also exclude case blocks belonging to an excluded function
  || any (\fn => fn == stripCaseBlockPrefixes name) config.functionNames
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

||| Which patterns dropped obligations here, and how many each dropped.
|||
||| THIS IS NOT `paths_excluded`. Stated first because I had it wrong on
||| 2026-08-08 and alice caught it: `paths_excluded` partitions the ENUMERATED
||| paths by `ObligationClass` (LogicallyUnreachable / CompilerInsertedArtifact /
||| ExternalEffectBoundary), a classification made downstream. `filterPathObligations`
||| is a different mechanism entirely — it drops obligations by NAME before they
||| are ever enumerated.
|||
||| WHY IT MATTERS ANYWAY, and why it is the sharper of the two. Because the
||| filter runs upstream of enumeration, what it removes never enters
||| `paths_total`. The conservation law a consumer checks —
||| `total = denominator + excluded + unknown` — therefore holds perfectly over a
||| universe that was ALREADY TRIMMED by an unreported amount. All five buckets
||| conserve; none of them can see the trim. Recomputing from the buckets, which
||| is the discipline that catches a moving denominator, cannot catch this one.
|||
||| Attribution uses `isMethodExcluded`, which returns the FIRST matching
||| pattern's reason — the same first-match semantics `shouldExcludePath` uses,
||| so every count names the pattern that actually did the dropping rather than
||| every pattern that could have. The counts partition: their sum is exactly
||| `length paths - length (filterPathObligations excl config paths)`.
export
exclusionBreakdown : LoadedExclusions -> ExclusionConfig -> List PathObligation -> List (String, Nat)
exclusionBreakdown excl config paths =
  let reasons = mapMaybe reasonFor paths
  in map (\k => (k, length (filter (== k) reasons))) (nub reasons)
  where
    reasonFor : PathObligation -> Maybe String
    reasonFor path =
      case isMethodExcluded excl.patterns path.functionName of
        Just r  => Just r
        Nothing => if matchesConfig config path.functionName
                     then Just "exclusion config (package/module/function list)"
                     else Nothing

||| Render the breakdown as one line per pattern, largest first, for printing
||| next to the bucket counts. Empty when nothing was dropped — a run that
||| removed nothing should say nothing, not print an empty heading.
|||
||| The heading says "before enumeration" because these obligations are absent
||| from `paths_total`, not sorted into `paths_excluded`. A reader who conflates
||| the two concludes the buckets already account for them; they do not.
export
renderExclusionBreakdown : List (String, Nat) -> List String
renderExclusionBreakdown [] = []
renderExclusionBreakdown bd =
  let dropped = sum (map snd bd)
      ordered = sortBy (\a, b => compare (snd b) (snd a)) bd
  in ("    dropped " ++ show dropped ++ " obligations by name BEFORE enumeration "
      ++ "(absent from paths_total; NOT paths_excluded), by pattern:")
     :: map (\(reason, n) => "      " ++ padTo 6 (show n) ++ "  " ++ reason) ordered
  where
    padTo : Nat -> String -> String
    padTo w s = let l = length s in
                if l >= w then s else pack (replicate (minus w l) ' ') ++ s

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
