||| Automatic Dependency Exclusion for Coverage Analysis
|||
||| Automatically excludes modules from dependency packages so that
||| coverage analysis focuses on the project's own code.
module Coverage.Core.DependencyExclusion

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import Coverage.Core.Exclusions

%default covering

-- =============================================================================
-- Dependency Info
-- =============================================================================

||| Information about a single dependency
public export
record DependencyInfo where
  constructor MkDependencyInfo
  name : String
  modulePrefixes : List String

public export
Show DependencyInfo where
  show d = d.name ++ " -> " ++ show d.modulePrefixes

-- =============================================================================
-- Known Dependency Mappings
-- =============================================================================

||| Known dependency -> module prefix mappings
public export
knownDependencyPrefixes : List (String, List String)
knownDependencyPrefixes =
  [ ("idris2-subcontract", ["Subcontract."])
  , ("idris2-yul", ["EVM.", "Compiler.", "Yul."])
  , ("idris2-evm", ["EVM."])
  , ("idris2-evm-coverage", ["EvmCoverage."])
  , ("base", ["Prelude.", "Data.", "Control.", "System."])
  , ("contrib", ["Data.", "Text.", "Syntax."])
  , ("network", ["Network."])
  , ("linear", ["Linear."])
  ]

||| Look up module prefixes for a dependency
public export
lookupDependencyPrefixes : String -> Maybe (List String)
lookupDependencyPrefixes depName =
  map snd (find (\p => fst p == depName) knownDependencyPrefixes)

-- =============================================================================
-- Ipkg Parsing
-- =============================================================================

||| Helper to extract deps from a line
extractDepsFromLine : String -> List String
extractDepsFromLine line =
  case break (== '=') (unpack line) of
    (_, afterEq) =>
      let deps = pack (drop 1 afterEq)
          parts = forget (split (== ',') deps)  -- List1 -> List
      in map trim (filter (not . null . trim) parts)

||| Extract dependency names from ipkg content
public export
parseDependsLine : String -> List String
parseDependsLine content =
  let ls = lines content
      dependsLines = filter (isInfixOf "depends") ls
  in concatMap extractDepsFromLine dependsLines

||| Parse ipkg file and extract dependencies
public export
parseIpkgDependencies : String -> IO (List String)
parseIpkgDependencies ipkgPath = do
  Right content <- readFile ipkgPath
    | Left _ => pure []
  pure (parseDependsLine content)

-- =============================================================================
-- Resolution
-- =============================================================================

||| Resolve a single dependency
resolveSingleDep : String -> Maybe DependencyInfo
resolveSingleDep dep =
  case lookupDependencyPrefixes dep of
    Just prefixes => Just (MkDependencyInfo dep prefixes)
    Nothing => Nothing

||| Resolve module prefixes for a list of dependencies
public export
resolveDependencyPrefixes : List String -> List DependencyInfo
resolveDependencyPrefixes deps = mapMaybe resolveSingleDep deps

-- =============================================================================
-- Exclusion Pattern Generation
-- =============================================================================

||| Make exclusion pattern for a dependency prefix
public export
mkDepExclusion : String -> String -> ExclPattern
mkDepExclusion depName modulePrefix =
  MkExclPattern modulePrefix Prefix ("dependency:" ++ depName)

||| Generate exclusion patterns from dependency info
public export
dependencyToExclusions : DependencyInfo -> List ExclPattern
dependencyToExclusions dep =
  map (mkDepExclusion dep.name) dep.modulePrefixes

||| Generate all exclusion patterns from dependency list
public export
dependenciesToExclusions : List DependencyInfo -> List ExclPattern
dependenciesToExclusions deps = concatMap dependencyToExclusions deps

-- =============================================================================
-- High-Level API
-- =============================================================================

||| Load dependency exclusions from ipkg file
public export
loadDependencyExclusions : String -> IO (List ExclPattern)
loadDependencyExclusions ipkgPath = do
  deps <- parseIpkgDependencies ipkgPath
  let resolved = resolveDependencyPrefixes deps
  pure (dependenciesToExclusions resolved)

||| Combine default + dependency exclusions
public export
loadAllExclusions : String -> IO (List ExclPattern)
loadAllExclusions ipkgPath = do
  depExcl <- loadDependencyExclusions ipkgPath
  pure (defaultExclusions ++ depExcl)

||| Full exclusion loading: defaults + custom files + dependencies
public export
loadFullExclusions : String -> List String -> IO (List ExclPattern)
loadFullExclusions ipkgPath exclFiles = do
  depExcl <- loadDependencyExclusions ipkgPath
  customExcl <- loadExclusionFiles exclFiles
  pure (defaultExclusions ++ depExcl ++ customExcl)

-- =============================================================================
-- Extended ExclusionReason
-- =============================================================================

||| Extended exclusion reason including dependency tracking
public export
data ExtendedExclusionReason
  = ExtNotExcluded
  | ExtExcludedByPattern String
  | ExtExcludedByModule String
  | ExtExcludedByDependency String

public export
Show ExtendedExclusionReason where
  show ExtNotExcluded = ""
  show (ExtExcludedByPattern p) = "EXCLUDED: pattern " ++ p
  show (ExtExcludedByModule m) = "EXCLUDED: module " ++ m
  show (ExtExcludedByDependency d) = "EXCLUDED: dependency " ++ d

||| Determine extended exclusion reason
public export
determineExtendedExclusionReason : List ExclPattern -> String -> ExtendedExclusionReason
determineExtendedExclusionReason patterns funcName =
  case isMethodExcluded patterns funcName of
    Just reason =>
      if isPrefixOf "dependency:" reason
        then ExtExcludedByDependency (pack (drop 11 (unpack reason)))
        else ExtExcludedByPattern reason
    Nothing =>
      case findExcludedModule funcName of
        Just m => ExtExcludedByModule m
        Nothing => ExtNotExcluded
