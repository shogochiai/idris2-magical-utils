||| Exclusion Pattern Matching for EVM Coverage
|||
||| Loads exclusion patterns from text files and filters out
||| functions that should not count toward production coverage.
|||
||| Pattern format:
|||   - prefix matching: pattern*
|||   - suffix matching: *pattern
|||   - contains matching: *pattern*
|||   - exact matching: pattern (no wildcards)
|||   - comments: lines starting with #
module EvmCoverage.Exclusions

import Data.List
import Data.String
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Exclusion pattern type
public export
data ExclPattern
  = ExactMatch String       -- exact string match
  | PrefixMatch String      -- pattern*
  | SuffixMatch String      -- *pattern
  | ContainsMatch String    -- *pattern*

public export
Show ExclPattern where
  show (ExactMatch s) = "exact:" ++ s
  show (PrefixMatch s) = "prefix:" ++ s ++ "*"
  show (SuffixMatch s) = "suffix:*" ++ s
  show (ContainsMatch s) = "contains:*" ++ s ++ "*"

-- =============================================================================
-- Pattern Parsing
-- =============================================================================

||| Parse a single pattern line
parsePattern : String -> Maybe ExclPattern
parsePattern s =
  let trimmed = trim s
  in if null trimmed || isPrefixOf "#" trimmed
       then Nothing  -- Empty or comment
       else Just $ parseNonEmpty trimmed
  where
    parseNonEmpty : String -> ExclPattern
    parseNonEmpty p =
      let startsWithStar = isPrefixOf "*" p
          endsWithStar = isSuffixOf "*" p
          stripped = if startsWithStar && endsWithStar
                       then substr 1 (minus (length p) 2) p
                     else if startsWithStar
                       then substr 1 (minus (length p) 1) p
                     else if endsWithStar
                       then substr 0 (minus (length p) 1) p
                     else p
      in if startsWithStar && endsWithStar
           then ContainsMatch stripped
         else if startsWithStar
           then SuffixMatch stripped
         else if endsWithStar
           then PrefixMatch stripped
         else ExactMatch stripped

||| Parse exclusion patterns from file content
export
parseExclusions : String -> List ExclPattern
parseExclusions content =
  mapMaybe parsePattern (lines content)

-- =============================================================================
-- Pattern Matching
-- =============================================================================

||| Check if a function name matches an exclusion pattern
matchesPattern : String -> ExclPattern -> Bool
matchesPattern name (ExactMatch p) = name == p
matchesPattern name (PrefixMatch p) = isPrefixOf p name
matchesPattern name (SuffixMatch p) = isSuffixOf p name
matchesPattern name (ContainsMatch p) = isInfixOf p name

||| Check if a function name should be excluded
export
isExcluded : String -> List ExclPattern -> Bool
isExcluded name patterns = any (matchesPattern name) patterns

||| Filter out excluded functions from a list
export
filterExcluded : List ExclPattern -> List String -> List String
filterExcluded patterns = filter (\name => not (isExcluded name patterns))

-- =============================================================================
-- File I/O
-- =============================================================================

||| Load exclusion patterns from a file
export
loadExclusionsFile : String -> IO (Either String (List ExclPattern))
loadExclusionsFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read exclusions file: " ++ show err
  pure $ Right $ parseExclusions content

||| Load and merge multiple exclusion files
export
loadExclusionFiles : List String -> IO (List ExclPattern)
loadExclusionFiles paths = do
  results <- traverse loadExclusionsFile paths
  pure $ concatMap (either (const []) id) results

-- =============================================================================
-- Default Exclusions Path Resolution
-- =============================================================================

||| Get the base exclusions file path (relative to package)
export
baseExclusionsPath : String -> String
baseExclusionsPath pkgDir = pkgDir ++ "/exclusions/base.txt"

||| Get version-specific exclusions file path
export
versionExclusionsPath : String -> String -> String -> String
versionExclusionsPath pkgDir toolName version =
  pkgDir ++ "/exclusions/" ++ toolName ++ "-" ++ version ++ ".txt"

