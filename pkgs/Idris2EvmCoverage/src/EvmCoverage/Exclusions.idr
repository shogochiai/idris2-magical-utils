||| Exclusion Pattern Matching for EVM Coverage
|||
||| Re-exports shared types from Coverage.Core.Exclusions and provides
||| EVM-specific default exclusion patterns.
|||
||| Pattern format (from text files):
|||   - prefix matching: pattern*
|||   - suffix matching: *pattern
|||   - contains matching: *pattern*
|||   - exact matching: pattern (no wildcards)
|||   - comments: lines starting with #
module EvmCoverage.Exclusions

import Data.List
import Data.Maybe
import Data.String
import System.File

-- Re-export core exclusion types
import public Coverage.Core.Exclusions

%default covering

-- =============================================================================
-- File Pattern Parsing (for text-based exclusion files)
-- =============================================================================

||| Parse a single pattern line from exclusion file
||| Converts wildcard syntax to ExclPattern
parsePatternLine : String -> Maybe ExclPattern
parsePatternLine s =
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
          ptype = if startsWithStar && endsWithStar
                    then Contains
                  else if startsWithStar
                    then Suffix
                  else if endsWithStar
                    then Prefix
                  else Exact
      in MkExclPattern stripped ptype "file pattern"

||| Parse exclusion patterns from file content
export
parseExclusions : String -> List ExclPattern
parseExclusions content =
  mapMaybe parsePatternLine (lines content)

-- =============================================================================
-- Compatibility Layer (maps to core functions)
-- =============================================================================

||| Check if a function name should be excluded
||| (Wrapper for core isMethodExcluded, returns Bool instead of Maybe String)
export
isExcluded : String -> List ExclPattern -> Bool
isExcluded name patterns = isJust (isMethodExcluded patterns name)

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

-- =============================================================================
-- EVM-Specific Default Exclusions
-- =============================================================================

||| Default exclusions for EVM/Yul coverage
public export
evmDefaultExclusions : List ExclPattern
evmDefaultExclusions =
  [ -- Yul runtime functions
    prefixPattern "abi_" "ABI encoding/decoding"
  , prefixPattern "cleanup_" "Type cleanup functions"
  , prefixPattern "validator_" "Input validation"
  , prefixPattern "panic_" "Panic handlers"
  , prefixPattern "revert_" "Revert handlers"
  , containsPattern "checked_" "Overflow checks"
    -- Memory management
  , exactPattern "allocate_memory" "Memory allocation"
  , exactPattern "allocate_unbounded" "Unbounded allocation"
  , exactPattern "finalize_allocation" "Allocation finalization"
  , exactPattern "round_up_to_mul_of_32" "Memory alignment"
    -- Internal helpers
  , prefixPattern "__" "Internal function"
  , prefixPattern "{" "Compiler-generated"
  ]

||| Combine core defaults with EVM-specific exclusions
public export
evmFullExclusions : List ExclPattern
evmFullExclusions = defaultExclusions ++ evmDefaultExclusions
