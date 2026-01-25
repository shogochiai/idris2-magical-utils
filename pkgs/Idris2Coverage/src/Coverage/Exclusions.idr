||| Load exclusion patterns from exclusions/ directory
||| Patterns are version-specific and loaded at runtime
|||
||| Re-exports shared types from Coverage.Core.Exclusions and provides
||| file-based loading with version-specific pattern files.
module Coverage.Exclusions

import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Data.String
import System.File

-- Re-export core exclusion types
import public Coverage.Core.Exclusions

%default partial

-- =============================================================================
-- File Pattern Parsing
-- =============================================================================

||| Remove trailing asterisk from pattern
removeTrailingStar : String -> String
removeTrailingStar s =
  let cs = unpack s
  in case reverse cs of
       ('*' :: rest) => pack (reverse rest)
       _ => s

||| Parse a single pattern from file
||| "Prelude.*" -> prefix match "Prelude."
||| "{csegen:*}" -> prefix match "{csegen:"
||| "exact" -> exact match "exact"
export
parsePatternFromFile : String -> Maybe ExclPattern
parsePatternFromFile s =
  let trimmed = trim s
  in if trimmed == "" || isPrefixOf "#" trimmed
     then Nothing
     else if isSuffixOf "*" trimmed
          then Just $ MkExclPattern (removeTrailingStar trimmed) Prefix "file pattern"
          else Just $ MkExclPattern trimmed Exact "file pattern"

-- =============================================================================
-- Loaded Exclusions (version-specific wrapper)
-- =============================================================================

||| All loaded exclusion patterns with version info
public export
record LoadedExclusions where
  constructor MkLoadedExclusions
  patterns : List ExclPattern
  idris2Version : String

public export
emptyExclusions : LoadedExclusions
emptyExclusions = MkLoadedExclusions [] "unknown"

||| Check if a function name should be excluded
export
shouldExclude : LoadedExclusions -> String -> Bool
shouldExclude excl name = isJust (isMethodExcluded excl.patterns name)

-- =============================================================================
-- File Loading
-- =============================================================================

||| Parse file content into patterns
export
parsePatternFile : String -> List ExclPattern
parsePatternFile content =
  mapMaybe parsePatternFromFile (lines content)

||| Load patterns from a file
export
loadPatternFile : String -> IO (List ExclPattern)
loadPatternFile path = do
  Right content <- readFile path
    | Left _ => pure []
  pure $ parsePatternFile content

||| Get Idris2 semver from version string
||| "Idris 2, version 0.8.0-95333b3ad" -> "0.8.0"
export
extractSemver : String -> String
extractSemver versionStr =
  case filter isDigitOrDot (unpack versionStr) of
    [] => "unknown"
    cs =>
      let parts = forget $ split (== '.') (pack $ takeWhile (\c => c /= '-') cs)
      in case parts of
           (a :: b :: c :: _) => a ++ "." ++ b ++ "." ++ c
           _ => pack cs
  where
    isDigitOrDot : Char -> Bool
    isDigitOrDot c = isDigit c || c == '.' || c == '-'

||| Load exclusions from exclusions/ directory
||| Loads base.txt + version-specific file
export
loadExclusions : String -> String -> IO LoadedExclusions
loadExclusions exclusionsDir idris2Version = do
  let semver = extractSemver idris2Version
  basePatterns <- loadPatternFile (exclusionsDir ++ "/base.txt")
  versionPatterns <- loadPatternFile (exclusionsDir ++ "/" ++ semver ++ ".txt")
  pure $ MkLoadedExclusions (basePatterns ++ versionPatterns) semver

||| Load exclusions using the package directory
||| Assumes exclusions/ is relative to the package root
export
loadExclusionsFromPackage : String -> String -> IO LoadedExclusions
loadExclusionsFromPackage packageDir idris2Version =
  loadExclusions (packageDir ++ "/exclusions") idris2Version

-- =============================================================================
-- Idris2-Specific Default Exclusions
-- =============================================================================

||| Default exclusions for Idris2 coverage analysis
public export
idris2DefaultExclusions : List ExclPattern
idris2DefaultExclusions =
  [ -- Compiler-generated
    prefixPattern "{csegen:" "Code generator"
  , prefixPattern "{arg:" "Argument handling"
  , prefixPattern "{lazyarg:" "Lazy argument"
  , prefixPattern "Builtin." "Builtin module"
  , prefixPattern "PrimIO." "Primitive IO"
    -- Standard library internals
  , prefixPattern "Prelude.Basics." "Basic prelude"
  , prefixPattern "Prelude.Types." "Type machinery"
  , prefixPattern "Prelude.IO." "IO primitives"
    -- Test infrastructure
  , prefixPattern "Test." "Test module"
  , prefixPattern "Tests." "Tests module"
  ]

||| Combine core defaults with Idris2-specific exclusions
public export
idris2FullExclusions : List ExclPattern
idris2FullExclusions = defaultExclusions ++ idris2DefaultExclusions
