||| Load exclusion patterns from exclusions/ directory
||| Patterns are version-specific and loaded at runtime
module Coverage.Exclusions

import Data.List
import Data.List1
import Data.Nat
import Data.String
import System.File

-- Import core exclusion types (ExclPattern with Prefix/Suffix/Contains/Exact)
-- Note: This module also defines its own ExclusionPattern for backward compatibility
-- with the existing version-specific exclusion file format
import public Coverage.Core.Exclusions as CoreExcl

%default partial

-- =============================================================================
-- Pattern Types
-- =============================================================================

||| A loaded exclusion pattern
||| Format: "prefix*" for prefix match, exact match otherwise
public export
record ExclusionPattern where
  constructor MkExclusionPattern
  pattern : String
  isPrefix : Bool  -- True if ends with *

public export
Show ExclusionPattern where
  show p = if p.isPrefix
           then p.pattern ++ "*"
           else p.pattern

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
parsePattern : String -> Maybe ExclusionPattern
parsePattern s =
  let trimmed = trim s
  in if trimmed == "" || isPrefixOf "#" trimmed
     then Nothing
     else if isSuffixOf "*" trimmed
          then Just $ MkExclusionPattern (removeTrailingStar trimmed) True
          else Just $ MkExclusionPattern trimmed False

||| Check if a name matches a pattern
export
matchesPattern : ExclusionPattern -> String -> Bool
matchesPattern p name =
  if p.isPrefix
  then isPrefixOf p.pattern name
  else p.pattern == name

-- =============================================================================
-- Loaded Exclusions
-- =============================================================================

||| All loaded exclusion patterns
public export
record LoadedExclusions where
  constructor MkLoadedExclusions
  patterns : List ExclusionPattern
  idris2Version : String

public export
emptyExclusions : LoadedExclusions
emptyExclusions = MkLoadedExclusions [] "unknown"

||| Check if a function name should be excluded
export
shouldExclude : LoadedExclusions -> String -> Bool
shouldExclude excl name = any (\p => matchesPattern p name) excl.patterns

-- =============================================================================
-- File Loading
-- =============================================================================

||| Parse file content into patterns
export
parsePatternFile : String -> List ExclusionPattern
parsePatternFile content =
  mapMaybe parsePattern (lines content)

||| Load patterns from a file
export
loadPatternFile : String -> IO (List ExclusionPattern)
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
