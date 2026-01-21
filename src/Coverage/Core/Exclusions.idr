||| Exclusion Patterns for Coverage Analysis
|||
||| Shared exclusion pattern types and matching logic for filtering
||| functions from coverage analysis across all backends.
|||
||| Supports:
|||   - Pattern types: Exact, Prefix, Suffix, Contains
|||   - Exclusion reasons: Pattern match, Module exclusion
|||   - Default module exclusions for standard library
module Coverage.Core.Exclusions

import Data.List
import Data.Maybe
import Data.String
import System.File

%default covering

-- =============================================================================
-- Pattern Types
-- =============================================================================

||| Exclusion pattern type
public export
data PatternType
  = Exact         -- Exact method name match
  | Prefix        -- Method name starts with pattern
  | Suffix        -- Method name ends with pattern
  | Contains      -- Method name contains pattern

public export
Show PatternType where
  show Exact = "exact"
  show Prefix = "prefix"
  show Suffix = "suffix"
  show Contains = "contains"

public export
Eq PatternType where
  Exact == Exact = True
  Prefix == Prefix = True
  Suffix == Suffix = True
  Contains == Contains = True
  _ == _ = False

||| Single exclusion pattern
public export
record ExclPattern where
  constructor MkExclPattern
  pattern : String
  patternType : PatternType
  reason : String

public export
Show ExclPattern where
  show p = show p.patternType ++ ":" ++ p.pattern ++ " (" ++ p.reason ++ ")"

public export
Eq ExclPattern where
  a == b = a.pattern == b.pattern && a.patternType == b.patternType

-- =============================================================================
-- Pattern Matching
-- =============================================================================

||| Check if a method name matches an exclusion pattern
public export
matchesPattern : ExclPattern -> String -> Bool
matchesPattern excl methodName =
  case excl.patternType of
    Exact => methodName == excl.pattern
    Prefix => isPrefixOf excl.pattern methodName
    Suffix => isSuffixOf excl.pattern methodName
    Contains => isInfixOf excl.pattern methodName

||| Check if a method should be excluded
|||
||| @patterns List of exclusion patterns
||| @methodName Method name to check
||| @returns Just reason if excluded, Nothing otherwise
public export
isMethodExcluded : List ExclPattern -> String -> Maybe String
isMethodExcluded patterns methodName =
  map (.reason) $ find (\p => matchesPattern p methodName) patterns

-- =============================================================================
-- Pattern Creation Helpers
-- =============================================================================

||| Create exact match pattern
public export
exactPattern : String -> String -> ExclPattern
exactPattern name reason = MkExclPattern name Exact reason

||| Create prefix pattern
public export
prefixPattern : String -> String -> ExclPattern
prefixPattern pre reason = MkExclPattern pre Prefix reason

||| Create suffix pattern
public export
suffixPattern : String -> String -> ExclPattern
suffixPattern suf reason = MkExclPattern suf Suffix reason

||| Create contains pattern
public export
containsPattern : String -> String -> ExclPattern
containsPattern sub reason = MkExclPattern sub Contains reason

-- =============================================================================
-- Exclusion Reason (ADT for type-safe exclusion tracking)
-- =============================================================================

||| Reason why a function was excluded from coverage analysis
public export
data ExclusionReason
  = NotExcluded                   -- Function is included
  | ExcludedByPattern String      -- Excluded by exclusion pattern
  | ExcludedByModule String       -- Excluded by module prefix

public export
Show ExclusionReason where
  show NotExcluded = ""
  show (ExcludedByPattern p) = "EXCLUDED: pattern " ++ p
  show (ExcludedByModule m) = "EXCLUDED: module " ++ m

public export
Eq ExclusionReason where
  NotExcluded == NotExcluded = True
  (ExcludedByPattern a) == (ExcludedByPattern b) = a == b
  (ExcludedByModule a) == (ExcludedByModule b) = a == b
  _ == _ = False

||| Check if exclusion reason indicates exclusion
public export
isExcludedReason : ExclusionReason -> Bool
isExcludedReason NotExcluded = False
isExcludedReason _ = True

-- =============================================================================
-- Default Module Exclusions
-- =============================================================================

||| Default module exclusions for Idris2
|||
||| These are standard library modules that typically don't need
||| direct testing in application coverage analysis.
public export
defaultModuleExclusions : List String
defaultModuleExclusions =
  [ "Prelude."
  , "Data."
  , "Control."
  , "System."
  , "Decidable."
  , "Language."
  , "Debug."
  , "Builtin."
  , "PrimIO."
  ]

||| Check if function is in excluded module
public export
findExcludedModule : String -> Maybe String
findExcludedModule funcName =
  find (\m => isPrefixOf m funcName) defaultModuleExclusions

||| Determine exclusion reason for a function
public export
determineExclusionReason : List ExclPattern -> String -> ExclusionReason
determineExclusionReason patterns funcName =
  case isMethodExcluded patterns funcName of
    Just reason => ExcludedByPattern reason
    Nothing => case findExcludedModule funcName of
                 Just m => ExcludedByModule m
                 Nothing => NotExcluded

-- =============================================================================
-- Default Exclusion Patterns
-- =============================================================================

||| Default exclusions for common internal/lifecycle patterns
public export
defaultExclusions : List ExclPattern
defaultExclusions =
  [ -- Internal/lifecycle methods
    prefixPattern "__" "Internal method"
  , prefixPattern "{" "Compiler-generated"
  , prefixPattern "_builtin." "Builtin constructor"
  , prefixPattern "prim__" "Primitive operation"

    -- Debug/development methods
  , prefixPattern "debug_" "Debug method"
  , prefixPattern "test_" "Test helper method"

    -- Default interface implementations (auto-generated, trivial)
  , suffixPattern "./=" "Default Eq./= implementation"
  ]

-- =============================================================================
-- File Loading
-- =============================================================================

||| Parse a single exclusion line
||| Format: TYPE:PATTERN # reason
||| Example: prefix:debug_ # Debug methods excluded
public export
parseExclusionLine : String -> Maybe ExclPattern
parseExclusionLine line =
  let trimmed = trim line
  in if null trimmed || isPrefixOf "#" trimmed
       then Nothing
       else parsePattern trimmed
  where
    parsePattern : String -> Maybe ExclPattern
    parsePattern s =
      case break (== '#') (unpack s) of
        (patternPart, reasonPart) =>
          let pat = trim (pack patternPart)
              reason = trim (pack (drop 1 reasonPart))
          in case break (== ':') (unpack pat) of
               (typePart, namePart) =>
                 let ptype = case toLower (trim (pack typePart)) of
                               "exact" => Just Exact
                               "prefix" => Just Prefix
                               "suffix" => Just Suffix
                               "contains" => Just Contains
                               _ => Nothing
                     name = trim (pack (drop 1 namePart))
                 in case ptype of
                      Just t => Just $ MkExclPattern name t
                                          (if null reason then "Excluded" else reason)
                      Nothing => Nothing

||| Load exclusion patterns from a file
public export
loadExclusionFile : String -> IO (List ExclPattern)
loadExclusionFile path = do
  Right content <- readFile path
    | Left _ => pure []
  let ls = lines content
  pure $ mapMaybe parseExclusionLine ls

||| Load exclusion patterns from multiple files
public export
loadExclusionFiles : List String -> IO (List ExclPattern)
loadExclusionFiles paths = do
  patternLists <- traverse loadExclusionFile paths
  pure $ concat patternLists

||| Combine default exclusions with loaded patterns
public export
loadWithDefaults : List String -> IO (List ExclPattern)
loadWithDefaults paths = do
  loaded <- loadExclusionFiles paths
  pure $ defaultExclusions ++ loaded
