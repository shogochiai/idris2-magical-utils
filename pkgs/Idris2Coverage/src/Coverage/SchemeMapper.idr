||| Scheme to Idris Name Mapper
||| Maps Chez Scheme generated function names back to Idris modules and functions
module Coverage.SchemeMapper

import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default total

-- =============================================================================
-- Scheme Name Encoding
-- =============================================================================

||| Idris2 encodes special characters in Scheme names:
||| - '.' becomes 'C-45' (hyphen-45)
||| - '-' becomes 'C-45' within identifiers
||| - 'u--' prefix for operator overloads

||| Split string on separator
covering
splitOn : String -> String -> List String
splitOn sep str =
  let chars = unpack str
      sepChars = unpack sep
  in map pack $ splitOnChars sepChars chars []
  where
    splitOnChars : List Char -> List Char -> List Char -> List (List Char)
    splitOnChars _ [] acc = [reverse acc]
    splitOnChars sep' (c :: cs) acc =
      if take (length sep') (c :: cs) == sep'
        then reverse acc :: splitOnChars sep' (drop (length sep') (c :: cs)) []
        else splitOnChars sep' cs (c :: acc)

||| Decode a single Scheme segment
||| "PreludeC-45Show" -> "Prelude.Show"
export
covering
decodeSchemeSegment : String -> String
decodeSchemeSegment s =
  let parts = splitOn "C-45" s
  in fastConcat $ intersperse "." parts

||| Parse Scheme function name into (Idris Module, Idris Function)
||| Examples:
|||   "Main-factorial" -> ("Main", "factorial")
|||   "PreludeC-45Show-u--show_Show_Nat" -> ("Prelude.Show", "show")
|||   "AuditC-45Orchestrator-runAudit" -> ("Audit.Orchestrator", "runAudit")
export
covering
parseSchemeFunction : String -> (String, String)
parseSchemeFunction schemeFunc =
  let parts = forget $ split (== '-') schemeFunc
  in case parts of
       [] => ("", "")
       [single] => ("", single)
       _ =>
         let lastPart = fromMaybe "" (last' parts)
             initParts = fromMaybe [] (init' parts)
             modPart = fastConcat $ intersperse "-" initParts
             moduleName = decodeSchemeSegment modPart
             funcName = cleanFuncName lastPart
         in (moduleName, funcName)
  where
    -- Clean function name (remove "u--" prefix for operators)
    cleanFuncName : String -> String
    cleanFuncName fn =
      if isPrefixOf "u--" fn
        then -- Operator overload: "u--show_Show_Nat" -> "show"
             let rest = strSubstr 3 (cast $ length fn) fn
             in case break (== '_') (unpack rest) of
                  (name, _) => pack name
        else fn

||| Check if a Scheme function is a runtime/library function (not user code)
export
isRuntimeFunction : String -> Bool
isRuntimeFunction name =
  isPrefixOf "blodwen-" name
  || isPrefixOf "prim__" name
  || isPrefixOf "cast-" name
  || isPrefixOf "exact-" name
  || isPrefixOf "string-" name
  || isPrefixOf "either-" name
  || isPrefixOf "random-" name
  || elem name ["bu+", "bu-", "bu*", "bu/", "bs+", "bs-", "bs*", "bs/"]
  || isPrefixOf "destroy-" name

||| Check if a Scheme function belongs to a specific Idris module
export
covering
belongsToModule : String -> String -> Bool
belongsToModule schemeFunc idrisModule =
  let (foundModule, _) = parseSchemeFunction schemeFunc
  in foundModule == idrisModule || isPrefixOf (idrisModule ++ ".") foundModule

-- =============================================================================
-- Module Name Utilities
-- =============================================================================

||| Convert Idris module name to expected Scheme prefix
||| "Audit.Orchestrator" -> "AuditC-45Orchestrator"
export
idrisToSchemePrefix : String -> String
idrisToSchemePrefix modName =
  fastConcat $ intersperse "C-45" $ forget $ split (== '.') modName

||| Extract all unique module names from a list of Scheme function names
export
covering
extractModules : List String -> List String
extractModules schemeFuncs =
  nub $ map (fst . parseSchemeFunction) schemeFuncs

-- =============================================================================
-- Coverage Filtering
-- =============================================================================

||| Filter functions to only include user code (not runtime)
export
filterUserFunctions : List String -> List String
filterUserFunctions = filter (not . isRuntimeFunction)

||| Filter functions belonging to a specific module
export
covering
filterByModule : String -> List String -> List String
filterByModule modName = filter (`belongsToModule` modName)

-- =============================================================================
-- Mapped Function Result
-- =============================================================================

||| A Scheme function mapped to Idris identifiers
public export
record MappedFunction where
  constructor MkMappedFunction
  schemeFunc : String
  idrisModule : String
  idrisFunc : String
  isUserCode : Bool

public export
Show MappedFunction where
  show mf = mf.idrisModule ++ "." ++ mf.idrisFunc ++ " (" ++ mf.schemeFunc ++ ")"

||| Map a Scheme function name to Idris identifiers
export
covering
mapFunction : String -> MappedFunction
mapFunction schemeFunc =
  let (modName, funcName) = parseSchemeFunction schemeFunc
      isUser = not $ isRuntimeFunction schemeFunc
  in MkMappedFunction schemeFunc modName funcName isUser

||| Map all functions and filter to user code only
export
covering
mapUserFunctions : List String -> List MappedFunction
mapUserFunctions = filter (.isUserCode) . map mapFunction
