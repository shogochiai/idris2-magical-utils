||| Candid Interface Parser
|||
||| Parses .did files to extract service method definitions.
||| Used to determine which canister methods should be tested.
module DfxCoverage.CandidParser

import Data.List
import Data.Maybe
import Data.String
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Method modifier (query, update, oneway)
public export
data MethodMode = Query | Update | Oneway

public export
Show MethodMode where
  show Query = "query"
  show Update = "update"
  show Oneway = "oneway"

public export
Eq MethodMode where
  Query == Query = True
  Update == Update = True
  Oneway == Oneway = True
  _ == _ = False

||| Candid primitive types (simplified - no CandidService to avoid mutual recursion)
public export
data CandidType
  = CandidNat
  | CandidInt
  | CandidText
  | CandidBool
  | CandidPrincipal
  | CandidBlob
  | CandidNull
  | CandidOpt CandidType
  | CandidVec CandidType
  | CandidRecord (List (String, CandidType))
  | CandidVariant (List (String, Maybe CandidType))
  | CandidFunc (List CandidType) (List CandidType)
  | CandidRef String  -- Reference to a type alias

public export
Show CandidType where
  show CandidNat = "nat"
  show CandidInt = "int"
  show CandidText = "text"
  show CandidBool = "bool"
  show CandidPrincipal = "principal"
  show CandidBlob = "blob"
  show CandidNull = "null"
  show (CandidOpt t) = "opt " ++ show t
  show (CandidVec t) = "vec " ++ show t
  show (CandidRecord fs) = "record { ... }"
  show (CandidVariant vs) = "variant { ... }"
  show (CandidFunc args rets) = "func (" ++ show (length args) ++ " args)"
  show (CandidRef refName) = refName

||| A single Candid method definition
public export
record CandidMethod where
  constructor MkCandidMethod
  name : String
  args : List CandidType
  returns : List CandidType
  mode : MethodMode

public export
Show CandidMethod where
  show m = m.name ++ ": (" ++ show (length m.args) ++ " args) -> (" ++
           show (length m.returns) ++ " rets) " ++ show m.mode

||| Parsed Candid interface
public export
record CandidInterface where
  constructor MkCandidInterface
  typeAliases : List (String, CandidType)
  methods : List CandidMethod

public export
Show CandidInterface where
  show ci = "CandidInterface { " ++
            show (length ci.typeAliases) ++ " types, " ++
            show (length ci.methods) ++ " methods }"

-- =============================================================================
-- Parsing Helpers
-- =============================================================================

||| Trim whitespace and remove comments from a line
cleanLine : String -> String
cleanLine s =
  let noComment = case break (== ';') (unpack s) of
                    (before, _) => pack before
  in trim noComment

||| Check if line defines a method (contains ":" and "->")
isMethodLine : String -> Bool
isMethodLine s = isInfixOf ":" s && isInfixOf "->" s

||| Extract method name from line like "  getVersion: (nat) -> (text) query;"
extractMethodName : String -> Maybe String
extractMethodName line =
  let cleaned = trim line
  in case break (== ':') (unpack cleaned) of
       (name, rest) =>
         if null rest
           then Nothing
           else Just (trim (pack name))

||| Determine method mode from line
extractMethodMode : String -> MethodMode
extractMethodMode line =
  if isInfixOf "query" line then Query
  else if isInfixOf "oneway" line then Oneway
  else Update

||| Parse a single method from a line
parseMethodLine : String -> Maybe CandidMethod
parseMethodLine line =
  do name <- extractMethodName line
     let mode = extractMethodMode line
     -- For now, we don't fully parse arg/return types
     -- Just extract method signature for coverage analysis
     Just $ MkCandidMethod name [] [] mode

-- =============================================================================
-- Main Parser
-- =============================================================================

||| Parse methods from "service : { ... }" block
parseServiceMethods : List String -> List CandidMethod
parseServiceMethods lines =
  mapMaybe parseMethodLine (filter isMethodLine lines)

||| Parse a Candid interface file
|||
||| @content The content of a .did file
public export
parseCandidFile : String -> CandidInterface
parseCandidFile content =
  let ls = lines content
      cleanedLines = map cleanLine ls
      methods = parseServiceMethods cleanedLines
  in MkCandidInterface [] methods

||| Read and parse a .did file
|||
||| @path Path to the .did file
public export
readCandidFile : String -> IO (Either String CandidInterface)
readCandidFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read Candid file: " ++ show err
  pure $ Right $ parseCandidFile content

-- =============================================================================
-- Query Helpers
-- =============================================================================

||| Get all method names from interface
public export
getMethodNames : CandidInterface -> List String
getMethodNames ci = map (.name) ci.methods

||| Get query methods only
public export
getQueryMethods : CandidInterface -> List CandidMethod
getQueryMethods ci = filter (\m => m.mode == Query) ci.methods

||| Get update methods only
public export
getUpdateMethods : CandidInterface -> List CandidMethod
getUpdateMethods ci = filter (\m => m.mode == Update) ci.methods

||| Check if a method exists in the interface
public export
hasMethod : CandidInterface -> String -> Bool
hasMethod ci name = any (\m => m.name == name) ci.methods
