||| Scheme to Idris2 name mapper
||| Maps Chez Scheme function names back to Idris2 module.function
module EvmCoverage.SchemeMapper

import EvmCoverage.Types
import Data.List
import Data.List1
import Data.String
import Data.Maybe

%default covering

-- =============================================================================
-- Idris2 -> Scheme Name Encoding
-- =============================================================================

||| Idris2 encodes special characters in Scheme function names
||| - becomes C-45
||| . becomes C-46
||| _ becomes C-95
||| etc.
|||
||| Example: "EVM.Interpreter" -> "EVMC-45Interpreter"

||| Decode a single C-XX escape sequence
decodeEscape : List Char -> Maybe (Char, List Char)
decodeEscape ('C' :: '-' :: d1 :: d2 :: rest) =
  case parseHexDigit d1 of
    Nothing => Nothing
    Just h1 => case parseHexDigit d2 of
                 Nothing => Nothing
                 Just h2 => Just (chr (h1 * 16 + h2), rest)
  where
    parseHexDigit : Char -> Maybe Int
    parseHexDigit c =
      if c >= '0' && c <= '9' then Just (ord c - ord '0')
      else if c >= 'a' && c <= 'f' then Just (ord c - ord 'a' + 10)
      else if c >= 'A' && c <= 'F' then Just (ord c - ord 'A' + 10)
      else Nothing
decodeEscape _ = Nothing

||| Decode all C-XX sequences in a string
export
decodeSchemeFunc : String -> String
decodeSchemeFunc s = pack $ go (unpack s)
  where
    go : List Char -> List Char
    go [] = []
    go ('C' :: '-' :: rest) =
      case decodeEscape ('C' :: '-' :: rest) of
        Just (c, remaining) => c :: go remaining
        Nothing => 'C' :: '-' :: go rest
    go (c :: rest) = c :: go rest

||| Encode Idris2 name to Scheme format
export
encodeToScheme : String -> String
encodeToScheme s = pack $ concatMap encodeChar (unpack s)
  where
    encodeChar : Char -> List Char
    encodeChar '-' = unpack "C-45"
    encodeChar '.' = unpack "C-46"
    encodeChar '_' = unpack "C-95"
    encodeChar '\'' = unpack "C-39"
    encodeChar '!' = unpack "C-33"
    encodeChar '?' = unpack "C-63"
    encodeChar '+' = unpack "C-43"
    encodeChar '*' = unpack "C-42"
    encodeChar '/' = unpack "C-47"
    encodeChar '<' = unpack "C-60"
    encodeChar '>' = unpack "C-62"
    encodeChar '=' = unpack "C-61"
    encodeChar c = [c]

-- =============================================================================
-- Scheme -> Idris2 Name Parsing
-- =============================================================================

||| Parse Scheme function name into (module, function)
||| After decoding, names are like "Module.Submodule-funcName"
||| The last dash separates module path from function name
export
parseSchemeFunc : String -> Maybe (String, String)
parseSchemeFunc schemeFunc =
  let decoded = decodeSchemeFunc schemeFunc
      -- Find last dash that separates module from function
  in case splitOnLastDash decoded of
       Nothing => Nothing
       Just (modPart, funcPart) =>
         if null modPart || null funcPart
           then Nothing
           else Just (modPart, funcPart)
  where
    splitOnLastDash : String -> Maybe (String, String)
    splitOnLastDash s =
      let chars = unpack s
          reversed = reverse chars
      in case break (== '-') reversed of
           (afterDash, []) => Nothing  -- No dash found
           (afterDash, _ :: beforeDash) =>
             Just (pack $ reverse beforeDash, pack $ reverse afterDash)

||| Alternative: split on first dash (simpler modules)
export
parseSimpleSchemeFunc : String -> Maybe (String, String)
parseSimpleSchemeFunc schemeFunc =
  let decoded = decodeSchemeFunc schemeFunc
  in case break (== '-') decoded of
       (modName, "") => Nothing
       (modName, rest) =>
         let funcName = pack $ drop 1 $ unpack rest  -- Drop the '-'
         in if null modName || null funcName
           then Nothing
           else Just (modName, funcName)

-- =============================================================================
-- Exclusion Patterns
-- =============================================================================

||| Check if a function should be excluded from coverage
||| Excludes: compiler-generated, standard library, primitives
export
shouldExclude : String -> Bool
shouldExclude name =
     isPrefixOf "{" name           -- Compiler-generated MN names
  || isPrefixOf "_builtin" name    -- Builtin constructors
  || isPrefixOf "prim__" name      -- Primitive operations
  || isPrefixOf "Prelude" name     -- Standard library
  || isPrefixOf "Data" name
  || isPrefixOf "System" name
  || isPrefixOf "Control" name
  || isPrefixOf "Decidable" name
  || isPrefixOf "Language" name
  || isPrefixOf "Debug" name
  || isPrefixOf "csegen" name      -- Code sharing optimization

||| Check if decoded name should be excluded
export
shouldExcludeDecoded : String -> Bool
shouldExcludeDecoded schemeFunc =
  let decoded = decodeSchemeFunc schemeFunc
  in shouldExclude decoded

-- =============================================================================
-- EVM-specific mappings
-- =============================================================================

||| Map EVM opcode branch to readable name
||| Scheme: EVMC-45InterpreterC-45executeOp
||| Decoded: EVM.Interpreter-executeOp
export
parseEvmFunc : String -> Maybe (String, String)
parseEvmFunc schemeFunc =
  let decoded = decodeSchemeFunc schemeFunc
  in if isPrefixOf "EVM" decoded
       then parseSchemeFunc schemeFunc
       else Nothing

||| Get list of EVM interpreter functions to track
export
evmInterpreterFuncs : List String
evmInterpreterFuncs =
  [ "EVM.Interpreter-executeOp"
  , "EVM.Interpreter-executeDupSwap"
  , "EVM.Interpreter-step"
  , "EVM.Interpreter-run"
  , "EVM.Interpreter-binaryOp"
  , "EVM.Interpreter-unaryOp"
  , "EVM.Interpreter-ternaryOp"
  , "EVM.Stack-push"
  , "EVM.Stack-pop"
  , "EVM.Stack-popN"
  , "EVM.Memory-mload"
  , "EVM.Memory-mstore"
  , "EVM.Storage-sload"
  , "EVM.Storage-sstore"
  , "EVM.Opcodes-fromByte"
  ]

-- =============================================================================
-- BranchId Mapping
-- =============================================================================

||| Create BranchId from Scheme function name
export
schemeFuncToBranchId : String -> Nat -> BranchId
schemeFuncToBranchId schemeFunc branchIdx =
  case parseSchemeFunc schemeFunc of
    Nothing => MkBranchId "" schemeFunc branchIdx
    Just (modName, funcName) => MkBranchId modName funcName branchIdx

||| Match ProfileHit to BranchId
export
matchHitToBranch : ProfileHit -> List ClassifiedBranch -> Maybe ClassifiedBranch
matchHitToBranch hit branches =
  let decoded = decodeSchemeFunc hit.schemeFunc
  in find (\b => isInfixOf decoded (show b.branchId)) branches
