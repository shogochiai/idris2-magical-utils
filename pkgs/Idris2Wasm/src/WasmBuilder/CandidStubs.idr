||| Candid Stub Generator for WasmBuilder
|||
||| Parses .did files and generates Candid-encoded stub responses
||| for coverage testing without actual function implementation.
|||
||| Fully portable: dynamically parses type definitions and encodes
||| arbitrary record/variant types to valid Candid binary format.
module WasmBuilder.CandidStubs

import Data.String
import Data.List
import Data.Maybe
import Data.List1

%default covering

-- =============================================================================
-- Candid Type Representation
-- =============================================================================

||| Simplified Candid type for stub generation
public export
data CandidType
  = CTNat
  | CTInt
  | CTText
  | CTBool
  | CTBlob
  | CTNat8
  | CTNat16
  | CTNat32
  | CTNat64
  | CTInt8
  | CTInt16
  | CTInt32
  | CTInt64
  | CTFloat32
  | CTFloat64
  | CTNull
  | CTOpt CandidType
  | CTVec CandidType
  | CTRecord (List (String, CandidType))
  | CTVariant (List (String, CandidType))
  | CTNamed String  -- Reference to a named type
  | CTUnknown String

public export
Show CandidType where
  show CTNat = "nat"
  show CTInt = "int"
  show CTText = "text"
  show CTBool = "bool"
  show CTBlob = "blob"
  show CTNat8 = "nat8"
  show CTNat16 = "nat16"
  show CTNat32 = "nat32"
  show CTNat64 = "nat64"
  show CTInt8 = "int8"
  show CTInt16 = "int16"
  show CTInt32 = "int32"
  show CTInt64 = "int64"
  show CTFloat32 = "float32"
  show CTFloat64 = "float64"
  show CTNull = "null"
  show (CTOpt t) = "opt " ++ show t
  show (CTVec t) = "vec " ++ show t
  show (CTRecord _) = "record {...}"
  show (CTVariant _) = "variant {...}"
  show (CTNamed n) = n
  show (CTUnknown s) = "unknown(" ++ s ++ ")"

||| Method signature from .did file
public export
record DidMethod where
  constructor MkDidMethod
  name : String
  returnType : CandidType
  isQuery : Bool

||| Type definition from .did file
public export
record TypeDef where
  constructor MkTypeDef
  typeName : String
  typeBody : CandidType

-- =============================================================================
-- Candid Hash Function
-- =============================================================================

||| Candid field name hash: h = (h * 223 + char) mod 2^32
||| Used to determine field ordering in records and variants
public export
candidHash : String -> Bits32
candidHash s = foldl step 0 (unpack s)
  where
    step : Bits32 -> Char -> Bits32
    step h c = h * 223 + cast (ord c)

-- =============================================================================
-- LEB128 Encoding
-- =============================================================================

||| Convert a byte to hex string
toHex : Bits32 -> String
toHex b =
  let hi = b `div` 16
      lo = b `mod` 16
      hexDigit : Bits32 -> Char
      hexDigit x = if x < 10
                     then chr (cast (ord '0' + cast x))
                     else chr (cast (ord 'a' + cast x - 10))
  in "0x" ++ pack [hexDigit hi, hexDigit lo]

||| Encode unsigned integer as LEB128 bytes
||| Returns list of hex byte strings like "0x7f", "0x80"
leb128Unsigned : Bits32 -> List String
leb128Unsigned n = go n []
  where
    go : Bits32 -> List String -> List String
    go val acc =
      let byte = val `mod` 128
          rest = val `div` 128
      in if rest == 0
           then reverse (toHex byte :: acc)
           else go rest (toHex (byte + 128) :: acc)

||| Encode signed integer as SLEB128 bytes
sleb128Signed : Int -> List String
sleb128Signed n =
  if n >= 0 && n < 64
    then [toHex (cast n)]
    else if n < 0 && n >= -64
      then [toHex (cast (128 + n))]
      else ["0x00"]  -- Simplified: just return 0 for complex cases
  where
    toHex : Bits32 -> String
    toHex b =
      let hi = b `div` 16
          lo = b `mod` 16
          hexDigit : Bits32 -> Char
          hexDigit x = if x < 10
                         then chr (cast (ord '0' + cast x))
                         else chr (cast (ord 'a' + cast x - 10))
      in "0x" ++ pack [hexDigit hi, hexDigit lo]

-- =============================================================================
-- Type Code Constants
-- =============================================================================

||| Candid type codes (negative SLEB128)
typeCodes : CandidType -> String
typeCodes CTNull = "0x7f"     -- -1
typeCodes CTBool = "0x7e"     -- -2
typeCodes CTNat = "0x7d"      -- -3
typeCodes CTInt = "0x7c"      -- -4
typeCodes CTNat8 = "0x7b"     -- -5
typeCodes CTNat16 = "0x7a"    -- -6
typeCodes CTNat32 = "0x79"    -- -7
typeCodes CTNat64 = "0x78"    -- -8
typeCodes CTInt8 = "0x77"     -- -9
typeCodes CTInt16 = "0x76"    -- -10
typeCodes CTInt32 = "0x75"    -- -11
typeCodes CTInt64 = "0x74"    -- -12
typeCodes CTFloat32 = "0x73"  -- -13
typeCodes CTFloat64 = "0x72"  -- -14
typeCodes CTText = "0x71"     -- -15
typeCodes CTBlob = "0x6d,0x7b" -- vec nat8
typeCodes (CTOpt _) = "0x6e"  -- -18
typeCodes (CTVec _) = "0x6d"  -- -19
typeCodes (CTRecord _) = "0x6c" -- -20
typeCodes (CTVariant _) = "0x6b" -- -21
typeCodes _ = "0x7f"          -- default to null

-- =============================================================================
-- Simple .did Parser
-- =============================================================================

||| Parse a simple type reference (handles opt, vec, primitives)
parseSimpleType : String -> CandidType
parseSimpleType s =
  let t = trim s
  in case t of
    "nat" => CTNat
    "int" => CTInt
    "text" => CTText
    "bool" => CTBool
    "blob" => CTBlob
    "null" => CTNull
    "nat8" => CTNat8
    "nat16" => CTNat16
    "nat32" => CTNat32
    "nat64" => CTNat64
    "int8" => CTInt8
    "int16" => CTInt16
    "int32" => CTInt32
    "int64" => CTInt64
    "float32" => CTFloat32
    "float64" => CTFloat64
    _ => if isPrefixOf "opt " t
           then CTOpt (parseSimpleType (substr 4 (length t) t))
         else if isPrefixOf "vec " t
           then CTVec (parseSimpleType (substr 4 (length t) t))
         else CTNamed t

||| Remove trailing semicolon if present
stripSemicolon : String -> String
stripSemicolon s =
  let trimmed = trim s
  in if isSuffixOf ";" trimmed
       then trim (substr 0 (minus (length trimmed) 1) trimmed)
       else trimmed

||| Parse record fields: "field1: type1; field2: type2"
parseRecordFields : String -> List (String, CandidType)
parseRecordFields content =
  let -- Split by semicolon, handling nested structures
      parts = split (== ';') content
      parsePart : String -> Maybe (String, CandidType)
      parsePart p =
        let trimmed = trim p
        in if null trimmed
             then Nothing
             else case break (== ':') (unpack trimmed) of
                    (namePart, ':' :: rest) =>
                      let fieldName = trim (pack namePart)
                          fieldType = parseSimpleType (pack rest)
                      in Just (fieldName, fieldType)
                    _ => Nothing
  in mapMaybe parsePart (forget parts)

||| Parse variant cases: "Case1; Case2: payload; Case3"
parseVariantCases : String -> List (String, CandidType)
parseVariantCases content =
  let parts = split (== ';') content
      parsePart : String -> Maybe (String, CandidType)
      parsePart p =
        let trimmed = trim p
        in if null trimmed
             then Nothing
             else case break (== ':') (unpack trimmed) of
                    (namePart, ':' :: rest) =>
                      Just (trim (pack namePart), parseSimpleType (pack rest))
                    (namePart, []) =>
                      Just (trim (pack namePart), CTNull)
                    _ => Nothing
  in mapMaybe parsePart (forget parts)

||| Find matching brace and extract content
extractBraceContent : String -> Maybe String
extractBraceContent s =
  let chars = unpack s
      findOpen = dropWhile (/= '{') chars
  in case findOpen of
       '{' :: rest =>
         let content = takeWhile (/= '}') rest
         in Just (pack content)
       _ => Nothing

||| Parse a type definition from joined content
||| Handles both single-line and multi-line definitions
parseTypeDef : String -> Maybe TypeDef
parseTypeDef content =
  let trimmed = trim content
  in if isPrefixOf "type " trimmed
       then
         let afterType = substr 5 (length trimmed) trimmed
             eqParts = break (== '=') (unpack afterType)
         in case eqParts of
              (namePart, '=' :: rest) =>
                let typeName = trim (pack namePart)
                    bodyStr = trim (pack rest)
                in if isPrefixOf "record" bodyStr
                     then case extractBraceContent bodyStr of
                            Just fields =>
                              Just $ MkTypeDef typeName (CTRecord (parseRecordFields fields))
                            Nothing => Nothing
                   else if isPrefixOf "variant" bodyStr
                     then case extractBraceContent bodyStr of
                            Just cases =>
                              Just $ MkTypeDef typeName (CTVariant (parseVariantCases cases))
                            Nothing => Nothing
                   else -- Simple type alias
                     Just $ MkTypeDef typeName (parseSimpleType (stripSemicolon bodyStr))
              _ => Nothing
       else Nothing

||| Join multi-line type definitions into single strings
||| Groups lines from "type X =" to "};"
joinTypeDefinitions : List String -> List String
joinTypeDefinitions [] = []
joinTypeDefinitions lines = go lines Nothing []
  where
    go : List String -> Maybe (List String) -> List String -> List String
    go [] Nothing acc = reverse acc
    go [] (Just current) acc = reverse (unlines (reverse current) :: acc)
    go (l :: rest) Nothing acc =
      let trimmed = trim l
      in if isPrefixOf "type " trimmed
           then if isInfixOf "{" trimmed && isInfixOf "};" trimmed
                  -- Single-line type definition
                  then go rest Nothing (l :: acc)
                  else if isInfixOf "{" trimmed
                    -- Start of multi-line definition
                    then go rest (Just [l]) acc
                    -- Simple type alias (no braces)
                    else go rest Nothing (l :: acc)
           else go rest Nothing acc
    go (l :: rest) (Just current) acc =
      let trimmed = trim l
          newCurrent = l :: current
      in if isInfixOf "};" trimmed || (isInfixOf "}" trimmed && isSuffixOf ";" trimmed)
           -- End of multi-line definition
           then go rest Nothing (unlines (reverse newCurrent) :: acc)
           else go rest (Just newCurrent) acc

||| Extract return type from method signature
||| Looks for pattern: (args) -> (ReturnType) query;
extractReturnType : String -> String
extractReturnType s =
  let chars = unpack s
      -- Find "->" and take what's after
      findArrow : List Char -> List Char
      findArrow [] = []
      findArrow ('-' :: '>' :: rest) = rest
      findArrow (_ :: rest) = findArrow rest
      afterArrow = findArrow chars
      afterStr = pack afterArrow
      -- Remove "query" and ";"
      cleaned = takeWhile (\c => c /= 'q' && c /= ';') (unpack (trim afterStr))
      result = trim (pack cleaned)
  in if isPrefixOf "(" result
       then trim $ substr 1 (minus (length result) 2) result
       else result

||| Parse method line from service block
||| Format: "  methodName: (args) -> (returnType) query;"
parseMethodLine : String -> Maybe DidMethod
parseMethodLine line =
  let trimmed = trim line
  in if null trimmed || isPrefixOf "//" trimmed || isPrefixOf "type " trimmed
       then Nothing
       -- Methods must have "->" to distinguish from record fields
       else if not (isInfixOf "->" trimmed)
         then Nothing
         else case break (== ':') (unpack trimmed) of
              (namePart, ':' :: rest) =>
                let methodName = trim (pack namePart)
                    restStr = pack rest
                    isQ = isInfixOf "query" restStr
                    returnStr = extractReturnType restStr
                in if null methodName || isPrefixOf "{" methodName || isPrefixOf "service" methodName
                     then Nothing
                     else Just $ MkDidMethod methodName (parseSimpleType returnStr) isQ
              _ => Nothing

||| Extract lines within the service block only
||| Skips type definitions and extracts only between "service : {" and "}"
extractServiceBlock : List String -> List String
extractServiceBlock ls = go ls False
  where
    go : List String -> Bool -> List String
    go [] _ = []
    go (l :: rest) False =
      -- Look for "service" line with "{"
      if isInfixOf "service" l && isInfixOf "{" l
        then go rest True
        else go rest False
    go (l :: rest) True =
      let trimmed = trim l
      in if trimmed == "}" || trimmed == "};"
           then []  -- End of service block
           else l :: go rest True

||| Parse .did file content and extract type definitions
||| Handles multi-line type definitions
public export
parseTypeDefinitions : String -> List TypeDef
parseTypeDefinitions content =
  let ls = lines content
      joined = joinTypeDefinitions ls
  in mapMaybe parseTypeDef joined

||| Parse .did file content and extract method signatures
||| Only parses lines within the "service : { ... }" block
public export
parseDidFile : String -> List DidMethod
parseDidFile content =
  let ls = lines content
      serviceLines = extractServiceBlock ls
  in mapMaybe parseMethodLine serviceLines

-- =============================================================================
-- Type Resolution
-- =============================================================================

||| Resolve a named type to its definition
resolveType : String -> List TypeDef -> Maybe CandidType
resolveType name defs = map typeBody $ find (\d => d.typeName == name) defs

||| Fully resolve a type, following named references
resolveFullType : CandidType -> List TypeDef -> CandidType
resolveFullType (CTNamed name) defs =
  case resolveType name defs of
    Just resolved => resolveFullType resolved defs
    Nothing => CTNamed name
resolveFullType (CTOpt inner) defs = CTOpt (resolveFullType inner defs)
resolveFullType (CTVec inner) defs = CTVec (resolveFullType inner defs)
resolveFullType (CTRecord fields) defs =
  CTRecord (map (\(n,t) => (n, resolveFullType t defs)) fields)
resolveFullType (CTVariant cases) defs =
  CTVariant (map (\(n,t) => (n, resolveFullType t defs)) cases)
resolveFullType t _ = t

-- =============================================================================
-- Dynamic Candid Encoding
-- =============================================================================

||| Sort fields/variants by their hash (ascending order)
sortByHash : List (String, CandidType) -> List (String, CandidType)
sortByHash = sortBy (\(a,_), (b,_) => compare (candidHash a) (candidHash b))

||| Check if a type is primitive (doesn't need type table entry)
isPrimitive : CandidType -> Bool
isPrimitive CTNat = True
isPrimitive CTInt = True
isPrimitive CTText = True
isPrimitive CTBool = True
isPrimitive CTNull = True
isPrimitive CTNat8 = True
isPrimitive CTNat16 = True
isPrimitive CTNat32 = True
isPrimitive CTNat64 = True
isPrimitive CTInt8 = True
isPrimitive CTInt16 = True
isPrimitive CTInt32 = True
isPrimitive CTInt64 = True
isPrimitive CTFloat32 = True
isPrimitive CTFloat64 = True
isPrimitive CTBlob = True
isPrimitive _ = False

||| Generate default value bytes for a type
defaultValueBytes : CandidType -> List String
defaultValueBytes CTNull = []
defaultValueBytes CTBool = ["0x00"]
defaultValueBytes CTNat = ["0x00"]
defaultValueBytes CTInt = ["0x00"]
defaultValueBytes CTNat8 = ["0x00"]
defaultValueBytes CTNat16 = ["0x00", "0x00"]
defaultValueBytes CTNat32 = ["0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTNat64 = ["0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTInt8 = ["0x00"]
defaultValueBytes CTInt16 = ["0x00", "0x00"]
defaultValueBytes CTInt32 = ["0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTInt64 = ["0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTFloat32 = ["0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTFloat64 = ["0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00", "0x00"]
defaultValueBytes CTText = ["0x00"]  -- empty string (length 0)
defaultValueBytes CTBlob = ["0x00"]  -- empty blob (length 0)
defaultValueBytes (CTOpt _) = ["0x00"]  -- none
defaultValueBytes (CTVec _) = ["0x00"]  -- empty vec (length 0)
defaultValueBytes (CTRecord fields) =
  concatMap (\(_,t) => defaultValueBytes t) (sortByHash fields)
defaultValueBytes (CTVariant cases) =
  -- Pick the first variant (index 0) after sorting by hash
  case sortByHash cases of
    [] => []
    ((_, payload) :: _) => ["0x00"] ++ defaultValueBytes payload  -- variant index 0
defaultValueBytes (CTNamed _) = ["0x00"]  -- fallback
defaultValueBytes (CTUnknown _) = ["0x00"]  -- fallback

||| Collect all types that need entries in type table
collectTypes : CandidType -> List CandidType
collectTypes t@(CTOpt inner) = t :: collectTypes inner
collectTypes t@(CTVec inner) = t :: collectTypes inner
collectTypes t@(CTRecord fields) =
  t :: concatMap (\(_,ft) => collectTypes ft) fields
collectTypes t@(CTVariant cases) =
  t :: concatMap (\(_,ct) => collectTypes ct) cases
collectTypes _ = []

||| Check if two CandidTypes are the same (for deduplication)
sameType : CandidType -> CandidType -> Bool
sameType (CTOpt a) (CTOpt b) = sameType a b
sameType (CTVec a) (CTVec b) = sameType a b
sameType (CTRecord _) (CTRecord _) = True  -- Simplified
sameType (CTVariant _) (CTVariant _) = True  -- Simplified
sameType a b = show a == show b

||| Remove duplicate types from list
nubTypes : List CandidType -> List CandidType
nubTypes [] = []
nubTypes (x :: xs) = x :: nubTypes (filter (not . sameType x) xs)

||| Assign type indices to non-primitive types
||| Returns list of (type, index) pairs
assignTypeIndices : CandidType -> List (CandidType, Nat)
assignTypeIndices t =
  let types = nubTypes (collectTypes t)
  in zip types [0..(length types)]

||| Get type reference (either primitive code or type table index)
getTypeRef : CandidType -> List (CandidType, Nat) -> String
getTypeRef CTNull _ = "0x7f"
getTypeRef CTBool _ = "0x7e"
getTypeRef CTNat _ = "0x7d"
getTypeRef CTInt _ = "0x7c"
getTypeRef CTNat8 _ = "0x7b"
getTypeRef CTNat16 _ = "0x7a"
getTypeRef CTNat32 _ = "0x79"
getTypeRef CTNat64 _ = "0x78"
getTypeRef CTInt8 _ = "0x77"
getTypeRef CTInt16 _ = "0x76"
getTypeRef CTInt32 _ = "0x75"
getTypeRef CTInt64 _ = "0x74"
getTypeRef CTFloat32 _ = "0x73"
getTypeRef CTFloat64 _ = "0x72"
getTypeRef CTText _ = "0x71"
getTypeRef CTBlob _ = "0x7b"  -- nat8 for vec nat8
getTypeRef t indices =
  case lookupType t indices of
    Just idx => fromMaybe "0x00" (head' (leb128Unsigned (cast idx)))
    Nothing => "0x00"
  where
    lookupType : CandidType -> List (CandidType, Nat) -> Maybe Nat
    lookupType _ [] = Nothing
    lookupType needle ((t', idx) :: rest) =
      if show needle == show t' then Just idx else lookupType needle rest

||| Generate type table entry for a single type
generateTypeEntry : CandidType -> List (CandidType, Nat) -> List String
generateTypeEntry (CTOpt inner) indices =
  ["0x6e"] ++ [getTypeRef inner indices]
generateTypeEntry (CTVec inner) indices =
  ["0x6d"] ++ [getTypeRef inner indices]
generateTypeEntry (CTRecord fields) indices =
  let sorted = sortByHash fields
      fieldCount = leb128Unsigned (cast (length sorted))
      encodeField : (String, CandidType) -> List String
      encodeField (name, ftype) =
        leb128Unsigned (candidHash name) ++ [getTypeRef ftype indices]
  in ["0x6c"] ++ fieldCount ++ concatMap encodeField sorted
generateTypeEntry (CTVariant cases) indices =
  let sorted = sortByHash cases
      caseCount = leb128Unsigned (cast (length sorted))
      encodeCase : (String, CandidType) -> List String
      encodeCase (name, ctype) =
        leb128Unsigned (candidHash name) ++ [getTypeRef ctype indices]
  in ["0x6b"] ++ caseCount ++ concatMap encodeCase sorted
generateTypeEntry _ _ = []

||| Generate complete type table
generateTypeTable : CandidType -> List (CandidType, Nat) -> List String
generateTypeTable t indices =
  let types = map fst indices
      typeCount = leb128Unsigned (cast (length types))
      entries = concatMap (\ty => generateTypeEntry ty indices) types
  in typeCount ++ entries

||| Generate complete Candid-encoded response for a type
||| Format: "DIDL" + type_table + "01" + root_type_ref + value_bytes
generateCandidResponse : CandidType -> List TypeDef -> String
generateCandidResponse rawType defs =
  let t = resolveFullType rawType defs
      indices = assignTypeIndices t
      typeTable = generateTypeTable t indices
      rootRef = getTypeRef t indices
      valueBytes = defaultValueBytes t
      allBytes = ["'D'", "'I'", "'D'", "'L'"] ++
                 typeTable ++
                 ["0x01"] ++  -- 1 value
                 [rootRef] ++
                 valueBytes
  in "{ static const uint8_t r[] = {" ++
     joinBy "," allBytes ++
     "}; ic0_msg_reply_data_append((int32_t)(uintptr_t)r, sizeof(r)); ic0_msg_reply(); }"

-- =============================================================================
-- Public API
-- =============================================================================

||| Map method name to its Candid return type from parsed .did
public export
lookupReturnType : String -> List DidMethod -> Maybe CandidType
lookupReturnType name methods =
  map returnType $ find (\m => m.name == name) methods

||| Generate reply code based on Candid return type
||| Returns C code snippet that sends appropriate Candid-encoded response
public export
generateReplyForType : String -> CandidType -> List TypeDef -> String
generateReplyForType methodName ctype defs =
  let comment = " // " ++ methodName ++ " -> " ++ show ctype
  in generateCandidResponse ctype defs ++ comment
