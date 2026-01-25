||| Subcontract Core: Declarative Storage Schema
|||
||| Provides human-readable storage layout definitions similar to Solidity structs.
||| Now integrated with EVM.Storage.Storable for type-safe access.
|||
||| Instead of manual slot calculations:
||| ```idris
||| SLOT_BALANCES : Integer
||| SLOT_BALANCES = 0x1234... + 1
||| slot <- mappingSlot SLOT_BALANCES addr
||| ```
|||
||| Use declarative schema with type-safe Ref:
||| ```idris
||| TokenSchema : Schema
||| TokenSchema = MkSchema "example.token" 0x1234...
|||   [ Value "totalSupply" TUint256
|||   , Mapping "balances" TAddress TUint256
|||   ]
|||
||| -- Get typed reference, then use Storable get/set
||| Just ref <- schemaValueRef TokenSchema "totalSupply"
||| supply <- get ref
||| ```
|||
||| Mirrors Solidity's struct-based storage:
||| ```solidity
||| /// @custom:storage-location erc7201:example.token
||| struct $Token {
|||     uint256 totalSupply;
|||     mapping(address => uint256) balances;
||| }
||| ```
module Subcontract.Core.Schema

import public EVM.Primitives
import public EVM.Storage.Namespace
import public Subcontract.Core.Storable

-- =============================================================================
-- Storage Types
-- =============================================================================

||| Solidity-like storage types
public export
data SType
  = TUint256
  | TUint128
  | TUint64
  | TUint32
  | TUint8
  | TInt256
  | TAddress
  | TBool
  | TBytes32
  | TBytes4

||| Get slot size (currently all types use 1 slot)
export
stypeSlots : SType -> Integer
stypeSlots _ = 1

-- =============================================================================
-- Field Definitions
-- =============================================================================

||| Storage field definition
public export
data Field : Type where
  ||| Simple value: `uint256 totalSupply;`
  Value : (name : String) -> (ty : SType) -> Field

  ||| Mapping: `mapping(address => uint256) balances;`
  Mapping : (name : String) -> (keyTy : SType) -> (valTy : SType) -> Field

  ||| Nested mapping: `mapping(address => mapping(address => uint256)) allowances;`
  Mapping2 : (name : String) -> (key1Ty : SType) -> (key2Ty : SType) -> (valTy : SType) -> Field

  ||| Dynamic array: `address[] members;`
  Array : (name : String) -> (elemTy : SType) -> Field

||| Get field name
export
fieldName : Field -> String
fieldName (Value n _) = n
fieldName (Mapping n _ _) = n
fieldName (Mapping2 n _ _ _) = n
fieldName (Array n _) = n

-- =============================================================================
-- Schema Definition
-- =============================================================================

||| Storage schema - mirrors Solidity struct
|||
||| Example:
||| ```idris
||| TokenSchema : Schema
||| TokenSchema = MkSchema "example.token" 0x1234...
|||   [ Value "totalSupply" TUint256
|||   , Mapping "balances" TAddress TUint256
|||   , Mapping2 "allowances" TAddress TAddress TUint256
|||   , Array "holders" TAddress
|||   ]
||| ```
public export
record Schema where
  constructor MkSchema
  nsId : String         -- ERC-7201 namespace ID (e.g., "example.token")
  rootSlot : Integer    -- Pre-computed root slot
  fields : List Field   -- Field definitions in order

-- =============================================================================
-- Field Lookup
-- =============================================================================

||| Find field by name
findField : String -> List Field -> Maybe (Field, Integer)
findField name = go 0
  where
    go : Integer -> List Field -> Maybe (Field, Integer)
    go _ [] = Nothing
    go offset (f :: fs) =
      if fieldName f == name
        then Just (f, offset)
        else go (offset + 1) fs

||| Get offset of field by name
export
fieldOffset : Schema -> String -> Maybe Integer
fieldOffset schema name = map snd (findField name schema.fields)

-- =============================================================================
-- Type-Safe Ref Access (NEW - Storable integration)
-- =============================================================================

||| Get a typed reference to a value field.
||| Returns Ref Bits256 for use with Storable get/set.
|||
||| Example:
||| ```idris
||| Just ref <- schemaValueRef TokenSchema "totalSupply"
||| supply <- get ref   -- type-safe!
||| set ref 1000        -- type-safe!
||| ```
export
schemaValueRef : Schema -> String -> Maybe (Ref Bits256)
schemaValueRef schema name =
  case fieldOffset schema name of
    Nothing => Nothing
    Just offset => Just (MkRef (schema.rootSlot + offset))

||| Get a typed reference to a mapping entry.
|||
||| Example:
||| ```idris
||| ref <- schemaMappingRef TokenSchema "balances" userAddr
||| balance <- get ref
||| ```
export
schemaMappingRef : Schema -> String -> Integer -> IO (Maybe (Ref Bits256))
schemaMappingRef schema name key =
  case fieldOffset schema name of
    Nothing => pure Nothing
    Just offset => do
      let baseSlot = schema.rootSlot + offset
      slot <- mappingSlot baseSlot key
      pure (Just (MkRef slot))

||| Get a typed reference to a nested mapping entry.
export
schemaMapping2Ref : Schema -> String -> Integer -> Integer -> IO (Maybe (Ref Bits256))
schemaMapping2Ref schema name key1 key2 =
  case fieldOffset schema name of
    Nothing => pure Nothing
    Just offset => do
      let baseSlot = schema.rootSlot + offset
      slot <- nestedMappingSlot baseSlot key1 key2
      pure (Just (MkRef slot))

||| Get a typed reference to an array element.
export
schemaArrayRef : Schema -> String -> Integer -> IO (Maybe (Ref Bits256))
schemaArrayRef schema name idx =
  case fieldOffset schema name of
    Nothing => pure Nothing
    Just offset => do
      let baseSlot = schema.rootSlot + offset
      slot <- arrayElementSlot baseSlot idx 1
      pure (Just (MkRef slot))

-- =============================================================================
-- Legacy Value Access (for backwards compatibility)
-- =============================================================================

||| Read a value field
|||
||| Example:
||| ```idris
||| supply <- schemaValue TokenSchema "totalSupply"
||| ```
export
schemaValue : Schema -> String -> IO (Maybe Integer)
schemaValue schema name =
  case schemaValueRef schema name of
    Nothing => pure Nothing
    Just ref => do
      val <- get ref
      pure (Just val)

||| Write a value field
export
schemaSetValue : Schema -> String -> Integer -> IO Bool
schemaSetValue schema name val =
  case schemaValueRef schema name of
    Nothing => pure False
    Just ref => do
      set ref val
      pure True

-- =============================================================================
-- Legacy Mapping Access (uses Ref internally)
-- =============================================================================

||| Read from a mapping field
export
schemaMapping : Schema -> String -> Integer -> IO (Maybe Integer)
schemaMapping schema name key = do
  mref <- schemaMappingRef schema name key
  case mref of
    Nothing => pure Nothing
    Just ref => Just <$> get ref

||| Write to a mapping field
export
schemaSetMapping : Schema -> String -> Integer -> Integer -> IO Bool
schemaSetMapping schema name key val = do
  mref <- schemaMappingRef schema name key
  case mref of
    Nothing => pure False
    Just ref => do
      set ref val
      pure True

-- =============================================================================
-- Legacy Nested Mapping Access
-- =============================================================================

||| Read from a nested mapping field
export
schemaMapping2 : Schema -> String -> Integer -> Integer -> IO (Maybe Integer)
schemaMapping2 schema name key1 key2 = do
  mref <- schemaMapping2Ref schema name key1 key2
  case mref of
    Nothing => pure Nothing
    Just ref => Just <$> get ref

||| Write to a nested mapping field
export
schemaSetMapping2 : Schema -> String -> Integer -> Integer -> Integer -> IO Bool
schemaSetMapping2 schema name key1 key2 val = do
  mref <- schemaMapping2Ref schema name key1 key2
  case mref of
    Nothing => pure False
    Just ref => do
      set ref val
      pure True

-- =============================================================================
-- Legacy Array Access
-- =============================================================================

||| Get array length
export
schemaArrayLength : Schema -> String -> IO (Maybe Integer)
schemaArrayLength schema name =
  case fieldOffset schema name of
    Nothing => pure Nothing
    Just offset => do
      len <- arrayLength (schema.rootSlot + offset)
      pure (Just len)

||| Read array element
export
schemaArrayAt : Schema -> String -> Integer -> IO (Maybe Integer)
schemaArrayAt schema name idx = do
  mref <- schemaArrayRef schema name idx
  case mref of
    Nothing => pure Nothing
    Just ref => Just <$> get ref

||| Push to array (increment length and write element)
export
schemaArrayPush : Schema -> String -> Integer -> IO Bool
schemaArrayPush schema name val =
  case fieldOffset schema name of
    Nothing => pure False
    Just offset => do
      let baseSlot = schema.rootSlot + offset
      len <- arrayLength baseSlot
      -- Write new element via Ref
      slot <- arrayElementSlot baseSlot len 1
      set (MkRef slot) val
      -- Increment length
      set (MkRef baseSlot) (len + 1)
      pure True

-- =============================================================================
-- Schema Validation (compile-time info)
-- =============================================================================

||| Get total number of slots used by schema
export
schemaSlotCount : Schema -> Integer
schemaSlotCount schema = cast (length schema.fields)

||| List all field names
export
schemaFieldNames : Schema -> List String
schemaFieldNames schema = map fieldName schema.fields

-- =============================================================================
-- Example Schemas
-- =============================================================================

||| Example: ERC20-like token schema
|||
||| Mirrors:
||| ```solidity
||| /// @custom:storage-location erc7201:example.token
||| struct $Token {
|||     uint256 totalSupply;
|||     mapping(address => uint256) balances;
|||     mapping(address => mapping(address => uint256)) allowances;
|||     address[] holders;
||| }
||| ```
export
exampleTokenSchema : Integer -> Schema
exampleTokenSchema rootSlot = MkSchema "example.token" rootSlot
  [ Value "totalSupply" TUint256
  , Mapping "balances" TAddress TUint256
  , Mapping2 "allowances" TAddress TAddress TUint256
  , Array "holders" TAddress
  ]

||| Example: Member list schema
export
exampleMemberSchema : Integer -> Schema
exampleMemberSchema rootSlot = MkSchema "example.members" rootSlot
  [ Array "members" TAddress
  , Mapping "isMember" TAddress TBool
  , Value "memberCount" TUint256
  ]
