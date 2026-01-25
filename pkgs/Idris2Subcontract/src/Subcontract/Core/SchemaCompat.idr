||| Subcontract Core: Schema Compatibility Analysis
|||
||| Provides Eq/Show instances for Schema types and pure validation logic
||| for detecting storage collisions in schema upgrades.
|||
||| Used by SchemaCheck.idr for compile-time validation via Elab.
module Subcontract.Core.SchemaCompat

import public Subcontract.Core.Schema

import Data.List
import Data.String

%default total

-- =============================================================================
-- Eq Implementations
-- =============================================================================

public export
Eq SType where
  TUint256  == TUint256  = True
  TUint128  == TUint128  = True
  TUint64   == TUint64   = True
  TUint32   == TUint32   = True
  TUint8    == TUint8    = True
  TInt256   == TInt256   = True
  TAddress  == TAddress  = True
  TBool     == TBool     = True
  TBytes32  == TBytes32  = True
  TBytes4   == TBytes4   = True
  _         == _         = False

public export
Eq Field where
  (Value n1 t1)            == (Value n2 t2)            = n1 == n2 && t1 == t2
  (Mapping n1 k1 v1)       == (Mapping n2 k2 v2)       = n1 == n2 && k1 == k2 && v1 == v2
  (Mapping2 n1 k1a k1b v1) == (Mapping2 n2 k2a k2b v2) = n1 == n2 && k1a == k2a && k1b == k2b && v1 == v2
  (Array n1 e1)            == (Array n2 e2)            = n1 == n2 && e1 == e2
  _                        == _                        = False

-- =============================================================================
-- Show Implementations (Solidity notation)
-- =============================================================================

public export
Show SType where
  show TUint256 = "uint256"
  show TUint128 = "uint128"
  show TUint64  = "uint64"
  show TUint32  = "uint32"
  show TUint8   = "uint8"
  show TInt256  = "int256"
  show TAddress = "address"
  show TBool    = "bool"
  show TBytes32 = "bytes32"
  show TBytes4  = "bytes4"

public export
Show Field where
  show (Value n t)          = n ++ " : " ++ show t
  show (Mapping n k v)      = "mapping(" ++ show k ++ " => " ++ show v ++ ") " ++ n
  show (Mapping2 n k1 k2 v) = "mapping(" ++ show k1 ++ " => mapping(" ++ show k2 ++ " => " ++ show v ++ ")) " ++ n
  show (Array n e)          = show e ++ "[] " ++ n

-- =============================================================================
-- Collision Classification
-- =============================================================================

||| Types of storage collisions detectable at compile time
public export
data CollisionType
  = FieldRemoved      -- Field in V1 not present in V2
  | FieldReordered    -- Field exists but at different slot offset
  | TypeChanged       -- Same field name, different type
  | FieldInserted     -- New field inserted before existing
  | NamespaceChanged  -- ERC-7201 namespace ID changed
  | RootSlotChanged   -- Root slot changed (catastrophic)

public export
Eq CollisionType where
  FieldRemoved     == FieldRemoved     = True
  FieldReordered   == FieldReordered   = True
  TypeChanged      == TypeChanged      = True
  FieldInserted    == FieldInserted    = True
  NamespaceChanged == NamespaceChanged = True
  RootSlotChanged  == RootSlotChanged  = True
  _                == _                = False

public export
Show CollisionType where
  show FieldRemoved     = "FIELD_REMOVED"
  show FieldReordered   = "FIELD_REORDERED"
  show TypeChanged      = "TYPE_CHANGED"
  show FieldInserted    = "FIELD_INSERTED"
  show NamespaceChanged = "NAMESPACE_CHANGED"
  show RootSlotChanged  = "ROOT_SLOT_CHANGED"

||| A detected collision with full diagnostic information
public export
record SchemaCollision where
  constructor MkCollision
  collisionType : CollisionType
  fieldName     : Maybe String
  oldPosition   : Maybe Nat
  newPosition   : Maybe Nat
  oldField      : Maybe Field
  newField      : Maybe Field
  explanation   : String

||| Format a collision for display
public export
Show SchemaCollision where
  show c = unlines
    [ "=== STORAGE COLLISION: " ++ show c.collisionType ++ " ==="
    , maybe "" (\n => "Field: " ++ n) c.fieldName
    , maybe "" (\p => "Old position: slot+" ++ show p) c.oldPosition
    , maybe "" (\p => "New position: slot+" ++ show p) c.newPosition
    , maybe "" (\f => "Old definition: " ++ show f) c.oldField
    , maybe "" (\f => "New definition: " ++ show f) c.newField
    , "Reason: " ++ c.explanation
    ]

||| Result of schema compatibility check
public export
data CompatResult
  = Compatible
  | Incompatible (List SchemaCollision)

||| Check if result indicates compatibility
public export
isCompatible : CompatResult -> Bool
isCompatible Compatible = True
isCompatible (Incompatible _) = False

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Show field type in Solidity notation (for error messages)
showFieldType : Field -> String
showFieldType (Value _ t)        = show t
showFieldType (Mapping _ k v)    = "mapping(" ++ show k ++ " => " ++ show v ++ ")"
showFieldType (Mapping2 _ k1 k2 v) = "mapping(" ++ show k1 ++ " => mapping(" ++ show k2 ++ " => " ++ show v ++ "))"
showFieldType (Array _ e)        = show e ++ "[]"

||| Map with index (0-based)
mapWithIndex : (Nat -> a -> b) -> List a -> List b
mapWithIndex f xs = go 0 xs
  where
    go : Nat -> List a -> List b
    go _ [] = []
    go i (x :: xs') = f i x :: go (S i) xs'

-- =============================================================================
-- Core Validation Logic
-- =============================================================================

||| Check field at given position matches between schemas
checkFieldAt : Nat -> Field -> List Field -> Maybe SchemaCollision
checkFieldAt pos oldField newFields =
  case inBounds pos newFields of
    Yes prf =>
      let newField = index pos newFields in
      if oldField == newField
        then Nothing  -- Match - no collision
        else if fieldName oldField == fieldName newField
          then Just $ MkCollision
            { collisionType = TypeChanged
            , fieldName     = Just (fieldName oldField)
            , oldPosition   = Just pos
            , newPosition   = Just pos
            , oldField      = Just oldField
            , newField      = Just newField
            , explanation   = "Type changed from " ++ showFieldType oldField ++ " to " ++ showFieldType newField
            }
          else Just $ MkCollision
            { collisionType = FieldReordered
            , fieldName     = Just (fieldName oldField)
            , oldPosition   = Just pos
            , newPosition   = Nothing
            , oldField      = Just oldField
            , newField      = Just newField
            , explanation   = "Expected '" ++ fieldName oldField ++ "' but found '" ++ fieldName newField ++ "'"
            }
    No _ => Just $ MkCollision
      { collisionType = FieldRemoved
      , fieldName     = Just (fieldName oldField)
      , oldPosition   = Just pos
      , newPosition   = Nothing
      , oldField      = Just oldField
      , newField      = Nothing
      , explanation   = "Field '" ++ fieldName oldField ++ "' at slot+" ++ show pos ++ " was removed"
      }

||| Validate schema upgrade - pure function
|||
||| Checks that `new` is a valid append-only extension of `old`:
||| - Same namespace ID
||| - Same root slot
||| - All fields from `old` present in `new` at the same positions
||| - New fields only appended at the end
|||
||| @ old The existing schema (V1)
||| @ new The proposed schema (V2)
export
checkSchemaCompat : (old : Schema) -> (new : Schema) -> CompatResult
checkSchemaCompat old new =
  let nsCollision = if old.nsId /= new.nsId
        then [MkCollision
              { collisionType = NamespaceChanged
              , fieldName     = Nothing
              , oldPosition   = Nothing
              , newPosition   = Nothing
              , oldField      = Nothing
              , newField      = Nothing
              , explanation   = "Namespace changed from '" ++ old.nsId ++ "' to '" ++ new.nsId ++ "'"
              }]
        else []
      rootCollision = if old.rootSlot /= new.rootSlot
        then [MkCollision
              { collisionType = RootSlotChanged
              , fieldName     = Nothing
              , oldPosition   = Nothing
              , newPosition   = Nothing
              , oldField      = Nothing
              , newField      = Nothing
              , explanation   = "Root slot changed from " ++ show old.rootSlot ++ " to " ++ show new.rootSlot
              }]
        else []
      fieldCollisions = catMaybes $ mapWithIndex (\i, f => checkFieldAt i f new.fields) old.fields
      allCollisions = nsCollision ++ rootCollision ++ fieldCollisions
  in if null allCollisions then Compatible else Incompatible allCollisions
