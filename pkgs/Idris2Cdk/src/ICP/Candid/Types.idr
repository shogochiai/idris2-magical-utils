||| Candid Type System
|||
||| This module defines the Candid type system for ICP inter-canister communication.
||| Candid is a language-agnostic interface description language (IDL).
|||
||| Reference: https://github.com/dfinity/candid/blob/master/spec/Candid.md
module ICP.Candid.Types

import Data.List
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Candid Type Codes (used in binary encoding)
--------------------------------------------------------------------------------

||| Candid primitive type codes (negative numbers in signed LEB128)
public export
data TypeCode : Type where
  TNull      : TypeCode  -- -1
  TBool      : TypeCode  -- -2
  TNat       : TypeCode  -- -3
  TInt       : TypeCode  -- -4
  TNat8      : TypeCode  -- -5
  TNat16     : TypeCode  -- -6
  TNat32     : TypeCode  -- -7
  TNat64     : TypeCode  -- -8
  TInt8      : TypeCode  -- -9
  TInt16     : TypeCode  -- -10
  TInt32     : TypeCode  -- -11
  TInt64     : TypeCode  -- -12
  TFloat32   : TypeCode  -- -13
  TFloat64   : TypeCode  -- -14
  TText      : TypeCode  -- -15
  TReserved  : TypeCode  -- -16
  TEmpty     : TypeCode  -- -17
  -- Compound types (also negative)
  TOpt       : TypeCode  -- -18
  TVec       : TypeCode  -- -19
  TRecord    : TypeCode  -- -20
  TVariant   : TypeCode  -- -21
  TFunc      : TypeCode  -- -22
  TService   : TypeCode  -- -23
  TPrincipal : TypeCode  -- -24

||| Convert TypeCode to its numeric value
public export
typeCodeValue : TypeCode -> Int
typeCodeValue TNull      = -1
typeCodeValue TBool      = -2
typeCodeValue TNat       = -3
typeCodeValue TInt       = -4
typeCodeValue TNat8      = -5
typeCodeValue TNat16     = -6
typeCodeValue TNat32     = -7
typeCodeValue TNat64     = -8
typeCodeValue TInt8      = -9
typeCodeValue TInt16     = -10
typeCodeValue TInt32     = -11
typeCodeValue TInt64     = -12
typeCodeValue TFloat32   = -13
typeCodeValue TFloat64   = -14
typeCodeValue TText      = -15
typeCodeValue TReserved  = -16
typeCodeValue TEmpty     = -17
typeCodeValue TOpt       = -18
typeCodeValue TVec       = -19
typeCodeValue TRecord    = -20
typeCodeValue TVariant   = -21
typeCodeValue TFunc      = -22
typeCodeValue TService   = -23
typeCodeValue TPrincipal = -24

--------------------------------------------------------------------------------
-- Candid Values (IDL Value representation)
--------------------------------------------------------------------------------

||| Hash for field names (used in records and variants)
||| Candid uses a specific hash function for field name deduplication
public export
FieldHash : Type
FieldHash = Nat

||| Candid value representation
||| This is an AST for Candid values that can be serialized/deserialized
public export
data CandidValue : Type where
  -- Primitive types
  CNull      : CandidValue
  CBool      : Bool -> CandidValue
  CNat       : Nat -> CandidValue
  CInt       : Integer -> CandidValue
  CNat8      : Bits8 -> CandidValue
  CNat16     : Bits16 -> CandidValue
  CNat32     : Bits32 -> CandidValue
  CNat64     : Bits64 -> CandidValue
  CInt8      : Int8 -> CandidValue
  CInt16     : Int16 -> CandidValue
  CInt32     : Int32 -> CandidValue
  CInt64     : Int64 -> CandidValue
  CFloat32   : Double -> CandidValue  -- Idris2 doesn't have Float32
  CFloat64   : Double -> CandidValue
  CText      : String -> CandidValue
  CReserved  : CandidValue
  CEmpty     : CandidValue
  -- Compound types
  COpt       : Maybe CandidValue -> CandidValue
  CVec       : List CandidValue -> CandidValue
  CRecord    : List (FieldHash, CandidValue) -> CandidValue
  CVariant   : FieldHash -> CandidValue -> CandidValue
  CPrincipal : List Bits8 -> CandidValue  -- Raw principal bytes
  CBlob      : List Bits8 -> CandidValue  -- vec nat8 shorthand
  -- Functions and services (opaque references)
  CFunc      : List Bits8 -> String -> CandidValue  -- principal + method name
  CService   : List Bits8 -> CandidValue  -- principal

--------------------------------------------------------------------------------
-- Candid Type Descriptors
--------------------------------------------------------------------------------

||| Function mode
public export
data FuncMode = Query | Oneway | CompositeQuery

mutual
  ||| Field descriptor for records and variants
  public export
  record FieldType where
    constructor MkFieldType
    hash : FieldHash
    name : Maybe String  -- Original name (optional, for debugging)
    typ  : CandidType

  ||| Candid type descriptor
  ||| Used for type table in binary encoding
  public export
  data CandidType : Type where
    -- Primitive types (no additional data needed)
    CtNull      : CandidType
    CtBool      : CandidType
    CtNat       : CandidType
    CtInt       : CandidType
    CtNat8      : CandidType
    CtNat16     : CandidType
    CtNat32     : CandidType
    CtNat64     : CandidType
    CtInt8      : CandidType
    CtInt16     : CandidType
    CtInt32     : CandidType
    CtInt64     : CandidType
    CtFloat32   : CandidType
    CtFloat64   : CandidType
    CtText      : CandidType
    CtReserved  : CandidType
    CtEmpty     : CandidType
    CtPrincipal : CandidType
    -- Compound types (reference inner types by index)
    CtOpt       : CandidType -> CandidType
    CtVec       : CandidType -> CandidType
    CtRecord    : List FieldType -> CandidType
    CtVariant   : List FieldType -> CandidType
    -- Function type
    CtFunc      : (args : List CandidType)
               -> (rets : List CandidType)
               -> (modes : List FuncMode)
               -> CandidType
    -- Service type (list of method name -> function type)
    CtService   : List (String, CandidType) -> CandidType

--------------------------------------------------------------------------------
-- Field Name Hashing
--------------------------------------------------------------------------------

||| Candid field name hash function
||| hash(name) = fold (\h c -> (h * 223 + ord(c)) mod 2^32) 0 name
public export
hashFieldName : String -> FieldHash
hashFieldName name = go 0 (unpack name)
  where
    go : Nat -> List Char -> Nat
    go h [] = h
    go h (c :: cs) =
      let h' = (h * 223 + cast (ord c)) `mod` 4294967296
      in go h' cs

--------------------------------------------------------------------------------
-- Common Field Hashes (precomputed)
--------------------------------------------------------------------------------

||| Hash for "ok" (used in Result type)
public export
hashOk : FieldHash
hashOk = hashFieldName "ok"

||| Hash for "err" (used in Result type)
public export
hashErr : FieldHash
hashErr = hashFieldName "err"

--------------------------------------------------------------------------------
-- Type Class for Candid Serialization
--------------------------------------------------------------------------------

||| Type class for types that can be converted to/from Candid
public export
interface Candidable a where
  ||| Get the Candid type descriptor for this type
  candidType : CandidType

  ||| Convert a value to Candid representation
  toCandid : a -> CandidValue

  ||| Convert from Candid representation (may fail)
  fromCandid : CandidValue -> Maybe a

--------------------------------------------------------------------------------
-- Candidable Instances for Primitive Types
--------------------------------------------------------------------------------

public export
Candidable () where
  candidType = CtNull
  toCandid () = CNull
  fromCandid CNull = Just ()
  fromCandid _ = Nothing

public export
Candidable Bool where
  candidType = CtBool
  toCandid b = CBool b
  fromCandid (CBool b) = Just b
  fromCandid _ = Nothing

public export
Candidable Nat where
  candidType = CtNat
  toCandid n = CNat n
  fromCandid (CNat n) = Just n
  fromCandid _ = Nothing

public export
Candidable Integer where
  candidType = CtInt
  toCandid i = CInt i
  fromCandid (CInt i) = Just i
  fromCandid _ = Nothing

public export
Candidable Bits8 where
  candidType = CtNat8
  toCandid b = CNat8 b
  fromCandid (CNat8 b) = Just b
  fromCandid _ = Nothing

public export
Candidable Bits16 where
  candidType = CtNat16
  toCandid b = CNat16 b
  fromCandid (CNat16 b) = Just b
  fromCandid _ = Nothing

public export
Candidable Bits32 where
  candidType = CtNat32
  toCandid b = CNat32 b
  fromCandid (CNat32 b) = Just b
  fromCandid _ = Nothing

public export
Candidable Bits64 where
  candidType = CtNat64
  toCandid b = CNat64 b
  fromCandid (CNat64 b) = Just b
  fromCandid _ = Nothing

public export
Candidable String where
  candidType = CtText
  toCandid s = CText s
  fromCandid (CText s) = Just s
  fromCandid _ = Nothing

public export
Candidable a => Candidable (Maybe a) where
  candidType = CtOpt (candidType {a})
  toCandid Nothing = COpt Nothing
  toCandid (Just x) = COpt (Just (toCandid x))
  fromCandid (COpt Nothing) = Just Nothing
  fromCandid (COpt (Just v)) = Just <$> fromCandid v
  fromCandid _ = Nothing

public export
Candidable a => Candidable (List a) where
  candidType = CtVec (candidType {a})
  toCandid xs = CVec (map toCandid xs)
  fromCandid (CVec vs) = traverse fromCandid vs
  fromCandid (CBlob bs) = case candidType {a} of
    CtNat8 => Just (believe_me bs)  -- List Bits8
    _ => Nothing
  fromCandid _ = Nothing

--------------------------------------------------------------------------------
-- Result Type (Candid variant with ok/err)
--------------------------------------------------------------------------------

||| Candid Result type
public export
data CandidResult e a = Ok a | Err e

public export
(Candidable e, Candidable a) => Candidable (CandidResult e a) where
  candidType = CtVariant
    [ MkFieldType hashOk (Just "ok") (candidType {a})
    , MkFieldType hashErr (Just "err") (candidType {a=e})
    ]
  toCandid (Ok x) = CVariant hashOk (toCandid x)
  toCandid (Err e) = CVariant hashErr (toCandid e)
  fromCandid (CVariant h v) =
    if h == hashOk
      then Ok <$> fromCandid v
      else if h == hashErr
        then Err <$> fromCandid v
        else Nothing
  fromCandid _ = Nothing

--------------------------------------------------------------------------------
-- Principal Type
--------------------------------------------------------------------------------

||| Principal (canister or user ID)
public export
record CandidPrincipal where
  constructor MkCandidPrincipal
  bytes : List Bits8

public export
Candidable CandidPrincipal where
  candidType = CtPrincipal
  toCandid (MkCandidPrincipal bs) = CPrincipal bs
  fromCandid (CPrincipal bs) = Just (MkCandidPrincipal bs)
  fromCandid _ = Nothing

--------------------------------------------------------------------------------
-- Blob Type (vec nat8)
--------------------------------------------------------------------------------

||| Blob (binary data, alias for vec nat8)
public export
record Blob where
  constructor MkBlob
  bytes : List Bits8

public export
Candidable Blob where
  candidType = CtVec CtNat8
  toCandid (MkBlob bs) = CBlob bs
  fromCandid (CBlob bs) = Just (MkBlob bs)
  fromCandid (CVec vs) = MkBlob <$> traverse fromCandid vs
  fromCandid _ = Nothing
