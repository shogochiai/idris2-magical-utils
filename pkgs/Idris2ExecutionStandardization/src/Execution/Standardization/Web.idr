module Execution.Standardization.Web

import Execution.Standardization.Types

%default total

public export
record JsMillis where
  constructor MkJsMillis
  rawMillis : Integer

public export
record JsByteOffset where
  constructor MkJsByteOffset
  rawOffset : Integer

public export
record JsByteLength where
  constructor MkJsByteLength
  rawLength : Integer

public export
Show JsMillis where show x = show x.rawMillis

public export
Show JsByteOffset where show x = show x.rawOffset

public export
Show JsByteLength where show x = show x.rawLength

public export
webValueProfile : List (ValueRole, String)
webValueProfile =
  [ (TimestampRole, "JsMillis")
  , (CoverageOffsetRole, "JsByteOffset")
  , (BytesRole, "JsByteLength")
  ]

public export
zeroJsByteOffset : JsByteOffset
zeroJsByteOffset = MkJsByteOffset 0

public export
zeroJsByteLength : JsByteLength
zeroJsByteLength = MkJsByteLength 0

public export
jsByteOffsetFromNat : Nat -> JsByteOffset
jsByteOffsetFromNat n = MkJsByteOffset (cast n)

public export
jsByteLengthFromNat : Nat -> JsByteLength
jsByteLengthFromNat n = MkJsByteLength (cast n)

public export
toNatJsByteOffset : JsByteOffset -> Nat
toNatJsByteOffset x = cast x.rawOffset

public export
toNatJsByteLength : JsByteLength -> Nat
toNatJsByteLength x = cast x.rawLength

public export
offsetGE : JsByteOffset -> JsByteOffset -> Bool
offsetGE a b = a.rawOffset >= b.rawOffset

public export
offsetLT : JsByteOffset -> JsByteOffset -> Bool
offsetLT a b = a.rawOffset < b.rawOffset

public export
offsetMinus : JsByteOffset -> JsByteOffset -> JsByteLength
offsetMinus a b =
  if a.rawOffset > b.rawOffset
     then MkJsByteLength (a.rawOffset - b.rawOffset)
     else MkJsByteLength 0

public export
maxOffset : JsByteOffset -> JsByteOffset -> JsByteOffset
maxOffset a b = if a.rawOffset >= b.rawOffset then a else b
