module Execution.Standardization.IcWasm

import Execution.Standardization.Types

%default total

public export
record StableBytes where
  constructor MkStableBytes
  rawBytes : Integer

public export
record StablePageCount where
  constructor MkStablePageCount
  rawPages : Integer

public export
record StableOffset where
  constructor MkStableOffset
  rawOffset : Integer

public export
record IcTimestamp where
  constructor MkIcTimestamp
  rawTimestamp : Integer

public export
record IcCycles where
  constructor MkIcCycles
  rawCycles : Integer

public export
record IcLedgerAmount where
  constructor MkIcLedgerAmount
  rawAmount : Integer

public export
record IcIdentifier where
  constructor MkIcIdentifier
  rawIdentifier : Integer

public export
record IcCount where
  constructor MkIcCount
  rawCount : Integer

public export
record IcQueryLimit where
  constructor MkIcQueryLimit
  rawLimit : Integer

public export
Eq StableBytes where
  a == b = a.rawBytes == b.rawBytes

public export
Eq StablePageCount where
  a == b = a.rawPages == b.rawPages

public export
Eq StableOffset where
  a == b = a.rawOffset == b.rawOffset

public export
Eq IcTimestamp where
  a == b = a.rawTimestamp == b.rawTimestamp

public export
Ord IcTimestamp where
  compare a b = compare a.rawTimestamp b.rawTimestamp

public export
Eq IcCycles where
  a == b = a.rawCycles == b.rawCycles

public export
Ord IcCycles where
  compare a b = compare a.rawCycles b.rawCycles

public export
Eq IcLedgerAmount where
  a == b = a.rawAmount == b.rawAmount

public export
Ord IcLedgerAmount where
  compare a b = compare a.rawAmount b.rawAmount

public export
Eq IcIdentifier where
  a == b = a.rawIdentifier == b.rawIdentifier

public export
Ord IcIdentifier where
  compare a b = compare a.rawIdentifier b.rawIdentifier

public export
Eq IcCount where
  a == b = a.rawCount == b.rawCount

public export
Ord IcCount where
  compare a b = compare a.rawCount b.rawCount

public export
Eq IcQueryLimit where
  a == b = a.rawLimit == b.rawLimit

public export
Show StableBytes where
  show x = show x.rawBytes

public export
Show StablePageCount where
  show x = show x.rawPages

public export
Show StableOffset where
  show x = show x.rawOffset

public export
Show IcTimestamp where
  show x = show x.rawTimestamp

public export
Show IcCycles where
  show x = show x.rawCycles

public export
Show IcLedgerAmount where
  show x = show x.rawAmount

public export
Show IcIdentifier where
  show x = show x.rawIdentifier

public export
Show IcCount where
  show x = show x.rawCount

public export
Show IcQueryLimit where
  show x = show x.rawLimit

public export
icWasmValueProfile : List (ValueRole, String)
icWasmValueProfile =
  [ (BytesRole, "StableBytes")
  , (PageCountRole, "StablePageCount")
  , (OffsetRole, "StableOffset")
  , (TimestampRole, "IcTimestamp")
  , (CyclesRole, "IcCycles")
  , (BalanceRole, "IcLedgerAmount")
  , (IdentifierRole, "IcIdentifier")
  , (CountRole, "IcCount")
  , (LimitRole, "IcQueryLimit")
  ]

public export
zeroStablePageCount : StablePageCount
zeroStablePageCount = MkStablePageCount 0

public export
defaultIcQueryLimit : IcQueryLimit
defaultIcQueryLimit = MkIcQueryLimit 50

public export
zeroIcTimestamp : IcTimestamp
zeroIcTimestamp = MkIcTimestamp 0

public export
zeroIcIdentifier : IcIdentifier
zeroIcIdentifier = MkIcIdentifier 0

public export
zeroIcCount : IcCount
zeroIcCount = MkIcCount 0

public export
zeroIcCycles : IcCycles
zeroIcCycles = MkIcCycles 0

public export
zeroIcLedgerAmount : IcLedgerAmount
zeroIcLedgerAmount = MkIcLedgerAmount 0

public export
mkStableBytes : Integer -> Maybe StableBytes
mkStableBytes n = if n < 0 then Nothing else Just (MkStableBytes n)

public export
mkStablePageCount : Integer -> Maybe StablePageCount
mkStablePageCount n = if n < 0 then Nothing else Just (MkStablePageCount n)

public export
mkIcTimestamp : Integer -> Maybe IcTimestamp
mkIcTimestamp n = if n < 0 then Nothing else Just (MkIcTimestamp n)

public export
mkIcCycles : Integer -> Maybe IcCycles
mkIcCycles n = if n < 0 then Nothing else Just (MkIcCycles n)

public export
mkIcLedgerAmount : Integer -> Maybe IcLedgerAmount
mkIcLedgerAmount n = if n < 0 then Nothing else Just (MkIcLedgerAmount n)

public export
mkIcIdentifier : Integer -> Maybe IcIdentifier
mkIcIdentifier n = if n < 0 then Nothing else Just (MkIcIdentifier n)

public export
mkIcCount : Integer -> Maybe IcCount
mkIcCount n = if n < 0 then Nothing else Just (MkIcCount n)

public export
mkIcQueryLimit : Integer -> Maybe IcQueryLimit
mkIcQueryLimit n = if n <= 0 then Nothing else Just (MkIcQueryLimit n)

public export
stableBytesFromNat : Nat -> StableBytes
stableBytesFromNat n = MkStableBytes (cast n)

public export
pagesFromNat : Nat -> StablePageCount
pagesFromNat n = MkStablePageCount (cast n)

public export
identifierFromNat : Nat -> IcIdentifier
identifierFromNat n = MkIcIdentifier (cast n)

public export
countFromNat : Nat -> IcCount
countFromNat n = MkIcCount (cast n)

public export
timestampFromNat : Nat -> IcTimestamp
timestampFromNat n = MkIcTimestamp (cast n)

public export
queryLimitFromNat : Nat -> IcQueryLimit
queryLimitFromNat n = MkIcQueryLimit (cast n)

public export
cyclesFromNat : Nat -> IcCycles
cyclesFromNat n = MkIcCycles (cast n)

public export
toNatStableBytes : StableBytes -> Nat
toNatStableBytes x = cast x.rawBytes

public export
toNatStablePages : StablePageCount -> Nat
toNatStablePages x = cast x.rawPages

public export
toIntStableBytes : StableBytes -> Int
toIntStableBytes x = cast x.rawBytes

public export
toIntStablePages : StablePageCount -> Int
toIntStablePages x = cast x.rawPages

public export
toNatQueryLimit : IcQueryLimit -> Nat
toNatQueryLimit x = cast x.rawLimit

public export
toNatIcIdentifier : IcIdentifier -> Nat
toNatIcIdentifier x = cast x.rawIdentifier

public export
toNatIcCount : IcCount -> Nat
toNatIcCount x = cast x.rawCount

public export
toIntegerIcTimestamp : IcTimestamp -> Integer
toIntegerIcTimestamp x = x.rawTimestamp

public export
toIntegerIcCycles : IcCycles -> Integer
toIntegerIcCycles x = x.rawCycles

public export
toIntegerIcLedgerAmount : IcLedgerAmount -> Integer
toIntegerIcLedgerAmount x = x.rawAmount

public export
toIntegerIcIdentifier : IcIdentifier -> Integer
toIntegerIcIdentifier x = x.rawIdentifier

public export
toIntegerIcCount : IcCount -> Integer
toIntegerIcCount x = x.rawCount

public export
toNatIcTimestamp : IcTimestamp -> Nat
toNatIcTimestamp x = cast x.rawTimestamp

public export
timestampGE : IcTimestamp -> IcTimestamp -> Bool
timestampGE a b = a.rawTimestamp >= b.rawTimestamp

public export
timestampGT : IcTimestamp -> IcTimestamp -> Bool
timestampGT a b = a.rawTimestamp > b.rawTimestamp

public export
timestampAddNs : IcTimestamp -> Integer -> IcTimestamp
timestampAddNs ts delta = MkIcTimestamp (ts.rawTimestamp + delta)

public export
timestampDiffNs : IcTimestamp -> IcTimestamp -> Integer
timestampDiffNs now earlier =
  if now.rawTimestamp >= earlier.rawTimestamp
     then now.rawTimestamp - earlier.rawTimestamp
     else 0

public export
pagesToBytes : StablePageCount -> StableBytes
pagesToBytes pages = MkStableBytes (pages.rawPages * 65536)

public export
bytesToPagesRoundUp : StableBytes -> StablePageCount
bytesToPagesRoundUp bytes =
  MkStablePageCount ((bytes.rawBytes + 65535) `div` 65536)

public export
bytesGT : StableBytes -> StableBytes -> Bool
bytesGT a b = a.rawBytes > b.rawBytes

public export
ninetyPercentBytes : StableBytes -> StableBytes
ninetyPercentBytes bytes = MkStableBytes ((bytes.rawBytes * 9) `div` 10)

public export
countGE : IcCount -> IcCount -> Bool
countGE a b = a.rawCount >= b.rawCount

public export
countGT : IcCount -> IcCount -> Bool
countGT a b = a.rawCount > b.rawCount

public export
countMinus : IcCount -> IcCount -> IcCount
countMinus a b =
  if a.rawCount > b.rawCount
     then MkIcCount (a.rawCount - b.rawCount)
     else MkIcCount 0

public export
countFromLength : List a -> IcCount
countFromLength xs = MkIcCount (cast (length xs))

public export
cyclesGE : IcCycles -> IcCycles -> Bool
cyclesGE a b = a.rawCycles >= b.rawCycles

public export
cyclesAdd : IcCycles -> IcCycles -> IcCycles
cyclesAdd a b = MkIcCycles (a.rawCycles + b.rawCycles)

public export
cyclesGT : IcCycles -> IcCycles -> Bool
cyclesGT a b = a.rawCycles > b.rawCycles

public export
cyclesMinus : IcCycles -> IcCycles -> IcCycles
cyclesMinus a b =
  if a.rawCycles >= b.rawCycles
     then MkIcCycles (a.rawCycles - b.rawCycles)
     else MkIcCycles 0

public export
cyclesDivByNat : IcCycles -> Nat -> IcCycles
cyclesDivByNat a n =
  case n of
    Z => MkIcCycles 0
    S _ => MkIcCycles (a.rawCycles `div` cast n)

public export
cyclesMulNat : IcCycles -> Nat -> IcCycles
cyclesMulNat a n = MkIcCycles (a.rawCycles * cast n)

public export
ledgerAdd : IcLedgerAmount -> IcLedgerAmount -> IcLedgerAmount
ledgerAdd a b = MkIcLedgerAmount (a.rawAmount + b.rawAmount)

public export
ledgerMinus : IcLedgerAmount -> IcLedgerAmount -> IcLedgerAmount
ledgerMinus a b =
  if a.rawAmount >= b.rawAmount
     then MkIcLedgerAmount (a.rawAmount - b.rawAmount)
     else MkIcLedgerAmount 0
