module Execution.Standardization.Evm

import Execution.Standardization.Types

%default total

public export
record Wei where
  constructor MkWei
  rawWei : Integer

public export
record Gas where
  constructor MkGas
  rawGas : Integer

public export
record EvmTimestamp where
  constructor MkEvmTimestamp
  rawTimestamp : Integer

public export
record BlockNumber where
  constructor MkBlockNumber
  rawBlockNumber : Integer

public export
record ChainId where
  constructor MkChainId
  rawChainId : Integer

public export
record LogIndex where
  constructor MkLogIndex
  rawLogIndex : Integer

public export
record Nonce where
  constructor MkNonce
  rawNonce : Integer

public export
record StorageSlot where
  constructor MkStorageSlot
  rawSlot : Integer

public export
Eq Wei where
  a == b = a.rawWei == b.rawWei

public export
Eq Gas where
  a == b = a.rawGas == b.rawGas

public export
Eq EvmTimestamp where
  a == b = a.rawTimestamp == b.rawTimestamp

public export
Eq BlockNumber where
  a == b = a.rawBlockNumber == b.rawBlockNumber

public export
Ord BlockNumber where
  compare a b = compare a.rawBlockNumber b.rawBlockNumber

public export
Eq ChainId where
  a == b = a.rawChainId == b.rawChainId

public export
Ord ChainId where
  compare a b = compare a.rawChainId b.rawChainId

public export
Eq LogIndex where
  a == b = a.rawLogIndex == b.rawLogIndex

public export
Eq Nonce where
  a == b = a.rawNonce == b.rawNonce

public export
Ord Nonce where
  compare a b = compare a.rawNonce b.rawNonce

public export
Eq StorageSlot where
  a == b = a.rawSlot == b.rawSlot

public export
Show Wei where show x = show x.rawWei

public export
Show Gas where show x = show x.rawGas

public export
Show EvmTimestamp where show x = show x.rawTimestamp

public export
Show BlockNumber where show x = show x.rawBlockNumber

public export
Show ChainId where show x = show x.rawChainId

public export
Show LogIndex where show x = show x.rawLogIndex

public export
Show Nonce where show x = show x.rawNonce

public export
Show StorageSlot where show x = show x.rawSlot

public export
evmValueProfile : List (ValueRole, String)
evmValueProfile =
  [ (BalanceRole, "Wei")
  , (GasRole, "Gas")
  , (TimestampRole, "EvmTimestamp")
  , (IdentifierRole, "ChainId")
  , (BlockNumberRole, "BlockNumber")
  , (NonceRole, "Nonce")
  , (OffsetRole, "StorageSlot")
  ]

public export
weiFromNat : Nat -> Wei
weiFromNat n = MkWei (cast n)

public export
gasFromNat : Nat -> Gas
gasFromNat n = MkGas (cast n)

public export
timestampFromNat : Nat -> EvmTimestamp
timestampFromNat n = MkEvmTimestamp (cast n)

public export
chainIdFromNat : Nat -> ChainId
chainIdFromNat n = MkChainId (cast n)

public export
blockNumberFromNat : Nat -> BlockNumber
blockNumberFromNat n = MkBlockNumber (cast n)

public export
logIndexFromNat : Nat -> LogIndex
logIndexFromNat n = MkLogIndex (cast n)

public export
nonceFromNat : Nat -> Nonce
nonceFromNat n = MkNonce (cast n)

public export
toNatChainId : ChainId -> Nat
toNatChainId x = cast x.rawChainId

public export
toNatBlockNumber : BlockNumber -> Nat
toNatBlockNumber x = cast x.rawBlockNumber

public export
toNatLogIndex : LogIndex -> Nat
toNatLogIndex x = cast x.rawLogIndex

public export
toNatNonce : Nonce -> Nat
toNatNonce x = cast x.rawNonce

public export
toNatWei : Wei -> Nat
toNatWei x = cast x.rawWei

public export
toNatGas : Gas -> Nat
toNatGas x = cast x.rawGas

public export
toIntegerWei : Wei -> Integer
toIntegerWei x = x.rawWei

public export
toIntegerGas : Gas -> Integer
toIntegerGas x = x.rawGas

public export
toIntegerBlockNumber : BlockNumber -> Integer
toIntegerBlockNumber x = x.rawBlockNumber

public export
weiGT : Wei -> Wei -> Bool
weiGT a b = a.rawWei > b.rawWei

public export
weiGE : Wei -> Wei -> Bool
weiGE a b = a.rawWei >= b.rawWei

public export
weiAdd : Wei -> Wei -> Wei
weiAdd a b = MkWei (a.rawWei + b.rawWei)

public export
weiMinus : Wei -> Wei -> Wei
weiMinus a b =
  if a.rawWei >= b.rawWei
     then MkWei (a.rawWei - b.rawWei)
     else MkWei 0

public export
gasAdd : Gas -> Gas -> Gas
gasAdd a b = MkGas (a.rawGas + b.rawGas)

public export
gasMulNat : Gas -> Nat -> Gas
gasMulNat a n = MkGas (a.rawGas * cast n)

public export
safeDiv10Wei : Wei -> Wei
safeDiv10Wei a = MkWei (a.rawWei `div` 10)
