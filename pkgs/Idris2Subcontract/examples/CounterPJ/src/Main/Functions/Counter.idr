||| CounterPJ: Counter Functions
module Main.Functions.Counter

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Subcontract.Core.Outcome
import Subcontract.Core.FR
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

||| CountChanged(uint256 oldValue, uint256 newValue)
EVENT_COUNT_CHANGED : Integer
EVENT_COUNT_CHANGED = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef

emitCountChanged : Integer -> Integer -> IO ()
emitCountChanged oldVal newVal = do
  mstore 0 oldVal
  mstore 32 newVal
  log1 0 64 EVENT_COUNT_CHANGED

-- =============================================================================
-- Signatures
-- =============================================================================

public export
incrementSig : Sig
incrementSig = MkSig "increment" [] [TUint256]

public export
incrementSel : Sel incrementSig
incrementSel = MkSel 0xd09de08a

public export
decrementSig : Sig
decrementSig = MkSig "decrement" [] [TUint256]

public export
decrementSel : Sel decrementSig
decrementSel = MkSel 0x2baeceb7

public export
addSig : Sig
addSig = MkSig "add" [TUint256] [TUint256]

public export
addSel : Sel addSig
addSel = MkSel 0x1003e2d2

public export
getCountSig : Sig
getCountSig = MkSig "getCount" [] [TUint256]

public export
getCountSel : Sel getCountSig
getCountSel = MkSel 0xa87d942c

-- =============================================================================
-- Implementation
-- =============================================================================

increment : IO (Outcome Integer)
increment = do
  old <- getCount
  let new = old + 1
  setCount new
  emitCountChanged old new
  pure (Ok new)

decrement : IO (Outcome Integer)
decrement = do
  old <- getCount
  if old == 0
    then pure (Fail ArithmeticError (tagEvidence "decrement: underflow"))
    else do
      let new = old - 1
      setCount new
      emitCountChanged old new
      pure (Ok new)

add : Integer -> IO (Outcome Integer)
add amount = do
  old <- getCount
  let new = old + amount
  -- Could add overflow check: if new < old then Fail ArithmeticError ...
  setCount new
  emitCountChanged old new
  pure (Ok new)

-- =============================================================================
-- Entry Points
-- =============================================================================

export
incrementEntry : Entry incrementSig
incrementEntry = MkEntry incrementSel $
  runFRReturn increment

export
decrementEntry : Entry decrementSig
decrementEntry = MkEntry decrementSel $
  runFRReturn decrement

export
addEntry : Entry addSig
addEntry = MkEntry addSel $ do
  amount <- runDecoder decodeUint256
  runFRReturn (add (uint256Value amount))

export
getCountEntry : Entry getCountSig
getCountEntry = MkEntry getCountSel $ do
  count <- getCount
  returnUint count
