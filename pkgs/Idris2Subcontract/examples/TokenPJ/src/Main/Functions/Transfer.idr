||| TokenPJ: Transfer Functions
|||
||| ERC20-compatible transfer functions.
module Main.Functions.Transfer

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

||| Transfer(address indexed from, address indexed to, uint256 value)
EVENT_TRANSFER : Integer
EVENT_TRANSFER = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef

emitTransfer : Integer -> Integer -> Integer -> IO ()
emitTransfer from to amount = do
  mstore 0 amount
  log3 0 32 EVENT_TRANSFER from to

-- =============================================================================
-- Signatures
-- =============================================================================

public export
transferSig : Sig
transferSig = MkSig "transfer" [TAddress, TUint256] [TBool]

public export
transferSel : Sel transferSig
transferSel = MkSel 0xa9059cbb

-- =============================================================================
-- Implementation
-- =============================================================================

||| Transfer tokens from caller to recipient
export
transfer : Integer -> Integer -> Integer -> IO Bool
transfer from to amount = do
  fromBal <- getBalance from
  if fromBal < amount
    then pure False
    else do
      setBalance from (fromBal - amount)
      toBal <- getBalance to
      setBalance to (toBal + amount)
      emitTransfer from to amount
      pure True

-- =============================================================================
-- Entry Point
-- =============================================================================

export
transferEntry : Entry transferSig
transferEntry = MkEntry transferSel $ do
  to <- runDecoder decodeAddress
  amount <- runDecoder decodeUint256
  from <- caller
  success <- transfer from (addrValue to) (uint256Value amount)
  if success
    then returnBool True
    else evmRevert 0 0
