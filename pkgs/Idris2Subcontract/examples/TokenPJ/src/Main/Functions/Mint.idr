||| TokenPJ: Mint Functions
|||
||| Owner-only minting functionality.
module Main.Functions.Mint

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

EVENT_TRANSFER : Integer
EVENT_TRANSFER = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef

emitMint : Integer -> Integer -> IO ()
emitMint to amount = do
  mstore 0 amount
  log3 0 32 EVENT_TRANSFER 0 to  -- from = 0 for mint

-- =============================================================================
-- Signatures
-- =============================================================================

public export
mintSig : Sig
mintSig = MkSig "mint" [TAddress, TUint256] []

public export
mintSel : Sel mintSig
mintSel = MkSel 0x40c10f19

-- =============================================================================
-- Access Control
-- =============================================================================

||| Require caller to be owner
requireOwner : IO ()
requireOwner = do
  owner <- getOwner
  callerAddr <- caller
  if owner == callerAddr
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Implementation
-- =============================================================================

||| Mint new tokens to address
export
mint : Integer -> Integer -> IO ()
mint to amount = do
  -- Update balance
  bal <- getBalance to
  setBalance to (bal + amount)
  -- Update total supply
  supply <- getTotalSupply
  setTotalSupply (supply + amount)
  -- Emit event
  emitMint to amount

-- =============================================================================
-- Entry Point
-- =============================================================================

export
mintEntry : Entry mintSig
mintEntry = MkEntry mintSel $ do
  requireOwner
  to <- runDecoder decodeAddress
  amount <- runDecoder decodeUint256
  mint (addrValue to) (uint256Value amount)
  stop
