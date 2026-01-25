||| VaultPJ: Deposit Functions
module Main.Functions.Deposit

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

||| Deposited(address indexed user, uint256 amount)
EVENT_DEPOSITED : Integer
EVENT_DEPOSITED = 0x2da466a7b24304f47e87fa2e1e5a81b9831ce54fec19055ce277ca2f39ba42c4

emitDeposited : Integer -> Integer -> IO ()
emitDeposited user amount = do
  mstore 0 amount
  log2 0 32 EVENT_DEPOSITED user

-- =============================================================================
-- Signatures
-- =============================================================================

public export
depositSig : Sig
depositSig = MkSig "deposit" [] []

public export
depositSel : Sel depositSig
depositSel = MkSel 0xd0e30db0

public export
depositOfSig : Sig
depositOfSig = MkSig "depositOf" [TAddress] [TUint256]

public export
depositOfSel : Sel depositOfSig
depositOfSel = MkSel 0x8f601f66

-- =============================================================================
-- Guards
-- =============================================================================

requireNotPaused : IO ()
requireNotPaused = do
  paused <- isPaused
  if paused then evmRevert 0 0 else pure ()

-- =============================================================================
-- Implementation
-- =============================================================================

||| Deposit ETH to vault
deposit : Integer -> Integer -> IO ()
deposit user amount = do
  requireNotPaused
  -- Update user deposit
  current <- getDeposit user
  setDeposit user (current + amount)
  -- Update total
  total <- getTotalDeposits
  setTotalDeposits (total + amount)
  -- Emit event
  emitDeposited user amount

-- =============================================================================
-- Entry Points
-- =============================================================================

export
depositEntry : Entry depositSig
depositEntry = MkEntry depositSel $ do
  user <- caller
  amount <- callvalue
  deposit user amount
  stop

export
depositOfEntry : Entry depositOfSig
depositOfEntry = MkEntry depositOfSel $ do
  addr <- runDecoder decodeAddress
  dep <- getDeposit (addrValue addr)
  returnUint dep
