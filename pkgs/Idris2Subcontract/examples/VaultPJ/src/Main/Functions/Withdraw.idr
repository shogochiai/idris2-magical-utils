||| VaultPJ: Withdraw Functions
module Main.Functions.Withdraw

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

||| Withdrawn(address indexed user, uint256 amount)
EVENT_WITHDRAWN : Integer
EVENT_WITHDRAWN = 0x7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65

emitWithdrawn : Integer -> Integer -> IO ()
emitWithdrawn user amount = do
  mstore 0 amount
  log2 0 32 EVENT_WITHDRAWN user

-- =============================================================================
-- Signatures
-- =============================================================================

public export
withdrawSig : Sig
withdrawSig = MkSig "withdraw" [TUint256] []

public export
withdrawSel : Sel withdrawSig
withdrawSel = MkSel 0x2e1a7d4d

public export
withdrawAllSig : Sig
withdrawAllSig = MkSig "withdrawAll" [] []

public export
withdrawAllSel : Sel withdrawAllSig
withdrawAllSel = MkSel 0x853828b6

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

||| Withdraw ETH from vault
withdraw : Integer -> Integer -> IO ()
withdraw user amount = do
  requireNotPaused
  -- Check balance
  current <- getDeposit user
  if current < amount
    then evmRevert 0 0
    else do
      -- Update user deposit
      setDeposit user (current - amount)
      -- Update total
      total <- getTotalDeposits
      setTotalDeposits (total - amount)
      -- Transfer ETH
      sendEth user amount
      -- Emit event
      emitWithdrawn user amount
  where
    sendEth : Integer -> Integer -> IO ()
    sendEth to val = do
      g <- gas
      _ <- call g to val 0 0 0 0  -- Simple ETH transfer
      pure ()

-- =============================================================================
-- Entry Points
-- =============================================================================

export
withdrawEntry : Entry withdrawSig
withdrawEntry = MkEntry withdrawSel $ do
  amount <- runDecoder decodeUint256
  user <- caller
  withdraw user (uint256Value amount)
  stop

export
withdrawAllEntry : Entry withdrawAllSig
withdrawAllEntry = MkEntry withdrawAllSel $ do
  user <- caller
  amount <- getDeposit user
  withdraw user amount
  stop
