||| VaultPJ: Admin Functions
module Main.Functions.Admin

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

EVENT_PAUSED : Integer
EVENT_PAUSED = 0x62e78cea01bee320cd4e420270b5ea74000d11b0c9f74754ebdbfc544b05a258

EVENT_UNPAUSED : Integer
EVENT_UNPAUSED = 0x5db9ee0a495bf2e6ff9c91a7834c1ba4fdd244a5e8aa4e537bd38aeae4b073aa

-- =============================================================================
-- Signatures
-- =============================================================================

public export
pauseSig : Sig
pauseSig = MkSig "pause" [] []

public export
pauseSel : Sel pauseSig
pauseSel = MkSel 0x8456cb59

public export
unpauseSig : Sig
unpauseSig = MkSig "unpause" [] []

public export
unpauseSel : Sel unpauseSig
unpauseSel = MkSel 0x3f4ba83a

public export
ownerSig : Sig
ownerSig = MkSig "owner" [] [TAddress]

public export
ownerSel : Sel ownerSig
ownerSel = MkSel 0x8da5cb5b

public export
totalDepositsSig : Sig
totalDepositsSig = MkSig "totalDeposits" [] [TUint256]

public export
totalDepositsSel : Sel totalDepositsSig
totalDepositsSel = MkSel 0x7d882097

-- =============================================================================
-- Guards
-- =============================================================================

requireOwner : IO ()
requireOwner = do
  owner <- getOwner
  callerAddr <- caller
  if owner == callerAddr then pure () else evmRevert 0 0

-- =============================================================================
-- Entry Points
-- =============================================================================

export
pauseEntry : Entry pauseSig
pauseEntry = MkEntry pauseSel $ do
  requireOwner
  setPaused True
  log1 0 0 EVENT_PAUSED
  stop

export
unpauseEntry : Entry unpauseSig
unpauseEntry = MkEntry unpauseSel $ do
  requireOwner
  setPaused False
  log1 0 0 EVENT_UNPAUSED
  stop

export
ownerEntry : Entry ownerSig
ownerEntry = MkEntry ownerSel $ do
  owner <- getOwner
  returnUint owner

export
totalDepositsEntry : Entry totalDepositsSig
totalDepositsEntry = MkEntry totalDepositsSel $ do
  total <- getTotalDeposits
  returnUint total
