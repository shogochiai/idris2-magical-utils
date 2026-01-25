||| TokenPJ: Approve Functions
|||
||| ERC20-compatible approval functions.
module Main.Functions.Approve

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Events
-- =============================================================================

||| Approval(address indexed owner, address indexed spender, uint256 value)
EVENT_APPROVAL : Integer
EVENT_APPROVAL = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

emitApproval : Integer -> Integer -> Integer -> IO ()
emitApproval owner spender amount = do
  mstore 0 amount
  log3 0 32 EVENT_APPROVAL owner spender

-- =============================================================================
-- Signatures
-- =============================================================================

public export
approveSig : Sig
approveSig = MkSig "approve" [TAddress, TUint256] [TBool]

public export
approveSel : Sel approveSig
approveSel = MkSel 0x095ea7b3

public export
allowanceSig : Sig
allowanceSig = MkSig "allowance" [TAddress, TAddress] [TUint256]

public export
allowanceSel : Sel allowanceSig
allowanceSel = MkSel 0xdd62ed3e

-- =============================================================================
-- Implementation
-- =============================================================================

||| Approve spender to spend owner's tokens
export
approve : Integer -> Integer -> Integer -> IO ()
approve owner spender amount = do
  setAllowance owner spender amount
  emitApproval owner spender amount

-- =============================================================================
-- Entry Points
-- =============================================================================

export
approveEntry : Entry approveSig
approveEntry = MkEntry approveSel $ do
  spender <- runDecoder decodeAddress
  amount <- runDecoder decodeUint256
  owner <- caller
  approve owner (addrValue spender) (uint256Value amount)
  returnBool True

export
allowanceEntry : Entry allowanceSig
allowanceEntry = MkEntry allowanceSel $ do
  owner <- runDecoder decodeAddress
  spender <- runDecoder decodeAddress
  allowed <- getAllowance (addrValue owner) (addrValue spender)
  returnUint allowed
