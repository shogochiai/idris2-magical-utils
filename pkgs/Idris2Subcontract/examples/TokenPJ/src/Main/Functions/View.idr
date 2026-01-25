||| TokenPJ: View Functions
|||
||| Read-only query functions.
module Main.Functions.View

import EVM.Primitives
import Subcontract.Core.Entry
import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Main.Storages.Schema

-- =============================================================================
-- Signatures
-- =============================================================================

public export
totalSupplySig : Sig
totalSupplySig = MkSig "totalSupply" [] [TUint256]

public export
totalSupplySel : Sel totalSupplySig
totalSupplySel = MkSel 0x18160ddd

public export
balanceOfSig : Sig
balanceOfSig = MkSig "balanceOf" [TAddress] [TUint256]

public export
balanceOfSel : Sel balanceOfSig
balanceOfSel = MkSel 0x70a08231

public export
ownerSig : Sig
ownerSig = MkSig "owner" [] [TAddress]

public export
ownerSel : Sel ownerSig
ownerSel = MkSel 0x8da5cb5b

-- =============================================================================
-- Entry Points
-- =============================================================================

export
totalSupplyEntry : Entry totalSupplySig
totalSupplyEntry = MkEntry totalSupplySel $ do
  supply <- getTotalSupply
  returnUint supply

export
balanceOfEntry : Entry balanceOfSig
balanceOfEntry = MkEntry balanceOfSel $ do
  addr <- runDecoder decodeAddress
  bal <- getBalance (addrValue addr)
  returnUint bal

export
ownerEntry : Entry ownerSig
ownerEntry = MkEntry ownerSel $ do
  owner <- getOwner
  returnUint owner
