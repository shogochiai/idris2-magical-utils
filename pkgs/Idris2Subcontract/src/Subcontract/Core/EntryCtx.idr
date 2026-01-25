||| Subcontract Core: Entry Context
|||
||| MVP-2: Track how a function was entered from external world.
|||
||| Key insight: Not all entries are equal.
||| - DirectCall: Normal function call, full permissions
||| - Receive: ETH receive, should be minimal
||| - Fallback: Unknown selector, dangerous
||| - ERC721Receive: Callback from token transfer
||| - ForcedEther: SELFDESTRUCT victim, no control
|||
||| Solidity: receive() and fallback() are just functions
||| Idris2: EntryCtx types restrict what can happen
module Subcontract.Core.EntryCtx

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence
import public Subcontract.Core.Outcome

%default total

-- =============================================================================
-- Entry Context: How We Got Here
-- =============================================================================

||| Classification of external entry points
public export
data EntryCtx : Type where
  ||| Direct function call with known selector
  DirectCall : EntryCtx
  ||| ETH receive (empty calldata, value > 0)
  Receive : EntryCtx
  ||| Fallback (unknown selector or empty calldata)
  Fallback : EntryCtx
  ||| ERC721 onERC721Received callback
  ERC721Receive : EntryCtx
  ||| ERC1155 onERC1155Received callback
  ERC1155Receive : EntryCtx
  ||| ERC1155 onERC1155BatchReceived callback
  ERC1155BatchReceive : EntryCtx
  ||| Forced ether via SELFDESTRUCT (no code execution)
  ForcedEther : EntryCtx
  ||| Running in delegatecall context (msg.sender preserved)
  DelegateContext : EntryCtx
  ||| Running in staticcall context (no state changes)
  StaticContext : EntryCtx

||| Entry context equality
public export
Eq EntryCtx where
  DirectCall == DirectCall = True
  Receive == Receive = True
  Fallback == Fallback = True
  ERC721Receive == ERC721Receive = True
  ERC1155Receive == ERC1155Receive = True
  ERC1155BatchReceive == ERC1155BatchReceive = True
  ForcedEther == ForcedEther = True
  DelegateContext == DelegateContext = True
  StaticContext == StaticContext = True
  _ == _ = False

||| Human-readable name
public export
ctxName : EntryCtx -> String
ctxName DirectCall = "DirectCall"
ctxName Receive = "Receive"
ctxName Fallback = "Fallback"
ctxName ERC721Receive = "ERC721Receive"
ctxName ERC1155Receive = "ERC1155Receive"
ctxName ERC1155BatchReceive = "ERC1155BatchReceive"
ctxName ForcedEther = "ForcedEther"
ctxName DelegateContext = "DelegateContext"
ctxName StaticContext = "StaticContext"

-- =============================================================================
-- Context Policy: What's Allowed
-- =============================================================================

||| Policy for what operations are allowed in a context
public export
record CtxPolicy where
  constructor MkCtxPolicy
  ||| Can read/write storage
  allowStorage : Bool
  ||| Can make external calls
  allowExternal : Bool
  ||| Can emit logs/events
  allowLog : Bool
  ||| Can create contracts
  allowCreate : Bool
  ||| Can self-destruct
  allowSelfdestruct : Bool
  ||| Can transfer ETH
  allowTransfer : Bool

||| Full permissions (DirectCall)
public export
fullPolicy : CtxPolicy
fullPolicy = MkCtxPolicy True True True True True True

||| No permissions (ForcedEther - code doesn't even run)
public export
noPolicy : CtxPolicy
noPolicy = MkCtxPolicy False False False False False False

||| Minimal permissions (Receive)
public export
minimalPolicy : CtxPolicy
minimalPolicy = MkCtxPolicy False False True False False False

||| Read-only (StaticContext)
public export
readOnlyPolicy : CtxPolicy
readOnlyPolicy = MkCtxPolicy False False False False False False

||| Storage + log only (conservative fallback)
public export
conservativePolicy : CtxPolicy
conservativePolicy = MkCtxPolicy True False True False False False

||| Get policy for a context
public export
policyOf : EntryCtx -> CtxPolicy
policyOf DirectCall = fullPolicy
policyOf Receive = minimalPolicy
policyOf Fallback = conservativePolicy
policyOf ERC721Receive = conservativePolicy
policyOf ERC1155Receive = conservativePolicy
policyOf ERC1155BatchReceive = conservativePolicy
policyOf ForcedEther = noPolicy
policyOf DelegateContext = { allowCreate := False, allowSelfdestruct := False } fullPolicy
policyOf StaticContext = readOnlyPolicy

-- =============================================================================
-- Policy Checks
-- =============================================================================

||| Check if storage access is allowed
public export
checkStorage : EntryCtx -> Outcome ()
checkStorage ctx =
  if (policyOf ctx).allowStorage
    then Ok ()
    else Fail UnsafeEntryContext (tagsEvidence [ctxName ctx, "storage forbidden"])

||| Check if external call is allowed
public export
checkExternal : EntryCtx -> Outcome ()
checkExternal ctx =
  if (policyOf ctx).allowExternal
    then Ok ()
    else Fail ExternalCallForbidden (tagsEvidence [ctxName ctx, "external call forbidden"])

||| Check if log is allowed
public export
checkLog : EntryCtx -> Outcome ()
checkLog ctx =
  if (policyOf ctx).allowLog
    then Ok ()
    else Fail UnsafeEntryContext (tagsEvidence [ctxName ctx, "log forbidden"])

||| Check if create is allowed
public export
checkCreate : EntryCtx -> Outcome ()
checkCreate ctx =
  if (policyOf ctx).allowCreate
    then Ok ()
    else Fail UnsafeEntryContext (tagsEvidence [ctxName ctx, "create forbidden"])

||| Check if transfer is allowed
public export
checkTransfer : EntryCtx -> Outcome ()
checkTransfer ctx =
  if (policyOf ctx).allowTransfer
    then Ok ()
    else Fail UnsafeEntryContext (tagsEvidence [ctxName ctx, "transfer forbidden"])

-- =============================================================================
-- Context Detection (Runtime)
-- =============================================================================

||| Detect entry context from calldata and value
||| In real impl: check msg.data, msg.value, etc.
public export
detectContext : Bits256 -> Bits256 -> Bits256 -> EntryCtx
detectContext calldataSize selector value =
  if calldataSize == 0 && value > 0 then Receive
  else if calldataSize == 0 then Fallback
  else if selector == 0x150b7a02 then ERC721Receive      -- onERC721Received
  else if selector == 0xf23a6e61 then ERC1155Receive     -- onERC1155Received
  else if selector == 0xbc197c81 then ERC1155BatchReceive -- onERC1155BatchReceived
  else DirectCall

-- =============================================================================
-- Context-Guarded Operations
-- =============================================================================

||| Run operation only if context allows storage
public export
guardedStorage : EntryCtx -> IO (Outcome a) -> IO (Outcome a)
guardedStorage ctx action =
  case checkStorage ctx of
    Ok () => action
    Fail c e => pure (Fail c e)

||| Run operation only if context allows external calls
public export
guardedExternal : EntryCtx -> IO (Outcome a) -> IO (Outcome a)
guardedExternal ctx action =
  case checkExternal ctx of
    Ok () => action
    Fail c e => pure (Fail c e)

||| Run operation only if context allows creates
public export
guardedCreate : EntryCtx -> IO (Outcome a) -> IO (Outcome a)
guardedCreate ctx action =
  case checkCreate ctx of
    Ok () => action
    Fail c e => pure (Fail c e)

-- =============================================================================
-- Context Stack (for Nested Calls)
-- =============================================================================

||| Stack of entry contexts (for call depth tracking)
public export
CtxStack : Type
CtxStack = List EntryCtx

||| Push context onto stack
public export
pushCtx : EntryCtx -> CtxStack -> CtxStack
pushCtx = (::)

||| Pop context from stack
public export
popCtx : CtxStack -> (Maybe EntryCtx, CtxStack)
popCtx [] = (Nothing, [])
popCtx (c :: rest) = (Just c, rest)

||| Get current (top) context
public export
currentCtx : CtxStack -> Maybe EntryCtx
currentCtx [] = Nothing
currentCtx (c :: _) = Just c

||| Check if we're in a callback context
public export
inCallback : CtxStack -> Bool
inCallback stack = any isCallback stack
  where
    isCallback : EntryCtx -> Bool
    isCallback ERC721Receive = True
    isCallback ERC1155Receive = True
    isCallback ERC1155BatchReceive = True
    isCallback _ = False
