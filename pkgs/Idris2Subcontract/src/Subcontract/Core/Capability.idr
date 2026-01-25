||| Subcontract Core: Capability-Based Access Control
|||
||| Encodes ERC-7546 UCS affordances as type-level capabilities.
||| Functions declare what capabilities they require, making
||| the "what this code can do" visible in the type signature.
|||
||| Example:
|||   proxyMain : Contract [CanReadDictionary, CanDelegate] ()
|||   -- Type tells reviewer: "This reads dictionary and delegates"
|||
module Subcontract.Core.Capability

import Subcontract.Core.ABI.Decoder
import Data.List.Elem

-- =============================================================================
-- Capabilities (What code is allowed to do)
-- =============================================================================

||| Capabilities for EVM operations
public export
data Cap
  = CanReadStorage      -- Can SLOAD
  | CanWriteStorage     -- Can SSTORE
  | CanReadDictionary   -- Can query selector->impl mapping
  | CanDelegate         -- Can DELEGATECALL
  | CanCall             -- Can CALL external contracts
  | CanTransfer         -- Can send ETH
  | CanSelfDestruct     -- Can SELFDESTRUCT
  | CanReadCalldata     -- Can read input data
  | CanReturn           -- Can RETURN
  | CanRevert           -- Can REVERT

export
Eq Cap where
  CanReadStorage == CanReadStorage = True
  CanWriteStorage == CanWriteStorage = True
  CanReadDictionary == CanReadDictionary = True
  CanDelegate == CanDelegate = True
  CanCall == CanCall = True
  CanTransfer == CanTransfer = True
  CanSelfDestruct == CanSelfDestruct = True
  CanReadCalldata == CanReadCalldata = True
  CanReturn == CanReturn = True
  CanRevert == CanRevert = True
  _ == _ = False

export
Show Cap where
  show CanReadStorage = "read-storage"
  show CanWriteStorage = "write-storage"
  show CanReadDictionary = "read-dictionary"
  show CanDelegate = "delegate"
  show CanCall = "call"
  show CanTransfer = "transfer"
  show CanSelfDestruct = "selfdestruct"
  show CanReadCalldata = "read-calldata"
  show CanReturn = "return"
  show CanRevert = "revert"

-- =============================================================================
-- Capability Sets
-- =============================================================================

||| Standard capability sets for common contract patterns
public export
ProxyCaps : List Cap
ProxyCaps = [CanReadDictionary, CanDelegate, CanReadCalldata]

public export
DictionaryCaps : List Cap
DictionaryCaps = [CanReadStorage, CanWriteStorage, CanReturn, CanRevert]

public export
FunctionCaps : List Cap
FunctionCaps = [CanReadStorage, CanWriteStorage, CanReadCalldata, CanReturn, CanRevert]

public export
ViewCaps : List Cap
ViewCaps = [CanReadStorage, CanReadCalldata, CanReturn]

-- =============================================================================
-- Capability-Checked Contract Monad
-- =============================================================================

||| Contract monad parameterized by required capabilities
||| The `caps` parameter documents what this code can do
public export
data Contract : (caps : List Cap) -> Type -> Type where
  PureC : a -> Contract caps a
  BindC : Contract caps a -> (a -> Contract caps b) -> Contract caps b

export
Functor (Contract caps) where
  map f (PureC a) = PureC (f a)
  map f (BindC m k) = BindC m (\a => map f (k a))

export
Applicative (Contract caps) where
  pure = PureC
  mf <*> ma = BindC mf (\f => BindC ma (\a => PureC (f a)))

export
Monad (Contract caps) where
  (>>=) = BindC

-- =============================================================================
-- Capability Assertions (for documentation/runtime checks)
-- =============================================================================

||| Assert that a capability is in the set (documentation)
export
requireCap : (c : Cap) -> {auto prf : Elem c caps} -> Contract caps ()
requireCap _ = PureC ()

||| Describe required capabilities (for pretty-printing)
export
describeCapabilities : List Cap -> String
describeCapabilities caps = "Requires: " ++ show caps
