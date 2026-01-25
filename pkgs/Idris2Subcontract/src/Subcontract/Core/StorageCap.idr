||| Subcontract Core: Storage Capability
|||
||| Implements "DO NOT USE STORAGE DIRECTLY" as a type-level constraint.
||| StorageCap is an opaque capability that must be passed to handlers.
||| Direct storage access (sload/sstore) is only possible with StorageCap.
|||
||| This makes "direct storage access" visible in code review:
||| - If a function takes StorageCap, it can touch storage
||| - If it doesn't, it cannot (enforced by module boundary)
|||
module Subcontract.Core.StorageCap

import EVM.Primitives
import EVM.Storage.Namespace

-- =============================================================================
-- StorageCap: Opaque Capability (constructor not exported)
-- =============================================================================

||| Storage capability token
||| The constructor is NOT exported, so only the framework can create it
export
data StorageCap : Type where
  MkStorageCap : StorageCap

-- =============================================================================
-- Storage Operations (require StorageCap)
-- =============================================================================

||| Read from storage (requires capability)
export
sloadCap : StorageCap -> Integer -> IO Integer
sloadCap MkStorageCap slot = sload slot

||| Write to storage (requires capability)
export
sstoreCap : StorageCap -> Integer -> Integer -> IO ()
sstoreCap MkStorageCap slot val = sstore slot val

||| Memory store (requires capability for consistency)
export
mstoreCap : StorageCap -> Integer -> Integer -> IO ()
mstoreCap MkStorageCap off val = mstore off val

||| Keccak256 (requires capability)
export
keccak256Cap : StorageCap -> Integer -> Integer -> IO Integer
keccak256Cap MkStorageCap off len = keccak256 off len

-- =============================================================================
-- Mapping Operations (require StorageCap)
-- =============================================================================

||| Calculate mapping slot (requires capability)
export
mappingSlotCap : StorageCap -> Integer -> Integer -> IO Integer
mappingSlotCap cap baseSlot key = do
  mstoreCap cap 0 key
  mstoreCap cap 32 baseSlot
  keccak256Cap cap 0 64

||| Read from mapping (requires capability)
export
readMappingCap : StorageCap -> Integer -> Integer -> IO Integer
readMappingCap cap baseSlot key = do
  slot <- mappingSlotCap cap baseSlot key
  sloadCap cap slot

||| Write to mapping (requires capability)
export
writeMappingCap : StorageCap -> Integer -> Integer -> Integer -> IO ()
writeMappingCap cap baseSlot key val = do
  slot <- mappingSlotCap cap baseSlot key
  sstoreCap cap slot val

-- =============================================================================
-- Handler Type (function that receives StorageCap)
-- =============================================================================

||| A handler is a function that receives StorageCap from the framework
||| This is the ONLY way to access storage - direct access is impossible
public export
Handler : Type -> Type
Handler a = StorageCap -> IO a

||| Run a handler with storage capability
||| Only the framework calls this - user code receives the cap
export
runHandler : Handler a -> IO a
runHandler h = h MkStorageCap

-- =============================================================================
-- Composing Handlers
-- =============================================================================

||| Pure handler (no storage access needed, but fits the type)
export
pureHandler : a -> Handler a
pureHandler x = \_ => pure x

||| Bind handlers
export
bindHandler : Handler a -> (a -> Handler b) -> Handler b
bindHandler ha f = \cap => do
  a <- ha cap
  f a cap

||| Sequence handlers
export
seqHandler : Handler () -> Handler b -> Handler b
seqHandler h1 h2 = \cap => do
  h1 cap
  h2 cap
