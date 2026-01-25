||| IC Stable Memory - Idris2 bindings
|||
||| Stable memory persists across canister upgrades.
||| Use this instead of C global variables for persistent state.
|||
||| Example usage:
|||   -- Store counter at offset 0
|||   stableWriteNat 0 auditorCount
|||
|||   -- Read counter back
|||   auditorCount <- stableReadNat 0
module WasmBuilder.IC0.Stable

import Data.List

%default covering

-- =============================================================================
-- Low-level Stable Memory API (matches ic0_stubs.c)
-- =============================================================================

||| Get stable memory size in pages (64KB each)
export
%foreign "C:ic0_stable_size,libic0"
prim__stableSize : PrimIO Int

||| Get stable memory size in pages
export
stableSize : IO Int
stableSize = primIO prim__stableSize

||| Grow stable memory by n pages
||| Returns previous size, or -1 on failure
export
%foreign "C:ic0_stable_grow,libic0"
prim__stableGrow : Int -> PrimIO Int

||| Grow stable memory by n pages
export
stableGrow : Int -> IO Int
stableGrow pages = primIO $ prim__stableGrow pages

||| Read from stable memory
||| @dst Destination pointer (WASM linear memory)
||| @offset Offset in stable memory
||| @size Number of bytes to read
export
%foreign "C:ic0_stable_read,libic0"
prim__stableRead : Int -> Int -> Int -> PrimIO ()

||| Write to stable memory
||| @offset Offset in stable memory
||| @src Source pointer (WASM linear memory)
||| @size Number of bytes to write
export
%foreign "C:ic0_stable_write,libic0"
prim__stableWrite : Int -> Int -> Int -> PrimIO ()

-- =============================================================================
-- 64-bit Stable Memory API (for larger storage)
-- =============================================================================

||| Get stable memory size in pages (64-bit version)
export
%foreign "C:ic0_stable64_size,libic0"
prim__stable64Size : PrimIO Int

||| Grow stable memory (64-bit version)
export
%foreign "C:ic0_stable64_grow,libic0"
prim__stable64Grow : Int -> PrimIO Int

-- =============================================================================
-- High-level Helpers (using FFI bridge buffer)
-- =============================================================================

-- These use the ic_ffi_bridge.c buffer for data transfer

||| Write a single Int64 to stable memory at offset
export
%foreign "C:ic_stable_write_i64,libic0"
prim__stableWriteI64 : Int -> Int -> PrimIO ()

||| Write Int64 to stable memory
export
stableWriteI64 : (offset : Int) -> (value : Int) -> IO ()
stableWriteI64 off val = primIO $ prim__stableWriteI64 off val

||| Read a single Int64 from stable memory at offset
export
%foreign "C:ic_stable_read_i64,libic0"
prim__stableReadI64 : Int -> PrimIO Int

||| Read Int64 from stable memory
export
stableReadI64 : (offset : Int) -> IO Int
stableReadI64 off = primIO $ prim__stableReadI64 off

||| Write a single Int32 to stable memory at offset
export
%foreign "C:ic_stable_write_i32,libic0"
prim__stableWriteI32 : Int -> Int -> PrimIO ()

||| Write Int32 to stable memory
export
stableWriteI32 : (offset : Int) -> (value : Int) -> IO ()
stableWriteI32 off val = primIO $ prim__stableWriteI32 off val

||| Read a single Int32 from stable memory at offset
export
%foreign "C:ic_stable_read_i32,libic0"
prim__stableReadI32 : Int -> PrimIO Int

||| Read Int32 from stable memory
export
stableReadI32 : (offset : Int) -> IO Int
stableReadI32 off = primIO $ prim__stableReadI32 off

-- =============================================================================
-- Convenience: Named Counters (common pattern)
-- =============================================================================

||| Counter storage layout:
||| Offset 0-7:   Magic "IDRS_CNT" (8 bytes)
||| Offset 8-11:  Counter count (4 bytes)
||| Offset 12+:   Counter values (8 bytes each)

||| Counter slot size
counterSlotSize : Int
counterSlotSize = 8

||| Header size (magic + count)
counterHeaderSize : Int
counterHeaderSize = 12

||| Get counter value by index
export
getCounter : (index : Int) -> IO Int
getCounter idx = stableReadI64 (counterHeaderSize + idx * counterSlotSize)

||| Set counter value by index
export
setCounter : (index : Int) -> (value : Int) -> IO ()
setCounter idx val = stableWriteI64 (counterHeaderSize + idx * counterSlotSize) val

||| Increment counter and return new value
export
incCounter : (index : Int) -> IO Int
incCounter idx = do
  val <- getCounter idx
  let newVal = val + 1
  setCounter idx newVal
  pure newVal

||| Decrement counter and return new value
export
decCounter : (index : Int) -> IO Int
decCounter idx = do
  val <- getCounter idx
  let newVal = val - 1
  setCounter idx newVal
  pure newVal
