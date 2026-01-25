# idris2-subcontract Architecture

This document explains the layer design of the Idris2 EVM smart contract stack and the rationale behind architectural decisions.

## Overview

The stack consists of three layers:

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Application Layer                                │
│                    (idris2-textdao, etc.)                          │
│                    Business logic, domain-specific code            │
└──────────────────────────────┬──────────────────────────────────────┘
                               │ imports
┌──────────────────────────────▼──────────────────────────────────────┐
│                    Framework Layer                                  │
│                    (idris2-subcontract)                            │
│                    Standards, patterns, DSLs                        │
└──────────────────────────────┬──────────────────────────────────────┘
                               │ imports
┌──────────────────────────────▼──────────────────────────────────────┐
│                    Compiler Layer                                   │
│                    (idris2-yul)                                    │
│                    FFI primitives, code generation                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Layer Responsibilities

### idris2-yul (Compiler Layer)

**Purpose**: Provide the lowest-level EVM interface and Yul code generation.

**Modules**:
- `EVM.Primitives` - All `%foreign "evm:*"` FFI definitions
- `EVM.Storage.Namespace` - ERC-7201 slot calculations
- `EVM.ABI.*` - ABI type definitions and JSON generation
- `Compiler.EVM.*` - Yul IR and code generation

**Design Principle**: Minimal, low-level, no business logic.

```idris
-- EVM.Primitives: Pure FFI wrappers
%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

export sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)
```

### idris2-subcontract (Framework Layer)

**Purpose**: Provide reusable patterns, standards implementations, and DSLs.

**Modules**:
```
Subcontract/
├── Standards/
│   └── ERC7546/         # ERC-7546 UCS implementation
├── Core/
│   ├── Entry.idr        # Type-safe entry points
│   ├── StorageCap.idr   # Storage capability pattern
│   └── ABI/             # Sig, Decoder
└── Std/
    └── Functions/       # Standard function implementations
```

**Design Principle**: Provide patterns that applications compose, not inherit.

### Application Layer (your project)

**Purpose**: Business logic using framework patterns.

**Example**: idris2-textdao uses Subcontract.Core.Entry for type-safe dispatch.

## Key Design Decisions

### 1. FFI Consolidation

**Problem**: FFI definitions (`%foreign "evm:*"`) were scattered across multiple files.

**Solution**: All FFI definitions in `EVM.Primitives`.

**Rationale**:
- Single source of truth for EVM interface
- Easier to audit for security
- Clear dependency: everything imports Primitives

```
Before:                          After:
┌─────────────┐                 ┌─────────────┐
│ ERC7201.idr │ ←── FFI         │ Primitives  │ ←── ALL FFI
│ ERC7546.idr │ ←── FFI         └──────┬──────┘
│ Entry.idr   │ ←── FFI                │
└─────────────┘                        ▼
                                ┌──────────────┐
                                │ Namespace.idr│ (no FFI)
                                │ ERC7546/*    │ (no FFI)
                                └──────────────┘
```

### 2. ERC-7546 in idris2-subcontract

**Problem**: ERC-7546 is a standard, not a compiler feature. Having it in idris2-yul mixed abstraction levels.

**Solution**: Move ERC-7546 to `Subcontract.Standards.ERC7546.*`.

**Rationale**:
- Standards belong in the framework layer
- Compiler layer should be standard-agnostic
- Applications can use different standards

### 3. Storage Capability Pattern

**Problem**: Direct storage access (`sload`/`sstore`) anywhere makes it hard to reason about what code can modify state.

**Solution**: `StorageCap` - an opaque capability token.

```idris
-- Constructor NOT exported
data StorageCap : Type where
  MkStorageCap : StorageCap

-- Storage operations require the cap
sloadCap : StorageCap -> Integer -> IO Integer

-- Handler receives cap from framework
Handler : Type -> Type
Handler a = StorageCap -> IO a

-- Only framework can run handlers
runHandler : Handler a -> IO a
runHandler h = h MkStorageCap
```

**Rationale**:
- Type signatures show storage intent
- Code review: "takes StorageCap = modifies storage"
- Enforced at module boundary (MkStorageCap not exported)

### 4. Type-Safe Entry Points

**Problem**: Manual selector dispatch is error-prone.

**Solution**: Phantom-typed selectors bound to signatures.

```idris
-- Selector bound to signature via phantom type
data Sel : (sig : Sig) -> Type where
  MkSel : (value : Integer) -> Sel sig

-- Entry point requires matching types
record Entry (sig : Sig) where
  constructor MkEntry
  selector : Sel sig
  handler : IO ()
```

**Rationale**:
- Type system prevents selector/handler mismatch
- Signature changes force selector updates
- Compile-time safety for ABI compatibility

### 5. Decoder Monad

**Problem**: Manual offset tracking in calldata parsing is error-prone.

**Solution**: Decoder monad with automatic offset advancement.

```idris
-- Offset tracked in state
record Decoder a where
  runDec : Integer -> IO (a, Integer)

-- Each decode advances offset by 32
decodeAddress : Decoder Address
decodeUint256 : Decoder Uint256

-- Compose naturally
decodeTransfer : Decoder (Address, Uint256)
decodeTransfer = do
  to <- decodeAddress
  amount <- decodeUint256
  pure (to, amount)
```

**Rationale**:
- Eliminates offset calculation errors
- Composable via standard monad operations
- Type-safe: Address vs Uint256 distinct

## Module Dependencies

```
┌────────────────────────────────────────────────────────────────┐
│ Subcontract.Std.Functions.*                                    │
│   └── imports: Core.*, Standards.*, EVM.Primitives             │
├────────────────────────────────────────────────────────────────┤
│ Subcontract.Core.Entry                                         │
│   └── imports: Core.ABI.*, EVM.Primitives                      │
├────────────────────────────────────────────────────────────────┤
│ Subcontract.Core.StorageCap                                    │
│   └── imports: EVM.Primitives, EVM.Storage.Namespace           │
├────────────────────────────────────────────────────────────────┤
│ Subcontract.Standards.ERC7546.*                                │
│   └── imports: EVM.Primitives                                  │
├────────────────────────────────────────────────────────────────┤
│ EVM.Storage.Namespace                                          │
│   └── imports: EVM.Primitives                                  │
├────────────────────────────────────────────────────────────────┤
│ EVM.Primitives (no dependencies except base/contrib)           │
└────────────────────────────────────────────────────────────────┘
```

## Future Directions

### Error DSL

Current limitation: `evmRevert 0 0` provides no error message.

Planned:
```idris
-- Define custom errors
data MyError = NotOwner | InsufficientBalance Integer

-- Revert with encoded error
revertWith : MyError -> IO ()
```

### Cheat DSL (Testing)

Planned testing utilities:
```idris
-- Expect next call to revert
expectRevert : Error -> Script () -> Script ()

-- Impersonate address
prank : Address -> Script a -> Script a

-- Set storage directly
store : Address -> Slot -> Value -> Script ()
```

### Storage DSL

Declarative schema definition:
```idris
-- Define schema like Solidity struct
TokenSchema : Schema
TokenSchema = schema "myapp.token" $ do
  field "totalSupply" TUint256
  mapping "balances" TAddress TUint256
  array "holders" TAddress

-- Auto-generated accessors
getTotalSupply : IO Integer
getBalance : Address -> IO Integer
```

## Summary

| Layer | Package | Responsibility |
|-------|---------|----------------|
| Compiler | idris2-yul | FFI, storage calculations, code gen |
| Framework | idris2-subcontract | Standards, patterns, DSLs |
| Application | your-project | Business logic |

The separation ensures:
- Clear responsibilities per layer
- Reusable framework patterns
- Type-safe composition
- Auditable FFI boundary
