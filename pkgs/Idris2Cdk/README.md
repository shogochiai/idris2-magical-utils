# idris2-cdk

Idris2 Canister Development Kit for Internet Computer Protocol (ICP).

Type-safe, dependently-typed bindings for building ICP canisters in Idris2.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      idris2-ouc                             │
│               (OptimisticUpgraderCanister)                  │
└─────────────────────────────────────────────────────────────┘
                            │ uses
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                      idris2-cdk                             │
│   ICP.IC0 │ ICP.API │ ICP.Candid │ ICP.StableMemory        │
└─────────────────────────────────────────────────────────────┘
                            │ built on
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                      idris2-wasm                            │
│   Idris2 → RefC → Emscripten → WASM → WASI stub → IC       │
│   (Reference implementation proving the pipeline works)     │
└─────────────────────────────────────────────────────────────┘
```

## Features

- **Type-safe IC0 bindings** - Raw FFI primitives with proper Idris2 types
- **Safe API wrappers** - High-level IO-based operations
- **Candid support** - `Candidable` typeclass for serialization
- **Stable memory** - `Storable` typeclass with `StableValue`/`StableSeq`
- **HTTP outcalls** - Typed HTTP request/response handling
- **Threshold ECDSA** - Sign messages with distributed keys

## Installation

Add to your `pack.toml`:

```toml
[custom.all.idris2-cdk]
type = "github"
url  = "https://github.com/shogochiai/idris2-cdk"
ipkg = "idris2-cdk.ipkg"
```

## Quick Start

```idris
import ICP.API
import ICP.Candid.Types

-- Get caller and current time
greet : IO ()
greet = do
  c <- caller
  t <- time
  debugPrint "Hello from canister!"
```

## Modules

| Module | Description |
|--------|-------------|
| `ICP.IC0` | FFI boundary layer (raw ic0 system calls) |
| `ICP.API` | Safe wrappers (caller, time, cycles, etc.) |
| `ICP.Candid.Types` | Candid type system with `Candidable` |
| `ICP.StableMemory` | `Storable` typeclass, `StableValue`/`StableSeq` |
| `ICP.Management.HttpOutcall` | HTTP outcall types and helpers |
| `ICP.Management.TECDSA` | Threshold ECDSA signing |

## Build

```bash
idris2 --build idris2-cdk.ipkg
# or
pack build idris2-cdk
```

## Related Projects

| Project | Description |
|---------|-------------|
| [idris2-wasm](https://github.com/shogochiai/idris2-wasm) | Reference implementation - Idris2→WASM→IC pipeline |
| [idris2-ouc](https://github.com/shogochiai/idris2-optimisticupgradercanister) | OptimisticUpgraderCanister built with idris2-cdk |

## License

MIT
