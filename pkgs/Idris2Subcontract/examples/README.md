# idris2-subcontract Examples

## ⚠️ Important: How ERC-7546 Works

**Main.idr is NOT the entry point for users!**

```
User tx → PROXY → Dictionary lookup → DELEGATECALL → Main.idr (impl)
          ▲                                              │
          └──────── Storage lives here ◄─────────────────┘
```

1. **Proxy**: The address users interact with. Holds all storage.
2. **Dictionary**: Maps function selectors → implementation addresses
3. **Main.idr**: Implementation code. Runs via DELEGATECALL, writes to Proxy's storage.

When you deploy, users call the **Proxy address**, not the implementation.

---

## Project Structure

All projects follow the same pattern:

```
{ProjectName}PJ/
├── src/
│   ├── Main.idr                    # Entry point (dispatch)
│   └── Main/
│       ├── Storages/
│       │   └── Schema.idr          # Storage layout
│       ├── Functions/
│       │   └── *.idr               # Function modules
│       └── Tests/
│           └── *Test.idr           # Unit tests
└── scripts/
    ├── deploy.idr
    └── upgrade.idr
```

## Examples

### TokenPJ - ERC20 Token

Full ERC20-compatible token with:
- transfer, approve, transferFrom
- mint (onlyOwner)
- Events: Transfer, Approval

```idris
TokenSchema : Schema
TokenSchema = MkSchema "tokenpj.token.v1" TOKEN_ROOT
  [ Value "totalSupply" TUint256
  , Mapping "balances" TAddress TUint256
  , Mapping2 "allowances" TAddress TAddress TUint256
  , Value "owner" TAddress
  ]
```

### CounterPJ - Simple Counter

Minimal example for learning:
- increment, decrement, add
- getCount
- Event: CountChanged

```idris
CounterSchema : Schema
CounterSchema = MkSchema "counterpj.counter.v1" COUNTER_ROOT
  [ Value "count" TUint256
  , Value "owner" TAddress
  ]
```

### VaultPJ - ETH Vault

ETH deposit/withdrawal vault with:
- deposit (payable), withdraw, withdrawAll
- pause/unpause (onlyOwner)
- Events: Deposited, Withdrawn, Paused, Unpaused

```idris
VaultSchema : Schema
VaultSchema = MkSchema "vaultpj.vault.v1" VAULT_ROOT
  [ Mapping "deposits" TAddress TUint256
  , Value "totalDeposits" TUint256
  , Value "owner" TAddress
  , Value "paused" TBool
  ]
```

## Architecture

```
                          ┌──────────────┐
  User ──call──► Proxy ──►│  Dictionary  │
                 │        │ sel → impl   │
                 │        └──────┬───────┘
                 │ DELEGATECALL  │
                 │◄──────────────┘
                 ▼
           ┌───────────┐
           │   *PJ     │  ◄── Your implementation
           │ (Schema)  │
           └───────────┘
                 │
    Storage in Proxy (via Schema)
```

## Commands

### Build

```bash
cd ~/code/idris2-yul
./scripts/build-contract.sh ../idris2-subcontract/examples/TokenPJ/src/Main.idr
./scripts/build-contract.sh ../idris2-subcontract/examples/CounterPJ/src/Main.idr
./scripts/build-contract.sh ../idris2-subcontract/examples/VaultPJ/src/Main.idr
```

### Test

```bash
pack run idris2-evm -- \
  --contract 0x1000:build/output/Main.bin \
  --call 0x1000 \
  --calldata 0x...
```

## Schema Field Types

| Field | Solidity | Accessor |
|-------|----------|----------|
| `Value name ty` | `uint256 name;` | `schemaValue` |
| `Mapping name k v` | `mapping(K => V)` | `schemaMapping` |
| `Mapping2 name k1 k2 v` | `mapping(K1 => mapping(K2 => V))` | `schemaMapping2` |
| `Array name ty` | `T[]` | `schemaArrayAt` |
