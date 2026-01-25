# TokenPJ - ERC20 Token Implementation

## What This Is

This is **implementation code** for an ERC20 token. It is NOT a standalone contract.

## Architecture (ERC-7546)

```
                              ┌──────────────────┐
  User tx ───────────────────►│      PROXY       │ ◄── User interacts HERE
  (to Proxy address)          │  (holds storage) │
                              └────────┬─────────┘
                                       │ lookup selector
                              ┌────────▼─────────┐
                              │    DICTIONARY    │
                              │  sel → implAddr  │
                              └────────┬─────────┘
                                       │ DELEGATECALL
                              ┌────────▼─────────┐
                              │   TokenPJ impl   │ ◄── This code
                              │ (Functions/*.idr)│
                              └──────────────────┘
```

**Storage lives in Proxy, code runs from implementation.**

## Entry Point

There is no single "main" entry point. Each function module exports an `Entry`:

```idris
-- In Functions/Transfer.idr
export transferEntry : Entry transferSig

-- In Functions/Mint.idr
export mintEntry : Entry mintSig
```

These entries are registered in the **Dictionary** at deploy time.

## Deployment Flow

```
1. Build each function module → EVM bytecode
2. Deploy bytecode → get implementation addresses
3. Deploy Dictionary:
   - Register 0xa9059cbb (transfer) → transferImpl
   - Register 0x40c10f19 (mint)     → mintImpl
   - ... etc
4. Deploy Proxy pointing to Dictionary
5. Give users the PROXY address
```

See `scripts/deploy.idr` for deployment script.

## Files

```
src/
├── Storages/
│   └── Schema.idr        # Storage layout (balances, allowances, etc.)
└── Functions/
    ├── Transfer.idr      # transfer, transferFrom
    ├── Approve.idr       # approve, allowance
    ├── Mint.idr          # mint (onlyOwner)
    └── View.idr          # balanceOf, totalSupply, owner
```

## Storage Schema

```idris
TokenSchema : Schema
TokenSchema = MkSchema "tokenpj.token.v1" TOKEN_ROOT
  [ Value "totalSupply" TUint256
  , Mapping "balances" TAddress TUint256
  , Mapping2 "allowances" TAddress TAddress TUint256
  , Value "owner" TAddress
  ]
```

## Function Selectors

| Function | Selector | Module |
|----------|----------|--------|
| `transfer(address,uint256)` | `0xa9059cbb` | Transfer.idr |
| `approve(address,uint256)` | `0x095ea7b3` | Approve.idr |
| `transferFrom(address,address,uint256)` | `0x23b872dd` | Transfer.idr |
| `mint(address,uint256)` | `0x40c10f19` | Mint.idr |
| `balanceOf(address)` | `0x70a08231` | View.idr |
| `totalSupply()` | `0x18160ddd` | View.idr |
| `allowance(address,address)` | `0xdd62ed3e` | Approve.idr |
