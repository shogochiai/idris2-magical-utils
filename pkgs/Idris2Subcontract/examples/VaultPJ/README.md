# VaultPJ - ETH Vault Implementation

## What This Is

ETH deposit/withdrawal vault with pause functionality. This is **implementation code**, not a standalone contract.

## Architecture (ERC-7546)

```
  User tx ──► PROXY ──► Dictionary ──► VaultPJ impl
   (ETH)      ▲                              │
              └──── Storage (deposits) ◄─────┘
```

**ETH sent to Proxy stays in Proxy. Code runs via DELEGATECALL.**

## Entry Point

No "main". Each function is an independent entry:

```idris
-- Deposit
export depositEntry    : Entry depositSig      -- payable
export depositOfEntry  : Entry depositOfSig

-- Withdraw
export withdrawEntry    : Entry withdrawSig
export withdrawAllEntry : Entry withdrawAllSig

-- Admin
export pauseEntry   : Entry pauseSig
export unpauseEntry : Entry unpauseSig
export ownerEntry   : Entry ownerSig
```

## Deployment Flow

1. Build function modules → bytecode
2. Deploy implementations
3. Deploy Dictionary with selector mappings
4. Deploy Proxy → users send ETH here

## Files

```
src/
├── Storages/
│   └── Schema.idr       # deposits mapping, totalDeposits, paused
└── Functions/
    ├── Deposit.idr      # deposit (payable), depositOf
    ├── Withdraw.idr     # withdraw, withdrawAll
    └── Admin.idr        # pause, unpause, owner, totalDeposits
```

## Storage Schema

```idris
VaultSchema : Schema
VaultSchema = MkSchema "vaultpj.vault.v1" VAULT_ROOT
  [ Mapping "deposits" TAddress TUint256
  , Value "totalDeposits" TUint256
  , Value "owner" TAddress
  , Value "paused" TBool
  ]
```

## Function Selectors

| Function | Selector | Notes |
|----------|----------|-------|
| `deposit()` | `0xd0e30db0` | payable |
| `depositOf(address)` | `0x8f601f66` | view |
| `withdraw(uint256)` | `0x2e1a7d4d` | |
| `withdrawAll()` | `0x853828b6` | |
| `pause()` | `0x8456cb59` | onlyOwner |
| `unpause()` | `0x3f4ba83a` | onlyOwner |
| `owner()` | `0x8da5cb5b` | view |
| `totalDeposits()` | `0x7d882097` | view |

## Events

| Event | Topic0 |
|-------|--------|
| `Deposited(address indexed, uint256)` | `0x2da466a7...` |
| `Withdrawn(address indexed, uint256)` | `0x7fcf532c...` |
| `Paused()` | `0x62e78cea...` |
| `Unpaused()` | `0x5db9ee0a...` |
