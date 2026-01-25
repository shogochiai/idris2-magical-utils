# CounterPJ - Simple Counter Implementation

## What This Is

Minimal example for learning idris2-subcontract. This is **implementation code**, not a standalone contract.

## Architecture (ERC-7546)

```
  User tx ──► PROXY ──► Dictionary ──► CounterPJ impl
              ▲                              │
              └──── Storage (count) ◄────────┘
```

**Users call the Proxy. Storage lives in Proxy. Code runs via DELEGATECALL.**

## Entry Point

No "main". Each function is an independent entry:

```idris
-- In Functions/Counter.idr
export incrementEntry : Entry incrementSig
export decrementEntry : Entry decrementSig
export addEntry       : Entry addSig
export getCountEntry  : Entry getCountSig
```

## Deployment Flow

1. Build `Functions/Counter.idr` → bytecode
2. Deploy → get implAddr
3. Deploy Dictionary, register all selectors → implAddr
4. Deploy Proxy → give this address to users

## Files

```
src/
├── Storages/
│   └── Schema.idr       # count, owner
└── Functions/
    └── Counter.idr      # increment, decrement, add, getCount
```

## Storage Schema

```idris
CounterSchema : Schema
CounterSchema = MkSchema "counterpj.counter.v1" COUNTER_ROOT
  [ Value "count" TUint256
  , Value "owner" TAddress
  ]
```

## Function Selectors

| Function | Selector |
|----------|----------|
| `increment()` | `0xd09de08a` |
| `decrement()` | `0x2baeceb7` |
| `add(uint256)` | `0x1003e2d2` |
| `getCount()` | `0xa87d942c` |
