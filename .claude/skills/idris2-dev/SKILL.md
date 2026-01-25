---
name: idris2-dev
description: Idris2 development guidelines including OOM avoidance, project conventions, and idris2-evm EVM compilation
triggers:
  - Idris2
  - idris2
  - .idr file
  - .ipkg file
  - pack.toml
  - idris2-evm
  - Yul compilation
  - EVM bytecode
  - OOM
  - memory explosion
---

# Idris2 Development

## Project Policy

**idris2-icwasm / idris2-evm projects are Idris2-complete**

- **IC WASM:** No Rust or C. Use idris2-icwasm only.
- **EVM:** No Solidity, Foundry, Hardhat. Use idris2-evm only (includes Yul codegen).

Minimize external toolchain dependencies. Maximize Idris2 type safety.

## Compilation Targets

Idris2 packages have 4 backend targets:

| Target | Backend | Build | `%foreign` prefix |
|--------|---------|-------|-------------------|
| Native | Chez Scheme | `pack build <pkg>` | (standard) |
| EVM | idris2-evm codegen | `idris2-yul` executable (from idris2-evm package) | `evm:*` |
| IC WASM | idris2-icwasm codegen | `idris2-icwasm` executable | `wasm:*`, `ic0:*` |
| JavaScript | Idris2 built-in | `--cg javascript` / `--cg node` | `javascript:*` |

### Target Detection

Detect target via `%foreign` declarations in source:
- `"evm:*"` -> EVM target. `pack build` will **always fail** (expected behavior)
- `"javascript:*"` -> JS target. ipkg needs `--cg javascript`
- `"wasm:*"` / `"ic0:*"` -> IC WASM target
- none of above -> Native (Chez) target

### Per-Package Build Table

| Package | Target | `pack build` | Correct Build |
|---------|--------|--------------|---------------|
| idris2-textdao | EVM | expected fail | `idris2-yul` executable |
| idris2-ouf | EVM | expected fail | `idris2-yul` executable |
| idris2-subcontract | EVM (lib) | lib only | `idris2-yul` executable |
| oucdashboard | JS | (`--cg javascript` in ipkg) | `pack build` / ipkg opts |
| ouc | IC WASM | tests only | `idris2-icwasm` executable |
| icp-indexer | IC WASM | tests only | `idris2-icwasm` executable |
| lazyweb | Native | OK | `pack build` |
| (other magical-utils) | Native | OK | `pack build` |

### Architecture Table

EVM and IC have symmetric 3-layer structure:

| Layer | EVM | IC |
|-------|-----|-----|
| Low-level types + codegen | idris2-evm (interpreter + Yul codegen, single package) | idris2-icwasm (codegen + IC0 FFI, single package) |
| App framework | idris2-subcontract (UCS/ERC-7546) | idris2-cdk (StableMemory/FR Monad/ICP API) |
| Coverage | idris2-evm-coverage | idris2-dfx-coverage |

**Note:** idris2-yul was merged into idris2-evm. The `idris2-yul` executable is now built from the idris2-evm package (entry point: `YulMain.idr`).

## Memory Explosion Patterns (OOM Avoidance)

Idris2 compilation can explode from ~165MB to 16GB+ RAM:

1. **`{auto prf}` overuse** - Proof search explodes. Demote to runtime verification.
2. **Large Nat pattern matching** - `mkFoo 11155111 = ...` is NG. Use if-else.
3. **Giant single modules** - Split modules over 500 lines. Use re-export facades for compatibility.
4. **Type-level state machines** - If many states, demote to runtime verification.
5. **Existential + multi-branch** - N branches x M functions = NxM expansion. Consolidate to single record.

**Development environment:** 256GB RAM recommended.

Details: `docs/idris2-memory-eater.md`

## Type Ambiguity

Same type name in multiple modules causes compiler backtracking during type inference. Keep packages cleanly separated. Don't duplicate types across packages. If RAM explodes, check for type name collisions first.

## idris2-evm Yul Codegen Known Bugs

### `/=` Operator Reverses Branch Logic
`if x /= 0 then A else B` compiles as `if x == 0 then A else B`.
**Workaround:** Always use `== 0` with swapped branches.

### Closure Parameter Ordering
When the Yul codegen creates closures across many let bindings, parameter order can get shuffled.
**Workaround:** Restructure code to minimize deep closure nesting. Read calldata after state changes.

### Missing EVM.Primitives
`codecopy`, `extcodecopy`, `extcodesize` IO wrapper functions are missing. Constants exist but IO functions don't.

## Git / pack.toml

**Always use SSH URLs for GitHub:**
- `git@github.com:org/repo.git` (correct)
- `https://github.com/org/repo` (wrong - auth issues)

## EVM Smart Contract Selector Pattern

For EVM smart contracts using idris2-subcontract, **always use `MkSig` + `selectorOf`** to auto-compute function selectors. Never hardcode selector values.

```idris
import Subcontract.Core.ABI.Sig

-- Define signature: name, input types, output types
transferSig : Sig
transferSig = MkSig "transfer" [TAddress, TUint256] [TBool]

-- Auto-compute selector (computes keccak256 at runtime, no hardcoding)
transferSel : Sel transferSig
transferSel = selectorOf transferSig  -- 0xa9059cbb

-- Use in Entry points
transferEntry : Entry transferSig
transferEntry = MkEntry transferSel $ do
  ...
```

**Available ABI types:** `TUint256`, `TBytes32`, `TAddress`, `TBool`

**Why:** Hardcoded selectors (e.g., `MkSel 0xa9059cbb`) are error-prone and opaque. `selectorOf` computes keccak256("transfer(address,uint256)") automatically, binding the selector to its signature via phantom types.

## Generated Files - No Manual Edits

| File | Source | Command |
|------|--------|---------|
| `canister_entry.c` | `can.did` | `idris2-icwasm gen-entry` |
| `*.ttc` (TTC cache) | `*.idr` | `idris2 --build` |
| `build/` directory | Source | Build system |
