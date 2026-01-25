---
name: idris2-dev
description: Idris2 development guidelines including OOM avoidance, project conventions, and idris2-yul/idris2-evm EVM compilation
triggers:
  - Idris2
  - idris2
  - .idr file
  - .ipkg file
  - pack.toml
  - idris2-yul
  - idris2-evm
  - Yul compilation
  - EVM bytecode
  - OOM
  - memory explosion
---

# Idris2 Development

## Project Policy

**idris2-wasm / idris2-yul projects are Idris2-complete**

- **IC WASM:** No Rust or C. Use idris2-wasm only.
- **EVM:** No Solidity, Foundry, Hardhat. Use idris2-yul only.

Minimize external toolchain dependencies. Maximize Idris2 type safety.

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

## idris2-yul Known Bugs

### `/=` Operator Reverses Branch Logic
`if x /= 0 then A else B` compiles as `if x == 0 then A else B`.
**Workaround:** Always use `== 0` with swapped branches.

### Closure Parameter Ordering
When idris2-yul creates closures across many let bindings, parameter order can get shuffled.
**Workaround:** Restructure code to minimize deep closure nesting. Read calldata after state changes.

### Missing EVM.Primitives
`codecopy`, `extcodecopy`, `extcodesize` IO wrapper functions are missing. Constants exist but IO functions don't.

## Git / pack.toml

**Always use SSH URLs for GitHub:**
- `git@github.com:org/repo.git` (correct)
- `https://github.com/org/repo` (wrong - auth issues)

## Generated Files - No Manual Edits

| File | Source | Command |
|------|--------|---------|
| `canister_entry.c` | `can.did` | `idris2-wasm gen-entry` |
| `*.ttc` (TTC cache) | `*.idr` | `idris2 --build` |
| `build/` directory | Source | Build system |
