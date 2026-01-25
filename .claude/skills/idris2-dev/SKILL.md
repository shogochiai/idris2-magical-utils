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

**idris2-icwasm / idris2-yul projects are Idris2-complete**

- **IC WASM:** No Rust or C. Use idris2-icwasm only.
- **EVM:** No Solidity, Foundry, Hardhat. Use idris2-yul only.

Minimize external toolchain dependencies. Maximize Idris2 type safety.

## Compilation Targets

Idris2 パッケージは4つのバックエンドターゲットがある:

| Target | Backend | Build | `%foreign` prefix |
|--------|---------|-------|-------------------|
| Native | Chez Scheme | `pack build <pkg>` | (standard) |
| EVM | idris2-yul codegen | `idris2-yul` 経由 | `evm:*` |
| IC WASM | idris2-icwasm codegen | `idris2-icwasm` 経由 | `wasm:*`, `ic0:*` |
| JavaScript | Idris2 built-in | `--cg javascript` / `--cg node` | `javascript:*` |

### ターゲット判定方法

ソース内の `%foreign` 宣言で判定:
- `"evm:*"` → EVM ターゲット。`pack build` は **必ず失敗**（正常動作）
- `"javascript:*"` → JS ターゲット。ipkg に `--cg javascript` が必要
- `"wasm:*"` / `"ic0:*"` → IC WASM ターゲット
- 上記なし → Native (Chez) ターゲット

### パッケージ別ターゲット一覧

| Package | Target | `pack build` | 正しいビルド |
|---------|--------|--------------|-------------|
| idris2-textdao | EVM | ❌ expected fail | `idris2-yul` 経由 |
| idris2-ouf | EVM | ❌ expected fail | `idris2-yul` 経由 |
| idris2-subcontract | EVM (lib) | ✅ lib のみ | `idris2-yul` 経由 |
| oucdashboard | JS | ✅ (`--cg javascript` in ipkg) | `pack build` / ipkg opts |
| ouc | IC WASM | ✅ tests のみ | `idris2-icwasm` 経由 |
| icp-indexer | IC WASM | ✅ tests のみ | `idris2-icwasm` 経由 |
| lazyweb | Native | ✅ | `pack build` |
| (その他 magical-utils) | Native | ✅ | `pack build` |

### アーキテクチャ対応表

EVM 系と IC 系は対称的な3層構造:

| Layer | EVM | IC |
|-------|-----|-----|
| 低レベル型・生成 | idris2-evm (型・解釈器) + idris2-yul (codegen) | idris2-icwasm (生成+IC0 FFI、1パッケージで両方) |
| アプリFW | idris2-subcontract (UCS/ERC-7546) | idris2-cdk (StableMemory/FR Monad/ICP API) |
| カバレッジ | idris2-evm-coverage | idris2-dfx-coverage |

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
| `canister_entry.c` | `can.did` | `idris2-icwasm gen-entry` |
| `*.ttc` (TTC cache) | `*.idr` | `idris2 --build` |
| `build/` directory | Source | Build system |
