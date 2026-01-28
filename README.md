# idris2-magical-utils

Idris2 で構築されたブロックチェーン開発ライブラリ群。
EVM、ICP (Internet Computer)、Bitcoin Script 向けの型安全なツールチェーンを提供する。

## Packages

### Blockchain Backends

| Package | Description |
|---------|-------------|
| `Idris2Evm` | EVM インタプリタ、Yul codegen、FFI、ABI |
| `Idris2IcWasm` | Idris2 → IC WASM コンパイルパイプライン |
| `Idris2IcWasmSQLite` | ICP WASM canister 用 SQLite FFI バインディング |
| `Idris2Cdk` | Internet Computer Protocol (ICP) 向け CDK |
| `Idris2Subcontract` | UCS パターンの Subcontract フレームワーク |
| `Idris2BitcoinScript` | スタック安全な Bitcoin Script DSL (OP_CAT covenant 対応) |
| `Idris2Scrypt` | Idris2 → Bitcoin Script (sCrypt) コンパイル |

### Coverage Analysis

| Package | Description |
|---------|-------------|
| `Idris2CoverageCore` | カバレッジ解析の共通型 |
| `Idris2Coverage` | Idris2 コードカバレッジ (型駆動解析) |
| `Idris2EvmCoverage` | EVM セマンティックカバレッジ (Chez Scheme profiler) |
| `Idris2DfxCoverage` | ICP Canister カバレッジ解析 |
| `Idris2WebCoverage` | Web フロントエンドカバレッジ |

### Testing

| Package | Description |
|---------|-------------|
| `Idris2E2eHarness` | クロスチェーン (EVM/ICP) E2E テストハーネス |

## Usage

`pack.toml` で依存関係を宣言:

```toml
[custom.all.idris2-evm]
type   = "github"
url    = "git@github.com:shogochiai/idris2-magical-utils.git"
commit = "main"
ipkg   = "pkgs/Idris2Evm/idris2-evm.ipkg"
```

`.ipkg` に追加:

```
depends = base, idris2-evm
```

## Build

```bash
# 単体パッケージのビルド
pack build pkgs/Idris2Evm/idris2-evm.ipkg

# 品質チェック (lazy CLI)
lazy core ask pkgs/Idris2Cdk
```

## CI

GitHub Actions で `lazy core ask --steps=1,2,4` を実行し、`stparity`, `testorphans`, `testandcoverage` を検証する。
