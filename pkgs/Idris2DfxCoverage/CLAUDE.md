# idris2-dfx-coverage

ICP Canister (Idris2) 向けカバレッジ分析ツール。

## Coverage Calculation Formula

```
カバレッジ = 実行された関数 / テスト対象関数
           = Profiling traces  / Source Map names (または Dumpcases)
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  分子: 実行された関数                                        │
├─────────────────────────────────────────────────────────────┤
│  ic-wasm __get_profiling → WASM関数ID一覧                   │
│    ↓                                                        │
│  WASM関数ID → C関数名 → Idris関数名                         │
│    (Source Mapで変換)                                       │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  分母: テスト対象関数                                        │
├─────────────────────────────────────────────────────────────┤
│  Option 1: idris2-c.map の names 配列                       │
│  Option 2: idris2 --dumpcases の出力                        │
└─────────────────────────────────────────────────────────────┘
```

## ic-wasm Profiling Usage

```bash
# 1. Instrument
ic-wasm xxx_stubbed.wasm -o xxx_instrumented.wasm instrument --stub-wasi

# 2. Deploy
dfx canister install xxx --wasm xxx_instrumented.wasm

# 3. Get traces (重要: __toggle_tracingは呼ばない！)
dfx canister call xxx __toggle_entry '()'  # ログ保持モード有効化
dfx canister call xxx someMethod '()'       # テスト実行
dfx canister call xxx '__get_profiling' '(0 : nat32)'
```

### Profiling Output Format

```
(vec {
  record { <func_id> : int32; <cycles> : int64 };  // 関数entry
  record { -<func_id> : int32; <cycles> : int64 }; // 関数exit (負のID)
}, <next_cursor>)
```

### Key Insight: Tracing State

```
canister_init終了時:
  is_init = 0  → トレース有効（デフォルト）

__toggle_tracing呼び出し:
  is_init XOR 1 → トレース無効化！

__toggle_entry呼び出し:
  is_entry = 1  → 呼び出し間でログ保持
```

**罠**: `__toggle_tracing`を呼ぶとトレースが**無効化**される。呼ばないこと。

## Source Map Integration

`build/idris2-c.map` から関数名を取得:

```json
{
  "names": [
    "Main.canisterUpdate",
    "Main.computeSum",
    "PrimIO.unsafePerformIO",
    ...
  ]
}
```

### Filtering Idris Functions

```bash
# Idris由来の関数のみ抽出
cat idris2-c.map | jq -r '.names[]' | grep -E "^(Main\.|Prelude\.|PrimIO\.)"
```

## Existing Parsers

- `src/DfxCoverage/IcWasm/ProfilingParser.idr` - `__get_profiling`出力パーサー
- `src/DfxCoverage/Idris2/DumpcasesParser.idr` - dumpcasesパーサー

## wasm-objdump による関数インデックス取得（推奨）

WASM debug infoから直接関数インデックス↔名前のマッピングを取得：

```bash
wasm-objdump -x build/xxx_stubbed.wasm | grep -E "^ - func\["
```

出力例：
```
 - func[11] sig=0 <Main_main>
 - func[14] sig=0 <Main_dispatchCommand>
 - func[203] sig=2 <canister_update_initialOUCState>
```

### Python でのパース

```python
import subprocess, re

result = subprocess.run(['wasm-objdump', '-x', 'build/xxx.wasm'], capture_output=True, text=True)
funcs = {}
for line in result.stdout.split('\n'):
    m = re.search(r'func\[(\d+)\].*<([^>]+)>', line)
    if m:
        idx, name = int(m.group(1)), m.group(2)
        funcs[idx] = name

# Idris関数のみフィルタ
idris_funcs = {k: v for k, v in funcs.items() if not any(v.startswith(p) for p in
    ['ic0_', '__wasi', '__wasm', 'csegen', '_brace', 'idris2_', 'canister_'])}
```

## High Impact Targets 特定

未カバー＋重要な関数を静的解析で特定：

```python
high_impact_patterns = ['donate', 'reward', 'transfer', 'submit', 'register', 'execute', 'owner', 'admin']
for idx, name in idris_funcs.items():
    for pattern in high_impact_patterns:
        if pattern in name.lower():
            print(f"⚠️ [{idx}] {name} [{pattern}]")
```

### FunctionCoverage.idr

`src/DfxCoverage/FunctionCoverage.idr` が以下を提供：
- `parseWasmFunctions` - wasm-objdumpで関数一覧取得
- `calculateCoverage` - プロファイリングデータとの突合
- `findHighImpactTargets` - 未カバー重要関数特定

## Stable Memory Layout for Profiling

```
┌─────────────────────────────────────────┐
│ Pages 0-9:  Canister data (stable vars) │
│ Pages 10-25: ic-wasm profiling traces   │
└─────────────────────────────────────────┘
```

ic-wasm instrument時に `--start-page 10 --page-limit 16` を指定。
canister_initで `ic0_stable64_grow(26)` で事前確保（idris2-wasmが自動生成）。

## Dependency Chain (このPJの依存関係)

```
idris2-dfx-coverage (このPJ)
  └── idris2-wasm ← ic-wasm追跡義務はこちらが持つ
        └── ic-wasm (Fork of dfinity/ic-wasm)
              PR#104, PR#107 マージ待ち

依存されている側:
Lazy → LazyDfx → idris2-dfx-coverage
```

**監視責任の所在:**
- ic-wasm の追跡は `idris2-wasm/lazy.toml` が担当（直接依存のため）
- このPJは idris2-wasm に依存しているが、ic-wasm を直接使っていないため追跡義務なし

## Limitations

1. **Entry Mode の制限**
   - `__toggle_entry`はエクスポート関数のentry/exitのみ記録
   - 内部Idris関数呼び出しは記録されない
   - → メソッドカバレッジには使える、関数カバレッジには不十分

2. **Full Tracing が動作しない**
   - `__toggle_tracing` は空のトレースを返す（2024年1月時点）
   - 要調査

3. **Cスタブ問題**
   - canister_entry.cがCスタブを提供する場合、Idris関数が呼ばれない
   - 解決: Cから`ensure_idris2_init()`を再実行してIdris main経由で呼び出す

4. **Dead Code Elimination**
   - `main`から到達不能な関数はWASMに含まれない
   - 解決: mainから全テスト対象関数を呼び出すようにする
