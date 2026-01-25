# idris2-wasm

Idris2 → WebAssembly compilation pipeline for ICP canisters.

## Strategy

```
Idris2 (.idr) → RefC backend → C code → Emscripten → WASM
```

## Build

```bash
# 1. Generate C from Idris2
idris2 --codegen refc -o main.c Main.idr

# 2. Compile C to WASM with Emscripten
emcc main.c -o main.wasm
```

## Candid→C Code Generation (CandidStubs)

**canister_entry.c の自動生成機能**

`WasmBuilder/CandidStubs.idr` (596行) は .did ファイルから C コードを生成:

| 機能 | 状態 | 説明 |
|------|------|------|
| .did パーサー | ✅ 実装済 | 20+ Candid型をサポート (Nat, Int, Text, Bool, Record, Variant等) |
| 型テーブル生成 | ✅ 実装済 | LEB128エンコード + フィールドハッシュ |
| レスポンスエンコード | ✅ 実装済 | DIDL binary format 生成 |
| **引数デコード** | ❌ 未実装 | Candid→C引数パース (Gap) |

### 使用方法

```idris
import WasmBuilder.WasmBuilder
import WasmBuilder.CandidStubs

-- can.did から canister_entry.c を生成
generateCanisterEntryC : String -> String -> IO ()
```

### 生成される C コード例

```c
__attribute__((export_name("canister_query getVersion")))
void canister_query_getVersion(void) {
    // Candid binary reply (DIDL + type table + value)
    static const uint8_t r[] = {
        'D','I','D','L',  // magic
        0x00,             // empty type table
        0x01, 0x7D,       // 1 return value, type: nat
        0x01              // value: 1
    };
    ic0_msg_reply_data_append((int32_t)(uintptr_t)r, sizeof(r));
    ic0_msg_reply();
}
```

### 利用側プロジェクトへの指示

**原則:** `can.did` を単一真実源とし、`canister_entry.c` は生成物として扱う

手動で `canister_entry.c` を書いている場合 (idris2-ouc 等):
1. それは tech debt
2. 将来的に CandidStubs の引数デコード実装後、自動生成に移行すべき
3. 現時点では `can.did` に追加したメソッドは手動で C に追加が必要

## Source Map Generation

ビルド時に `build/idris2-c.map` を生成。C行番号→Idrisソースファイル/関数名のマッピング。

```json
{
  "names": ["Main.canisterUpdate", "Main.computeSum", "PrimIO.unsafePerformIO", ...],
  "sources": ["Main.idr", "Prelude/IO.idr", ...]
}
```

カバレッジ計算の分母（テスト対象関数一覧）として使用可能。

## WASI Stubbing

ICP rejects WASI imports (`fd_close`, `fd_write`, etc.). Remove them post-build:

```bash
# Convert to WAT, remove WASI imports, convert back
wasm2wat build/canister.wasm -o /tmp/temp.wat
python3 stub_wasi.py /tmp/temp.wat /tmp/temp_stubbed.wat
wat2wasm --debug-names /tmp/temp_stubbed.wat -o build/canister_stubbed.wasm
```

### stub_wasi.py

```python
import sys, re
input_file, output_file = sys.argv[1], sys.argv[2]
with open(input_file, 'r') as f:
    content = f.read()
wasi_imports = re.findall(r'\(import "wasi_snapshot_preview1"[^)]+\)', content)
content = re.sub(r'\(import "wasi_snapshot_preview1"[^)]+\)\n', '', content)
with open(output_file, 'w') as f:
    f.write(content)
print(f"Removed {len(wasi_imports)} WASI imports")
```

## ic-wasm Instrumentation

ICP canister向けにprofilingを追加する場合：

```bash
# 1. WASI stubbing済みWASMを生成
idris2-wasm build --canister=xxx

# 2. ic-wasmでinstrument
# 重要: --start-page 10 でcanisterのstable memory (0-9) と衝突回避
ic-wasm build/xxx_stubbed.wasm -o build/xxx_instrumented.wasm instrument \
  --start-page 10 --page-limit 16
```

**重要**: ic-wasmは以下の関数を追加：
- `__get_profiling` - トレースデータ取得（query）
- `__toggle_tracing` - トレース有効/無効切替（update）※呼ぶとトレース無効化！
- `__toggle_entry` - ログ保持モード切替（update）
- `__get_cycles` - サイクルカウント取得（query）

### Stable Memory Pre-allocation for Profiling

ic-wasmはstable memoryにトレースを書き込む。canisterが独自にstable memoryを使う場合、
衝突を避けるため`canister_init`で事前確保が必要：

```c
// canister_entry.c (自動生成)
void canister_init(void) {
    // Pre-allocate 26 pages: 0-9 for canister data, 10-25 for ic-wasm profiling
    ic0_stable64_grow(26);
    ensure_idris2_init();
}
```

WasmBuilder.idrがこれを自動生成する。

### Entry Mode vs Full Tracing

```
Entry Mode (__toggle_entry):
  - エクスポート関数の entry/exit のみ記録
  - 内部関数呼び出しは記録されない
  - メソッドカバレッジ用

Full Tracing (__toggle_tracing):
  - 全関数の entry/exit を記録
  - ※現状空を返す（要調査）
```

## IC0 Inter-Canister Call FFI

`support/ic0/ic_call.c` provides C helper functions for ICP inter-canister calls.
Required by `WasmBuilder.IC0.Call` FFI bindings.

### Buffer Management

```c
// Buffer IDs
#define BUFFER_CALLEE  0  // 29 bytes for Principal
#define BUFFER_METHOD  1  // 64 bytes for method name
#define BUFFER_PAYLOAD 2  // 1024 bytes for Candid payload

// FFI Functions
void ic_call_write_byte(int32_t buf_id, int32_t idx, int32_t byte);
int32_t ic_call_get_ptr(int32_t buf_id);
void ic_call_set_len(int32_t buf_id, int32_t len);
int32_t ic_call_response_ptr(void);
int32_t ic_call_response_len(void);
int32_t ic_call_response_byte(int32_t idx);
int32_t ic_call_status(void);
void ic_call_set_status(int32_t status);
```

### Usage from Idris2

```idris
-- In WasmBuilder.IC0.Call
%foreign "C:ic_call_write_byte,libic_call"
prim__ic_call_write_byte : Int -> Int -> Int -> PrimIO ()

-- Write Principal bytes to callee buffer
export
setCalleeByte : Int -> Int -> IO ()
setCalleeByte idx byte = primIO $ prim__ic_call_write_byte 0 idx byte
```

### Build Integration

`scripts/build-canister.sh` automatically includes `ic_call.c`:

```bash
emcc "$C_FILE" ... \
    "$IC0_SUPPORT/ic_call.c" \
    ...
```

## Dead Code Elimination

RefCバックエンドは積極的にデッドコード除去を行う。
`main`から到達不能な関数はWASMに含まれない。

```idris
-- これだけだとcanisterUpdateは除去される
main = canisterInit

-- canisterUpdateを含めるには呼び出しが必要
main = do
  canisterInit
  _ <- canisterUpdate 0
  pure ()
```

## References

- [Emscripten](https://emscripten.org/docs/compiling/WebAssembly.html)
- [Idris2 RefC backend](https://github.com/idris-lang/Idris2)
- [ic-wasm](https://github.com/dfinity/ic-wasm) - WASM instrumentation for ICP

## Dependency Chain (このPJの依存関係)

```
idris2-wasm (このPJ)
  └── ic-wasm (Fork of dfinity/ic-wasm) ← このPJが追跡義務を持つ
        PR#104, PR#107 マージ待ち

依存されている側:
idris2-dfx-coverage → idris2-wasm
Lazy → LazyDfx → idris2-dfx-coverage → idris2-wasm
```

**監視責任:**
- `lazy.toml` で ic-wasm の Fork と PR を追跡中
- `lazy core ask` 実行時に UpstreamBehind / UpstreamNewEvents を検出

## Upstream Tracking (lazy.toml)

このPJは ic-wasm に直接依存しているため、`lazy.toml` で追跡:

```toml
[[upstream.forks]]
name = "ic-wasm"
local_path = "/Users/bob/code/ic-wasm"
upstream = "dfinity/ic-wasm"
fork = "pochi/ic-wasm"
base_branch = "main"

[[tracking.prs]]
url = "https://github.com/dfinity/ic-wasm/pull/104"

[[tracking.prs]]
url = "https://github.com/dfinity/ic-wasm/pull/107"
```

### Gap検出時の行動

`lazy core ask` で Upstream Gap が検出されたら:

```bash
# UpstreamNewEvents - PR/Issueに新イベント
gh pr view https://github.com/dfinity/ic-wasm/pull/104
gh pr checks https://github.com/dfinity/ic-wasm/pull/104

# UpstreamBehind - Forkがupstreamより遅れている
cd /Users/bob/code/ic-wasm
git fetch upstream
git log HEAD..upstream/main --oneline
git merge upstream/main  # 必要なら
```

## Upstream Dependencies (マージ状況)

### Merged
- **Idris2 RefC WASM比較演算子**: main にマージ済み

### Pending
- **ic-wasm PR#104**: Merge待ち
- **ic-wasm PR#107**: Merge待ち
