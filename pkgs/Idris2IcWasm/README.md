# idris2-icwasm

Idris2 → ICP Canister toolchain. Type-safe SQLite persistence with automatic stable memory save.

## Key Features

### SQLite + StableStorage 密結合

**SQLite を使う = stable save が必須。忘れる余地がない。**

```idris
import IcWasm.SQLite

-- SqliteHandle は initSqlite でのみ取得可能
-- initSqlite は StableConfig を要求する
-- → StableConfig なしに SQL 実行不可 = stable save を忘れない
handle <- initSqlite (MkStableConfig 1 0 1024)

-- 全 SQL 操作に handle が必要
_ <- sqlExec handle "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"
_ <- sqlPrepare handle "SELECT * FROM users"
SqlRow <- sqlStep handle
  | _ => pure ()
name <- sqlColumnText handle 1
_ <- sqlFinalize handle
```

### 型レベルの保証

```
sqlOpen  — private (直接呼べない)
sqlExec  — SqliteHandle が必要 (handle なしに SQL 実行不可)
initSqlite — StableConfig が必要 (config なしに handle 取得不可)
```

```
gen-entry + --init=<any function>
  → canister_pre_upgrade に sqlite_stable_save を自動挿入
  → 開発者が pre_upgrade save を忘れる経路がゼロ
```

### Timer API (heartbeat 置換)

```idris
import WasmBuilder.IC0.FFI

-- heartbeat は使わない ($34,000/月)
-- timer で必要な時だけ発火 ($112/月 idle)
_ <- setGlobalTimer timestampNanos
```

## Quick Start

### 1. Create canister

```bash
# can.did にメソッド定義
# can.cmd-map に CMD ID マッピング
# Main.idr にディスパッチハンドラ
```

### 2. Generate canister_entry.c

```bash
idris2-icwasm gen-entry \
  --did=can.did \
  --prefix=myapp \
  --init=sql_ffi_open \
  --cmd-map=can.cmd-map \
  --timer-cmd=20 \
  --out=lib/ic0/canister_entry.c
```

`--init=sql_ffi_open` → `canister_pre_upgrade` に `sqlite_stable_save` が自動挿入。

### 3. Use in Idris2

```idris
import IcWasm.SQLite
import IcWasm.StableStorage

main : IO ()
main = do
  -- C layer (canister_entry.c) が sql_ffi_open + sqlite_stable_load を実行済み
  -- Idris2 側では wrapSqlite で handle を取得
  handle <- wrapSqlite (MkStableConfig 1 0 1024)

  -- 全操作に handle が必要
  _ <- sqlExec handle "CREATE TABLE IF NOT EXISTS kv (k TEXT PRIMARY KEY, v TEXT)"
  _ <- sqlExec handle "INSERT OR REPLACE INTO kv VALUES ('hello', 'world')"

  rows <- sqlQuery handle "SELECT v FROM kv WHERE k = 'hello'"
  -- ...
```

## API Reference

### StableConfig

```idris
record StableConfig where
  constructor MkStableConfig
  version   : Nat    -- Schema version (for migration detection)
  startPage : Nat    -- Starting page in stable memory
  maxPages  : Nat    -- Maximum pages for SQLite snapshot
```

### SqliteHandle

```idris
-- ONLY way to obtain:
initSqlite : StableConfig -> IO SqliteHandle  -- opens DB
wrapSqlite : StableConfig -> IO SqliteHandle  -- wraps already-open DB

-- All operations require handle:
sqlExec         : SqliteHandle -> String -> IO SqlResult
sqlPrepare      : SqliteHandle -> String -> IO SqlResult
sqlStep         : SqliteHandle -> IO SqlResult
sqlFinalize     : SqliteHandle -> IO SqlResult
sqlColumnInt    : SqliteHandle -> Nat -> IO Int
sqlColumnText   : SqliteHandle -> Nat -> IO String
sqlQuery        : SqliteHandle -> String -> IO (List QueryRow)
getSchemaVersion : SqliteHandle -> IO SchemaVersion
setSchemaVersion : SqliteHandle -> SchemaVersion -> IO SqlResult
```

### StableStorage

```idris
stableSave        : StableConfig -> IO (Either String ())
stableLoad        : StableConfig -> IO (Either String ())
stableHasSnapshot : IO Bool

-- Header: [magic "ICSQ" 4B] [version 4B] [data_len 8B] [checksum 4B CRC32]
-- Checksum mismatch → Left "checksum mismatch: data corrupted"
-- Version mismatch → Left "version mismatch: expected N, got M"
```

### Timer API

```idris
setGlobalTimer : Int -> IO Int   -- Set timer (nanoseconds since epoch)

-- gen-entry --timer-cmd=N generates canister_global_timer export
-- Timer fires once → tally → done. No idle cost.
```

## Cost Comparison

| Method | cycles/min | Monthly Cost |
|--------|-----------|-------------|
| canister_heartbeat (old) | ~180B | ~$34,000 |
| canister_global_timer | ~0 (idle) | ~$112 |

## Pipeline

```
Idris2 (.idr)
    │
    ▼ idris2 --codegen refc
C code (.c)
    │
    ▼ gen-entry (auto: pre_upgrade save + timer)
canister_entry.c
    │
    ▼ emcc
WASM (.wasm)
    │
    ▼ stub-wasi.py
Stubbed WASM → dfx deploy
```
