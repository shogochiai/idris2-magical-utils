||| IcWasm StableStorage - Typed stable memory persistence for SQLite
|||
||| SQLite を使う = stable save が必須。この密結合により「SQLite 使うけど
||| stable save 忘れた」を型レベルで防止する。
|||
||| Header format:
|||   [magic: 4B "ICSQ"] [version: 4B] [data_len: 8B] [checksum: 4B CRC32]
|||   → 部分書き込み検出 (checksum 不一致 → 復元拒否 → エラー報告)
|||   → バージョン不一致 → マイグレーション実行
|||
||| Usage:
|||   cfg <- pure (MkStableConfig 1 0 1024)
|||   Right () <- stableSave cfg
|||   Right () <- stableLoad cfg
module IcWasm.StableStorage

import Data.List

%default covering

-- =============================================================================
-- StableConfig: 密結合の鍵
-- =============================================================================

||| Stable memory configuration — required to create SqliteHandle.
||| This type enforces the invariant: no SQLite without stable storage.
public export
record StableConfig where
  constructor MkStableConfig
  ||| Application schema version (for migration detection)
  version   : Nat
  ||| Starting page in stable memory (default: 0)
  startPage : Nat
  ||| Maximum pages allocated for SQLite snapshot (default: 1024 = 64MB)
  maxPages  : Nat

-- =============================================================================
-- FFI Declarations (C bridge in sqlite_stable.c)
-- =============================================================================

%foreign "C:sqlite_stable_ffi_save,libic0"
prim__stableSave : Int -> Int -> PrimIO Int

%foreign "C:sqlite_stable_ffi_load,libic0"
prim__stableLoad : PrimIO Int

%foreign "C:sqlite_stable_ffi_get_schema_version,libic0"
prim__stableGetSchemaVersion : PrimIO Int

%foreign "C:sqlite_stable_ffi_has_snapshot,libic0"
prim__stableHasSnapshot : PrimIO Int

%foreign "C:sqlite_stable_ffi_clear,libic0"
prim__stableClear : PrimIO ()

-- =============================================================================
-- High-Level API
-- =============================================================================

||| Save SQLite database to stable memory.
||| Called automatically by gen-entry during canister_pre_upgrade.
|||
||| Returns Left on error:
|||   - "database not open"
|||   - "serialize failed"
|||   - "database too large"
|||   - "checksum mismatch: data corrupted"
export
stableSave : StableConfig -> IO (Either String ())
stableSave cfg = do
  rc <- primIO $ prim__stableSave (cast cfg.version) 0
  pure $ case rc of
    0 => Right ()
    1 => Left "database not open"
    2 => Left "serialize failed: could not get size"
    3 => Left "database too large for stable memory"
    4 => Left "serialize size mismatch"
    _ => Left ("stable save error: code " ++ show rc)

||| Load SQLite database from stable memory.
||| Called during canister_post_upgrade after sql_ffi_open.
|||
||| Returns Left on error:
|||   - "no valid snapshot"
|||   - "checksum mismatch: data corrupted"
|||   - "deserialize failed"
|||   - "version mismatch: expected N, got M"
export
stableLoad : StableConfig -> IO (Either String ())
stableLoad cfg = do
  has <- primIO prim__stableHasSnapshot
  if has == 0
    then pure (Left "no valid snapshot")
    else do
      rc <- primIO prim__stableLoad
      case rc of
        0 => do
          storedVer <- primIO prim__stableGetSchemaVersion
          let expected = cast {to=Int} cfg.version
          if storedVer == expected || expected == 0
            then pure (Right ())
            else pure (Left ("version mismatch: expected " ++ show expected ++ ", got " ++ show storedVer))
        1 => pure (Left "no valid snapshot")
        2 => pure (Left "checksum mismatch: data corrupted")
        3 => pure (Left "deserialize failed")
        _ => pure (Left ("stable load error: code " ++ show rc))

||| Check if a valid snapshot exists in stable memory
export
stableHasSnapshot : IO Bool
stableHasSnapshot = do
  rc <- primIO prim__stableHasSnapshot
  pure (rc == 1)

||| Get the schema version stored in the last loaded snapshot
export
stableGetSchemaVersion : IO Nat
stableGetSchemaVersion = do
  v <- primIO prim__stableGetSchemaVersion
  pure (cast (max 0 v))

||| Clear the snapshot from stable memory (invalidates magic)
export
stableClear : IO ()
stableClear = primIO prim__stableClear
