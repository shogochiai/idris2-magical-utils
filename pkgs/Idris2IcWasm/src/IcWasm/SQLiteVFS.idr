||| IC-native SQLite VFS API.
|||
||| This module is the Idris2-side contract for a direct SQLite VFS backend like
||| humandebri/ic-sqlite-vfs. Unlike IcWasm.SQLite + StableStorage, the database
||| is not serialized wholesale during pre_upgrade. SQLite opens a stable-memory
||| backed `/main.db`, and the backend commits dirty pages through its VFS.
module IcWasm.SQLiteVFS

import public IcWasm.SQLite

%default total

-- =============================================================================
-- FFI Declarations
-- =============================================================================

%foreign "C:sql_vfs_ffi_init,libic0"
prim__sqlVfsInit : Int -> PrimIO Int

%foreign "C:sql_vfs_ffi_open_read_write,libic0"
prim__sqlVfsOpenReadWrite : PrimIO Int

%foreign "C:sql_vfs_ffi_open_read_only,libic0"
prim__sqlVfsOpenReadOnly : PrimIO Int

%foreign "C:sql_vfs_ffi_begin_update,libic0"
prim__sqlVfsBeginUpdate : PrimIO Int

%foreign "C:sql_vfs_ffi_commit_update,libic0"
prim__sqlVfsCommitUpdate : PrimIO Int

%foreign "C:sql_vfs_ffi_rollback_update,libic0"
prim__sqlVfsRollbackUpdate : PrimIO Int

%foreign "C:sql_vfs_ffi_close,libic0"
prim__sqlVfsClose : PrimIO Int

%foreign "C:sql_vfs_ffi_memory_id,libic0"
prim__sqlVfsMemoryId : PrimIO Int

%foreign "C:sql_vfs_ffi_page_count,libic0"
prim__sqlVfsPageCount : PrimIO Int

%foreign "C:sql_vfs_ffi_allocated_bytes,libic0"
prim__sqlVfsAllocatedBytes : PrimIO Int

%foreign "C:sql_vfs_ffi_active_bytes,libic0"
prim__sqlVfsActiveBytes : PrimIO Int

-- =============================================================================
-- Public Types
-- =============================================================================

||| Stable-memory slot used by the SQLite VFS.
|||
||| `memoryId` must remain stable across canister upgrades. The upstream Rust
||| crate uses `120` in examples because that matches the historical
||| ic-rusqlite default. Do not reuse the chosen MemoryId for another stable
||| structure.
public export
record StableVFSConfig where
  constructor MkStableVFSConfig
  memoryId : Nat
  schemaVersion : SchemaVersion

public export
data SqliteVFSMode = ReadOnly | ReadWrite

public export
Eq SqliteVFSMode where
  ReadOnly == ReadOnly = True
  ReadWrite == ReadWrite = True
  _ == _ = False

public export
Show SqliteVFSMode where
  show ReadOnly = "read-only"
  show ReadWrite = "read-write"

||| Opaque proof that the IC SQLite VFS has been initialized and opened.
export
data SqliteVFSHandle = MkSqliteVFSHandle StableVFSConfig SqliteVFSMode

public export
record SQLiteVFSStats where
  constructor MkSQLiteVFSStats
  statMemoryId : Nat
  statPageCount : Nat
  statAllocatedBytes : Nat
  statActiveBytes : Nat

-- =============================================================================
-- Helpers
-- =============================================================================

sqliteVfsResult : String -> Int -> Either String ()
sqliteVfsResult _ 0 = Right ()
sqliteVfsResult op code = Left (op ++ " failed with code " ++ show code)

natFromFFI : Int -> Nat
natFromFFI n = cast (max 0 n)

-- =============================================================================
-- Lifecycle
-- =============================================================================

||| Initialize the global VFS with the selected stable MemoryId.
export
initStableVFS : StableVFSConfig -> IO (Either String ())
initStableVFS cfg = do
  rc <- primIO $ prim__sqlVfsInit (cast (memoryId cfg))
  pure (sqliteVfsResult "sql_vfs_ffi_init" rc)

||| Open the stable-memory backed database for an update message.
|||
||| This starts the backend write overlay before opening the SQLite connection,
||| matching the upstream VFS order.
|||
||| The caller must not perform an `await` between begin/commit/rollback.
export
openStableVFSReadWrite : StableVFSConfig -> IO (Either String SqliteVFSHandle)
openStableVFSReadWrite cfg = do
  initResult <- initStableVFS cfg
  case initResult of
    Left err => pure (Left err)
    Right () => do
      beginRc <- primIO prim__sqlVfsBeginUpdate
      case sqliteVfsResult "sql_vfs_ffi_begin_update" beginRc of
        Left err => pure (Left err)
        Right () => do
          openRc <- primIO prim__sqlVfsOpenReadWrite
          case sqliteVfsResult "sql_vfs_ffi_open_read_write" openRc of
            Left err => do
              _ <- primIO prim__sqlVfsRollbackUpdate
              pure (Left err)
            Right () => pure (Right (MkSqliteVFSHandle cfg ReadWrite))

||| Open the stable-memory backed database for a query message.
export
openStableVFSReadOnly : StableVFSConfig -> IO (Either String SqliteVFSHandle)
openStableVFSReadOnly cfg = do
  initResult <- initStableVFS cfg
  case initResult of
    Left err => pure (Left err)
    Right () => do
      rc <- primIO prim__sqlVfsOpenReadOnly
      case sqliteVfsResult "sql_vfs_ffi_open_read_only" rc of
        Left err => pure (Left err)
        Right () => pure (Right (MkSqliteVFSHandle cfg ReadOnly))

||| Wrap a database opened by the canister entry layer.
export
wrapStableVFS : StableVFSConfig -> SqliteVFSMode -> IO SqliteVFSHandle
wrapStableVFS cfg mode = pure (MkSqliteVFSHandle cfg mode)

export
closeStableVFS : SqliteVFSHandle -> IO (Either String ())
closeStableVFS _ = do
  rc <- primIO prim__sqlVfsClose
  pure (sqliteVfsResult "sql_vfs_ffi_close" rc)

-- =============================================================================
-- Transaction Boundaries
-- =============================================================================

export
beginSqliteUpdate : SqliteVFSHandle -> IO (Either String ())
beginSqliteUpdate (MkSqliteVFSHandle _ ReadOnly) =
  pure (Left "cannot begin an update transaction on a read-only SQLite VFS handle")
beginSqliteUpdate _ = do
  rc <- primIO prim__sqlVfsBeginUpdate
  pure (sqliteVfsResult "sql_vfs_ffi_begin_update" rc)

export
commitSqliteUpdate : SqliteVFSHandle -> IO (Either String ())
commitSqliteUpdate (MkSqliteVFSHandle _ ReadOnly) =
  pure (Left "cannot commit an update transaction on a read-only SQLite VFS handle")
commitSqliteUpdate _ = do
  rc <- primIO prim__sqlVfsCommitUpdate
  pure (sqliteVfsResult "sql_vfs_ffi_commit_update" rc)

export
rollbackSqliteUpdate : SqliteVFSHandle -> IO (Either String ())
rollbackSqliteUpdate _ = do
  rc <- primIO prim__sqlVfsRollbackUpdate
  pure (sqliteVfsResult "sql_vfs_ffi_rollback_update" rc)

||| Run one update-message SQLite transaction.
|||
||| The body must stay synchronous. IC message atomicity plus the backend overlay
||| provide durability; crossing an `await` boundary would break that contract.
covering
export
withSqliteUpdate : StableVFSConfig ->
                   (SqliteVFSHandle -> IO (Either String a)) ->
                   IO (Either String a)
withSqliteUpdate cfg body = do
  opened <- openStableVFSReadWrite cfg
  case opened of
    Left err => pure (Left err)
    Right handle => do
      result <- body handle
      case result of
        Left err => do
          _ <- rollbackSqliteUpdate handle
          _ <- closeStableVFS handle
          pure (Left err)
        Right value => do
          committed <- commitSqliteUpdate handle
          _ <- closeStableVFS handle
          case committed of
            Left err => pure (Left err)
            Right () => pure (Right value)

covering
export
withSqliteQuery : StableVFSConfig ->
                  (SqliteVFSHandle -> IO (Either String a)) ->
                  IO (Either String a)
withSqliteQuery cfg body = do
  opened <- openStableVFSReadOnly cfg
  case opened of
    Left err => pure (Left err)
    Right handle => do
      result <- body handle
      _ <- closeStableVFS handle
      pure result

-- =============================================================================
-- SQLite API Wrappers
-- =============================================================================

export
sqlExecVFS : SqliteVFSHandle -> String -> IO SqlResult
sqlExecVFS _ sql = sqlExec sql

covering
export
sqlQueryVFS : SqliteVFSHandle -> String -> IO (List QueryRow)
sqlQueryVFS _ sql = sqlQuery sql

export
sqlPrepareVFS : SqliteVFSHandle -> String -> IO SqlResult
sqlPrepareVFS _ sql = sqlPrepare sql

export
setSchemaVersionVFS : SqliteVFSHandle -> SchemaVersion -> IO SqlResult
setSchemaVersionVFS _ version = setSchemaVersion version

covering
export
getSchemaVersionVFS : SqliteVFSHandle -> IO SchemaVersion
getSchemaVersionVFS _ = getSchemaVersion

export
getStableVFSConfig : SqliteVFSHandle -> StableVFSConfig
getStableVFSConfig (MkSqliteVFSHandle cfg _) = cfg

export
getStableVFSMode : SqliteVFSHandle -> SqliteVFSMode
getStableVFSMode (MkSqliteVFSHandle _ mode) = mode

-- =============================================================================
-- Stats
-- =============================================================================

export
sqliteVFSStats : IO SQLiteVFSStats
sqliteVFSStats = do
  mid <- primIO prim__sqlVfsMemoryId
  pages <- primIO prim__sqlVfsPageCount
  allocated <- primIO prim__sqlVfsAllocatedBytes
  active <- primIO prim__sqlVfsActiveBytes
  pure (MkSQLiteVFSStats (natFromFFI mid)
                         (natFromFFI pages)
                         (natFromFFI allocated)
                         (natFromFFI active))
