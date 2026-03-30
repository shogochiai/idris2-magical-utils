||| IcWasm SQLite Module
|||
||| FFI bindings for SQLite in-memory database with stable memory persistence.
||| Provides a safe, typed interface over the C bridge for ICP canisters.
|||
||| ALL SQL operations require a SqliteHandle, obtained ONLY via initSqlite.
||| initSqlite requires a StableConfig. This enforces:
|||   no SQLite without stable storage = no forgotten pre_upgrade save.
|||
||| Usage:
|||   handle <- initSqlite (MkStableConfig 1 0 1024)
|||   _ <- sqlExec handle "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
|||   _ <- sqlPrepare handle "SELECT * FROM foo"
|||   row <- sqlStep handle
|||   val <- sqlColumnInt handle 0
|||   _ <- sqlFinalize handle
module IcWasm.SQLite

import Data.List
import Data.String
import public IcWasm.StableStorage

%default total

-- =============================================================================
-- FFI Declarations (private — not directly accessible)
-- =============================================================================

%foreign "C:sql_ffi_open,libic0"
prim__sqlOpen : PrimIO ()

%foreign "C:sql_ffi_close,libic0"
prim__sqlClose : PrimIO ()

%foreign "C:sql_ffi_is_open,libic0"
prim__sqlIsOpen : PrimIO Int

%foreign "C:sql_ffi_exec,libic0"
prim__sqlExec : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_prepare,libic0"
prim__sqlPrepare : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_step,libic0"
prim__sqlStep : PrimIO Int

%foreign "C:sql_ffi_reset,libic0"
prim__sqlReset : PrimIO Int

%foreign "C:sql_ffi_finalize,libic0"
prim__sqlFinalize : PrimIO Int

%foreign "C:sql_ffi_column_count,libic0"
prim__sqlColumnCount : PrimIO Int

%foreign "C:sql_ffi_bind_null,libic0"
prim__sqlBindNull : Int -> PrimIO Int

%foreign "C:sql_ffi_bind_int,libic0"
prim__sqlBindInt : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_bind_text,libic0"
prim__sqlBindText : Int -> Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_bind_blob,libic0"
prim__sqlBindBlob : Int -> Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_column_type,libic0"
prim__sqlColumnType : Int -> PrimIO Int

%foreign "C:sql_ffi_column_int,libic0"
prim__sqlColumnInt : Int -> PrimIO Int

%foreign "C:sql_ffi_column_text_len,libic0"
prim__sqlColumnTextLen : Int -> PrimIO Int

%foreign "C:sql_ffi_column_text_byte,libic0"
prim__sqlColumnTextByte : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_changes,libic0"
prim__sqlChanges : PrimIO Int

%foreign "C:sql_ffi_last_insert_rowid,libic0"
prim__sqlLastInsertRowid : PrimIO Int

%foreign "C:sql_ffi_serialize_size,libic0"
prim__sqlSerializeSize : PrimIO Int

%foreign "C:sql_ffi_serialize,libic0"
prim__sqlSerialize : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_deserialize,libic0"
prim__sqlDeserialize : Int -> Int -> PrimIO Int

%foreign "C:sql_ffi_str_reset,libic0"
prim__sqlStrReset : PrimIO ()

%foreign "C:sql_ffi_str_push,libic0"
prim__sqlStrPush : Int -> PrimIO ()

%foreign "C:sql_ffi_str_ptr,libic0"
prim__sqlStrPtr : PrimIO Int

%foreign "C:sql_ffi_str_len,libic0"
prim__sqlStrLen : PrimIO Int

-- =============================================================================
-- SQLite Result Codes (public — needed for pattern matching)
-- =============================================================================

||| SQLite result code
public export
data SqlResult
  = SqlOk            -- 0: Success
  | SqlError         -- 1: Generic error
  | SqlBusy          -- 5: Database busy
  | SqlLocked        -- 6: Table locked
  | SqlNoMem         -- 7: Out of memory
  | SqlConstraint    -- 19: Constraint violation
  | SqlRow           -- 100: Row available
  | SqlDone          -- 101: Statement finished
  | SqlOther Int     -- Other code

public export
Eq SqlResult where
  SqlOk == SqlOk = True
  SqlError == SqlError = True
  SqlBusy == SqlBusy = True
  SqlLocked == SqlLocked = True
  SqlNoMem == SqlNoMem = True
  SqlConstraint == SqlConstraint = True
  SqlRow == SqlRow = True
  SqlDone == SqlDone = True
  (SqlOther n) == (SqlOther m) = n == m
  _ == _ = False

public export
Show SqlResult where
  show SqlOk = "SQLITE_OK"
  show SqlError = "SQLITE_ERROR"
  show SqlBusy = "SQLITE_BUSY"
  show SqlLocked = "SQLITE_LOCKED"
  show SqlNoMem = "SQLITE_NOMEM"
  show SqlConstraint = "SQLITE_CONSTRAINT"
  show SqlRow = "SQLITE_ROW"
  show SqlDone = "SQLITE_DONE"
  show (SqlOther n) = "SQLITE_" ++ show n

||| Convert integer to SqlResult
public export
toSqlResult : Int -> SqlResult
toSqlResult 0 = SqlOk
toSqlResult 1 = SqlError
toSqlResult 5 = SqlBusy
toSqlResult 6 = SqlLocked
toSqlResult 7 = SqlNoMem
toSqlResult 19 = SqlConstraint
toSqlResult 100 = SqlRow
toSqlResult 101 = SqlDone
toSqlResult n = SqlOther n

-- =============================================================================
-- Column Types (public — needed for pattern matching)
-- =============================================================================

public export
data SqlType = SqlInteger | SqlFloat | SqlText | SqlBlob | SqlNull

public export
Show SqlType where
  show SqlInteger = "INTEGER"
  show SqlFloat = "FLOAT"
  show SqlText = "TEXT"
  show SqlBlob = "BLOB"
  show SqlNull = "NULL"

public export
Eq SqlType where
  SqlInteger == SqlInteger = True
  SqlFloat == SqlFloat = True
  SqlText == SqlText = True
  SqlBlob == SqlBlob = True
  SqlNull == SqlNull = True
  _ == _ = False

public export
toSqlType : Int -> SqlType
toSqlType 1 = SqlInteger
toSqlType 2 = SqlFloat
toSqlType 3 = SqlText
toSqlType 4 = SqlBlob
toSqlType _ = SqlNull

-- =============================================================================
-- Query Row (public type for results)
-- =============================================================================

public export
record QueryRow where
  constructor MkQueryRow
  columns : List Int

public export
SchemaVersion : Type
SchemaVersion = Nat

-- =============================================================================
-- SqliteHandle: THE entry point. No handle = no SQL.
-- =============================================================================

||| Opaque handle proving SQLite was initialized with StableConfig.
||| Obtained ONLY via initSqlite (or wrapSqlite for post_upgrade).
||| All SQL operations require this handle.
export
data SqliteHandle = MkSqliteHandle StableConfig

-- Internal string buffer helpers (private)
sqlStrReset : IO ()
sqlStrReset = primIO prim__sqlStrReset

sqlStrPush : Int -> IO ()
sqlStrPush byte = primIO $ prim__sqlStrPush byte

sqlStrPtr : IO Int
sqlStrPtr = primIO prim__sqlStrPtr

sqlStrLen : IO Int
sqlStrLen = primIO prim__sqlStrLen

sqlWriteString : String -> IO ()
sqlWriteString s = do
  sqlStrReset
  traverse_ (\c => sqlStrPush (cast (ord c))) (unpack s)

-- =============================================================================
-- PUBLIC API: All operations require SqliteHandle
-- =============================================================================

||| Initialize SQLite with stable storage configuration.
||| This is the ONLY way to create a SqliteHandle.
||| @cfg Stable memory configuration (version, startPage, maxPages)
export
initSqlite : StableConfig -> IO SqliteHandle
initSqlite cfg = do
  primIO prim__sqlOpen
  pure (MkSqliteHandle cfg)

||| Wrap an already-open database with a StableConfig.
||| Use when C layer (canister_entry.c) already called sql_ffi_open + sqlite_stable_load.
export
wrapSqlite : StableConfig -> IO SqliteHandle
wrapSqlite cfg = pure (MkSqliteHandle cfg)

||| Get the StableConfig from a handle
export
getStableConfig : SqliteHandle -> StableConfig
getStableConfig (MkSqliteHandle cfg) = cfg

||| Close database
export
sqlClose : SqliteHandle -> IO ()
sqlClose _ = primIO prim__sqlClose

||| Check if database is open
export
sqlIsOpen : SqliteHandle -> IO Bool
sqlIsOpen _ = do
  result <- primIO prim__sqlIsOpen
  pure (result == 1)

-- =============================================================================
-- SQL Execution (handle required)
-- =============================================================================

||| Execute SQL statement (DDL/DML)
export
sqlExec : SqliteHandle -> String -> IO SqlResult
sqlExec _ sql = do
  sqlWriteString sql
  ptr <- sqlStrPtr
  len <- sqlStrLen
  rc <- primIO $ prim__sqlExec ptr len
  pure (toSqlResult rc)

||| Get number of rows changed by last statement
export
sqlChanges : SqliteHandle -> IO Int
sqlChanges _ = primIO prim__sqlChanges

||| Get last inserted rowid
export
sqlLastInsertRowid : SqliteHandle -> IO Int
sqlLastInsertRowid _ = primIO prim__sqlLastInsertRowid

-- =============================================================================
-- Prepared Statement API (handle required)
-- =============================================================================

||| Prepare SQL statement
export
sqlPrepare : SqliteHandle -> String -> IO SqlResult
sqlPrepare _ sql = do
  sqlWriteString sql
  ptr <- sqlStrPtr
  len <- sqlStrLen
  rc <- primIO $ prim__sqlPrepare ptr len
  pure (toSqlResult rc)

||| Execute one step of prepared statement
export
sqlStep : SqliteHandle -> IO SqlResult
sqlStep _ = do
  rc <- primIO prim__sqlStep
  pure (toSqlResult rc)

||| Reset prepared statement
export
sqlReset : SqliteHandle -> IO SqlResult
sqlReset _ = do
  rc <- primIO prim__sqlReset
  pure (toSqlResult rc)

||| Finalize prepared statement
export
sqlFinalize : SqliteHandle -> IO SqlResult
sqlFinalize _ = do
  rc <- primIO prim__sqlFinalize
  pure (toSqlResult rc)

||| Get number of result columns
export
sqlColumnCount : SqliteHandle -> IO Nat
sqlColumnCount _ = do
  n <- primIO prim__sqlColumnCount
  pure (cast (max 0 n))

-- =============================================================================
-- Column Accessors (handle required)
-- =============================================================================

||| Get column type
export
sqlColumnType : SqliteHandle -> Nat -> IO SqlType
sqlColumnType _ idx = do
  t <- primIO $ prim__sqlColumnType (cast idx)
  pure (toSqlType t)

||| Get integer column value
export
sqlColumnInt : SqliteHandle -> Nat -> IO Int
sqlColumnInt _ idx = primIO $ prim__sqlColumnInt (cast idx)

||| Get text column length
export
sqlColumnTextLen : SqliteHandle -> Nat -> IO Nat
sqlColumnTextLen _ idx = do
  n <- primIO $ prim__sqlColumnTextLen (cast idx)
  pure (cast (max 0 n))

||| Get single byte from text column
export
sqlColumnTextByte : SqliteHandle -> Nat -> Nat -> IO Int
sqlColumnTextByte _ colIdx byteIdx =
  primIO $ prim__sqlColumnTextByte (cast colIdx) (cast byteIdx)

||| Get text column value as String
export
sqlColumnText : SqliteHandle -> Nat -> IO String
sqlColumnText h idx = do
  len <- sqlColumnTextLen h idx
  case len of
    Z => pure ""
    _ => do
      let indices = [0 .. minus len 1]
      bytes <- traverse (sqlColumnTextByte h idx) indices
      pure (pack (map cast bytes))

-- =============================================================================
-- High-Level Query Interface (handle required)
-- =============================================================================

||| Execute SELECT and collect all rows
covering
export
sqlQuery : SqliteHandle -> String -> IO (List QueryRow)
sqlQuery h sql = do
  rc <- sqlPrepare h sql
  case rc of
    SqlOk => collectRows []
    _ => pure []
  where
    collectRows : List QueryRow -> IO (List QueryRow)
    collectRows acc = do
      stepResult <- sqlStep h
      case stepResult of
        SqlRow => do
          colCount <- sqlColumnCount h
          cols <- traverse (sqlColumnInt h) [0 .. minus colCount 1]
          collectRows (MkQueryRow cols :: acc)
        SqlDone => do
          _ <- sqlFinalize h
          pure (reverse acc)
        _ => do
          _ <- sqlFinalize h
          pure (reverse acc)

-- =============================================================================
-- Schema Management (handle required)
-- =============================================================================

||| Get current schema version
covering
export
getSchemaVersion : SqliteHandle -> IO SchemaVersion
getSchemaVersion h = do
  rows <- sqlQuery h "PRAGMA user_version"
  case rows of
    (MkQueryRow (v :: _) :: _) => pure (cast (max 0 v))
    _ => pure 0

||| Set schema version
export
setSchemaVersion : SqliteHandle -> SchemaVersion -> IO SqlResult
setSchemaVersion h v =
  sqlExec h ("PRAGMA user_version = " ++ show v)

||| Get serialized database size
export
sqlSerializeSize : SqliteHandle -> IO Int
sqlSerializeSize _ = primIO prim__sqlSerializeSize
