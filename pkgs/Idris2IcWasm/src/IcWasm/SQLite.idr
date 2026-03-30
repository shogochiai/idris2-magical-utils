||| IcWasm SQLite Module
|||
||| FFI bindings for SQLite in-memory database with stable memory persistence.
||| Provides a safe, typed interface over the C bridge for ICP canisters.
|||
||| The coupled API enforces: no SQLite without StableConfig.
|||   handle <- initSqlite (MkStableConfig 1 0 1024)
|||   _ <- sqlExecH handle "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
|||
||| Legacy API (sqlOpen/sqlExec) remains for backward compatibility.
module IcWasm.SQLite

import Data.List
import Data.String
import public IcWasm.StableStorage

%default total

-- =============================================================================
-- FFI Declarations
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

-- String buffer FFI for passing strings to C
%foreign "C:sql_ffi_str_reset,libic0"
prim__sqlStrReset : PrimIO ()

%foreign "C:sql_ffi_str_push,libic0"
prim__sqlStrPush : Int -> PrimIO ()

%foreign "C:sql_ffi_str_ptr,libic0"
prim__sqlStrPtr : PrimIO Int

%foreign "C:sql_ffi_str_len,libic0"
prim__sqlStrLen : PrimIO Int

-- =============================================================================
-- SQLite Result Codes
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
-- Column Types
-- =============================================================================

||| SQLite column type
public export
data SqlType
  = SqlInteger
  | SqlFloat
  | SqlText
  | SqlBlob
  | SqlNull

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

||| Convert integer to SqlType
public export
toSqlType : Int -> SqlType
toSqlType 1 = SqlInteger
toSqlType 2 = SqlFloat
toSqlType 3 = SqlText
toSqlType 4 = SqlBlob
toSqlType _ = SqlNull

-- =============================================================================
-- Database Operations
-- =============================================================================

||| Open in-memory database
export
sqlOpen : IO ()
sqlOpen = primIO prim__sqlOpen

||| Close database
export
sqlClose : IO ()
sqlClose = primIO prim__sqlClose

||| Check if database is open
export
sqlIsOpen : IO Bool
sqlIsOpen = do
  result <- primIO prim__sqlIsOpen
  pure (result == 1)

-- =============================================================================
-- String Buffer Operations
-- =============================================================================

||| Reset string buffer
export
sqlStrReset : IO ()
sqlStrReset = primIO prim__sqlStrReset

||| Push a byte to string buffer
export
sqlStrPush : Int -> IO ()
sqlStrPush byte = primIO $ prim__sqlStrPush byte

||| Get string buffer pointer
export
sqlStrPtr : IO Int
sqlStrPtr = primIO prim__sqlStrPtr

||| Get string buffer length
export
sqlStrLen : IO Int
sqlStrLen = primIO prim__sqlStrLen

||| Write string to buffer (helper)
export
sqlWriteString : String -> IO ()
sqlWriteString s = do
  sqlStrReset
  traverse_ (\c => sqlStrPush (cast (ord c))) (unpack s)

-- =============================================================================
-- SQL Execution
-- =============================================================================

||| Execute SQL statement (DDL/DML)
export
sqlExec : String -> IO SqlResult
sqlExec sql = do
  sqlWriteString sql
  ptr <- sqlStrPtr
  len <- sqlStrLen
  rc <- primIO $ prim__sqlExec ptr len
  pure (toSqlResult rc)

||| Get number of rows changed by last statement
export
sqlChanges : IO Int
sqlChanges = primIO prim__sqlChanges

||| Get last inserted rowid
export
sqlLastInsertRowid : IO Int
sqlLastInsertRowid = primIO prim__sqlLastInsertRowid

-- =============================================================================
-- Prepared Statement API
-- =============================================================================

||| Prepare SQL statement
export
sqlPrepare : String -> IO SqlResult
sqlPrepare sql = do
  sqlWriteString sql
  ptr <- sqlStrPtr
  len <- sqlStrLen
  rc <- primIO $ prim__sqlPrepare ptr len
  pure (toSqlResult rc)

||| Execute one step of prepared statement
export
sqlStep : IO SqlResult
sqlStep = do
  rc <- primIO prim__sqlStep
  pure (toSqlResult rc)

||| Reset prepared statement
export
sqlReset : IO SqlResult
sqlReset = do
  rc <- primIO prim__sqlReset
  pure (toSqlResult rc)

||| Finalize prepared statement
export
sqlFinalize : IO SqlResult
sqlFinalize = do
  rc <- primIO prim__sqlFinalize
  pure (toSqlResult rc)

||| Get number of result columns
export
sqlColumnCount : IO Nat
sqlColumnCount = do
  n <- primIO prim__sqlColumnCount
  pure (cast (max 0 n))

-- =============================================================================
-- Column Accessors
-- =============================================================================

||| Get column type
export
sqlColumnType : Nat -> IO SqlType
sqlColumnType idx = do
  t <- primIO $ prim__sqlColumnType (cast idx)
  pure (toSqlType t)

||| Get integer column value
export
sqlColumnInt : Nat -> IO Int
sqlColumnInt idx = primIO $ prim__sqlColumnInt (cast idx)

||| Get text column length
export
sqlColumnTextLen : Nat -> IO Nat
sqlColumnTextLen idx = do
  n <- primIO $ prim__sqlColumnTextLen (cast idx)
  pure (cast (max 0 n))

||| Get single byte from text column
export
sqlColumnTextByte : Nat -> Nat -> IO Int
sqlColumnTextByte colIdx byteIdx =
  primIO $ prim__sqlColumnTextByte (cast colIdx) (cast byteIdx)

||| Get text column value as String
||| Uses traverse over index list to read bytes
export
sqlColumnText : Nat -> IO String
sqlColumnText idx = do
  len <- sqlColumnTextLen idx
  case len of
    Z => pure ""
    _ => do
      let indices = [0 .. minus len 1]
      bytes <- traverse (sqlColumnTextByte idx) indices
      pure (pack (map cast bytes))

-- =============================================================================
-- Serialization (for stable memory)
-- =============================================================================

||| Get serialized database size
export
sqlSerializeSize : IO Int
sqlSerializeSize = primIO prim__sqlSerializeSize

-- =============================================================================
-- High-Level Query Interface
-- =============================================================================

||| Query result row (simplified - integers only for now)
public export
record QueryRow where
  constructor MkQueryRow
  columns : List Int

||| Execute SELECT and collect all rows
||| Note: This is a simplified interface that only handles integer columns
covering
export
sqlQuery : String -> IO (List QueryRow)
sqlQuery sql = do
  rc <- sqlPrepare sql
  case rc of
    SqlOk => collectRows []
    _ => pure []
  where
    collectRows : List QueryRow -> IO (List QueryRow)
    collectRows acc = do
      stepResult <- sqlStep
      case stepResult of
        SqlRow => do
          colCount <- sqlColumnCount
          cols <- traverse sqlColumnInt [0 .. minus colCount 1]
          collectRows (MkQueryRow cols :: acc)
        SqlDone => do
          _ <- sqlFinalize
          pure (reverse acc)
        _ => do
          _ <- sqlFinalize
          pure (reverse acc)

-- =============================================================================
-- Schema Management
-- =============================================================================

||| Schema version (stored in user_version pragma)
public export
SchemaVersion : Type
SchemaVersion = Nat

||| Get current schema version
covering
export
getSchemaVersion : IO SchemaVersion
getSchemaVersion = do
  rows <- sqlQuery "PRAGMA user_version"
  case rows of
    (MkQueryRow (v :: _) :: _) => pure (cast (max 0 v))
    _ => pure 0

||| Set schema version
export
setSchemaVersion : SchemaVersion -> IO SqlResult
setSchemaVersion v =
  sqlExec ("PRAGMA user_version = " ++ show v)

-- =============================================================================
-- Coupled SQLite + StableStorage API
-- =============================================================================

||| Opaque handle proving SQLite was initialized with StableConfig.
||| You can only obtain this via `initSqlite`, which requires a StableConfig.
||| This enforces: no SQL execution without stable storage configuration.
export
data SqliteHandle = MkSqliteHandle StableConfig

||| Initialize SQLite with stable storage configuration.
||| This is the ONLY way to obtain a SqliteHandle.
|||
||| Opens the in-memory database and stores the config for later use
||| by stableSave/stableLoad. After post_upgrade, call stableLoad first,
||| then initSqlite to get a handle for subsequent SQL operations.
|||
||| @cfg Stable memory configuration (version, startPage, maxPages)
export
initSqlite : StableConfig -> IO SqliteHandle
initSqlite cfg = do
  sqlOpen
  pure (MkSqliteHandle cfg)

||| Get the StableConfig from a SqliteHandle
export
getStableConfig : SqliteHandle -> StableConfig
getStableConfig (MkSqliteHandle cfg) = cfg

||| Execute SQL statement with handle proof
export
sqlExecH : SqliteHandle -> String -> IO SqlResult
sqlExecH _ sql = sqlExec sql

||| Execute SELECT with handle proof
covering
export
sqlQueryH : SqliteHandle -> String -> IO (List QueryRow)
sqlQueryH _ sql = sqlQuery sql

||| Get schema version with handle proof
covering
export
getSchemaVersionH : SqliteHandle -> IO SchemaVersion
getSchemaVersionH _ = getSchemaVersion

||| Set schema version with handle proof
export
setSchemaVersionH : SqliteHandle -> SchemaVersion -> IO SqlResult
setSchemaVersionH _ v = setSchemaVersion v

||| Prepare statement with handle proof
export
sqlPrepareH : SqliteHandle -> String -> IO SqlResult
sqlPrepareH _ sql = sqlPrepare sql
