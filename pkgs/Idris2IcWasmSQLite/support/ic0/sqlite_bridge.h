/*
 * SQLite Bridge for IC Canisters
 *
 * Minimal SQLite interface for in-memory database with stable memory persistence.
 * No filesystem required - uses sqlite3_serialize/deserialize for snapshots.
 *
 * Build flags required:
 *   -DSQLITE_THREADSAFE=0
 *   -DSQLITE_OMIT_LOAD_EXTENSION
 *   -DSQLITE_OMIT_DEPRECATED
 *   -DSQLITE_OMIT_PROGRESS_CALLBACK
 *   -DSQLITE_OMIT_SHARED_CACHE
 *   -DSQLITE_DEFAULT_MEMSTATUS=0
 *   -DSQLITE_DQS=0
 */
#ifndef SQLITE_BRIDGE_H
#define SQLITE_BRIDGE_H

#include <stdint.h>

/* =============================================================================
 * Database Lifecycle
 * ============================================================================= */

/*
 * Open in-memory database
 * Returns: 0 on success, error code on failure
 */
int sql_open(void);

/*
 * Close database and free resources
 * Returns: 0 on success
 */
int sql_close(void);

/*
 * Check if database is open
 * Returns: 1 if open, 0 if closed
 */
int sql_is_open(void);

/* =============================================================================
 * SQL Execution (Simple)
 * ============================================================================= */

/*
 * Execute SQL statement (DDL/DML without results)
 * @sql  SQL string to execute
 * Returns: 0 on success, error code on failure
 */
int sql_exec(const char* sql);

/*
 * Get last error message
 * Returns: Error string (valid until next call)
 */
const char* sql_errmsg(void);

/*
 * Get number of rows changed by last INSERT/UPDATE/DELETE
 */
int sql_changes(void);

/*
 * Get last inserted rowid
 */
int64_t sql_last_insert_rowid(void);

/* =============================================================================
 * Prepared Statement API
 * ============================================================================= */

/*
 * Prepare SQL statement
 * @sql  SQL string to prepare
 * Returns: 0 on success, error code on failure
 *
 * Note: Only one statement can be prepared at a time (simplified API)
 */
int sql_prepare(const char* sql);

/*
 * Execute one step of prepared statement
 * Returns: 100 (SQLITE_ROW) if row available
 *          101 (SQLITE_DONE) if finished
 *          other values on error
 */
int sql_step(void);

/*
 * Reset prepared statement for re-execution
 */
int sql_reset(void);

/*
 * Finalize (free) prepared statement
 */
int sql_finalize(void);

/*
 * Get number of columns in result set
 */
int sql_column_count(void);

/* =============================================================================
 * Parameter Binding
 * ============================================================================= */

/*
 * Bind NULL to parameter
 * @idx  Parameter index (1-based)
 */
int sql_bind_null(int idx);

/*
 * Bind integer to parameter
 * @idx  Parameter index (1-based)
 * @val  Integer value
 */
int sql_bind_int(int idx, int64_t val);

/*
 * Bind text to parameter
 * @idx  Parameter index (1-based)
 * @val  Text value
 * @len  Length (-1 for strlen)
 */
int sql_bind_text(int idx, const char* val, int len);

/*
 * Bind blob to parameter
 * @idx  Parameter index (1-based)
 * @val  Blob data
 * @len  Length
 */
int sql_bind_blob(int idx, const void* val, int len);

/* =============================================================================
 * Column Accessors
 * ============================================================================= */

/*
 * Get column type
 * Returns: 1=INTEGER, 2=FLOAT, 3=TEXT, 4=BLOB, 5=NULL
 */
int sql_column_type(int idx);

/*
 * Get integer column value
 * @idx  Column index (0-based)
 */
int64_t sql_column_int(int idx);

/*
 * Get text column value
 * @idx  Column index (0-based)
 * Returns: Text string (valid until next step/finalize)
 */
const char* sql_column_text(int idx);

/*
 * Get text column length
 * @idx  Column index (0-based)
 */
int sql_column_bytes(int idx);

/*
 * Get blob column value
 * @idx  Column index (0-based)
 * Returns: Blob pointer (valid until next step/finalize)
 */
const void* sql_column_blob(int idx);

/* =============================================================================
 * Serialization (Stable Memory Persistence)
 * ============================================================================= */

/*
 * Get serialized database size
 * Returns: Size in bytes, or -1 on error
 */
int64_t sql_serialize_size(void);

/*
 * Serialize database to buffer
 * @buf      Destination buffer
 * @max_len  Maximum bytes to write
 * Returns: Bytes written, or -1 on error
 */
int64_t sql_serialize(uint8_t* buf, int64_t max_len);

/*
 * Deserialize database from buffer
 * @buf  Source buffer
 * @len  Buffer length
 * Returns: 0 on success, error code on failure
 *
 * Note: Closes existing DB and opens new one from serialized data
 */
int sql_deserialize(const uint8_t* buf, int64_t len);

/* =============================================================================
 * SQLite VFS Open Path
 * ============================================================================= */

/*
 * Open SQLite using a named sqlite3_vfs implementation.
 * The VFS must be registered before this is called.
 */
int sql_open_vfs(const char* path, const char* vfs_name, int read_only);

/*
 * Transaction helpers used by stable-memory VFS backends.
 */
int sql_tx_begin(void);
int sql_tx_commit(void);
int sql_tx_rollback(void);

/* =============================================================================
 * String Buffer for Idris FFI
 * ============================================================================= */

#define SQL_STRING_BUFFER_SIZE 65536  /* 64KB buffer for SQL strings */

/*
 * Reset string buffer (call before building new string)
 */
void sql_ffi_str_reset(void);

/*
 * Append a byte to string buffer
 * @byte  ASCII byte value (0-255)
 */
void sql_ffi_str_push(int64_t byte);

/*
 * Get string buffer pointer (for passing to sql_ffi_exec/prepare)
 */
int64_t sql_ffi_str_ptr(void);

/*
 * Get current string length
 */
int64_t sql_ffi_str_len(void);

/* =============================================================================
 * Idris FFI Wrappers (int64_t for Idris Int compatibility)
 * ============================================================================= */

void sql_ffi_open(void);
void sql_ffi_close(void);
int64_t sql_ffi_is_open(void);

int64_t sql_ffi_exec(int64_t sql_ptr, int64_t sql_len);
int64_t sql_ffi_prepare(int64_t sql_ptr, int64_t sql_len);
int64_t sql_ffi_step(void);
int64_t sql_ffi_reset(void);
int64_t sql_ffi_finalize(void);
int64_t sql_ffi_column_count(void);

int64_t sql_ffi_bind_null(int64_t idx);
int64_t sql_ffi_bind_int(int64_t idx, int64_t val);
int64_t sql_ffi_bind_text(int64_t idx, int64_t val_ptr, int64_t val_len);
int64_t sql_ffi_bind_blob(int64_t idx, int64_t val_ptr, int64_t val_len);

int64_t sql_ffi_column_type(int64_t idx);
int64_t sql_ffi_column_int(int64_t idx);
int64_t sql_ffi_column_text_len(int64_t idx);
int64_t sql_ffi_column_text_byte(int64_t idx, int64_t byte_idx);

int64_t sql_ffi_changes(void);
int64_t sql_ffi_last_insert_rowid(void);

int64_t sql_ffi_serialize_size(void);
int64_t sql_ffi_serialize(int64_t buf_ptr, int64_t max_len);
int64_t sql_ffi_deserialize(int64_t buf_ptr, int64_t len);

#endif /* SQLITE_BRIDGE_H */
