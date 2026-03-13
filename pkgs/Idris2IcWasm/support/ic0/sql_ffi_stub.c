/*
 * SQLite FFI Stubs for idris2-icwasm
 *
 * No-op implementations of sql_ffi_* functions declared in IcWasm.SQLite.
 * Used for:
 *   - RefC backend compilation (link-time symbol resolution)
 *   - macOS local testing via libic0.dylib
 *
 * The real implementations live in each canister's sqlite_bridge.c
 * (e.g. pkgs/Idris2TheWorld/lib/ic0/sqlite_bridge.c).
 */
#include <stdint.h>

/* ---- Database Lifecycle ---- */

void sql_ffi_open(void) {}
void sql_ffi_close(void) {}
int64_t sql_ffi_is_open(void) { return 0; }

/* ---- SQL Execution & Prepared Statements ---- */

int64_t sql_ffi_exec(int64_t sql_ptr, int64_t sql_len) { return 0; }
int64_t sql_ffi_prepare(int64_t sql_ptr, int64_t sql_len) { return 0; }
int64_t sql_ffi_step(void) { return 101; }  /* SQLITE_DONE */
int64_t sql_ffi_reset(void) { return 0; }
int64_t sql_ffi_finalize(void) { return 0; }
int64_t sql_ffi_column_count(void) { return 0; }

/* ---- Parameter Binding ---- */

int64_t sql_ffi_bind_null(int64_t idx) { return 0; }
int64_t sql_ffi_bind_int(int64_t idx, int64_t val) { return 0; }
int64_t sql_ffi_bind_text(int64_t idx, int64_t val_ptr, int64_t val_len) { return 0; }
int64_t sql_ffi_bind_blob(int64_t idx, int64_t val_ptr, int64_t val_len) { return 0; }

/* ---- Column Accessors ---- */

int64_t sql_ffi_column_type(int64_t idx) { return 5; }  /* SQLITE_NULL */
int64_t sql_ffi_column_int(int64_t idx) { return 0; }
int64_t sql_ffi_column_text_len(int64_t idx) { return 0; }
int64_t sql_ffi_column_text_byte(int64_t idx, int64_t byte_idx) { return 0; }

/* ---- Statistics & Serialization ---- */

int64_t sql_ffi_changes(void) { return 0; }
int64_t sql_ffi_last_insert_rowid(void) { return 0; }
int64_t sql_ffi_serialize_size(void) { return 0; }
int64_t sql_ffi_serialize(int64_t buf_ptr, int64_t max_len) { return 0; }
int64_t sql_ffi_deserialize(int64_t buf_ptr, int64_t len) { return 0; }

/* ---- String Buffer ---- */

void sql_ffi_str_reset(void) {}
void sql_ffi_str_push(int64_t byte) {}
int64_t sql_ffi_str_ptr(void) { return 0; }
int64_t sql_ffi_str_len(void) { return 0; }
