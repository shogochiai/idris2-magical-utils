/*
 * SQLite VFS FFI Stubs for idris2-icwasm
 *
 * These functions make IcWasm.SQLiteVFS link in local RefC/macOS tests.
 * Canister builds should provide real implementations that register a direct
 * sqlite3_vfs backend and open SQLite against stable memory instead of an
 * in-memory serialized snapshot.
 */
#include <stdint.h>

static int64_t g_sql_vfs_memory_id = -1;

int64_t sql_vfs_ffi_init(int64_t memory_id) {
    g_sql_vfs_memory_id = memory_id;
    return 0;
}

int64_t sql_vfs_ffi_open_read_write(void) { return 0; }
int64_t sql_vfs_ffi_open_read_only(void) { return 0; }
int64_t sql_vfs_ffi_begin_update(void) { return 0; }
int64_t sql_vfs_ffi_commit_update(void) { return 0; }
int64_t sql_vfs_ffi_rollback_update(void) { return 0; }
int64_t sql_vfs_ffi_close(void) { return 0; }

int64_t sql_vfs_ffi_memory_id(void) { return g_sql_vfs_memory_id; }
int64_t sql_vfs_ffi_page_count(void) { return 0; }
int64_t sql_vfs_ffi_allocated_bytes(void) { return 0; }
int64_t sql_vfs_ffi_active_bytes(void) { return 0; }
