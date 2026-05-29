/*
 * Direct SQLite VFS backed by IC stable memory.
 *
 * This is the C backend for IcWasm.SQLiteVFS. It keeps SQLite's public API in
 * sqlite_bridge.c, but opens the connection on a named sqlite3_vfs instead of
 * the old :memory: + serialize/deserialize path.
 */
#ifndef SQLITE_VFS_BRIDGE_H
#define SQLITE_VFS_BRIDGE_H

#include <stdint.h>

#define SQL_VFS_NAME "icstable"
#define SQL_VFS_MAIN_DB "/main.db"
#define SQL_VFS_STABLE_PAGE_SIZE 65536ULL
#define SQL_VFS_SQLITE_PAGE_SIZE 16384ULL
#define SQL_VFS_SUPERBLOCK_SIZE SQL_VFS_STABLE_PAGE_SIZE

int sql_vfs_register(int64_t memory_id);
int sql_vfs_begin_update(void);
int sql_vfs_commit_update(void);
void sql_vfs_rollback_update(void);

int64_t sql_vfs_ffi_init(int64_t memory_id);
int64_t sql_vfs_ffi_open_read_write(void);
int64_t sql_vfs_ffi_open_read_only(void);
int64_t sql_vfs_ffi_begin_update(void);
int64_t sql_vfs_ffi_commit_update(void);
int64_t sql_vfs_ffi_rollback_update(void);
int64_t sql_vfs_ffi_close(void);
int64_t sql_vfs_ffi_memory_id(void);
int64_t sql_vfs_ffi_page_count(void);
int64_t sql_vfs_ffi_allocated_bytes(void);
int64_t sql_vfs_ffi_active_bytes(void);

#endif /* SQLITE_VFS_BRIDGE_H */
