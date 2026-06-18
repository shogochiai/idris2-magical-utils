/*
 * Native unit test for BulletproofVFS.
 *
 * Mocks the IC stable-memory FFI (ic0_stable64_*) with a host array, registers
 * BulletproofVFS as the default SQLite VFS, runs CRUD, and asserts the key
 * property the paper claims (定理 5.1.1): under normal operation — including
 * DELETE — the stable allocation (high-water mark) does NOT grow once the
 * logical size stops increasing. This is the decisive difference from the
 * append-only VFS, which grows on every commit.
 *
 * Build (native, links system sqlite3):
 *   clang -O0 -g bulletproof_vfs.c bulletproof_vfs_test.c -lsqlite3 -o /tmp/bpvfs_test
 *   /tmp/bpvfs_test
 */
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sqlite3.h>

/* ---- mock IC stable memory: a growable host array ---- */
#define MOCK_PAGE 65536ULL
static uint8_t* g_mock = NULL;
static uint64_t g_mock_pages = 0;     /* allocated pages (the high-water mark) */

uint64_t ic0_stable64_size(void) { return g_mock_pages; }
uint64_t ic0_stable64_grow(uint64_t add) {
    uint64_t old = g_mock_pages;
    uint64_t newp = old + add;
    uint8_t* p = realloc(g_mock, (size_t)(newp * MOCK_PAGE));
    if (!p) return UINT64_MAX;
    memset(p + old * MOCK_PAGE, 0, (size_t)(add * MOCK_PAGE));
    g_mock = p; g_mock_pages = newp;
    return old;
}
void ic0_stable64_read(uint64_t dst, uint64_t off, uint64_t size) {
    memcpy((void*)(uintptr_t)dst, g_mock + off, (size_t)size);
}
void ic0_stable64_write(uint64_t off, uint64_t src, uint64_t size) {
    memcpy(g_mock + off, (const void*)(uintptr_t)src, (size_t)size);
}

/* BulletproofVFS FFI (from bulletproof_vfs.c) */
extern int sql_vfs_register(int64_t memory_id);
extern int sql_vfs_begin_update(void);
extern int sql_vfs_commit_update(void);
extern int64_t sql_vfs_ffi_allocated_bytes(void);
extern int64_t sql_vfs_ffi_active_bytes(void);

#define CHECK(cond, msg) do { if (!(cond)) { printf("FAIL: %s\n", msg); fails++; } else { printf("ok:   %s\n", msg); } } while (0)

static int fails = 0;

static int exec(sqlite3* db, const char* sql) {
    char* err = NULL;
    int rc = sqlite3_exec(db, sql, NULL, NULL, &err);
    if (rc != SQLITE_OK) { printf("  sql error: %s (%s)\n", err ? err : "?", sql); sqlite3_free(err); }
    return rc;
}

static long count_rows(sqlite3* db, const char* tbl) {
    char sql[128]; snprintf(sql, sizeof(sql), "SELECT COUNT(*) FROM %s", tbl);
    sqlite3_stmt* st; long n = -1;
    if (sqlite3_prepare_v2(db, sql, -1, &st, NULL) == SQLITE_OK) {
        if (sqlite3_step(st) == SQLITE_ROW) n = sqlite3_column_int64(st, 0);
        sqlite3_finalize(st);
    }
    return n;
}

int main(void) {
    printf("=== BulletproofVFS native unit test ===\n");

    /* register BulletproofVFS at memory_id 1 (base = 64KiB) */
    int rc = sql_vfs_register(1);
    CHECK(rc == SQLITE_OK, "VFS registers");

    sqlite3* db = NULL;
    rc = sqlite3_open_v2("/main.db", &db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, "icstable");
    CHECK(rc == SQLITE_OK, "open db on icstable VFS");
    if (rc != SQLITE_OK) { printf("open failed: %s\n", sqlite3_errmsg(db)); return 1; }

    /* BulletproofVFS provides message-atomic pages, so SQLite's own on-file
     * journaling/locking/temp is unnecessary AND must be steered to the single
     * main.db (this VFS serves only main.db; a separate journal/temp file would
     * hit xOpen for a non-main name and break). These three PRAGMAs are the
     * required operating mode for BulletproofVFS:
     *   journal_mode=MEMORY : rollback journal in heap, not a side file
     *   temp_store=MEMORY   : temp tables/indices in heap, not a side file
     *   locking_mode=EXCLUSIVE : no separate lock-byte file access
     */
    exec(db, "PRAGMA journal_mode=MEMORY");
    exec(db, "PRAGMA temp_store=MEMORY");
    exec(db, "PRAGMA locking_mode=EXCLUSIVE");
    exec(db, "PRAGMA page_size=16384");

    sql_vfs_begin_update();
    CHECK(exec(db, "CREATE TABLE t(id INTEGER PRIMARY KEY, v TEXT)") == SQLITE_OK, "create table");
    /* insert 200 rows with sizable text */
    exec(db, "BEGIN");
    for (int i = 0; i < 200; i++) {
        char sql[256];
        snprintf(sql, sizeof(sql),
            "INSERT INTO t(id,v) VALUES(%d,'row-%d-padding-padding-padding-padding')", i, i);
        exec(db, sql);
    }
    exec(db, "COMMIT");
    sql_vfs_commit_update();

    long n1 = count_rows(db, "t");
    CHECK(n1 == 200, "200 rows inserted");

    int64_t alloc_after_insert = sql_vfs_ffi_allocated_bytes();
    int64_t active_after_insert = sql_vfs_ffi_active_bytes();
    printf("  after insert: allocated=%lld active=%lld\n",
           (long long)alloc_after_insert, (long long)active_after_insert);

    /* --- the decisive test: DELETE most rows, then do many update cycles, and
     * assert the allocation does NOT keep growing (paper 定理 5.1.1). With the
     * old append-only VFS, every commit below would grow stable. --- */
    sql_vfs_begin_update();
    exec(db, "DELETE FROM t WHERE id >= 20");   /* keep 20 rows */
    sql_vfs_commit_update();
    long n2 = count_rows(db, "t");
    CHECK(n2 == 20, "20 rows after delete");

    int64_t alloc_after_delete = sql_vfs_ffi_allocated_bytes();
    printf("  after delete: allocated=%lld\n", (long long)alloc_after_delete);

    /* churn: 50 update/commit cycles that rewrite existing rows in place */
    int64_t alloc_before_churn = sql_vfs_ffi_allocated_bytes();
    for (int c = 0; c < 50; c++) {
        sql_vfs_begin_update();
        char sql[256];
        snprintf(sql, sizeof(sql), "UPDATE t SET v='churn-%d-padding-padding-padding' WHERE id < 20", c);
        exec(db, sql);
        sql_vfs_commit_update();
    }
    int64_t alloc_after_churn = sql_vfs_ffi_allocated_bytes();
    printf("  before churn: allocated=%lld | after 50 in-place churn cycles: allocated=%lld\n",
           (long long)alloc_before_churn, (long long)alloc_after_churn);

    /* THE bulletproof property: in-place churn over a non-growing logical size
     * does not grow the stable allocation. (append-only would grow every cycle.) */
    CHECK(alloc_after_churn == alloc_before_churn,
          "allocation does NOT grow under in-place churn (定理 5.1.1)");

    /* data integrity after churn */
    sqlite3_stmt* st;
    int integrity_ok = 1;
    if (sqlite3_prepare_v2(db, "SELECT v FROM t WHERE id=5", -1, &st, NULL) == SQLITE_OK) {
        if (sqlite3_step(st) == SQLITE_ROW) {
            const char* v = (const char*)sqlite3_column_text(st, 0);
            integrity_ok = (v && strncmp(v, "churn-49-", 9) == 0);
        } else integrity_ok = 0;
        sqlite3_finalize(st);
    }
    CHECK(integrity_ok, "row content correct after in-place churn (read-your-writes)");

    /* close, reopen, verify persistence across a fresh VFS load (superblock) */
    sqlite3_close(db);
    extern int64_t sql_vfs_ffi_close(void);
    sql_vfs_ffi_close();
    rc = sql_vfs_register(1);
    db = NULL;
    rc = sqlite3_open_v2("/main.db", &db, SQLITE_OPEN_READWRITE, "icstable");
    CHECK(rc == SQLITE_OK, "reopen db");
    long n3 = count_rows(db, "t");
    CHECK(n3 == 20, "20 rows persist across reopen (superblock)");
    sqlite3_close(db);

    printf("=== %s (%d failures) ===\n", fails == 0 ? "ALL PASS" : "FAILURES", fails);
    free(g_mock);
    return fails == 0 ? 0 : 1;
}
