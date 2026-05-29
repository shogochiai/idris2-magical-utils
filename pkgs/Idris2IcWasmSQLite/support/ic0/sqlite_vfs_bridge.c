/*
 * SQLite VFS for IC stable memory.
 *
 * This is a compact C port boundary inspired by humandebri/ic-sqlite-vfs:
 * SQLite talks to sqlite3_io_methods, writes are collected in a heap overlay,
 * and commit appends dirty logical pages plus a page table before publishing a
 * final superblock. The selected memory_id is interpreted as a raw stable-memory
 * page anchor: memory_id 120 starts at stable byte 120 * 64KiB.
 */
#include "sqlite_vfs_bridge.h"
#include "sqlite_bridge.h"
#include "sqlite3.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>

extern uint64_t ic0_stable64_size(void);
extern uint64_t ic0_stable64_grow(uint64_t new_pages);
extern void ic0_stable64_read(uint64_t dst, uint64_t offset, uint64_t size);
extern void ic0_stable64_write(uint64_t offset, uint64_t src, uint64_t size);
extern uint64_t ic0_time(void);

#define SQL_VFS_MAGIC0 'I'
#define SQL_VFS_MAGIC1 'C'
#define SQL_VFS_MAGIC2 'V'
#define SQL_VFS_MAGIC3 'F'
#define SQL_VFS_MAGIC4 'S'
#define SQL_VFS_MAGIC5 'D'
#define SQL_VFS_MAGIC6 'B'
#define SQL_VFS_VERSION 1U
#define SQL_VFS_SUPERBLOCK_ENCODED_LEN 80U
#define SQL_VFS_MAX_DB_SIZE (256ULL * 1024ULL * 1024ULL)

typedef struct {
    uint64_t db_size;
    uint64_t page_count;
    uint64_t page_table_offset;
    uint64_t append_offset;
    uint64_t last_tx_id;
    uint64_t checksum;
    uint64_t memory_id;
} SqlVfsSuperblock;

typedef struct {
    uint64_t page_no;
    uint8_t data[SQL_VFS_SQLITE_PAGE_SIZE];
} SqlVfsDirtyPage;

typedef struct {
    sqlite3_file base;
    int is_main;
    int read_only;
    uint8_t* temp;
    uint64_t temp_size;
    uint64_t temp_cap;
} SqlVfsFile;

static sqlite3_io_methods g_sql_vfs_io_methods;
static sqlite3_vfs g_sql_vfs;
static int g_sql_vfs_registered = 0;
static int g_sql_vfs_prepared = 0;

static int64_t g_memory_id = -1;
static uint64_t g_base_offset = 0;
static SqlVfsSuperblock g_block;
static int g_block_loaded = 0;

static int g_overlay_active = 0;
static uint64_t g_overlay_size = 0;
static SqlVfsDirtyPage* g_dirty_pages = NULL;
static uint64_t g_dirty_count = 0;
static uint64_t g_dirty_cap = 0;

static uint64_t read_u64(const uint8_t* p) {
    uint64_t v = 0;
    for (int i = 0; i < 8; i++) v |= ((uint64_t)p[i]) << (8 * i);
    return v;
}

static void write_u64(uint8_t* p, uint64_t v) {
    for (int i = 0; i < 8; i++) p[i] = (uint8_t)((v >> (8 * i)) & 0xff);
}

static uint32_t read_u32(const uint8_t* p) {
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static void write_u32(uint8_t* p, uint32_t v) {
    p[0] = (uint8_t)(v & 0xff);
    p[1] = (uint8_t)((v >> 8) & 0xff);
    p[2] = (uint8_t)((v >> 16) & 0xff);
    p[3] = (uint8_t)((v >> 24) & 0xff);
}

static uint64_t fnv1a64(const uint8_t* data, uint64_t len) {
    uint64_t hash = 1469598103934665603ULL;
    for (uint64_t i = 0; i < len; i++) {
        hash ^= (uint64_t)data[i];
        hash *= 1099511628211ULL;
    }
    return hash;
}

static int ensure_stable_capacity(uint64_t end_offset) {
    uint64_t pages = ic0_stable64_size();
    uint64_t bytes = pages * SQL_VFS_STABLE_PAGE_SIZE;
    if (end_offset <= bytes) return SQLITE_OK;
    uint64_t needed_pages =
        (end_offset + SQL_VFS_STABLE_PAGE_SIZE - 1) / SQL_VFS_STABLE_PAGE_SIZE;
    uint64_t delta = needed_pages - pages;
    uint64_t old = ic0_stable64_grow(delta);
    return old == UINT64_MAX ? SQLITE_FULL : SQLITE_OK;
}

static uint64_t superblock_checksum(const uint8_t* encoded) {
    uint8_t tmp[SQL_VFS_SUPERBLOCK_ENCODED_LEN];
    memcpy(tmp, encoded, sizeof(tmp));
    memset(tmp + 56, 0, 8);
    return fnv1a64(tmp, sizeof(tmp));
}

static void encode_superblock(const SqlVfsSuperblock* b, uint8_t* out) {
    memset(out, 0, SQL_VFS_SUPERBLOCK_ENCODED_LEN);
    out[0] = SQL_VFS_MAGIC0; out[1] = SQL_VFS_MAGIC1;
    out[2] = SQL_VFS_MAGIC2; out[3] = SQL_VFS_MAGIC3;
    out[4] = SQL_VFS_MAGIC4; out[5] = SQL_VFS_MAGIC5;
    out[6] = SQL_VFS_MAGIC6; out[7] = 0;
    write_u32(out + 8, SQL_VFS_VERSION);
    write_u32(out + 12, (uint32_t)SQL_VFS_SQLITE_PAGE_SIZE);
    write_u64(out + 16, b->db_size);
    write_u64(out + 24, b->page_count);
    write_u64(out + 32, b->page_table_offset);
    write_u64(out + 40, b->append_offset);
    write_u64(out + 48, b->last_tx_id);
    write_u64(out + 56, 0);
    write_u64(out + 64, b->memory_id);
    write_u64(out + 72, 0);
    write_u64(out + 56, superblock_checksum(out));
}

static int decode_superblock(const uint8_t* in, SqlVfsSuperblock* out) {
    if (in[0] != SQL_VFS_MAGIC0 || in[1] != SQL_VFS_MAGIC1 ||
        in[2] != SQL_VFS_MAGIC2 || in[3] != SQL_VFS_MAGIC3 ||
        in[4] != SQL_VFS_MAGIC4 || in[5] != SQL_VFS_MAGIC5 ||
        in[6] != SQL_VFS_MAGIC6) {
        return SQLITE_EMPTY;
    }
    if (read_u32(in + 8) != SQL_VFS_VERSION) return SQLITE_NOTADB;
    if (read_u32(in + 12) != (uint32_t)SQL_VFS_SQLITE_PAGE_SIZE) return SQLITE_NOTADB;
    uint64_t stored = read_u64(in + 56);
    if (stored != superblock_checksum(in)) return SQLITE_CORRUPT;
    out->db_size = read_u64(in + 16);
    out->page_count = read_u64(in + 24);
    out->page_table_offset = read_u64(in + 32);
    out->append_offset = read_u64(in + 40);
    out->last_tx_id = read_u64(in + 48);
    out->checksum = stored;
    out->memory_id = read_u64(in + 64);
    return SQLITE_OK;
}

static int store_superblock(const SqlVfsSuperblock* b) {
    uint8_t encoded[SQL_VFS_SUPERBLOCK_ENCODED_LEN];
    int rc = ensure_stable_capacity(g_base_offset + SQL_VFS_SUPERBLOCK_SIZE);
    if (rc != SQLITE_OK) return rc;
    encode_superblock(b, encoded);
    ic0_stable64_write(g_base_offset, (uint64_t)(uintptr_t)encoded, sizeof(encoded));
    g_block = *b;
    g_block_loaded = 1;
    return SQLITE_OK;
}

static int load_superblock(void) {
    if (g_block_loaded) return SQLITE_OK;
    int rc = ensure_stable_capacity(g_base_offset + SQL_VFS_SUPERBLOCK_SIZE);
    if (rc != SQLITE_OK) return rc;
    uint8_t encoded[SQL_VFS_SUPERBLOCK_ENCODED_LEN];
    memset(encoded, 0, sizeof(encoded));
    ic0_stable64_read((uint64_t)(uintptr_t)encoded, g_base_offset, sizeof(encoded));
    rc = decode_superblock(encoded, &g_block);
    if (rc == SQLITE_EMPTY) {
        SqlVfsSuperblock fresh;
        memset(&fresh, 0, sizeof(fresh));
        fresh.memory_id = (uint64_t)g_memory_id;
        fresh.append_offset = g_base_offset + SQL_VFS_SUPERBLOCK_SIZE;
        return store_superblock(&fresh);
    }
    if (rc != SQLITE_OK) return rc;
    g_block_loaded = 1;
    return SQLITE_OK;
}

static uint64_t page_count_for_size(uint64_t size) {
    return (size + SQL_VFS_SQLITE_PAGE_SIZE - 1) / SQL_VFS_SQLITE_PAGE_SIZE;
}

static int read_page_table_entry(uint64_t page_no, uint64_t* out) {
    *out = 0;
    if (load_superblock() != SQLITE_OK) return SQLITE_IOERR_READ;
    if (page_no >= g_block.page_count || g_block.page_table_offset == 0) return SQLITE_OK;
    uint64_t offset = g_block.page_table_offset + page_no * 8;
    ic0_stable64_read((uint64_t)(uintptr_t)out, offset, 8);
    return SQLITE_OK;
}

static SqlVfsDirtyPage* find_dirty_page(uint64_t page_no) {
    for (uint64_t i = 0; i < g_dirty_count; i++) {
        if (g_dirty_pages[i].page_no == page_no) return &g_dirty_pages[i];
    }
    return NULL;
}

static int read_base_page(uint64_t page_no, uint8_t* out) {
    memset(out, 0, SQL_VFS_SQLITE_PAGE_SIZE);
    uint64_t phys = 0;
    int rc = read_page_table_entry(page_no, &phys);
    if (rc != SQLITE_OK || phys == 0) return rc;
    ic0_stable64_read((uint64_t)(uintptr_t)out, phys, SQL_VFS_SQLITE_PAGE_SIZE);
    return SQLITE_OK;
}

static SqlVfsDirtyPage* ensure_dirty_page(uint64_t page_no) {
    SqlVfsDirtyPage* existing = find_dirty_page(page_no);
    if (existing != NULL) return existing;
    if (g_dirty_count == g_dirty_cap) {
        uint64_t new_cap = g_dirty_cap == 0 ? 8 : g_dirty_cap * 2;
        void* next = sqlite3_realloc64(
            g_dirty_pages, new_cap * (uint64_t)sizeof(SqlVfsDirtyPage));
        if (next == NULL) return NULL;
        g_dirty_pages = (SqlVfsDirtyPage*)next;
        g_dirty_cap = new_cap;
    }
    SqlVfsDirtyPage* page = &g_dirty_pages[g_dirty_count++];
    page->page_no = page_no;
    if (read_base_page(page_no, page->data) != SQLITE_OK) {
        memset(page->data, 0, sizeof(page->data));
    }
    return page;
}

static int main_read(uint64_t offset, uint8_t* dst, uint64_t len) {
    uint64_t visible_size = g_overlay_active ? g_overlay_size : g_block.db_size;
    int short_read = 0;
    if (offset >= visible_size) {
        memset(dst, 0, (size_t)len);
        return SQLITE_IOERR_SHORT_READ;
    }
    if (offset + len > visible_size) {
        uint64_t keep = visible_size - offset;
        memset(dst + keep, 0, (size_t)(len - keep));
        len = keep;
        short_read = 1;
    }
    while (len > 0) {
        uint64_t page_no = offset / SQL_VFS_SQLITE_PAGE_SIZE;
        uint64_t in_page = offset % SQL_VFS_SQLITE_PAGE_SIZE;
        uint64_t take = SQL_VFS_SQLITE_PAGE_SIZE - in_page;
        if (take > len) take = len;
        SqlVfsDirtyPage* dirty = g_overlay_active ? find_dirty_page(page_no) : NULL;
        if (dirty != NULL) {
            memcpy(dst, dirty->data + in_page, (size_t)take);
        } else {
            uint8_t page[SQL_VFS_SQLITE_PAGE_SIZE];
            int rc = read_base_page(page_no, page);
            if (rc != SQLITE_OK) return SQLITE_IOERR_READ;
            memcpy(dst, page + in_page, (size_t)take);
        }
        offset += take;
        dst += take;
        len -= take;
    }
    return short_read ? SQLITE_IOERR_SHORT_READ : SQLITE_OK;
}

static int main_write(uint64_t offset, const uint8_t* src, uint64_t len) {
    if (!g_overlay_active) return SQLITE_IOERR_WRITE;
    if (offset + len > SQL_VFS_MAX_DB_SIZE) return SQLITE_FULL;
    uint64_t end = offset + len;
    while (len > 0) {
        uint64_t page_no = offset / SQL_VFS_SQLITE_PAGE_SIZE;
        uint64_t in_page = offset % SQL_VFS_SQLITE_PAGE_SIZE;
        uint64_t take = SQL_VFS_SQLITE_PAGE_SIZE - in_page;
        if (take > len) take = len;
        SqlVfsDirtyPage* page = ensure_dirty_page(page_no);
        if (page == NULL) return SQLITE_NOMEM;
        memcpy(page->data + in_page, src, (size_t)take);
        offset += take;
        src += take;
        len -= take;
    }
    if (end > g_overlay_size) g_overlay_size = end;
    return SQLITE_OK;
}

static int temp_reserve(SqlVfsFile* f, uint64_t need) {
    if (need <= f->temp_cap) return SQLITE_OK;
    uint64_t cap = f->temp_cap == 0 ? 4096 : f->temp_cap;
    while (cap < need) cap *= 2;
    void* next = sqlite3_realloc64(f->temp, cap);
    if (next == NULL) return SQLITE_NOMEM;
    if (cap > f->temp_cap) memset((uint8_t*)next + f->temp_cap, 0, (size_t)(cap - f->temp_cap));
    f->temp = (uint8_t*)next;
    f->temp_cap = cap;
    return SQLITE_OK;
}

static int vfs_xClose(sqlite3_file* file) {
    SqlVfsFile* f = (SqlVfsFile*)file;
    if (!f->is_main && f->temp != NULL) {
        sqlite3_free(f->temp);
    }
    memset(f, 0, sizeof(*f));
    return SQLITE_OK;
}

static int vfs_xRead(sqlite3_file* file, void* buf, int amount, sqlite3_int64 offset) {
    if (amount < 0 || offset < 0) return SQLITE_IOERR_READ;
    SqlVfsFile* f = (SqlVfsFile*)file;
    if (!f->is_main) {
        uint64_t off = (uint64_t)offset;
        uint64_t len = (uint64_t)amount;
        if (off >= f->temp_size) {
            memset(buf, 0, (size_t)len);
            return SQLITE_IOERR_SHORT_READ;
        }
        uint64_t keep = len;
        int short_read = 0;
        if (off + keep > f->temp_size) {
            keep = f->temp_size - off;
            short_read = 1;
        }
        memcpy(buf, f->temp + off, (size_t)keep);
        if (keep < len) memset((uint8_t*)buf + keep, 0, (size_t)(len - keep));
        return short_read ? SQLITE_IOERR_SHORT_READ : SQLITE_OK;
    }
    return main_read((uint64_t)offset, (uint8_t*)buf, (uint64_t)amount);
}

static int vfs_xWrite(sqlite3_file* file, const void* buf, int amount, sqlite3_int64 offset) {
    if (amount < 0 || offset < 0) return SQLITE_IOERR_WRITE;
    SqlVfsFile* f = (SqlVfsFile*)file;
    if (f->read_only) return SQLITE_READONLY;
    if (!f->is_main) {
        uint64_t off = (uint64_t)offset;
        uint64_t len = (uint64_t)amount;
        int rc = temp_reserve(f, off + len);
        if (rc != SQLITE_OK) return rc;
        memcpy(f->temp + off, buf, (size_t)len);
        if (off + len > f->temp_size) f->temp_size = off + len;
        return SQLITE_OK;
    }
    return main_write((uint64_t)offset, (const uint8_t*)buf, (uint64_t)amount);
}

static int vfs_xTruncate(sqlite3_file* file, sqlite3_int64 size) {
    if (size < 0) return SQLITE_IOERR_TRUNCATE;
    SqlVfsFile* f = (SqlVfsFile*)file;
    if (f->read_only) return SQLITE_READONLY;
    if (!f->is_main) {
        if ((uint64_t)size < f->temp_size) f->temp_size = (uint64_t)size;
        return SQLITE_OK;
    }
    if (!g_overlay_active) return SQLITE_IOERR_TRUNCATE;
    g_overlay_size = (uint64_t)size;
    if ((g_overlay_size % SQL_VFS_SQLITE_PAGE_SIZE) != 0) {
        uint64_t page_no = g_overlay_size / SQL_VFS_SQLITE_PAGE_SIZE;
        uint64_t in_page = g_overlay_size % SQL_VFS_SQLITE_PAGE_SIZE;
        SqlVfsDirtyPage* page = ensure_dirty_page(page_no);
        if (page == NULL) return SQLITE_NOMEM;
        memset(page->data + in_page, 0, (size_t)(SQL_VFS_SQLITE_PAGE_SIZE - in_page));
    }
    return SQLITE_OK;
}

static int vfs_xSync(sqlite3_file* file, int flags) {
    (void)file; (void)flags;
    return SQLITE_OK;
}

static int vfs_xFileSize(sqlite3_file* file, sqlite3_int64* out) {
    SqlVfsFile* f = (SqlVfsFile*)file;
    if (!f->is_main) {
        *out = (sqlite3_int64)f->temp_size;
        return SQLITE_OK;
    }
    if (load_superblock() != SQLITE_OK) return SQLITE_IOERR_FSTAT;
    *out = (sqlite3_int64)(g_overlay_active ? g_overlay_size : g_block.db_size);
    return SQLITE_OK;
}

static int vfs_xLock(sqlite3_file* file, int lock) {
    (void)file; (void)lock;
    return SQLITE_OK;
}

static int vfs_xUnlock(sqlite3_file* file, int lock) {
    (void)file; (void)lock;
    return SQLITE_OK;
}

static int vfs_xCheckReservedLock(sqlite3_file* file, int* out) {
    (void)file;
    *out = 0;
    return SQLITE_OK;
}

static int vfs_xFileControl(sqlite3_file* file, int op, void* arg) {
    (void)file;
    if (op == SQLITE_FCNTL_LOCKSTATE && arg != NULL) {
        *(int*)arg = 0;
        return SQLITE_OK;
    }
    return SQLITE_NOTFOUND;
}

static int vfs_xSectorSize(sqlite3_file* file) {
    (void)file;
    return (int)SQL_VFS_SQLITE_PAGE_SIZE;
}

static int vfs_xDeviceCharacteristics(sqlite3_file* file) {
    (void)file;
    return 0;
}

static int is_main_name(const char* name, int flags) {
    if ((flags & SQLITE_OPEN_MAIN_DB) != 0) return 1;
    if (name == NULL) return 0;
    return strcmp(name, SQL_VFS_MAIN_DB) == 0 || strcmp(name, "main.db") == 0;
}

static int vfs_xOpen(sqlite3_vfs* vfs, const char* name, sqlite3_file* file,
                     int flags, int* out_flags) {
    (void)vfs;
    if ((flags & SQLITE_OPEN_WAL) != 0) return SQLITE_CANTOPEN;
    SqlVfsFile* f = (SqlVfsFile*)file;
    memset(f, 0, sizeof(*f));
    f->base.pMethods = &g_sql_vfs_io_methods;
    f->is_main = is_main_name(name, flags);
    f->read_only = (flags & SQLITE_OPEN_READONLY) != 0;
    if (out_flags != NULL) *out_flags = flags;
    if (f->is_main && load_superblock() != SQLITE_OK) return SQLITE_CANTOPEN;
    return SQLITE_OK;
}

static int vfs_xDelete(sqlite3_vfs* vfs, const char* name, int sync_dir) {
    (void)vfs; (void)sync_dir;
    return is_main_name(name, 0) ? SQLITE_IOERR_DELETE : SQLITE_OK;
}

static int vfs_xAccess(sqlite3_vfs* vfs, const char* name, int flags, int* out) {
    (void)vfs; (void)flags;
    *out = is_main_name(name, 0) ? 1 : 0;
    return SQLITE_OK;
}

static int vfs_xFullPathname(sqlite3_vfs* vfs, const char* name, int out_len, char* out) {
    (void)vfs;
    const char* src = (name == NULL || strcmp(name, "main.db") == 0) ? SQL_VFS_MAIN_DB : name;
    size_t len = strlen(src);
    if (out_len <= 0 || len >= (size_t)out_len) return SQLITE_CANTOPEN;
    memcpy(out, src, len + 1);
    return SQLITE_OK;
}

static int vfs_xRandomness(sqlite3_vfs* vfs, int amount, char* out) {
    (void)vfs;
    uint64_t state = ic0_time() ^ g_block.last_tx_id ^ 0x9e3779b97f4a7c15ULL;
    for (int i = 0; i < amount; i++) {
        state ^= state << 7;
        state ^= state >> 9;
        state *= 1099511628211ULL;
        out[i] = (char)(state & 0xff);
    }
    return amount;
}

static int vfs_xSleep(sqlite3_vfs* vfs, int microseconds) {
    (void)vfs;
    return microseconds;
}

static int vfs_xCurrentTimeInt64(sqlite3_vfs* vfs, sqlite3_int64* out) {
    (void)vfs;
    uint64_t unix_ms = ic0_time() / 1000000ULL;
    *out = (sqlite3_int64)(210866760000000ULL + unix_ms);
    return SQLITE_OK;
}

static int vfs_xCurrentTime(sqlite3_vfs* vfs, double* out) {
    sqlite3_int64 t = 0;
    int rc = vfs_xCurrentTimeInt64(vfs, &t);
    if (rc != SQLITE_OK) return rc;
    *out = ((double)t) / 86400000.0;
    return SQLITE_OK;
}

static int vfs_xGetLastError(sqlite3_vfs* vfs, int len, char* out) {
    (void)vfs;
    if (len > 0 && out != NULL) out[0] = 0;
    return 0;
}

static void sql_vfs_prepare_structs(void) {
    if (g_sql_vfs_prepared) return;
    memset(&g_sql_vfs_io_methods, 0, sizeof(g_sql_vfs_io_methods));
    g_sql_vfs_io_methods.iVersion = 1;
    g_sql_vfs_io_methods.xClose = vfs_xClose;
    g_sql_vfs_io_methods.xRead = vfs_xRead;
    g_sql_vfs_io_methods.xWrite = vfs_xWrite;
    g_sql_vfs_io_methods.xTruncate = vfs_xTruncate;
    g_sql_vfs_io_methods.xSync = vfs_xSync;
    g_sql_vfs_io_methods.xFileSize = vfs_xFileSize;
    g_sql_vfs_io_methods.xLock = vfs_xLock;
    g_sql_vfs_io_methods.xUnlock = vfs_xUnlock;
    g_sql_vfs_io_methods.xCheckReservedLock = vfs_xCheckReservedLock;
    g_sql_vfs_io_methods.xFileControl = vfs_xFileControl;
    g_sql_vfs_io_methods.xSectorSize = vfs_xSectorSize;
    g_sql_vfs_io_methods.xDeviceCharacteristics = vfs_xDeviceCharacteristics;

    memset(&g_sql_vfs, 0, sizeof(g_sql_vfs));
    g_sql_vfs.iVersion = 1;
    g_sql_vfs.szOsFile = (int)sizeof(SqlVfsFile);
    g_sql_vfs.mxPathname = 256;
    g_sql_vfs.zName = SQL_VFS_NAME;
    g_sql_vfs.xOpen = vfs_xOpen;
    g_sql_vfs.xDelete = vfs_xDelete;
    g_sql_vfs.xAccess = vfs_xAccess;
    g_sql_vfs.xFullPathname = vfs_xFullPathname;
    g_sql_vfs.xRandomness = vfs_xRandomness;
    g_sql_vfs.xSleep = vfs_xSleep;
    g_sql_vfs.xCurrentTime = vfs_xCurrentTime;
    g_sql_vfs.xGetLastError = vfs_xGetLastError;
    g_sql_vfs.xCurrentTimeInt64 = vfs_xCurrentTimeInt64;
    g_sql_vfs_prepared = 1;
}

static int sql_vfs_register_default(void) {
    if (g_sql_vfs_registered) return SQLITE_OK;
    sql_vfs_prepare_structs();
    int rc = sqlite3_vfs_register(&g_sql_vfs, 1);
    if (rc == SQLITE_OK) g_sql_vfs_registered = 1;
    return rc;
}

/*
 * SQLITE_OS_OTHER=1 removes SQLite's Unix/WASI OS backend and asks the
 * embedding application to provide sqlite3_os_init/sqlite3_os_end. Register the
 * IC stable-memory VFS here so sqlite3_initialize() has exactly one default VFS
 * without pulling in WASI fd/read/write/seek paths.
 */
int sqlite3_os_init(void) {
    return sql_vfs_register_default();
}

int sqlite3_os_end(void) {
    return SQLITE_OK;
}

int sql_vfs_register(int64_t memory_id) {
    if (memory_id < 0) return SQLITE_MISUSE;
    int rc = sql_vfs_register_default();
    if (rc != SQLITE_OK) return rc;
    g_memory_id = memory_id;
    g_base_offset = ((uint64_t)memory_id) * SQL_VFS_STABLE_PAGE_SIZE;
    g_block_loaded = 0;
    return load_superblock();
}

int sql_vfs_begin_update(void) {
    int rc = load_superblock();
    if (rc != SQLITE_OK) return rc;
    sql_vfs_rollback_update();
    g_overlay_active = 1;
    g_overlay_size = g_block.db_size;
    return SQLITE_OK;
}

int sql_vfs_commit_update(void) {
    if (!g_overlay_active) return SQLITE_MISUSE;
    uint64_t new_page_count = page_count_for_size(g_overlay_size);
    uint64_t* page_map = NULL;
    if (new_page_count > 0) {
        page_map = (uint64_t*)sqlite3_malloc64(new_page_count * 8);
        if (page_map == NULL) return SQLITE_NOMEM;
        memset(page_map, 0, (size_t)(new_page_count * 8));
        uint64_t copy = g_block.page_count < new_page_count ? g_block.page_count : new_page_count;
        if (copy > 0 && g_block.page_table_offset != 0) {
            ic0_stable64_read((uint64_t)(uintptr_t)page_map,
                              g_block.page_table_offset,
                              copy * 8);
        }
    }

    uint64_t cursor = g_block.append_offset;
    for (uint64_t i = 0; i < g_dirty_count; i++) {
        uint64_t page_no = g_dirty_pages[i].page_no;
        if (page_no >= new_page_count) continue;
        int rc = ensure_stable_capacity(cursor + SQL_VFS_SQLITE_PAGE_SIZE);
        if (rc != SQLITE_OK) {
            sqlite3_free(page_map);
            return rc;
        }
        ic0_stable64_write(cursor,
                           (uint64_t)(uintptr_t)g_dirty_pages[i].data,
                           SQL_VFS_SQLITE_PAGE_SIZE);
        page_map[page_no] = cursor;
        cursor += SQL_VFS_SQLITE_PAGE_SIZE;
    }

    uint64_t page_table_offset = 0;
    if (new_page_count > 0) {
        page_table_offset = cursor;
        uint64_t table_bytes = new_page_count * 8;
        int rc = ensure_stable_capacity(cursor + table_bytes);
        if (rc != SQLITE_OK) {
            sqlite3_free(page_map);
            return rc;
        }
        ic0_stable64_write(cursor, (uint64_t)(uintptr_t)page_map, table_bytes);
        cursor += table_bytes;
    }
    sqlite3_free(page_map);

    SqlVfsSuperblock next = g_block;
    next.db_size = g_overlay_size;
    next.page_count = new_page_count;
    next.page_table_offset = page_table_offset;
    next.append_offset = cursor;
    next.last_tx_id = g_block.last_tx_id + 1;
    next.memory_id = (uint64_t)g_memory_id;
    int rc = store_superblock(&next);
    if (rc == SQLITE_OK) sql_vfs_rollback_update();
    return rc;
}

void sql_vfs_rollback_update(void) {
    if (g_dirty_pages != NULL) {
        sqlite3_free(g_dirty_pages);
    }
    g_dirty_pages = NULL;
    g_dirty_count = 0;
    g_dirty_cap = 0;
    g_overlay_size = 0;
    g_overlay_active = 0;
}

int64_t sql_vfs_ffi_init(int64_t memory_id) {
    return (int64_t)sql_vfs_register(memory_id);
}

int64_t sql_vfs_ffi_open_read_write(void) {
    if (!g_overlay_active) {
        int begin_rc = sql_vfs_begin_update();
        if (begin_rc != SQLITE_OK) return begin_rc;
    }
    int rc = sql_open_vfs(SQL_VFS_MAIN_DB, SQL_VFS_NAME, 0);
    if (rc != SQLITE_OK) return rc;
    return (int64_t)sql_tx_begin();
}

int64_t sql_vfs_ffi_open_read_only(void) {
    return (int64_t)sql_open_vfs(SQL_VFS_MAIN_DB, SQL_VFS_NAME, 1);
}

int64_t sql_vfs_ffi_begin_update(void) {
    return (int64_t)sql_vfs_begin_update();
}

int64_t sql_vfs_ffi_commit_update(void) {
    int rc = sql_tx_commit();
    if (rc != SQLITE_OK) {
        sql_tx_rollback();
        sql_vfs_rollback_update();
        return rc;
    }
    return (int64_t)sql_vfs_commit_update();
}

int64_t sql_vfs_ffi_rollback_update(void) {
    sql_tx_rollback();
    sql_vfs_rollback_update();
    return 0;
}

int64_t sql_vfs_ffi_close(void) {
    return (int64_t)sql_close();
}

int64_t sql_vfs_ffi_memory_id(void) {
    return g_memory_id;
}

int64_t sql_vfs_ffi_page_count(void) {
    return load_superblock() == SQLITE_OK ? (int64_t)g_block.page_count : -1;
}

int64_t sql_vfs_ffi_allocated_bytes(void) {
    return load_superblock() == SQLITE_OK
        ? (int64_t)(g_block.append_offset - g_base_offset)
        : -1;
}

int64_t sql_vfs_ffi_active_bytes(void) {
    return load_superblock() == SQLITE_OK ? (int64_t)g_block.db_size : -1;
}
