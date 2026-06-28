/*
 * BulletproofVFS — a direct, in-place SQLite VFS backed by IC stable memory.
 *
 * This is the implementation of the "vertex BC" design from the paper
 * "SQLite VFS on the Internet Computer のトリレンマとその超越" (2026-06):
 * each logical page n lives at a single, computed physical position
 *     phys(n) = B + n * P
 * and writes overwrite that position in place. There is NO append-only log,
 * NO page table, NO shadow/overlay, and NO write-ahead journal of our own.
 *
 * Why this is correct without a self-made atomicity mechanism:
 *   The Internet Computer commits all stable-memory writes of an update message
 *   atomically — on success every write persists, on trap every write is
 *   discarded (orthogonal persistence). As long as one SQLite commit happens
 *   within ONE update message and contains NO inter-canister call (the
 *   single-message constraint, D1), a mid-commit trap rolls back every in-place
 *   write, so no torn state is ever observed. The VFS therefore BORROWS its
 *   atomicity from the substrate. (Paper: 定理 8.4, 系 8.5.)
 *
 * Why this is "bulletproof": the previous append-only VFS satisfied atomicity
 * and locality but NOT bounded allocation — stable grew without limit, which
 * starved cycles and halted the canister (the bug this whole series chased).
 * BulletproofVFS keeps allocation at ~M (one slot per logical page, no append),
 * keeps write locality (contiguous layout, paper 命題 7.4.2), and borrows
 * atomicity. It plugs all three holes of the trilemma at once, under the
 * single-message constraint. (Paper: §0 "防弾の定義".)
 *
 * LIMITS (not invincible):
 *   - Relies on IC message atomicity. Not portable to a substrate without it.
 *   - The single-message constraint (D1) must hold: a commit must not span
 *     messages (no inter-canister call mid-commit). SQLite commits are local
 *     computations, so this holds naturally.
 *   - cap is grow-only at the substrate (公理 3.2): a transient size peak fixes
 *     cap at 2-slot-free max-K; lowering the water line still needs reinstall.
 *
 * Stable layout (memory_id anchors the base byte = memory_id * 64KiB):
 *   [ superblock slot 0 | 64KiB ][ superblock slot 1 | 64KiB ][ page region … ]
 *   g0 = base                      g1 = base + 64KiB            B  = base + 128KiB
 * Two superblock slots in DISTINCT charge pages so a partial write cannot
 * corrupt both (paper 修正 9.2 / 判定 1.3.5). The valid slot is the one with the
 * larger tx_id whose checksum verifies.
 *
 * This file shares the exact FFI boundary of sqlite_vfs_bridge.c
 * (sql_vfs_ffi_* and sql_vfs_register/begin/commit/rollback). Build with
 * -DUSE_BULLETPROOF_VFS to link this instead of the append-only one.
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

/* --- TEMPORARY debug instrumentation (BP_DEBUG) for the SQL-write-no-op probe.
 * Remove once the persistence bug is root-fixed. Logs key VFS events + two
 * numbers to the canister log so storeTestEvent's write→commit→next-call-load
 * is observable. */
#ifdef BP_DEBUG
extern void ic0_debug_print(int32_t src, int32_t size);
static void bp_dbg(const char* tag, uint64_t a, uint64_t b) {
    char buf[96]; int n = 0;
    for (const char* p = tag; *p && n < 40; ++p) buf[n++] = *p;
    buf[n++] = ' ';
    char tmp[24]; int t = 0; uint64_t x = a;
    if (x == 0) tmp[t++] = '0'; else { while (x) { tmp[t++] = (char)('0' + (x % 10)); x /= 10; } }
    while (t) buf[n++] = tmp[--t];
    buf[n++] = ','; buf[n++] = ' ';
    t = 0; x = b;
    if (x == 0) tmp[t++] = '0'; else { while (x) { tmp[t++] = (char)('0' + (x % 10)); x /= 10; } }
    while (t) buf[n++] = tmp[--t];
    ic0_debug_print((int32_t)(uintptr_t)buf, n);
}
#else
static inline void bp_dbg(const char* tag, uint64_t a, uint64_t b) { (void)tag; (void)a; (void)b; }
#endif

/* ------------------------------------------------------------------ */
/* Constants                                                          */
/* ------------------------------------------------------------------ */

#define BP_MAGIC0 'B'
#define BP_MAGIC1 'P'
#define BP_MAGIC2 'V'
#define BP_MAGIC3 'F'
#define BP_MAGIC4 'S'
#define BP_MAGIC5 'D'
#define BP_MAGIC6 'B'
#define BP_VERSION 1U

#define BP_P            SQL_VFS_SQLITE_PAGE_SIZE   /* logical page length P */
#define BP_STABLE_PAGE  SQL_VFS_STABLE_PAGE_SIZE   /* 64KiB grow unit       */
#define BP_SB_ENCODED   64U                        /* bytes we actually write */
#define BP_MAX_DB_SIZE  (256ULL * 1024ULL * 1024ULL)

/* superblock slots occupy one 64KiB stable page each (distinct charge pages),
 * the page region begins after both slots. */
#define BP_SLOT0_OFF(base)  (base)
#define BP_SLOT1_OFF(base)  ((base) + BP_STABLE_PAGE)
#define BP_REGION_OFF(base) ((base) + 2ULL * BP_STABLE_PAGE)

/* ------------------------------------------------------------------ */
/* Superblock                                                         */
/* ------------------------------------------------------------------ */

typedef struct {
    uint64_t db_size;     /* logical DB byte size (SQLite file size)        */
    uint64_t tx_id;       /* monotonic; the larger valid slot is authoritative */
    uint64_t memory_id;   /* stable anchor (memory_id * 64KiB = base byte)  */
    uint64_t checksum;    /* FNV-1a over the other fields                    */
} BpSuper;

typedef struct {
    sqlite3_file base;
    int is_main;
    int read_only;
    /* temp/transient DBs (e.g. VACUUM's scratch) live on the wasm heap, never
     * in stable — exactly like the append-only VFS, so VACUUM never touches
     * stable cap (paper 観測 4.7). */
    uint8_t* temp;
    uint64_t temp_size;
    uint64_t temp_cap;
} BpFile;

static sqlite3_io_methods g_bp_io;
static sqlite3_vfs        g_bp_vfs;
static int      g_bp_registered = 0;
static int64_t  g_memory_id = -1;
static uint64_t g_base = 0;          /* base byte = memory_id * 64KiB */
static BpSuper  g_sb;
static int      g_sb_loaded = 0;
static int      g_sb_dirty = 0;      /* in-memory db_size differs from persisted superblock */
static int      g_in_update = 0;     /* logical transaction open? (read-your-writes is automatic in-place) */

/* ------------------------------------------------------------------ */
/* Small helpers                                                      */
/* ------------------------------------------------------------------ */

static uint64_t bp_read_u64(const uint8_t* p) {
    uint64_t v = 0;
    for (int i = 0; i < 8; i++) v |= ((uint64_t)p[i]) << (8 * i);
    return v;
}
static void bp_write_u64(uint8_t* p, uint64_t v) {
    for (int i = 0; i < 8; i++) p[i] = (uint8_t)((v >> (8 * i)) & 0xff);
}
static uint64_t bp_fnv1a(const uint8_t* d, size_t n) {
    uint64_t h = 1469598103934665603ULL;
    for (size_t i = 0; i < n; i++) { h ^= d[i]; h *= 1099511628211ULL; }
    return h;
}

/* grow stable so that byte [0, end_offset) is allocated. cap is grow-only. */
static int bp_ensure_capacity(uint64_t end_offset) {
    uint64_t pages = ic0_stable64_size();
    uint64_t bytes = pages * BP_STABLE_PAGE;
    if (end_offset <= bytes) return SQLITE_OK;
    uint64_t need_pages = (end_offset + BP_STABLE_PAGE - 1) / BP_STABLE_PAGE;
    uint64_t old = ic0_stable64_grow(need_pages - pages);
    return old == UINT64_MAX ? SQLITE_FULL : SQLITE_OK;
}

/* encode/decode the 64-byte superblock image. checksum is field index 5. */
static void bp_encode(const BpSuper* s, uint8_t* out) {
    memset(out, 0, BP_SB_ENCODED);
    out[0]=BP_MAGIC0; out[1]=BP_MAGIC1; out[2]=BP_MAGIC2; out[3]=BP_MAGIC3;
    out[4]=BP_MAGIC4; out[5]=BP_MAGIC5; out[6]=BP_MAGIC6; out[7]=0;
    bp_write_u64(out + 8,  BP_VERSION);
    bp_write_u64(out + 16, s->db_size);
    bp_write_u64(out + 24, s->tx_id);
    bp_write_u64(out + 32, s->memory_id);
    bp_write_u64(out + 40, 0);            /* reserved */
    /* checksum over bytes [0,48) with the checksum slot (48..56) zeroed */
    bp_write_u64(out + 48, 0);
    uint64_t ck = bp_fnv1a(out, 48);
    bp_write_u64(out + 48, ck);
}
static int bp_decode(const uint8_t* in, BpSuper* out) {
    if (in[0]!=BP_MAGIC0||in[1]!=BP_MAGIC1||in[2]!=BP_MAGIC2||in[3]!=BP_MAGIC3||
        in[4]!=BP_MAGIC4||in[5]!=BP_MAGIC5||in[6]!=BP_MAGIC6) return SQLITE_EMPTY;
    if (bp_read_u64(in + 8) != BP_VERSION) return SQLITE_NOTADB;
    uint8_t tmp[BP_SB_ENCODED];
    memcpy(tmp, in, BP_SB_ENCODED);
    uint64_t stored = bp_read_u64(tmp + 48);
    bp_write_u64(tmp + 48, 0);
    if (bp_fnv1a(tmp, 48) != stored) return SQLITE_CORRUPT;
    out->db_size   = bp_read_u64(in + 16);
    out->tx_id     = bp_read_u64(in + 24);
    out->memory_id = bp_read_u64(in + 32);
    out->checksum  = stored;
    return SQLITE_OK;
}

/* Read a slot; SQLITE_OK if it decodes, else an error/empty code. */
static int bp_read_slot(uint64_t slot_off, BpSuper* out) {
    uint8_t enc[BP_SB_ENCODED];
    memset(enc, 0, sizeof(enc));
    ic0_stable64_read((uint64_t)(uintptr_t)enc, slot_off, sizeof(enc));
    return bp_decode(enc, out);
}

/* Write the superblock to the NON-active slot, then it becomes active by virtue
 * of having the larger tx_id. The two slots alternate by tx_id parity. */
static int bp_store_super(const BpSuper* s) {
    int rc = bp_ensure_capacity(BP_REGION_OFF(g_base));
    if (rc != SQLITE_OK) return rc;
    uint8_t enc[BP_SB_ENCODED];
    bp_encode(s, enc);
    /* write into the slot determined by tx_id parity: even→slot0, odd→slot1.
     * Since tx_id strictly increases, consecutive commits alternate slots, so a
     * mid-write trap can only corrupt the slot being written; the previous
     * (other) slot, holding tx_id-1, stays intact and is selected on reload. */
    uint64_t slot = (s->tx_id & 1ULL) ? BP_SLOT1_OFF(g_base) : BP_SLOT0_OFF(g_base);
    ic0_stable64_write(slot, (uint64_t)(uintptr_t)enc, sizeof(enc));
    g_sb = *s;
    g_sb_loaded = 1;
    return SQLITE_OK;
}

/* Persist the in-memory superblock (db_size) if it diverged from what is on
 * stable. Bumps tx_id and writes the alternate slot so the size update is itself
 * crash-consistent. Called at commit/xSync/truncate — once per transaction, not
 * once per page. No-op if nothing changed. */
static int bp_flush_super(void) {
    bp_dbg("flush_super dirty,db_size", (uint64_t)g_sb_dirty, g_sb.db_size);
    if (!g_sb_dirty) return SQLITE_OK;
    BpSuper next = g_sb;
    next.tx_id = g_sb.tx_id + 1;
    int rc = bp_store_super(&next);   /* updates g_sb to `next` on success */
    if (rc == SQLITE_OK) g_sb_dirty = 0;
    bp_dbg("flush_super wrote tx,db_size", next.tx_id, next.db_size);
    return rc;
}

/* Load the authoritative superblock = the valid slot with the larger tx_id.
 * If neither slot decodes, the DB is fresh: initialise tx_id 0, db_size 0. */
static int bp_load_super(void) {
    if (g_sb_loaded) return SQLITE_OK;
    int rc = bp_ensure_capacity(BP_REGION_OFF(g_base));
    if (rc != SQLITE_OK) return rc;
    BpSuper s0, s1;
    int r0 = bp_read_slot(BP_SLOT0_OFF(g_base), &s0);
    int r1 = bp_read_slot(BP_SLOT1_OFF(g_base), &s1);
    const BpSuper* pick = NULL;
    if (r0 == SQLITE_OK && r1 == SQLITE_OK)
        pick = (s0.tx_id >= s1.tx_id) ? &s0 : &s1;
    else if (r0 == SQLITE_OK) pick = &s0;
    else if (r1 == SQLITE_OK) pick = &s1;
    if (pick != NULL) {
        g_sb = *pick;
        g_sb_loaded = 1;
        bp_dbg("load_super pick tx,db_size", pick->tx_id, pick->db_size);
        return SQLITE_OK;
    }
    /* fresh */
    bp_dbg("load_super FRESH r0,r1", (uint64_t)r0, (uint64_t)r1);
    BpSuper fresh;
    memset(&fresh, 0, sizeof(fresh));
    fresh.memory_id = (uint64_t)g_memory_id;
    fresh.tx_id = 0;
    fresh.db_size = 0;
    return bp_store_super(&fresh);
}

/* ------------------------------------------------------------------ */
/* In-place page I/O: phys(n) = B + n * P                              */
/* ------------------------------------------------------------------ */

static int bp_main_read(uint64_t offset, uint8_t* dst, uint64_t len) {
    if (bp_load_super() != SQLITE_OK) return SQLITE_IOERR_READ;
    uint64_t db_size = g_sb.db_size;
    int short_read = 0;
    if (offset >= db_size) { memset(dst, 0, (size_t)len); return SQLITE_IOERR_SHORT_READ; }
    if (offset + len > db_size) {
        uint64_t keep = db_size - offset;
        memset(dst + keep, 0, (size_t)(len - keep));
        len = keep; short_read = 1;
    }
    /* in-place: the logical byte at file offset `offset` lives at stable byte
     * B + offset. read-your-writes is automatic because writes already went
     * to the same place within this message. */
    ic0_stable64_read((uint64_t)(uintptr_t)dst, BP_REGION_OFF(g_base) + offset, len);
    return short_read ? SQLITE_IOERR_SHORT_READ : SQLITE_OK;
}

static int bp_main_write(uint64_t offset, const uint8_t* src, uint64_t len) {
    if (offset + len > BP_MAX_DB_SIZE) return SQLITE_FULL;
    if (bp_load_super() != SQLITE_OK) return SQLITE_IOERR_WRITE;
    uint64_t end = offset + len;
    int rc = bp_ensure_capacity(BP_REGION_OFF(g_base) + end);
    if (rc != SQLITE_OK) return rc;
    /* in-place overwrite at B + offset. NO append, NO overlay, NO journal. The
     * message either commits all of these writes or discards them on trap. */
    ic0_stable64_write(BP_REGION_OFF(g_base) + offset, (uint64_t)(uintptr_t)src, len);
    bp_dbg("xWrite off,len", offset, len);
    /* Track the new logical size IN MEMORY only. Persisting the superblock on
     * every write would write it once per page (a needless extra dirty page per
     * page, paper §3). Instead the superblock is flushed once at commit/xSync
     * (bp_flush_super), and read-your-writes within the message sees the live
     * g_sb.db_size. If the message traps before flush, all in-place writes AND
     * the size bump are discarded together (message atomicity) — consistent. */
    if (end > g_sb.db_size) { g_sb.db_size = end; g_sb_dirty = 1; }
    return SQLITE_OK;
}

/* ------------------------------------------------------------------ */
/* temp/transient buffer (heap, freed on close) — identical to append-only */
/* ------------------------------------------------------------------ */

static int bp_temp_reserve(BpFile* f, uint64_t need) {
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

/* ------------------------------------------------------------------ */
/* sqlite3_io_methods                                                 */
/* ------------------------------------------------------------------ */

static int bp_xClose(sqlite3_file* file) {
    BpFile* f = (BpFile*)file;
    if (!f->is_main && f->temp != NULL) sqlite3_free(f->temp);
    memset(f, 0, sizeof(*f));
    return SQLITE_OK;
}

static int bp_xRead(sqlite3_file* file, void* buf, int amount, sqlite3_int64 offset) {
    if (amount < 0 || offset < 0) return SQLITE_IOERR_READ;
    BpFile* f = (BpFile*)file;
    if (!f->is_main) {
        uint64_t off = (uint64_t)offset, len = (uint64_t)amount;
        if (off >= f->temp_size) { memset(buf, 0, (size_t)len); return SQLITE_IOERR_SHORT_READ; }
        uint64_t take = len; int sr = 0;
        if (off + len > f->temp_size) { take = f->temp_size - off; memset((uint8_t*)buf + take, 0, (size_t)(len - take)); sr = 1; }
        memcpy(buf, f->temp + off, (size_t)take);
        return sr ? SQLITE_IOERR_SHORT_READ : SQLITE_OK;
    }
    return bp_main_read((uint64_t)offset, (uint8_t*)buf, (uint64_t)amount);
}

static int bp_xWrite(sqlite3_file* file, const void* buf, int amount, sqlite3_int64 offset) {
    if (amount < 0 || offset < 0) return SQLITE_IOERR_WRITE;
    BpFile* f = (BpFile*)file;
    if (f->read_only) return SQLITE_READONLY;
    if (!f->is_main) {
        uint64_t off = (uint64_t)offset, len = (uint64_t)amount;
        int rc = bp_temp_reserve(f, off + len);
        if (rc != SQLITE_OK) return rc;
        memcpy(f->temp + off, buf, (size_t)len);
        if (off + len > f->temp_size) f->temp_size = off + len;
        return SQLITE_OK;
    }
    return bp_main_write((uint64_t)offset, (const uint8_t*)buf, (uint64_t)amount);
}

static int bp_xTruncate(sqlite3_file* file, sqlite3_int64 size) {
    if (size < 0) return SQLITE_IOERR_TRUNCATE;
    BpFile* f = (BpFile*)file;
    if (f->read_only) return SQLITE_READONLY;
    if (!f->is_main) {
        if ((uint64_t)size < f->temp_size) f->temp_size = (uint64_t)size;
        return SQLITE_OK;
    }
    if (bp_load_super() != SQLITE_OK) return SQLITE_IOERR_TRUNCATE;
    /* shrink the logical size only; stable cap stays (grow-only, 公理 3.2).
     * The freed region is simply unreferenced and will be overwritten in place
     * when the file grows again — NO append, so cap does not grow from this.
     * Defer persistence to flush (commit/xSync), like writes. */
    if ((uint64_t)size != g_sb.db_size) { g_sb.db_size = (uint64_t)size; g_sb_dirty = 1; }
    return SQLITE_OK;
}

static int bp_xSync(sqlite3_file* file, int flags) {
    (void)file; (void)flags;
    /* Page bytes already hit stable in place. The only deferred state is the
     * logical db_size in the superblock; flush it here so a reader after this
     * message sees the committed size. (The message boundary is the real
     * durability point; this just makes the size durable with the pages.) */
    BpFile* f = (BpFile*)file;
    if (f->is_main) return bp_flush_super();
    return SQLITE_OK;
}

static int bp_xFileSize(sqlite3_file* file, sqlite3_int64* pSize) {
    BpFile* f = (BpFile*)file;
    if (!f->is_main) { *pSize = (sqlite3_int64)f->temp_size; return SQLITE_OK; }
    if (bp_load_super() != SQLITE_OK) return SQLITE_IOERR_FSTAT;
    *pSize = (sqlite3_int64)g_sb.db_size;
    return SQLITE_OK;
}

static int bp_xLock(sqlite3_file* f, int l)            { (void)f; (void)l; return SQLITE_OK; }
static int bp_xUnlock(sqlite3_file* f, int l)          { (void)f; (void)l; return SQLITE_OK; }
static int bp_xCheckReservedLock(sqlite3_file* f, int* o){ (void)f; *o = 0; return SQLITE_OK; }
static int bp_xFileControl(sqlite3_file* f, int op, void* a){ (void)f; (void)op; (void)a; return SQLITE_NOTFOUND; }
static int bp_xSectorSize(sqlite3_file* f)             { (void)f; return (int)BP_P; }
static int bp_xDeviceChar(sqlite3_file* f)             { (void)f;
    /* ATOMIC at our page granularity, SAFE_APPEND, SEQUENTIAL: tell SQLite the
     * backing store gives atomic page writes (true under message atomicity),
     * which lets it skip some journal traffic. */
    return SQLITE_IOCAP_ATOMIC | SQLITE_IOCAP_SAFE_APPEND | SQLITE_IOCAP_SEQUENTIAL;
}

/* ------------------------------------------------------------------ */
/* sqlite3_vfs                                                        */
/* ------------------------------------------------------------------ */

static int bp_is_main_name(const char* name, int flags) {
    if ((flags & SQLITE_OPEN_MAIN_DB) != 0) return 1;
    if (name == NULL) return 0;
    return strcmp(name, SQL_VFS_MAIN_DB) == 0 || strcmp(name, "main.db") == 0;
}

static int bp_xOpen(sqlite3_vfs* vfs, const char* name, sqlite3_file* file,
                    int flags, int* out_flags) {
    (void)vfs;
    if ((flags & SQLITE_OPEN_WAL) != 0) return SQLITE_CANTOPEN;
    BpFile* f = (BpFile*)file;
    memset(f, 0, sizeof(*f));
    f->base.pMethods = &g_bp_io;
    f->is_main   = bp_is_main_name(name, flags);
    f->read_only = (flags & SQLITE_OPEN_READONLY) != 0;
    if (out_flags != NULL) *out_flags = flags;
    return SQLITE_OK;
}

static int bp_xDelete(sqlite3_vfs* v, const char* n, int s){ (void)v; (void)n; (void)s; return SQLITE_OK; }
static int bp_xAccess(sqlite3_vfs* v, const char* n, int f, int* o){ (void)v; (void)f;
    /* main.db ALWAYS "exists" (even empty): SQLite then opens it and reads its
     * size via xFileSize (0 for a fresh DB) — the correct way to present a
     * possibly-empty database. Reporting 0 here for a fresh DB would make SQLite
     * mishandle the open. Non-main names (journal/wal/temp) report NOT FOUND so
     * SQLite never tries to recover a hot journal this VFS does not have. */
    int is_main = bp_is_main_name(n, 0);
    *o = is_main ? 1 : 0;
    return SQLITE_OK;
}
static int bp_xFullPathname(sqlite3_vfs* v, const char* in, int n, char* out){ (void)v;
    size_t len = strlen(in); if ((int)len + 1 > n) return SQLITE_CANTOPEN;
    memcpy(out, in, len + 1); return SQLITE_OK;
}
static int bp_xRandomness(sqlite3_vfs* v, int n, char* out){ (void)v; memset(out, 0, n); return n; }
static int bp_xSleep(sqlite3_vfs* v, int us){ (void)v; (void)us; return 0; }
static int bp_xCurrentTime(sqlite3_vfs* v, double* out){ (void)v; *out = 0.0; return SQLITE_OK; }

/* ------------------------------------------------------------------ */
/* Public FFI boundary — identical names to sqlite_vfs_bridge.c        */
/* ------------------------------------------------------------------ */

int sql_vfs_register(int64_t memory_id) {
    g_memory_id = memory_id;
    g_base = (uint64_t)memory_id * BP_STABLE_PAGE;
    g_sb_loaded = 0;
    if (g_bp_registered) return SQLITE_OK;

    memset(&g_bp_io, 0, sizeof(g_bp_io));
    g_bp_io.iVersion = 1;
    g_bp_io.xClose = bp_xClose;
    g_bp_io.xRead = bp_xRead;
    g_bp_io.xWrite = bp_xWrite;
    g_bp_io.xTruncate = bp_xTruncate;
    g_bp_io.xSync = bp_xSync;
    g_bp_io.xFileSize = bp_xFileSize;
    g_bp_io.xLock = bp_xLock;
    g_bp_io.xUnlock = bp_xUnlock;
    g_bp_io.xCheckReservedLock = bp_xCheckReservedLock;
    g_bp_io.xFileControl = bp_xFileControl;
    g_bp_io.xSectorSize = bp_xSectorSize;
    g_bp_io.xDeviceCharacteristics = bp_xDeviceChar;

    memset(&g_bp_vfs, 0, sizeof(g_bp_vfs));
    g_bp_vfs.iVersion = 1;
    g_bp_vfs.szOsFile = (int)sizeof(BpFile);
    g_bp_vfs.mxPathname = 256;
    g_bp_vfs.zName = SQL_VFS_NAME;
    g_bp_vfs.xOpen = bp_xOpen;
    g_bp_vfs.xDelete = bp_xDelete;
    g_bp_vfs.xAccess = bp_xAccess;
    g_bp_vfs.xFullPathname = bp_xFullPathname;
    g_bp_vfs.xRandomness = bp_xRandomness;
    g_bp_vfs.xSleep = bp_xSleep;
    g_bp_vfs.xCurrentTime = bp_xCurrentTime;

    int rc = sqlite3_vfs_register(&g_bp_vfs, 1);   /* makeDefault = 1 */
    if (rc == SQLITE_OK) g_bp_registered = 1;
    return rc;
}

/* begin/commit/rollback are LOGICAL markers only. In-place writes already hit
 * stable; the IC message boundary is the durability/atomicity point. We keep a
 * flag for symmetry and so a future assertion can check we never nest. */
int sql_vfs_begin_update(void) {
    if (bp_load_super() != SQLITE_OK) return SQLITE_IOERR;
    g_in_update = 1;
    return SQLITE_OK;
}

int sql_vfs_commit_update(void) {
    /* Writes are already in place. Flush any deferred db_size into the
     * superblock (idempotent; no-op if xSync already did). Durability comes from
     * the message succeeding. */
    int rc = bp_flush_super();
    g_in_update = 0;
    return rc;
}

void sql_vfs_rollback_update(void) {
    /* In-place writes cannot be undone here. Rollback within a message is the
     * caller's responsibility OR — for crash-style rollback — the message traps
     * and the substrate discards everything (the bulletproof guarantee). We
     * only clear the marker; a logical rollback that must survive should be
     * driven by SQLite's own savepoint/rollback over the in-place file. */
    g_in_update = 0;
}

/* sql_open_vfs / sql_tx_begin live in sqlite_bridge.c; they actually connect the
 * SQLite handle g_db to this VFS via sqlite3_open_v2(path, vfs_name). The append-only
 * open_read_write does the same — without this call, the Idris layer's g_db is never
 * bound to the VFS and all writes vanish. */
extern int sql_open_vfs(const char* path, const char* vfs_name, int read_only);
extern int sql_tx_begin(void);
extern int sql_tx_commit(void);
extern int sql_tx_rollback(void);
extern int sql_close(void);

int64_t sql_vfs_ffi_init(int64_t memory_id)        { return (int64_t)sql_vfs_register(memory_id); }
int64_t sql_vfs_ffi_open_read_write(void) {
    g_sb_loaded = 0;
    int rc = (int)bp_load_super();
    if (rc != SQLITE_OK) return rc;
    rc = sql_open_vfs(SQL_VFS_MAIN_DB, SQL_VFS_NAME, 0);   /* bind g_db to this VFS */
    if (rc != SQLITE_OK) return rc;
    return (int64_t)sql_tx_begin();
}
int64_t sql_vfs_ffi_open_read_only(void) {
    g_sb_loaded = 0;
    int rc = (int)bp_load_super();
    if (rc != SQLITE_OK) return rc;
    return (int64_t)sql_open_vfs(SQL_VFS_MAIN_DB, SQL_VFS_NAME, 1);
}
int64_t sql_vfs_ffi_begin_update(void)             { return (int64_t)sql_vfs_begin_update(); }
int64_t sql_vfs_ffi_commit_update(void) {
    int rc = sql_tx_commit();                 /* SQLite COMMIT → flushes pages via xWrite */
    if (rc != SQLITE_OK) { sql_tx_rollback(); sql_vfs_rollback_update(); return rc; }
    return (int64_t)sql_vfs_commit_update();   /* flush superblock (db_size) */
}
int64_t sql_vfs_ffi_rollback_update(void)          { sql_tx_rollback(); sql_vfs_rollback_update(); return 0; }
int64_t sql_vfs_ffi_close(void)                    { g_sb_loaded = 0; return (int64_t)sql_close(); }
int64_t sql_vfs_ffi_memory_id(void)                { return g_memory_id; }

int64_t sql_vfs_ffi_page_count(void) {
    if (bp_load_super() != SQLITE_OK) return 0;
    return (int64_t)((g_sb.db_size + BP_P - 1) / BP_P);
}

/* allocated_bytes = stable cap (high-water mark). For BulletproofVFS this is
 * ~ 2 superblock pages + db_size, in contrast to append-only's monotone growth. */
int64_t sql_vfs_ffi_allocated_bytes(void) {
    return (int64_t)(ic0_stable64_size() * BP_STABLE_PAGE);
}

/* active_bytes = logical bytes actually used = db_size. Under BulletproofVFS the
 * gap between allocated and active stays bounded (no dead append history). */
int64_t sql_vfs_ffi_active_bytes(void) {
    if (bp_load_super() != SQLITE_OK) return 0;
    return (int64_t)g_sb.db_size;
}

/* ------------------------------------------------------------------ */
/* SQLITE_OS_OTHER hooks                                               */
/* ------------------------------------------------------------------ */
/*
 * With SQLITE_OS_OTHER=1, SQLite drops its built-in OS backend and calls
 * sqlite3_os_init() once at sqlite3_initialize() to obtain a default VFS.
 * Register the VFS struct here with a provisional anchor (memory_id 0); the real
 * memory_id is set later by sql_vfs_ffi_init()/sql_vfs_register(), which only
 * re-points g_base — the registered sqlite3_vfs object is the same singleton.
 */
int sqlite3_os_init(void) {
    return sql_vfs_register(g_memory_id >= 0 ? g_memory_id : 0);
}

int sqlite3_os_end(void) {
    return SQLITE_OK;
}
