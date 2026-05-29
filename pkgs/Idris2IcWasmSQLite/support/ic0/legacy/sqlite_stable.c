/*
 * SQLite Stable Memory Persistence - Implementation
 */
#include "sqlite_stable.h"
#include "sqlite_bridge.h"
#include <string.h>

/* =============================================================================
 * IC0 Stable Memory Imports
 * ============================================================================= */

extern uint64_t ic0_stable64_size(void);
extern uint64_t ic0_stable64_grow(uint64_t new_pages);
extern void ic0_stable64_read(uint64_t dst, uint64_t offset, uint64_t size);
extern void ic0_stable64_write(uint64_t offset, uint64_t src, uint64_t size);
extern uint64_t ic0_time(void);
extern void ic0_debug_print(int32_t src, int32_t size);

/* =============================================================================
 * Helper Functions
 * ============================================================================= */

static void debug_msg(const char* msg) {
    ic0_debug_print((int32_t)(uintptr_t)msg, strlen(msg));
}

/* Simple checksum: sum of all bytes */
static uint64_t compute_checksum(const uint8_t* data, uint64_t len) {
    uint64_t sum = 0;
    for (uint64_t i = 0; i < len; i++) {
        sum += data[i];
    }
    return sum;
}

/* Ensure stable memory has enough pages */
static void ensure_stable_capacity(uint64_t needed_bytes) {
    uint64_t current_pages = ic0_stable64_size();
    uint64_t current_bytes = current_pages * 65536;  /* 64KB per page */
    if (needed_bytes > current_bytes) {
        uint64_t needed_pages = (needed_bytes + 65535) / 65536;
        ic0_stable64_grow(needed_pages - current_pages);
    }
}

/* Write little-endian uint32 */
static void write_u32(uint8_t* buf, uint32_t val) {
    buf[0] = (uint8_t)(val & 0xFF);
    buf[1] = (uint8_t)((val >> 8) & 0xFF);
    buf[2] = (uint8_t)((val >> 16) & 0xFF);
    buf[3] = (uint8_t)((val >> 24) & 0xFF);
}

/* Read little-endian uint32 */
static uint32_t read_u32(const uint8_t* buf) {
    return (uint32_t)buf[0] |
           ((uint32_t)buf[1] << 8) |
           ((uint32_t)buf[2] << 16) |
           ((uint32_t)buf[3] << 24);
}

/* Write little-endian uint64 */
static void write_u64(uint8_t* buf, uint64_t val) {
    for (int i = 0; i < 8; i++) {
        buf[i] = (uint8_t)((val >> (i * 8)) & 0xFF);
    }
}

/* Read little-endian uint64 */
static uint64_t read_u64(const uint8_t* buf) {
    uint64_t val = 0;
    for (int i = 0; i < 8; i++) {
        val |= ((uint64_t)buf[i]) << (i * 8);
    }
    return val;
}

/* =============================================================================
 * Global State
 * ============================================================================= */

/* Cached schema version from last load */
static uint32_t g_loaded_schema_version = 0;

/* =============================================================================
 * Core Functions
 * ============================================================================= */

int sqlite_stable_save(uint32_t schema_version, uint64_t timestamp) {
    debug_msg("sqlite_stable: saving to stable memory");

    /* Check if database is open */
    if (!sql_is_open()) {
        debug_msg("sqlite_stable: database not open");
        return 1;
    }

    /* Get serialized size */
    int64_t db_size = sql_serialize_size();
    if (db_size < 0) {
        debug_msg("sqlite_stable: failed to get serialize size");
        return 2;
    }

    if (db_size > SQLITE_STABLE_MAX_SIZE) {
        debug_msg("sqlite_stable: database too large");
        return 3;
    }

    /* Ensure stable memory capacity */
    uint64_t total_size = SQLITE_STABLE_HEADER_SIZE + (uint64_t)db_size;
    ensure_stable_capacity(SQLITE_STABLE_OFFSET + total_size);

    /* Allocate buffer for serialized database */
    static uint8_t serialize_buf[SQLITE_STABLE_MAX_SIZE];
    int64_t serialized = sql_serialize(serialize_buf, (int64_t)db_size);
    if (serialized != db_size) {
        debug_msg("sqlite_stable: serialize size mismatch");
        return 4;
    }

    /* Compute checksum */
    uint64_t checksum = compute_checksum(serialize_buf, (uint64_t)db_size);

    /* Build header */
    uint8_t header[SQLITE_STABLE_HEADER_SIZE];
    memset(header, 0, sizeof(header));
    memcpy(header, SQLITE_STABLE_MAGIC, SQLITE_STABLE_MAGIC_LEN);
    write_u32(header + 8, SQLITE_STABLE_VERSION);
    write_u32(header + 12, schema_version);
    write_u64(header + 16, (uint64_t)db_size);
    write_u64(header + 24, timestamp);
    write_u64(header + 32, checksum);

    /* Write header to stable memory */
    ic0_stable64_write(SQLITE_STABLE_OFFSET,
                       (uint64_t)(uintptr_t)header,
                       SQLITE_STABLE_HEADER_SIZE);

    /* Write serialized database to stable memory */
    ic0_stable64_write(SQLITE_STABLE_OFFSET + SQLITE_STABLE_HEADER_SIZE,
                       (uint64_t)(uintptr_t)serialize_buf,
                       (uint64_t)db_size);

    debug_msg("sqlite_stable: save complete");
    return 0;
}

int sqlite_stable_load(uint32_t* out_schema_version) {
    debug_msg("sqlite_stable: loading from stable memory");

    /* Check if stable memory has any pages */
    if (ic0_stable64_size() == 0) {
        debug_msg("sqlite_stable: no stable memory");
        return 1;
    }

    /* Read header */
    uint8_t header[SQLITE_STABLE_HEADER_SIZE];
    ic0_stable64_read((uint64_t)(uintptr_t)header,
                      SQLITE_STABLE_OFFSET,
                      SQLITE_STABLE_HEADER_SIZE);

    /* Verify magic */
    if (memcmp(header, SQLITE_STABLE_MAGIC, SQLITE_STABLE_MAGIC_LEN) != 0) {
        debug_msg("sqlite_stable: invalid magic");
        return 1;  /* No valid snapshot */
    }

    /* Parse header */
    uint32_t format_version = read_u32(header + 8);
    uint32_t schema_version = read_u32(header + 12);
    uint64_t snapshot_size = read_u64(header + 16);
    /* uint64_t timestamp = read_u64(header + 24); */
    uint64_t stored_checksum = read_u64(header + 32);

    /* Check format version */
    if (format_version != SQLITE_STABLE_VERSION) {
        debug_msg("sqlite_stable: unsupported format version");
        return 1;
    }

    /* Check size */
    if (snapshot_size > SQLITE_STABLE_MAX_SIZE) {
        debug_msg("sqlite_stable: snapshot too large");
        return 1;
    }

    /* Read serialized database */
    static uint8_t deserialize_buf[SQLITE_STABLE_MAX_SIZE];
    ic0_stable64_read((uint64_t)(uintptr_t)deserialize_buf,
                      SQLITE_STABLE_OFFSET + SQLITE_STABLE_HEADER_SIZE,
                      snapshot_size);

    /* Verify checksum */
    uint64_t computed_checksum = compute_checksum(deserialize_buf, snapshot_size);
    if (computed_checksum != stored_checksum) {
        debug_msg("sqlite_stable: checksum mismatch");
        return 2;
    }

    /* Deserialize database */
    int rc = sql_deserialize(deserialize_buf, (int64_t)snapshot_size);
    if (rc != 0) {
        debug_msg("sqlite_stable: deserialize failed");
        return 3;
    }

    /* Store schema version */
    g_loaded_schema_version = schema_version;
    if (out_schema_version != NULL) {
        *out_schema_version = schema_version;
    }

    debug_msg("sqlite_stable: load complete");
    return 0;
}

int sqlite_stable_has_snapshot(void) {
    if (ic0_stable64_size() == 0) {
        return 0;
    }

    uint8_t magic[SQLITE_STABLE_MAGIC_LEN];
    ic0_stable64_read((uint64_t)(uintptr_t)magic,
                      SQLITE_STABLE_OFFSET,
                      SQLITE_STABLE_MAGIC_LEN);

    return memcmp(magic, SQLITE_STABLE_MAGIC, SQLITE_STABLE_MAGIC_LEN) == 0 ? 1 : 0;
}

int sqlite_stable_get_info(uint32_t* schema_version, uint64_t* snapshot_size,
                           uint64_t* timestamp) {
    if (!sqlite_stable_has_snapshot()) {
        return 1;
    }

    uint8_t header[SQLITE_STABLE_HEADER_SIZE];
    ic0_stable64_read((uint64_t)(uintptr_t)header,
                      SQLITE_STABLE_OFFSET,
                      SQLITE_STABLE_HEADER_SIZE);

    if (schema_version) *schema_version = read_u32(header + 12);
    if (snapshot_size) *snapshot_size = read_u64(header + 16);
    if (timestamp) *timestamp = read_u64(header + 24);

    return 0;
}

void sqlite_stable_clear(void) {
    if (ic0_stable64_size() == 0) {
        return;
    }

    /* Write zeroed magic to invalidate */
    uint8_t zeros[SQLITE_STABLE_MAGIC_LEN] = {0};
    ic0_stable64_write(SQLITE_STABLE_OFFSET,
                       (uint64_t)(uintptr_t)zeros,
                       SQLITE_STABLE_MAGIC_LEN);
}

/* =============================================================================
 * Idris FFI Wrappers
 * ============================================================================= */

int64_t sqlite_stable_ffi_save(int64_t schema_version, int64_t timestamp) {
    return (int64_t)sqlite_stable_save((uint32_t)schema_version, (uint64_t)timestamp);
}

int64_t sqlite_stable_ffi_load(void) {
    return (int64_t)sqlite_stable_load(NULL);
}

int64_t sqlite_stable_ffi_get_schema_version(void) {
    return (int64_t)g_loaded_schema_version;
}

int64_t sqlite_stable_ffi_has_snapshot(void) {
    return (int64_t)sqlite_stable_has_snapshot();
}

void sqlite_stable_ffi_clear(void) {
    sqlite_stable_clear();
}
