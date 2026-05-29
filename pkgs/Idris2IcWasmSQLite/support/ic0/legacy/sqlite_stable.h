/*
 * SQLite Stable Memory Persistence
 *
 * Handles serialization/deserialization of SQLite database to/from
 * IC stable memory for canister upgrades.
 *
 * Stable Memory Layout:
 *   [0..7]   : Magic "IDXRSQL\0" (8 bytes)
 *   [8..11]  : Format version (uint32, little-endian)
 *   [12..15] : Schema version (uint32, little-endian)
 *   [16..23] : Snapshot size in bytes (uint64, little-endian)
 *   [24..31] : Timestamp in nanoseconds (uint64, little-endian)
 *   [32..39] : Checksum (uint64, simple sum)
 *   [40..]   : SQLite serialized database
 */
#ifndef SQLITE_STABLE_H
#define SQLITE_STABLE_H

#include <stdint.h>

/* =============================================================================
 * Constants
 * ============================================================================= */

#define SQLITE_STABLE_MAGIC      "IDXRSQL"
#define SQLITE_STABLE_MAGIC_LEN  8
#define SQLITE_STABLE_VERSION    1
#define SQLITE_STABLE_HEADER_SIZE 40

/* Stable memory starts at page 0 for SQLite storage */
/* ic-wasm profiling uses pages 10-25, so we use pages 0-9 (640KB header space) */
#define SQLITE_STABLE_OFFSET     0

/* Maximum database size: 100MB (fits in ~1600 pages) */
#define SQLITE_STABLE_MAX_SIZE   (100 * 1024 * 1024)

/* =============================================================================
 * Header Structure
 * ============================================================================= */

typedef struct {
    char magic[8];           /* "IDXRSQL\0" */
    uint32_t format_version; /* Currently 1 */
    uint32_t schema_version; /* Application schema version */
    uint64_t snapshot_size;  /* Size of serialized DB in bytes */
    uint64_t timestamp;      /* Snapshot timestamp (nanoseconds) */
    uint64_t checksum;       /* Simple checksum for integrity */
} SqliteStableHeader;

/* =============================================================================
 * Core Functions
 * ============================================================================= */

/*
 * Save SQLite database to stable memory
 * Call during pre_upgrade
 *
 * @schema_version  Current schema version
 * @timestamp       Current time in nanoseconds
 * Returns: 0 on success, error code on failure
 */
int sqlite_stable_save(uint32_t schema_version, uint64_t timestamp);

/*
 * Load SQLite database from stable memory
 * Call during post_upgrade or canister_init (if data exists)
 *
 * @out_schema_version  Receives the stored schema version
 * Returns: 0 on success, error code on failure
 *          1 = no valid snapshot found (fresh start)
 *          2 = checksum mismatch
 *          3 = deserialize failed
 */
int sqlite_stable_load(uint32_t* out_schema_version);

/*
 * Check if valid snapshot exists in stable memory
 * Returns: 1 if valid snapshot exists, 0 otherwise
 */
int sqlite_stable_has_snapshot(void);

/*
 * Get snapshot metadata without loading
 * Returns: 0 on success, 1 if no snapshot
 */
int sqlite_stable_get_info(uint32_t* schema_version, uint64_t* snapshot_size,
                           uint64_t* timestamp);

/*
 * Clear snapshot from stable memory
 * (Writes zeroed magic to invalidate)
 */
void sqlite_stable_clear(void);

/* =============================================================================
 * Idris FFI Wrappers
 * ============================================================================= */

int64_t sqlite_stable_ffi_save(int64_t schema_version, int64_t timestamp);
int64_t sqlite_stable_ffi_load(void);
int64_t sqlite_stable_ffi_get_schema_version(void);
int64_t sqlite_stable_ffi_has_snapshot(void);
void sqlite_stable_ffi_clear(void);

#endif /* SQLITE_STABLE_H */
