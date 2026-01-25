/*
 * IC FFI Bridge - Generic C ↔ Idris2 Communication
 *
 * This provides the fundamental bridge for C entry points to communicate
 * with Idris2 code compiled via RefC backend.
 *
 * Usage pattern:
 *   C side (canister_entry.c):
 *     ic_ffi_reset();
 *     ic_ffi_c_set_arg(0, cmd);
 *     __mainExpression_0();  // Call Idris2
 *     result = ic_ffi_c_get_result();
 *
 *   Idris2 side (Main.idr):
 *     %foreign "C:ic_ffi_get_arg,libic0"
 *     getArg : Int -> Int
 *
 *     %foreign "C:ic_ffi_set_result,libic0"
 *     setResult : Int -> PrimIO ()
 */
#include <stdint.h>
#include <string.h>

/* =============================================================================
 * FFI Bridge: Argument/Result Passing
 * ============================================================================= */

/* Up to 8 int64 arguments (Idris2 Int is 64-bit) */
#define IC_FFI_MAX_ARGS 8
static int64_t ic_ffi_args[IC_FFI_MAX_ARGS] = {0};
static int64_t ic_ffi_result = 0;

/* Called from Idris2 via %foreign to get argument */
int64_t ic_ffi_get_arg(int64_t index) {
    if (index >= 0 && index < IC_FFI_MAX_ARGS) {
        return ic_ffi_args[index];
    }
    return 0;
}

/* Called from Idris2 via %foreign to set result */
void ic_ffi_set_result(int64_t value) {
    ic_ffi_result = value;
}

/* Called from C to set argument for Idris2 */
void ic_ffi_c_set_arg(int32_t index, int64_t value) {
    if (index >= 0 && index < IC_FFI_MAX_ARGS) {
        ic_ffi_args[index] = value;
    }
}

/* Called from C to get result from Idris2 */
int64_t ic_ffi_c_get_result(void) {
    return ic_ffi_result;
}

/* Reset communication state between calls */
void ic_ffi_reset(void) {
    ic_ffi_result = 0;
    for (int i = 0; i < IC_FFI_MAX_ARGS; i++) {
        ic_ffi_args[i] = 0;
    }
}

/* =============================================================================
 * Candid Buffer: Idris2 writes Candid bytes for C to send as reply
 * ============================================================================= */

#define IC_CANDID_BUF_SIZE 4096
static uint8_t ic_candid_buf[IC_CANDID_BUF_SIZE];
static int32_t ic_candid_len = 0;

/* Called from Idris2 to write a byte to Candid buffer */
void ic_candid_write_byte(int64_t index, int64_t byte) {
    if (index >= 0 && index < IC_CANDID_BUF_SIZE) {
        ic_candid_buf[index] = (uint8_t)byte;
    }
}

/* Called from Idris2 to set Candid buffer length */
void ic_candid_set_len(int64_t len) {
    if (len >= 0 && len <= IC_CANDID_BUF_SIZE) {
        ic_candid_len = (int32_t)len;
    }
}

/* Called from Idris2 to clear Candid buffer */
void ic_candid_clear(void) {
    ic_candid_len = 0;
    memset(ic_candid_buf, 0, IC_CANDID_BUF_SIZE);
}

/* Called from C to get Candid buffer pointer */
uint8_t* ic_candid_c_get_buf(void) {
    return ic_candid_buf;
}

/* Called from C to get Candid buffer length */
int32_t ic_candid_c_get_len(void) {
    return ic_candid_len;
}

/* =============================================================================
 * JSON Buffer: C sets JSON for Idris2 to parse
 * ============================================================================= */

#define IC_JSON_BUF_SIZE 4096
static char ic_json_buf[IC_JSON_BUF_SIZE];
static int32_t ic_json_len = 0;

/* Called from C to set JSON string for Idris2 */
void ic_json_c_set(const char* json) {
    if (json) {
        ic_json_len = strlen(json);
        if (ic_json_len >= IC_JSON_BUF_SIZE) {
            ic_json_len = IC_JSON_BUF_SIZE - 1;
        }
        memcpy(ic_json_buf, json, ic_json_len);
        ic_json_buf[ic_json_len] = '\0';
    } else {
        ic_json_len = 0;
        ic_json_buf[0] = '\0';
    }
}

/* Called from Idris2 to get JSON buffer length */
int64_t ic_json_get_len(void) {
    return (int64_t)ic_json_len;
}

/* Called from Idris2 to get byte at index */
int64_t ic_json_get_byte(int64_t index) {
    if (index >= 0 && index < ic_json_len) {
        return (int64_t)(uint8_t)ic_json_buf[index];
    }
    return 0;
}

/* =============================================================================
 * String Buffer: Idris2 writes string for C to read (e.g., for debug)
 * ============================================================================= */

#define IC_STR_BUF_SIZE 1024
static char ic_str_buf[IC_STR_BUF_SIZE];
static int32_t ic_str_len = 0;

/* Called from Idris2 to write a byte to string buffer */
void ic_str_write_byte(int64_t index, int64_t byte) {
    if (index >= 0 && index < IC_STR_BUF_SIZE - 1) {
        ic_str_buf[index] = (char)byte;
    }
}

/* Called from Idris2 to set string length */
void ic_str_set_len(int64_t len) {
    if (len >= 0 && len < IC_STR_BUF_SIZE) {
        ic_str_len = (int32_t)len;
        ic_str_buf[ic_str_len] = '\0';
    }
}

/* Called from C to get string buffer */
const char* ic_str_c_get(void) {
    return ic_str_buf;
}

/* Called from C to get string length */
int32_t ic_str_c_get_len(void) {
    return ic_str_len;
}

/* =============================================================================
 * Stable Memory Helpers (high-level wrappers for Idris2)
 *
 * These provide convenient Int32/Int64 read/write without pointer manipulation.
 * The underlying ic0_stable_* functions are in ic0_stubs.c
 * ============================================================================= */

/* Extern declarations for ic0_stable_* (defined in ic0_stubs.c) */
extern void ic0_stable_read(int32_t dst, int32_t offset, int32_t size);
extern void ic0_stable_write(int32_t offset, int32_t src, int32_t size);
extern int32_t ic0_stable_size(void);
extern int32_t ic0_stable_grow(int32_t new_pages);

/* Temporary buffer for stable memory operations */
static uint8_t ic_stable_tmp[8];

/* Write Int64 to stable memory at offset */
void ic_stable_write_i64(int64_t offset, int64_t value) {
    /* Ensure we have enough stable memory */
    int32_t needed_pages = ((int32_t)offset + 8 + 65535) / 65536;
    int32_t current_pages = ic0_stable_size();
    if (needed_pages > current_pages) {
        ic0_stable_grow(needed_pages - current_pages);
    }

    /* Write value to temp buffer (little-endian) */
    for (int i = 0; i < 8; i++) {
        ic_stable_tmp[i] = (uint8_t)(value >> (i * 8));
    }

    /* Write to stable memory */
    ic0_stable_write((int32_t)offset, (int32_t)(uintptr_t)ic_stable_tmp, 8);
}

/* Read Int64 from stable memory at offset */
int64_t ic_stable_read_i64(int64_t offset) {
    /* Read from stable memory */
    ic0_stable_read((int32_t)(uintptr_t)ic_stable_tmp, (int32_t)offset, 8);

    /* Reconstruct value (little-endian) */
    int64_t value = 0;
    for (int i = 0; i < 8; i++) {
        value |= ((int64_t)ic_stable_tmp[i]) << (i * 8);
    }
    return value;
}

/* Write Int32 to stable memory at offset */
void ic_stable_write_i32(int64_t offset, int64_t value) {
    /* Ensure we have enough stable memory */
    int32_t needed_pages = ((int32_t)offset + 4 + 65535) / 65536;
    int32_t current_pages = ic0_stable_size();
    if (needed_pages > current_pages) {
        ic0_stable_grow(needed_pages - current_pages);
    }

    /* Write value to temp buffer (little-endian) */
    int32_t val32 = (int32_t)value;
    for (int i = 0; i < 4; i++) {
        ic_stable_tmp[i] = (uint8_t)(val32 >> (i * 8));
    }

    /* Write to stable memory */
    ic0_stable_write((int32_t)offset, (int32_t)(uintptr_t)ic_stable_tmp, 4);
}

/* Read Int32 from stable memory at offset */
int64_t ic_stable_read_i32(int64_t offset) {
    /* Read from stable memory */
    ic0_stable_read((int32_t)(uintptr_t)ic_stable_tmp, (int32_t)offset, 4);

    /* Reconstruct value (little-endian) */
    int32_t value = 0;
    for (int i = 0; i < 4; i++) {
        value |= ((int32_t)ic_stable_tmp[i]) << (i * 8);
    }
    return (int64_t)value;
}
