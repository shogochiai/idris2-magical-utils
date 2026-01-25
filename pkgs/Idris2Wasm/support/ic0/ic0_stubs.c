/*
 * IC0 System API Stubs for idris2-cdk
 *
 * This file bridges idris2-cdk's %foreign "C:ic0_*,libic0" declarations
 * with actual WASM imports from the IC runtime.
 *
 * idris2-cdk expects: ic0_msg_reply() (C function)
 * IC runtime provides: ic0.msg_reply (WASM import)
 *
 * These stubs provide the C functions that wrap the WASM imports.
 */
#include <stdint.h>

/* =============================================================================
 * WASM Imports from IC Runtime
 * ============================================================================= */

/* Message reply */
extern void ic0_msg_reply_impl(void)
    __attribute__((import_module("ic0"), import_name("msg_reply")));
extern void ic0_msg_reply_data_append_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("msg_reply_data_append")));

/* Message arguments */
extern uint32_t ic0_msg_arg_data_size_impl(void)
    __attribute__((import_module("ic0"), import_name("msg_arg_data_size")));
extern void ic0_msg_arg_data_copy_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("msg_arg_data_copy")));

/* Caller information */
extern uint32_t ic0_msg_caller_size_impl(void)
    __attribute__((import_module("ic0"), import_name("msg_caller_size")));
extern void ic0_msg_caller_copy_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("msg_caller_copy")));

/* Message rejection */
extern void ic0_msg_reject_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("msg_reject")));
extern uint32_t ic0_msg_reject_code_impl(void)
    __attribute__((import_module("ic0"), import_name("msg_reject_code")));
extern uint32_t ic0_msg_reject_msg_size_impl(void)
    __attribute__((import_module("ic0"), import_name("msg_reject_msg_size")));
extern void ic0_msg_reject_msg_copy_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("msg_reject_msg_copy")));

/* Canister information */
extern uint32_t ic0_canister_self_size_impl(void)
    __attribute__((import_module("ic0"), import_name("canister_self_size")));
extern void ic0_canister_self_copy_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("canister_self_copy")));
extern void ic0_canister_cycle_balance128_impl(uint32_t dst)
    __attribute__((import_module("ic0"), import_name("canister_cycle_balance128")));
extern uint32_t ic0_canister_status_impl(void)
    __attribute__((import_module("ic0"), import_name("canister_status")));

/* Time */
extern uint64_t ic0_time_impl(void)
    __attribute__((import_module("ic0"), import_name("time")));

/* Stable memory */
extern uint32_t ic0_stable_size_impl(void)
    __attribute__((import_module("ic0"), import_name("stable_size")));
extern uint32_t ic0_stable_grow_impl(uint32_t new_pages)
    __attribute__((import_module("ic0"), import_name("stable_grow")));
extern void ic0_stable_read_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("stable_read")));
extern void ic0_stable_write_impl(uint32_t offset, uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("stable_write")));
extern uint64_t ic0_stable64_size_impl(void)
    __attribute__((import_module("ic0"), import_name("stable64_size")));
extern uint64_t ic0_stable64_grow_impl(uint64_t new_pages)
    __attribute__((import_module("ic0"), import_name("stable64_grow")));
extern void ic0_stable64_read_impl(uint64_t dst, uint64_t offset, uint64_t size)
    __attribute__((import_module("ic0"), import_name("stable64_read")));
extern void ic0_stable64_write_impl(uint64_t offset, uint64_t src, uint64_t size)
    __attribute__((import_module("ic0"), import_name("stable64_write")));

/* Certified data */
extern void ic0_certified_data_set_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("certified_data_set")));
extern uint32_t ic0_data_certificate_size_impl(void)
    __attribute__((import_module("ic0"), import_name("data_certificate_size")));
extern void ic0_data_certificate_copy_impl(uint32_t dst, uint32_t offset, uint32_t size)
    __attribute__((import_module("ic0"), import_name("data_certificate_copy")));

/* Inter-canister calls */
extern void ic0_call_new_impl(uint32_t callee_src, uint32_t callee_size,
                               uint32_t name_src, uint32_t name_size,
                               uint32_t reply_fun, uint32_t reply_env,
                               uint32_t reject_fun, uint32_t reject_env)
    __attribute__((import_module("ic0"), import_name("call_new")));
extern void ic0_call_data_append_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("call_data_append")));
extern void ic0_call_cycles_add128_impl(uint64_t high, uint64_t low)
    __attribute__((import_module("ic0"), import_name("call_cycles_add128")));
extern uint32_t ic0_call_perform_impl(void)
    __attribute__((import_module("ic0"), import_name("call_perform")));

/* Cycles */
extern void ic0_msg_cycles_available128_impl(uint32_t dst)
    __attribute__((import_module("ic0"), import_name("msg_cycles_available128")));
extern void ic0_msg_cycles_accept128_impl(uint64_t max_high, uint64_t max_low, uint32_t dst)
    __attribute__((import_module("ic0"), import_name("msg_cycles_accept128")));
extern void ic0_msg_cycles_refunded128_impl(uint32_t dst)
    __attribute__((import_module("ic0"), import_name("msg_cycles_refunded128")));

/* Debugging */
extern void ic0_debug_print_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("debug_print")));
extern void ic0_trap_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("trap")));

/* Performance & timers */
extern uint64_t ic0_performance_counter_impl(uint32_t type)
    __attribute__((import_module("ic0"), import_name("performance_counter")));
extern uint64_t ic0_global_timer_set_impl(uint64_t timestamp)
    __attribute__((import_module("ic0"), import_name("global_timer_set")));
extern uint64_t ic0_instruction_counter_impl(void)
    __attribute__((import_module("ic0"), import_name("instruction_counter")));
extern uint32_t ic0_is_controller_impl(uint32_t src, uint32_t size)
    __attribute__((import_module("ic0"), import_name("is_controller")));

/* =============================================================================
 * C Stubs for idris2-cdk FFI
 * These match the %foreign "C:ic0_*,libic0" declarations in ICP.IC0
 * ============================================================================= */

/* Message reply */
void ic0_msg_reply(void) { ic0_msg_reply_impl(); }
void ic0_msg_reply_data_append(int32_t src, int32_t size) {
    ic0_msg_reply_data_append_impl((uint32_t)src, (uint32_t)size);
}

/* Message arguments */
int32_t ic0_msg_arg_data_size(void) { return (int32_t)ic0_msg_arg_data_size_impl(); }
void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size) {
    ic0_msg_arg_data_copy_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}

/* Caller information */
int32_t ic0_msg_caller_size(void) { return (int32_t)ic0_msg_caller_size_impl(); }
void ic0_msg_caller_copy(int32_t dst, int32_t offset, int32_t size) {
    ic0_msg_caller_copy_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}

/* Message rejection */
void ic0_msg_reject(int32_t src, int32_t size) {
    ic0_msg_reject_impl((uint32_t)src, (uint32_t)size);
}
int32_t ic0_msg_reject_code(void) { return (int32_t)ic0_msg_reject_code_impl(); }
int32_t ic0_msg_reject_msg_size(void) { return (int32_t)ic0_msg_reject_msg_size_impl(); }
void ic0_msg_reject_msg_copy(int32_t dst, int32_t offset, int32_t size) {
    ic0_msg_reject_msg_copy_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}

/* Canister information */
int32_t ic0_canister_self_size(void) { return (int32_t)ic0_canister_self_size_impl(); }
void ic0_canister_self_copy(int32_t dst, int32_t offset, int32_t size) {
    ic0_canister_self_copy_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}
void ic0_canister_cycle_balance128(int32_t dst) {
    ic0_canister_cycle_balance128_impl((uint32_t)dst);
}
int32_t ic0_canister_status(void) { return (int32_t)ic0_canister_status_impl(); }

/* Time */
uint64_t ic0_time(void) { return ic0_time_impl(); }

/* Stable memory */
int32_t ic0_stable_size(void) { return (int32_t)ic0_stable_size_impl(); }
int32_t ic0_stable_grow(int32_t new_pages) {
    return (int32_t)ic0_stable_grow_impl((uint32_t)new_pages);
}
void ic0_stable_read(int32_t dst, int32_t offset, int32_t size) {
    ic0_stable_read_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}
void ic0_stable_write(int32_t offset, int32_t src, int32_t size) {
    ic0_stable_write_impl((uint32_t)offset, (uint32_t)src, (uint32_t)size);
}
uint64_t ic0_stable64_size(void) { return ic0_stable64_size_impl(); }
uint64_t ic0_stable64_grow(uint64_t new_pages) { return ic0_stable64_grow_impl(new_pages); }
void ic0_stable64_read(uint64_t dst, uint64_t offset, uint64_t size) {
    ic0_stable64_read_impl(dst, offset, size);
}
void ic0_stable64_write(uint64_t offset, uint64_t src, uint64_t size) {
    ic0_stable64_write_impl(offset, src, size);
}

/* Certified data */
void ic0_certified_data_set(int32_t src, int32_t size) {
    ic0_certified_data_set_impl((uint32_t)src, (uint32_t)size);
}
int32_t ic0_data_certificate_size(void) { return (int32_t)ic0_data_certificate_size_impl(); }
void ic0_data_certificate_copy(int32_t dst, int32_t offset, int32_t size) {
    ic0_data_certificate_copy_impl((uint32_t)dst, (uint32_t)offset, (uint32_t)size);
}

/* Inter-canister calls */
void ic0_call_new(int32_t callee_src, int32_t callee_size,
                  int32_t name_src, int32_t name_size,
                  int32_t reply_fun, int32_t reply_env,
                  int32_t reject_fun, int32_t reject_env) {
    ic0_call_new_impl((uint32_t)callee_src, (uint32_t)callee_size,
                      (uint32_t)name_src, (uint32_t)name_size,
                      (uint32_t)reply_fun, (uint32_t)reply_env,
                      (uint32_t)reject_fun, (uint32_t)reject_env);
}
void ic0_call_data_append(int32_t src, int32_t size) {
    ic0_call_data_append_impl((uint32_t)src, (uint32_t)size);
}
void ic0_call_cycles_add128(uint64_t high, uint64_t low) {
    ic0_call_cycles_add128_impl(high, low);
}
int32_t ic0_call_perform(void) { return (int32_t)ic0_call_perform_impl(); }

/* Cycles */
void ic0_msg_cycles_available128(int32_t dst) {
    ic0_msg_cycles_available128_impl((uint32_t)dst);
}
void ic0_msg_cycles_accept128(uint64_t max_high, uint64_t max_low, int32_t dst) {
    ic0_msg_cycles_accept128_impl(max_high, max_low, (uint32_t)dst);
}
void ic0_msg_cycles_refunded128(int32_t dst) {
    ic0_msg_cycles_refunded128_impl((uint32_t)dst);
}

/* Debugging */
void ic0_debug_print(int32_t src, int32_t size) {
    ic0_debug_print_impl((uint32_t)src, (uint32_t)size);
}
void ic0_trap(int32_t src, int32_t size) {
    ic0_trap_impl((uint32_t)src, (uint32_t)size);
}

/* Performance & timers */
uint64_t ic0_performance_counter(int32_t type) {
    return ic0_performance_counter_impl((uint32_t)type);
}
uint64_t ic0_global_timer_set(uint64_t timestamp) {
    return ic0_global_timer_set_impl(timestamp);
}
uint64_t ic0_instruction_counter(void) { return ic0_instruction_counter_impl(); }
int32_t ic0_is_controller(int32_t src, int32_t size) {
    return (int32_t)ic0_is_controller_impl((uint32_t)src, (uint32_t)size);
}

/* =============================================================================
 * OUC FFI Bridge
 * These enable communication between C (canister_entry.c) and Idris2 (Main.idr)
 * Note: Idris2 Int is 64-bit, so we accept int64_t and truncate/extend as needed
 * ============================================================================= */

/* Global variables for C<->Idris2 communication */
static int32_t ouc_result_i32 = 0;
static int32_t ouc_arg_i32[8] = {0};  /* Up to 8 int32 args */
static int32_t ouc_state_initialized = 0;  /* Persistent state flag */
static int32_t ouc_auditor_count = 0;  /* Persistent auditor count */

/* Called from Idris2 via %foreign to set result (Idris2 Int -> int64_t) */
void ouc_set_result_i32(int64_t value) {
    ouc_result_i32 = (int32_t)value;
}

/* Called from Idris2 via %foreign to get argument (returns Idris2 Int) */
int64_t ouc_get_arg_i32(int64_t index) {
    int64_t result = 0;
    if (index >= 0 && index < 8) {
        result = (int64_t)ouc_arg_i32[(int32_t)index];
    }
    /* Debug: log the argument being read */
    char buf[64];
    int len = 0;
    buf[len++] = 'g'; buf[len++] = 'e'; buf[len++] = 't';
    buf[len++] = '['; buf[len++] = '0' + (char)index; buf[len++] = ']';
    buf[len++] = '=';
    if (result < 0) { buf[len++] = '-'; result = -result; }
    if (result >= 100) buf[len++] = '0' + (char)((result / 100) % 10);
    if (result >= 10) buf[len++] = '0' + (char)((result / 10) % 10);
    buf[len++] = '0' + (char)(result % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (index >= 0 && index < 8) ? (int64_t)ouc_arg_i32[(int32_t)index] : 0;
}

/* Called from C to set argument for Idris2 */
void ouc_c_set_arg_i32(int32_t index, int32_t value) {
    if (index >= 0 && index < 8) {
        ouc_arg_i32[index] = value;
    }
}

/* Called from C to get result from Idris2 */
int32_t ouc_c_get_result_i32(void) {
    return ouc_result_i32;
}

/* Reset communication state */
void ouc_reset_ffi(void) {
    ouc_result_i32 = 0;
    for (int i = 0; i < 8; i++) {
        ouc_arg_i32[i] = 0;
    }
}

/* State initialization flag (persistent across calls) */
void ouc_set_state_initialized(int64_t value) {
    ouc_state_initialized = (int32_t)value;
}

int64_t ouc_get_state_initialized(void) {
    /* Debug: log the state check */
    char buf[32] = "state=";
    int len = 6;
    int32_t val = ouc_state_initialized;
    if (val < 0) { buf[len++] = '-'; val = -val; }
    buf[len++] = '0' + (char)(val % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (int64_t)ouc_state_initialized;
}

/* Proposal count (persistent across calls) */
static int32_t ouc_proposal_count = 0;  /* Persistent proposal count */

int64_t ouc_get_proposal_count(void) {
    char buf[32] = "propcnt=";
    int len = 8;
    int32_t val = ouc_proposal_count;
    if (val >= 10) buf[len++] = '0' + (char)((val / 10) % 10);
    buf[len++] = '0' + (char)(val % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (int64_t)ouc_proposal_count;
}

int64_t ouc_inc_proposal_count(void) {
    int32_t new_id = ouc_proposal_count;  /* ID is current count (0-indexed) */
    ouc_proposal_count++;
    char buf[32] = "inc->propcnt=";
    int len = 13;
    int32_t val = ouc_proposal_count;
    if (val >= 10) buf[len++] = '0' + (char)((val / 10) % 10);
    buf[len++] = '0' + (char)(val % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (int64_t)new_id;  /* Return the ID of the new proposal */
}

/* Auditor count (persistent across calls) */
void ouc_set_auditor_count(int64_t value) {
    ouc_auditor_count = (int32_t)value;
}

int64_t ouc_get_auditor_count(void) {
    /* Debug: log the count being read */
    char buf[32] = "audcnt=";
    int len = 7;
    int32_t val = ouc_auditor_count;
    if (val < 0) { buf[len++] = '-'; val = -val; }
    if (val >= 10) buf[len++] = '0' + (char)((val / 10) % 10);
    buf[len++] = '0' + (char)(val % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (int64_t)ouc_auditor_count;
}

int64_t ouc_inc_auditor_count(void) {
    ouc_auditor_count++;
    /* Debug: log the increment */
    char buf[32] = "inc->audcnt=";
    int len = 12;
    int32_t val = ouc_auditor_count;
    if (val < 0) { buf[len++] = '-'; val = -val; }
    if (val >= 10) buf[len++] = '0' + (char)((val / 10) % 10);
    buf[len++] = '0' + (char)(val % 10);
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    return (int64_t)ouc_auditor_count;  /* Return new count to prevent optimization */
}

/* =============================================================================
 * Stable KV Storage (Phase 1 for idris2-icp-indexer)
 *
 * Simple key-value storage backed by stable memory.
 * Layout:
 *   [0..3]   : Magic "STKV" (4 bytes)
 *   [4..7]   : Entry count (4 bytes, little-endian)
 *   [8..11]  : Next free offset (4 bytes)
 *   [12..]   : Entries [key_len:4, key:key_len, val_len:4, val:val_len]
 *
 * This is a simple linear scan implementation. For production, use BTree.
 * ============================================================================= */

#define STKV_MAGIC_OFFSET 0
#define STKV_COUNT_OFFSET 4
#define STKV_NEXT_OFFSET  8
#define STKV_DATA_START   12
#define STKV_PAGE_SIZE    65536  /* 64KB per page */

/* In-memory cache for header (avoids repeated stable reads) */
static uint32_t stkv_count = 0;
static uint32_t stkv_next_free = STKV_DATA_START;
static int stkv_initialized = 0;

/* Ensure stable memory has enough pages */
static void stkv_ensure_capacity(uint32_t needed_bytes) {
    uint32_t current_pages = ic0_stable_size_impl();
    uint32_t current_bytes = current_pages * STKV_PAGE_SIZE;
    if (needed_bytes > current_bytes) {
        uint32_t needed_pages = (needed_bytes + STKV_PAGE_SIZE - 1) / STKV_PAGE_SIZE;
        ic0_stable_grow_impl(needed_pages - current_pages);
    }
}

/* Initialize KV storage if needed */
static void stkv_init_if_needed(void) {
    if (stkv_initialized) return;

    /* Check if stable memory has valid magic */
    uint32_t current_pages = ic0_stable_size_impl();
    if (current_pages == 0) {
        /* First time: allocate 1 page and write header */
        ic0_stable_grow_impl(1);
        uint8_t header[12] = {
            'S', 'T', 'K', 'V',    /* Magic */
            0, 0, 0, 0,            /* Count = 0 */
            12, 0, 0, 0            /* Next free = 12 */
        };
        ic0_stable_write_impl(0, (uint32_t)(uintptr_t)header, 12);
        stkv_count = 0;
        stkv_next_free = STKV_DATA_START;
    } else {
        /* Read existing header */
        uint8_t header[12];
        ic0_stable_read_impl((uint32_t)(uintptr_t)header, 0, 12);
        if (header[0] == 'S' && header[1] == 'T' &&
            header[2] == 'K' && header[3] == 'V') {
            stkv_count = header[4] | (header[5] << 8) |
                        (header[6] << 16) | (header[7] << 24);
            stkv_next_free = header[8] | (header[9] << 8) |
                            (header[10] << 16) | (header[11] << 24);
        } else {
            /* Invalid magic, reinitialize */
            uint8_t init_header[12] = {
                'S', 'T', 'K', 'V',
                0, 0, 0, 0,
                12, 0, 0, 0
            };
            ic0_stable_write_impl(0, (uint32_t)(uintptr_t)init_header, 12);
            stkv_count = 0;
            stkv_next_free = STKV_DATA_START;
        }
    }
    stkv_initialized = 1;
}

/* Flush header to stable memory */
static void stkv_flush_header(void) {
    uint8_t header[12] = {
        'S', 'T', 'K', 'V',
        (uint8_t)(stkv_count & 0xFF),
        (uint8_t)((stkv_count >> 8) & 0xFF),
        (uint8_t)((stkv_count >> 16) & 0xFF),
        (uint8_t)((stkv_count >> 24) & 0xFF),
        (uint8_t)(stkv_next_free & 0xFF),
        (uint8_t)((stkv_next_free >> 8) & 0xFF),
        (uint8_t)((stkv_next_free >> 16) & 0xFF),
        (uint8_t)((stkv_next_free >> 24) & 0xFF)
    };
    ic0_stable_write_impl(0, (uint32_t)(uintptr_t)header, 12);
}

/* Compare memory regions */
static int mem_eq(const uint8_t* a, const uint8_t* b, uint32_t len) {
    for (uint32_t i = 0; i < len; i++) {
        if (a[i] != b[i]) return 0;
    }
    return 1;
}

/*
 * Put key-value pair
 * key_ptr: pointer to key bytes
 * key_len: length of key
 * val_ptr: pointer to value bytes
 * val_len: length of value
 * Returns: 0 on success, -1 on error
 */
int64_t stkv_put(int64_t key_ptr, int64_t key_len, int64_t val_ptr, int64_t val_len) {
    stkv_init_if_needed();

    uint32_t k_len = (uint32_t)key_len;
    uint32_t v_len = (uint32_t)val_len;
    uint8_t* key = (uint8_t*)(uintptr_t)key_ptr;
    uint8_t* val = (uint8_t*)(uintptr_t)val_ptr;

    /* Search for existing key (linear scan) */
    uint32_t offset = STKV_DATA_START;
    uint8_t len_buf[4];
    uint8_t key_buf[256];  /* Max key size */

    for (uint32_t i = 0; i < stkv_count && offset < stkv_next_free; i++) {
        /* Read key length */
        ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset, 4);
        uint32_t stored_key_len = len_buf[0] | (len_buf[1] << 8) |
                                  (len_buf[2] << 16) | (len_buf[3] << 24);

        if (stored_key_len == k_len && stored_key_len <= 256) {
            /* Read key and compare */
            ic0_stable_read_impl((uint32_t)(uintptr_t)key_buf, offset + 4, stored_key_len);
            if (mem_eq(key, key_buf, k_len)) {
                /* Key found - read value length and update in place if same size */
                ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset + 4 + stored_key_len, 4);
                uint32_t stored_val_len = len_buf[0] | (len_buf[1] << 8) |
                                          (len_buf[2] << 16) | (len_buf[3] << 24);
                if (stored_val_len == v_len) {
                    /* Update value in place */
                    ic0_stable_write_impl(offset + 4 + stored_key_len + 4,
                                          (uint32_t)(uintptr_t)val, v_len);
                    return 0;
                }
                /* Different size - for simplicity, append new (wastes space) */
                break;
            }
        }

        /* Skip to next entry */
        ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset + 4 + stored_key_len, 4);
        uint32_t stored_val_len = len_buf[0] | (len_buf[1] << 8) |
                                  (len_buf[2] << 16) | (len_buf[3] << 24);
        offset += 4 + stored_key_len + 4 + stored_val_len;
    }

    /* Append new entry */
    uint32_t entry_size = 4 + k_len + 4 + v_len;
    stkv_ensure_capacity(stkv_next_free + entry_size);

    /* Write key length */
    uint8_t entry_header[4] = {
        (uint8_t)(k_len & 0xFF),
        (uint8_t)((k_len >> 8) & 0xFF),
        (uint8_t)((k_len >> 16) & 0xFF),
        (uint8_t)((k_len >> 24) & 0xFF)
    };
    ic0_stable_write_impl(stkv_next_free, (uint32_t)(uintptr_t)entry_header, 4);

    /* Write key */
    ic0_stable_write_impl(stkv_next_free + 4, (uint32_t)(uintptr_t)key, k_len);

    /* Write value length */
    uint8_t val_header[4] = {
        (uint8_t)(v_len & 0xFF),
        (uint8_t)((v_len >> 8) & 0xFF),
        (uint8_t)((v_len >> 16) & 0xFF),
        (uint8_t)((v_len >> 24) & 0xFF)
    };
    ic0_stable_write_impl(stkv_next_free + 4 + k_len, (uint32_t)(uintptr_t)val_header, 4);

    /* Write value */
    ic0_stable_write_impl(stkv_next_free + 4 + k_len + 4, (uint32_t)(uintptr_t)val, v_len);

    stkv_next_free += entry_size;
    stkv_count++;
    stkv_flush_header();

    return 0;
}

/*
 * Get value by key
 * key_ptr: pointer to key bytes
 * key_len: length of key
 * val_ptr: pointer to buffer for value
 * max_val_len: maximum bytes to copy
 * Returns: actual value length, or -1 if not found
 */
int64_t stkv_get(int64_t key_ptr, int64_t key_len, int64_t val_ptr, int64_t max_val_len) {
    stkv_init_if_needed();

    uint32_t k_len = (uint32_t)key_len;
    uint8_t* key = (uint8_t*)(uintptr_t)key_ptr;
    uint8_t* val = (uint8_t*)(uintptr_t)val_ptr;
    uint32_t max_len = (uint32_t)max_val_len;

    /* Linear scan */
    uint32_t offset = STKV_DATA_START;
    uint8_t len_buf[4];
    uint8_t key_buf[256];

    for (uint32_t i = 0; i < stkv_count && offset < stkv_next_free; i++) {
        /* Read key length */
        ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset, 4);
        uint32_t stored_key_len = len_buf[0] | (len_buf[1] << 8) |
                                  (len_buf[2] << 16) | (len_buf[3] << 24);

        if (stored_key_len == k_len && stored_key_len <= 256) {
            /* Read key and compare */
            ic0_stable_read_impl((uint32_t)(uintptr_t)key_buf, offset + 4, stored_key_len);
            if (mem_eq(key, key_buf, k_len)) {
                /* Key found - read value */
                ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset + 4 + stored_key_len, 4);
                uint32_t stored_val_len = len_buf[0] | (len_buf[1] << 8) |
                                          (len_buf[2] << 16) | (len_buf[3] << 24);
                uint32_t copy_len = (stored_val_len < max_len) ? stored_val_len : max_len;
                ic0_stable_read_impl((uint32_t)(uintptr_t)val,
                                     offset + 4 + stored_key_len + 4, copy_len);
                return (int64_t)stored_val_len;
            }
        }

        /* Skip to next entry */
        ic0_stable_read_impl((uint32_t)(uintptr_t)len_buf, offset + 4 + stored_key_len, 4);
        uint32_t stored_val_len = len_buf[0] | (len_buf[1] << 8) |
                                  (len_buf[2] << 16) | (len_buf[3] << 24);
        offset += 4 + stored_key_len + 4 + stored_val_len;
    }

    return -1;  /* Not found */
}

/*
 * Delete key
 * key_ptr: pointer to key bytes
 * key_len: length of key
 * Returns: 0 on success (or not found), -1 on error
 *
 * Note: This implementation doesn't actually reclaim space (tombstone pattern).
 * For production, implement compaction.
 */
int64_t stkv_delete(int64_t key_ptr, int64_t key_len) {
    /* For simplicity, we don't implement delete in Phase 1 */
    /* A proper implementation would mark entries as deleted */
    (void)key_ptr;
    (void)key_len;
    return 0;
}

/*
 * Get entry count
 */
int64_t stkv_count_entries(void) {
    stkv_init_if_needed();
    return (int64_t)stkv_count;
}

/*
 * Clear all entries
 */
void stkv_clear(void) {
    stkv_count = 0;
    stkv_next_free = STKV_DATA_START;
    stkv_flush_header();
}

/* =============================================================================
 * Candid Encoding Buffer (for Idris2 -> C communication)
 *
 * Allows Idris2 to write Candid-encoded bytes that C can then use for
 * IC0 calls (e.g., EVM RPC requests).
 * ============================================================================= */

#define OUC_CANDID_BUF_SIZE 4096
static uint8_t ouc_candid_buf[OUC_CANDID_BUF_SIZE];
static int32_t ouc_candid_len = 0;

/* Called from Idris2 to write a byte at index */
void ouc_candid_write_byte(int64_t index, int64_t byte) {
    if (index >= 0 && index < OUC_CANDID_BUF_SIZE) {
        ouc_candid_buf[(int32_t)index] = (uint8_t)byte;
    }
}

/* Called from Idris2 to set the total length */
void ouc_candid_set_len(int64_t len) {
    if (len >= 0 && len <= OUC_CANDID_BUF_SIZE) {
        ouc_candid_len = (int32_t)len;
    }
}

/* Called from Idris2 to clear the buffer */
void ouc_candid_clear(void) {
    ouc_candid_len = 0;
}

/* Called from C to get buffer pointer */
uint8_t* ouc_c_get_candid_buf(void) {
    return ouc_candid_buf;
}

/* Called from C to get buffer length */
int32_t ouc_c_get_candid_len(void) {
    return ouc_candid_len;
}

/* =============================================================================
 * JSON Input Buffer (for C -> Idris2 communication)
 *
 * Allows C to pass JSON strings to Idris2 for Candid encoding.
 * ============================================================================= */

#define OUC_JSON_BUF_SIZE 1024
static char ouc_json_buf[OUC_JSON_BUF_SIZE];
static int32_t ouc_json_len = 0;

/* Called from C to set JSON string */
void ouc_c_set_json(const char* json) {
    ouc_json_len = 0;
    while (json[ouc_json_len] && ouc_json_len < OUC_JSON_BUF_SIZE - 1) {
        ouc_json_buf[ouc_json_len] = json[ouc_json_len];
        ouc_json_len++;
    }
    ouc_json_buf[ouc_json_len] = '\0';
}

/* Called from Idris2 to get JSON length */
int64_t ouc_json_get_len(void) {
    return (int64_t)ouc_json_len;
}

/* Called from Idris2 to get JSON byte at index */
int64_t ouc_json_get_byte(int64_t index) {
    if (index >= 0 && index < ouc_json_len) {
        return (int64_t)(uint8_t)ouc_json_buf[(int32_t)index];
    }
    return 0;
}

/* =============================================================================
 * A-Life Economics: Protocol Account Storage
 *
 * Manages protocol donations and tier system.
 * Each protocol identified by OU contract address (42 char hex string).
 *
 * Tier levels (monthly cost in cycles):
 *   0: Archive   -   3B cycles (~¥3/month)
 *   1: Economy   -  80B cycles (~¥80/month)
 *   2: Standard  - 300B cycles (~¥300/month)
 *   3: RealTime  - 4.5T cycles (~¥4,500/month)
 * ============================================================================= */

#define MAX_PROTOCOL_ACCOUNTS 256
#define PROTOCOL_ID_LEN 42  /* 0x + 40 hex chars */

/* Tier costs in cycles (per month) */
#define TIER_ARCHIVE_COST    3000000000ULL      /* 3B */
#define TIER_ECONOMY_COST    80000000000ULL     /* 80B */
#define TIER_STANDARD_COST   300000000000ULL    /* 300B */
#define TIER_REALTIME_COST   4500000000000ULL   /* 4.5T */

typedef struct {
    char protocol_id[PROTOCOL_ID_LEN + 1];  /* OU contract address */
    uint64_t balance;                        /* Cycles balance */
    uint8_t  tier;                           /* 0=Archive, 1=Economy, 2=Standard, 3=RealTime */
    uint64_t last_sync_block;                /* Last synced block */
    uint64_t expires_at;                     /* Tier expiration timestamp (nanoseconds) */
    uint8_t  active;                         /* 1 if slot is used, 0 if empty */
} ProtocolAccount;

static ProtocolAccount protocol_accounts[MAX_PROTOCOL_ACCOUNTS];
static uint32_t protocol_account_count = 0;

/* Find protocol account by ID, returns index or -1 if not found */
static int32_t find_protocol_account(const char* protocol_id) {
    for (uint32_t i = 0; i < protocol_account_count; i++) {
        if (protocol_accounts[i].active) {
            int match = 1;
            for (int j = 0; j < PROTOCOL_ID_LEN && protocol_id[j]; j++) {
                if (protocol_accounts[i].protocol_id[j] != protocol_id[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) return (int32_t)i;
        }
    }
    return -1;
}

/* Create new protocol account, returns index or -1 if full */
static int32_t create_protocol_account(const char* protocol_id) {
    if (protocol_account_count >= MAX_PROTOCOL_ACCOUNTS) {
        return -1;  /* Full */
    }
    int32_t idx = (int32_t)protocol_account_count;
    protocol_account_count++;

    /* Copy protocol ID */
    for (int j = 0; j < PROTOCOL_ID_LEN && protocol_id[j]; j++) {
        protocol_accounts[idx].protocol_id[j] = protocol_id[j];
    }
    protocol_accounts[idx].protocol_id[PROTOCOL_ID_LEN] = '\0';

    /* Initialize with Archive tier */
    protocol_accounts[idx].balance = 0;
    protocol_accounts[idx].tier = 0;  /* Archive */
    protocol_accounts[idx].last_sync_block = 0;
    protocol_accounts[idx].expires_at = 0;
    protocol_accounts[idx].active = 1;

    return idx;
}

/* Calculate affordable tier from balance */
static uint8_t calculate_tier(uint64_t balance) {
    if (balance >= TIER_REALTIME_COST) return 3;
    if (balance >= TIER_STANDARD_COST) return 2;
    if (balance >= TIER_ECONOMY_COST) return 1;
    return 0;  /* Archive */
}

/* Get tier name for debugging */
static const char* tier_name(uint8_t tier) {
    switch (tier) {
        case 0: return "Archive";
        case 1: return "Economy";
        case 2: return "Standard";
        case 3: return "RealTime";
        default: return "Unknown";
    }
}

/* Public API: Donate cycles to a protocol */
uint64_t ouc_donate(const char* protocol_id, uint64_t amount) {
    int32_t idx = find_protocol_account(protocol_id);
    if (idx < 0) {
        idx = create_protocol_account(protocol_id);
        if (idx < 0) return 0;  /* Failed to create */
    }

    /* Add donation to balance */
    protocol_accounts[idx].balance += amount;

    /* Calculate new tier */
    uint8_t old_tier = protocol_accounts[idx].tier;
    uint8_t new_tier = calculate_tier(protocol_accounts[idx].balance);

    if (new_tier > old_tier) {
        protocol_accounts[idx].tier = new_tier;
        /* Set expiration to 30 days from now */
        protocol_accounts[idx].expires_at = ic0_time_impl() + 30ULL * 24 * 60 * 60 * 1000000000ULL;

        char buf[64] = "tier_upgrade:";
        int len = 13;
        buf[len++] = '0' + old_tier;
        buf[len++] = '-';
        buf[len++] = '>';
        buf[len++] = '0' + new_tier;
        ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);
    }

    return protocol_accounts[idx].balance;
}

/* Public API: Get protocol balance */
uint64_t ouc_get_protocol_balance(const char* protocol_id) {
    int32_t idx = find_protocol_account(protocol_id);
    if (idx < 0) return 0;
    return protocol_accounts[idx].balance;
}

/* Public API: Get protocol tier */
uint8_t ouc_get_protocol_tier(const char* protocol_id) {
    int32_t idx = find_protocol_account(protocol_id);
    if (idx < 0) return 0;  /* Archive by default */
    return protocol_accounts[idx].tier;
}

/* Public API: Get total protocol count */
uint32_t ouc_get_protocol_count(void) {
    return protocol_account_count;
}

/* Public API: Accept cycles from message and donate to protocol */
uint64_t ouc_accept_and_donate(const char* protocol_id) {
    /* Get available cycles from the message */
    uint8_t available_buf[16];
    ic0_msg_cycles_available128_impl((uint32_t)(uintptr_t)available_buf);

    /* Read as 128-bit little-endian, but we only use low 64 bits */
    uint64_t available = 0;
    for (int i = 7; i >= 0; i--) {
        available = (available << 8) | available_buf[i];
    }

    /* Even with 0 cycles, register the protocol (Archive tier) */

    /* Accept all available cycles */
    uint8_t accepted_buf[16];
    ic0_msg_cycles_accept128_impl(0, available, (uint32_t)(uintptr_t)accepted_buf);

    /* Read accepted amount */
    uint64_t accepted = 0;
    for (int i = 7; i >= 0; i--) {
        accepted = (accepted << 8) | accepted_buf[i];
    }

    char buf[64] = "cycles_accepted:";
    int len = 16;
    /* Simple decimal output for debugging */
    if (accepted >= 1000000000000ULL) buf[len++] = 'T';
    else if (accepted >= 1000000000ULL) buf[len++] = 'B';
    else if (accepted >= 1000000ULL) buf[len++] = 'M';
    else buf[len++] = 'c';
    ic0_debug_print_impl((uint32_t)(uintptr_t)buf, len);

    /* Donate to protocol */
    return ouc_donate(protocol_id, accepted);
}
