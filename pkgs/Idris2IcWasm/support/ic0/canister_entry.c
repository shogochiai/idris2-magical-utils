/*
 * Generic Canister Entry Points Template
 *
 * This is a minimal template for Idris2 IC canisters.
 * Customize this file for your project's specific methods.
 *
 * Pipeline: Idris2 → RefC → C → Emscripten → WASM
 */
#include <stdint.h>
#include <string.h>

/* =============================================================================
 * IC0 Imports (from ic0_stubs.c - do NOT redeclare with import_module)
 * ============================================================================= */
extern void ic0_msg_reply(void);
extern void ic0_msg_reply_data_append(int32_t src, int32_t size);
extern int32_t ic0_msg_arg_data_size(void);
extern void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size);
extern void ic0_debug_print(int32_t src, int32_t size);
extern void ic0_trap(int32_t src, int32_t size);

/* =============================================================================
 * Idris2 RefC Runtime Interface
 * ============================================================================= */

/* Forward declaration from Idris2 generated code */
extern void* __mainExpression_0(void);  /* Idris2 main entry - returns IO closure */
extern void* idris2_trampoline(void*);  /* Execute Idris2 closure (RefC runtime) */

/* Initialize Idris2 runtime - call once at canister_init */
static int idris2_initialized = 0;

static void ensure_idris2_init(void) {
    if (!idris2_initialized) {
        void* closure = __mainExpression_0();
        idris2_trampoline(closure);
        idris2_initialized = 1;
    }
}

/* =============================================================================
 * Helper Functions
 * ============================================================================= */

static void debug_log(const char* msg) {
    ic0_debug_print((int32_t)msg, strlen(msg));
}

/* Candid Text reply: DIDL + 1 type (0x71 = text) + 1 arg + LEB128 len + bytes */
static void reply_text(const char* text) {
    size_t len = strlen(text);
    uint8_t header[16] = { 'D', 'I', 'D', 'L', 0x00, 0x01, 0x71 };
    int pos = 7;
    /* LEB128 encode length */
    size_t l = len;
    do {
        header[pos++] = (l & 0x7f) | (l > 0x7f ? 0x80 : 0);
        l >>= 7;
    } while (l > 0);
    ic0_msg_reply_data_append((int32_t)header, pos);
    ic0_msg_reply_data_append((int32_t)text, len);
    ic0_msg_reply();
}

/* =============================================================================
 * Canister Lifecycle Methods
 * ============================================================================= */

__attribute__((export_name("canister_init")))
void canister_init(void) {
    debug_log("Idris2 canister: init");
    ensure_idris2_init();
}

__attribute__((export_name("canister_post_upgrade")))
void canister_post_upgrade(void) {
    debug_log("Idris2 canister: post_upgrade");
    ensure_idris2_init();
}

__attribute__((export_name("canister_pre_upgrade")))
void canister_pre_upgrade(void) {
    debug_log("Idris2 canister: pre_upgrade");
    /* Save state to stable memory here */
}

/* =============================================================================
 * Example Query/Update Methods
 *
 * Customize these for your canister. Each method should:
 * 1. Parse arguments via ic0_msg_arg_data_copy if needed
 * 2. Call Idris2 functions via RefC interface
 * 3. Reply with Candid-encoded result
 * ============================================================================= */

__attribute__((export_name("canister_query greet")))
void canister_query_greet(void) {
    debug_log("greet called");
    reply_text("Hello from Idris2 on IC!");
}

__attribute__((export_name("canister_update ping")))
void canister_update_ping(void) {
    debug_log("ping called");
    reply_text("pong");
}
