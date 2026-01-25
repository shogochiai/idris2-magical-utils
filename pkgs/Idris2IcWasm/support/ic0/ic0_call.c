/*
 * IC0 Inter-Canister Call Support
 *
 * Buffer management for ic0_call_* API
 */
#include "ic0_call.h"
#include <string.h>

/* =============================================================================
 * Buffers
 * ============================================================================= */

static uint8_t g_callee_buffer[IC_CALL_CALLEE_SIZE];
static int32_t g_callee_len = 0;

static uint8_t g_method_buffer[IC_CALL_METHOD_SIZE];
static int32_t g_method_len = 0;

static uint8_t g_payload_buffer[IC_CALL_PAYLOAD_SIZE];
static int32_t g_payload_len = 0;

static uint8_t g_response_buffer[IC_CALL_RESPONSE_SIZE];
static int32_t g_response_len = 0;

static int32_t g_call_status = 0;  /* 0=idle, 1=pending, 2=success, -1=error */

/* =============================================================================
 * Buffer Management
 * ============================================================================= */

void ic_call_write_byte(int32_t buffer_id, int32_t index, int32_t byte) {
    switch (buffer_id) {
        case IC_CALL_BUFFER_CALLEE:
            if (index >= 0 && index < IC_CALL_CALLEE_SIZE) {
                g_callee_buffer[index] = (uint8_t)byte;
            }
            break;
        case IC_CALL_BUFFER_METHOD:
            if (index >= 0 && index < IC_CALL_METHOD_SIZE) {
                g_method_buffer[index] = (uint8_t)byte;
            }
            break;
        case IC_CALL_BUFFER_PAYLOAD:
            if (index >= 0 && index < IC_CALL_PAYLOAD_SIZE) {
                g_payload_buffer[index] = (uint8_t)byte;
            }
            break;
    }
}

int32_t ic_call_get_ptr(int32_t buffer_id) {
    switch (buffer_id) {
        case IC_CALL_BUFFER_CALLEE:
            return (int32_t)(uintptr_t)g_callee_buffer;
        case IC_CALL_BUFFER_METHOD:
            return (int32_t)(uintptr_t)g_method_buffer;
        case IC_CALL_BUFFER_PAYLOAD:
            return (int32_t)(uintptr_t)g_payload_buffer;
        default:
            return 0;
    }
}

void ic_call_set_len(int32_t buffer_id, int32_t len) {
    switch (buffer_id) {
        case IC_CALL_BUFFER_CALLEE:
            g_callee_len = (len >= 0 && len <= IC_CALL_CALLEE_SIZE) ? len : 0;
            break;
        case IC_CALL_BUFFER_METHOD:
            g_method_len = (len >= 0 && len <= IC_CALL_METHOD_SIZE) ? len : 0;
            break;
        case IC_CALL_BUFFER_PAYLOAD:
            g_payload_len = (len >= 0 && len <= IC_CALL_PAYLOAD_SIZE) ? len : 0;
            break;
    }
}

/* =============================================================================
 * Response Buffer
 * ============================================================================= */

int32_t ic_call_response_ptr(void) {
    return (int32_t)(uintptr_t)g_response_buffer;
}

int32_t ic_call_response_len(void) {
    return g_response_len;
}

int32_t ic_call_response_byte(int32_t index) {
    if (index >= 0 && index < g_response_len) {
        return (int32_t)g_response_buffer[index];
    }
    return 0;
}

void ic_call_response_write(int32_t index, int32_t byte) {
    if (index >= 0 && index < IC_CALL_RESPONSE_SIZE) {
        g_response_buffer[index] = (uint8_t)byte;
    }
}

void ic_call_response_set_len(int32_t len) {
    g_response_len = (len >= 0 && len <= IC_CALL_RESPONSE_SIZE) ? len : 0;
}

/* =============================================================================
 * Call Status
 * ============================================================================= */

int32_t ic_call_status(void) {
    return g_call_status;
}

void ic_call_set_status(int32_t status) {
    g_call_status = status;
}

/* =============================================================================
 * IC0 Callback Handlers (called by IC runtime)
 * ============================================================================= */

/* IC0 imports */
extern int32_t ic0_msg_arg_data_size(void);
extern void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size);

/* Default reply callback - copies response to buffer */
void ic_call_default_reply(void) {
    int32_t size = ic0_msg_arg_data_size();
    if (size > 0 && size <= IC_CALL_RESPONSE_SIZE) {
        ic0_msg_arg_data_copy((int32_t)(uintptr_t)g_response_buffer, 0, size);
        g_response_len = size;
        g_call_status = 2;  /* Success */
    } else {
        g_call_status = -1; /* Error */
    }
}

/* Default reject callback */
void ic_call_default_reject(void) {
    g_call_status = -1;  /* Error */
    g_response_len = 0;
}
