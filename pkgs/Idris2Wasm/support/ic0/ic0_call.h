/*
 * IC0 Inter-Canister Call Support
 *
 * Buffer management for ic0_call_* API
 */
#ifndef IC0_CALL_H
#define IC0_CALL_H

#include <stdint.h>

/* Buffer IDs */
#define IC_CALL_BUFFER_CALLEE  0
#define IC_CALL_BUFFER_METHOD  1
#define IC_CALL_BUFFER_PAYLOAD 2

/* Buffer sizes */
#define IC_CALL_CALLEE_SIZE  32   /* Principal max size */
#define IC_CALL_METHOD_SIZE  64   /* Method name max size */
#define IC_CALL_PAYLOAD_SIZE 4096 /* Request payload max size */
#define IC_CALL_RESPONSE_SIZE 8192 /* Response max size */

/* Buffer management */
void ic_call_write_byte(int32_t buffer_id, int32_t index, int32_t byte);
int32_t ic_call_get_ptr(int32_t buffer_id);
void ic_call_set_len(int32_t buffer_id, int32_t len);

/* Response buffer */
int32_t ic_call_response_ptr(void);
int32_t ic_call_response_len(void);
int32_t ic_call_response_byte(int32_t index);
void ic_call_response_write(int32_t index, int32_t byte);
void ic_call_response_set_len(int32_t len);

/* Call status */
int32_t ic_call_status(void);
void ic_call_set_status(int32_t status);

/* Callback registration */
typedef void (*ic_callback_fn)(void);
void ic_call_set_reply_callback(ic_callback_fn fn);
void ic_call_set_reject_callback(ic_callback_fn fn);

#endif /* IC0_CALL_H */
