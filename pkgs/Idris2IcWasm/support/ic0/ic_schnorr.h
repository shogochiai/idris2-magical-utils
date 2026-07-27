#ifndef ICW_SCHNORR_H
#define ICW_SCHNORR_H

#include <stdint.h>

/* Threshold Schnorr (ed25519) — sibling of ic_tecdsa.h/.c, same
 * inter-canister-call shape (Candid-encode request, ic0_call_new/
 * data_append/cycles_add128/call_perform, deferred reply via a callback).
 * Primary use: fleet SSH CA signing (an SSH user-certificate body is signed
 * directly, no pre-hashing — sign_with_schnorr takes an arbitrary-length
 * `message` blob, unlike sign_with_ecdsa's fixed 32-byte hash). */

void ic_schnorr_set_key(int64_t key_type);       /* 0=production(key_1), 1=test(test_key_1), 2=local(dfx_test_key) */
void ic_schnorr_clear_message(void);
void ic_schnorr_set_message_byte(int64_t idx, int64_t byte);
int64_t ic_schnorr_sign(void);
int64_t ic_schnorr_get_status(void);              /* 0=pending, 1=success, -1=error */
int64_t ic_schnorr_get_signature_len(void);        /* 64 on success (RFC 8032 ed25519) */
int64_t ic_schnorr_get_signature_byte(int64_t index);
int64_t ic_schnorr_request_pubkey(void);
int64_t ic_schnorr_get_pubkey_len(void);            /* 32 on success (raw ed25519 point) */
int64_t ic_schnorr_get_pubkey_byte(int64_t index);
int64_t ic_schnorr_get_last_error_len(void);
int64_t ic_schnorr_get_last_error_byte(int64_t index);

/* Entry points for direct CMD dispatch (same convention as
 * icw_tecdsa_sign_hash_entry / icw_tecdsa_get_evm_address_entry):
 * arg0 = hex-encoded message bytes (the SSH certificate body); reply is
 * either NULL (async, deferred reply fires from the callback) or a
 * synchronous "error:..." string. */
const char* icw_schnorr_sign_message_entry(const uint8_t* arg_buf, int32_t arg_buf_size);
const char* icw_schnorr_get_pubkey_entry(const uint8_t* arg_buf, int32_t arg_buf_size);

#endif
