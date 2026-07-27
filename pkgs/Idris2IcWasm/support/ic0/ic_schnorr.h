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

/* ---- Idris2-driven deferred-sign path (@defer_if_pending convention,
 * same shape as ic_vetkd's getMemoryVetKey/CMD 314) ----
 *
 * Unlike icw_schnorr_sign_message_entry (which owns the WHOLE request —
 * parses arg0 itself and signs it directly), THIS path lets an Idris2
 * command handler do its own gating/DB-writes FIRST (e.g. joinBuilderFleet
 * checking + writing builder_fleet_members), assemble a certificate body
 * with SshCert.Core, hand that body to the CA signer via
 * ic_schnorr_set_pending_cert_body, fire the async sign, and return
 * control to gen-entry's generated dispatcher WITHOUT replying — the
 * dispatcher's own auto-reply is then suppressed by
 * ic_schnorr_consume_pending_cert_reply (wired via @defer_if_pending), and
 * the ACTUAL reply (certificate body + signature, concatenated as one hex
 * string) is sent by the reply callback once sign_with_schnorr returns.
 * This keeps the whole join-and-certify flow as ONE client-visible
 * canister call, matching docs/papers/builder-fleet-ca-join-design.md's
 * "don't ask for auth twice" requirement. */

/* Load the certificate BODY (everything up to and including the signature
 * key, per PROTOCOL.certkeys/SshCert.Core.assembleCertBody) byte-at-a-time
 * — same scalar-only FFI convention as ic_schnorr_set_message_byte. Call
 * ic_schnorr_clear_pending_cert_body first. */
void ic_schnorr_clear_pending_cert_body(void);
void ic_schnorr_set_pending_cert_body_byte(int64_t idx, int64_t byte);

/* Fire the async sign over the pending cert body (production key_1 only —
 * the fleet has one CA key) and mark the deferred-reply flag. Returns 0 on
 * successful call initiation, nonzero on failure (caller should reply an
 * error synchronously in that case — nothing was deferred). */
int64_t ic_schnorr_sign_pending_cert(void);

/* Read + reset the deferred-reply flag (wired via @defer_if_pending): the
 * generated entry calls this AFTER the Idris2 handler returns, and skips
 * its own auto-reply iff this returns nonzero. */
int32_t ic_schnorr_consume_pending_cert_reply(void);

#endif
