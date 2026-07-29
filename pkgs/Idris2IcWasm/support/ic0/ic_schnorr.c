#include "ic_schnorr.h"

#include <stdint.h>
#include <string.h>

extern void ic0_call_new(int32_t callee_src, int32_t callee_size,
                         int32_t name_src, int32_t name_size,
                         int32_t reply_fun, int32_t reply_env,
                         int32_t reject_fun, int32_t reject_env);
extern void ic0_call_data_append(int32_t src, int32_t size);
extern void ic0_call_cycles_add128(uint64_t high, uint64_t low);
extern int32_t ic0_call_perform(void);
extern void ic0_debug_print(int32_t src, int32_t size);
extern void ic0_msg_reply(void);
extern void ic0_msg_reply_data_append(int32_t src, int32_t size);
extern int32_t ic0_msg_arg_data_size(void);
extern void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size);
extern int32_t ic0_msg_reject_msg_size(void);
extern void ic0_msg_reject_msg_copy(int32_t dst, int32_t offset, int32_t size);

#define debug(msg) do { \
    static const char _msg[] = msg; \
    ic0_debug_print((int32_t)(uintptr_t)_msg, (int32_t)(sizeof(_msg) - 1)); \
} while (0)

typedef void (*ic_schnorr_callback_fn)(int32_t env);

static const uint8_t MANAGEMENT_CANISTER[] = {};
static const char METHOD_SIGN[] = "sign_with_schnorr";
static const char METHOD_PUBKEY[] = "schnorr_public_key";
static const char KEY_PRODUCTION[] = "key_1";
static const char KEY_TEST[] = "test_key_1";
static const char KEY_LOCAL[] = "dfx_test_key";
static const char HEX[] = "0123456789abcdef";

/* message is variable-length, unlike ic_tecdsa's fixed 32-byte hash — the
 * whole reason for this sibling module rather than reusing t-ECDSA: an SSH
 * certificate body is signed directly (PROTOCOL.certkeys: "everything up to
 * and including signature key"), no pre-hashing.
 *
 * Must be >= SCHNORR_MAX_CERT_BODY: ic_schnorr_sign_pending_cert copies the
 * WHOLE assembled cert body into this buffer byte-for-byte before signing
 * (see below), so a smaller limit here would reintroduce the exact same
 * silent-truncation bug the 2026-07-29 SCHNORR_MAX_CERT_BODY fix closed,
 * one layer later in the same call. */
#define SCHNORR_MAX_MESSAGE 2048
static uint8_t g_schnorr_message[SCHNORR_MAX_MESSAGE];
static uint32_t g_schnorr_message_len = 0;
static uint8_t g_key_name[32];
static uint32_t g_key_name_len = 0;

static uint8_t g_signature[64];
static uint32_t g_signature_len = 0;
static int32_t g_sign_status = 0;

static uint8_t g_public_key[32];  /* raw ed25519 point, unlike ECDSA's SEC1 33/65 */
static uint32_t g_public_key_len = 0;

static char g_last_error[256] = "";
static int32_t g_last_error_len = 0;

/* ---- Idris2-driven deferred-sign state (joinBuilderFleet path) ----
 * A certificate BODY assembled by Idris2 (SshCert.Core.assembleCertBody),
 * held here until the async sign_with_schnorr reply arrives, at which
 * point the reply callback appends the signature and replies the whole
 * thing — see ic_schnorr.h's "Idris2-driven deferred-sign path" doc.
 *
 * SCHNORR_MAX_CERT_BODY was 512 until 2026-07-29 (carl, mfycd principal,
 * found live): the read+write unified force-command (747a35a) made a real
 * cert body ~521 bytes, 9 over the old limit. set_pending_cert_body_byte's
 * bounds check silently DROPPED bytes past the limit with no error signal,
 * so the certificate's trailing signature-key field (the CA pubkey) was
 * truncated mid-field — declared length 32, actual 19 bytes present — and
 * every issued certificate failed `ssh-keygen -L` with "invalid key:
 * invalid format". Raised to 2048 (4x headroom over the measured overrun,
 * cheap on a canister with a stable-memory-backed heap) and the bounds
 * check now records an error instead of dropping silently — see
 * g_pending_cert_overflow below. */
#define SCHNORR_MAX_CERT_BODY 2048
static uint8_t g_pending_cert_body[SCHNORR_MAX_CERT_BODY];
static uint32_t g_pending_cert_body_len = 0;
static int32_t g_pending_cert_reply = 0;  /* consumed by @defer_if_pending */
static int32_t g_pending_cert_overflow = 0;  /* set if any byte was dropped for being out of range */

static uint32_t encode_leb128_unsigned(uint8_t* buf, uint64_t value) {
    uint32_t len = 0;
    do {
        uint8_t byte = (uint8_t)(value & 0x7F);
        value >>= 7;
        if (value != 0) byte |= 0x80;
        buf[len++] = byte;
    } while (value != 0);
    return len;
}

static uint64_t parse_leb128_from(const uint8_t* buf, int32_t size, int32_t* offset) {
    uint64_t result = 0;
    int shift = 0;
    while (*offset < size) {
        uint8_t byte = buf[(*offset)++];
        result |= ((uint64_t)(byte & 0x7F)) << shift;
        if ((byte & 0x80) == 0) break;
        shift += 7;
    }
    return result;
}

static void clear_last_error(void) {
    g_last_error[0] = '\0';
    g_last_error_len = 0;
}

static void set_last_error(const char* msg) {
    uint32_t len = (uint32_t)strlen(msg);
    if (len >= sizeof(g_last_error)) len = sizeof(g_last_error) - 1;
    memcpy(g_last_error, msg, len);
    g_last_error[len] = '\0';
    g_last_error_len = (int32_t)len;
}

static void callback_reply_text(const char* text) {
    uint32_t tlen = (uint32_t)strlen(text);
    uint8_t header[] = { 0x44, 0x49, 0x44, 0x4C, 0x00, 0x01, 0x71 };
    ic0_msg_reply_data_append((int32_t)(uintptr_t)header, 7);
    uint8_t leb_buf[5];
    int32_t leb_len = 0;
    uint32_t val = tlen;
    do {
        uint8_t b = (uint8_t)(val & 0x7F);
        val >>= 7;
        if (val != 0) b |= 0x80;
        leb_buf[leb_len++] = b;
    } while (val != 0);
    ic0_msg_reply_data_append((int32_t)(uintptr_t)leb_buf, leb_len);
    ic0_msg_reply_data_append((int32_t)(uintptr_t)text, (int32_t)tlen);
    ic0_msg_reply();
}

static void format_reject_error(char* out, uint32_t out_size, const char* prefix) {
    uint32_t plen = (uint32_t)strlen(prefix);
    int32_t msg_size = ic0_msg_reject_msg_size();
    if ((uint32_t)msg_size > out_size - plen - 1) msg_size = (int32_t)(out_size - plen - 1);
    memcpy(out, prefix, plen);
    if (msg_size > 0) {
        ic0_msg_reject_msg_copy((int32_t)(uintptr_t)(out + plen), 0, msg_size);
    }
    out[plen + (uint32_t)msg_size] = '\0';
}

static void set_key_name_from_type(int64_t key_type) {
    const char* name = KEY_LOCAL;
    uint32_t len = (uint32_t)(sizeof(KEY_LOCAL) - 1);
    if (key_type == 0) {
        name = KEY_PRODUCTION;
        len = (uint32_t)(sizeof(KEY_PRODUCTION) - 1);
    } else if (key_type == 1) {
        name = KEY_TEST;
        len = (uint32_t)(sizeof(KEY_TEST) - 1);
    }
    memcpy(g_key_name, name, len);
    g_key_name_len = len;
}

/* =============================================================================
 * Candid Encoding
 *
 * sign_with_schnorr arg:
 *   record { aux: opt variant{bip341: record{merkle_root_hash: blob}};
 *            key_id: record{algorithm: variant{ed25519; bip340secp256k1}; name: text};
 *            derivation_path: vec blob; message: blob }
 *
 * schnorr_public_key arg:
 *   record { key_id: record{algorithm; name}; canister_id: opt principal;
 *            derivation_path: vec blob }
 *
 * HEADER BYTES below were built and round-trip-decoded with ic-py against
 * the exact record shapes from the IC interface spec
 * (https://docs.internetcomputer.org/references/ic-interface-spec/
 * management-canister/) BEFORE being transcribed here — not hand-computed
 * from the field-hash formula alone. ic_tecdsa.c's own history (see its
 * 2026-06-03 correction comment) is exactly the failure mode this avoids:
 * a wrong-but-plausible-looking hash compiles fine and only fails at the IC
 * management canister with "Cannot parse header", which is expensive to
 * debug blind. Field-hash values, computed via idl_hash(s) = sum(s[i] *
 * 223^(len-1-i)) mod 2^32 and cross-checked byte-for-byte against the ic-py
 * encoding: aux=0x004A0104, key_id=0x3FEB75BB, derivation_path=0x562C942D,
 * message=0x9A1135C7, algorithm=0x14EEAC6F, name=0x48FF724B,
 * ed25519=0x50E1A1BB, canister_id=0x4E4C6233.
 * ============================================================================= */
static const uint8_t SIGN_HEADER[] = {
    0x44, 0x49, 0x44, 0x4C, 0x08, 0x6D, 0x7B, 0x6C, 0x01, 0xD6, 0x98, 0xF8,
    0xE9, 0x07, 0x00, 0x6B, 0x01, 0xC7, 0x96, 0xEC, 0xF1, 0x0E, 0x01, 0x6E,
    0x02, 0x6B, 0x02, 0xBB, 0xC3, 0x86, 0x87, 0x05, 0x7F, 0x94, 0x96, 0x86,
    0xBA, 0x0D, 0x7F, 0x6C, 0x02, 0xEF, 0xD8, 0xBA, 0xA7, 0x01, 0x04, 0xCB,
    0xE4, 0xFD, 0xC7, 0x04, 0x71, 0x6D, 0x00, 0x6C, 0x04, 0x84, 0x82, 0xA8,
    0x02, 0x03, 0xBB, 0xEB, 0xAD, 0xFF, 0x03, 0x05, 0xAD, 0xA8, 0xB2, 0xB1,
    0x05, 0x06, 0xC7, 0xEB, 0xC4, 0xD0, 0x09, 0x00, 0x01, 0x07,
};

/* Wire value order (ascending field hash, matches SIGN_HEADER's arg type):
 * aux, key_id{algorithm,name}, derivation_path, message. */
static uint32_t encode_sign_request(uint8_t* buf) {
    uint32_t pos = 0;
    memcpy(buf, SIGN_HEADER, sizeof(SIGN_HEADER));
    pos = (uint32_t)sizeof(SIGN_HEADER);

    buf[pos++] = 0x00;  /* aux: opt = None */

    buf[pos++] = 0x00;  /* key_id.algorithm: variant idx 0 (ed25519), null payload */
    pos += encode_leb128_unsigned(buf + pos, g_key_name_len);
    memcpy(buf + pos, g_key_name, g_key_name_len);
    pos += g_key_name_len;

    buf[pos++] = 0x00;  /* derivation_path: vec blob, empty (one fixed CA key) */

    pos += encode_leb128_unsigned(buf + pos, g_schnorr_message_len);
    memcpy(buf + pos, g_schnorr_message, g_schnorr_message_len);
    pos += g_schnorr_message_len;

    return pos;
}

static const uint8_t PUBKEY_HEADER[] = {
    0x44, 0x49, 0x44, 0x4C, 0x06, 0x6B, 0x02, 0xBB, 0xC3, 0x86, 0x87, 0x05,
    0x7F, 0x94, 0x96, 0x86, 0xBA, 0x0D, 0x7F, 0x6C, 0x02, 0xEF, 0xD8, 0xBA,
    0xA7, 0x01, 0x00, 0xCB, 0xE4, 0xFD, 0xC7, 0x04, 0x71, 0x6E, 0x68, 0x6D,
    0x7B, 0x6D, 0x03, 0x6C, 0x03, 0xBB, 0xEB, 0xAD, 0xFF, 0x03, 0x01, 0xB3,
    0xC4, 0xB1, 0xF2, 0x04, 0x02, 0xAD, 0xA8, 0xB2, 0xB1, 0x05, 0x04, 0x01,
    0x05,
};

/* Wire value order (ascending field hash): key_id{algorithm,name}, canister_id, derivation_path. */
static uint32_t encode_pubkey_request(uint8_t* buf) {
    uint32_t pos = 0;
    memcpy(buf, PUBKEY_HEADER, sizeof(PUBKEY_HEADER));
    pos = (uint32_t)sizeof(PUBKEY_HEADER);

    buf[pos++] = 0x00;  /* key_id.algorithm: variant idx 0 (ed25519), null */
    pos += encode_leb128_unsigned(buf + pos, g_key_name_len);
    memcpy(buf + pos, g_key_name, g_key_name_len);
    pos += g_key_name_len;

    buf[pos++] = 0x00;  /* canister_id: opt = None (this canister's own key) */
    buf[pos++] = 0x00;  /* derivation_path: vec blob, empty */

    return pos;
}

/* Reply shape: record { signature : blob }, ed25519 sig = 64 raw bytes
 * (RFC 8032). Scan for a length-prefix byte of 64 (fits one LEB128 byte)
 * immediately followed by 64 in-bounds bytes — same minimal blob-scan
 * strategy ic_tecdsa.c uses for its 64-byte r||s signature. */
static int parse_signature_reply(uint8_t sig_out[64]) {
    uint8_t reply[256];
    int32_t reply_size = ic0_msg_arg_data_size();
    int32_t off;
    if (reply_size > (int32_t)sizeof(reply)) reply_size = (int32_t)sizeof(reply);
    if (reply_size <= 0) return 0;
    ic0_msg_arg_data_copy((int32_t)(uintptr_t)reply, 0, reply_size);

    for (off = 0; off + 65 <= reply_size; off++) {
        if (reply[off] == 0x40) {
            memcpy(sig_out, reply + off + 1, 64);
            return 1;
        }
    }
    return 0;
}

/* Reply shape: record { public_key : blob; chain_code : blob }, ed25519
 * public_key = 32 raw bytes (no SEC1 prefix, unlike ECDSA). */
static int parse_pubkey_reply(void) {
    uint8_t reply[256];
    int32_t reply_size = ic0_msg_arg_data_size();
    int32_t offset;
    if (reply_size > (int32_t)sizeof(reply)) reply_size = (int32_t)sizeof(reply);
    if (reply_size <= 0) return 0;
    ic0_msg_arg_data_copy((int32_t)(uintptr_t)reply, 0, reply_size);

    for (offset = 0; offset + 33 <= reply_size; offset++) {
        if (reply[offset] == 0x20) {  /* length-prefix 32 */
            g_public_key_len = 32;
            memcpy(g_public_key, reply + offset + 1, 32);
            return 1;
        }
    }
    return 0;
}

static int32_t begin_sign_call(int32_t reply_idx, int32_t reject_idx) {
    uint8_t request[sizeof(SIGN_HEADER) + 5 + SCHNORR_MAX_MESSAGE + 64];
    uint32_t request_len = encode_sign_request(request);
    clear_last_error();
    g_sign_status = 0;
    g_signature_len = 0;

    ic0_call_new((int32_t)(uintptr_t)MANAGEMENT_CANISTER, 0,
                 (int32_t)(uintptr_t)METHOD_SIGN, (int32_t)(sizeof(METHOD_SIGN) - 1),
                 reply_idx, 0, reject_idx, 0);
    ic0_call_data_append((int32_t)(uintptr_t)request, (int32_t)request_len);
    /* Same order of magnitude as sign_with_ecdsa's fee; 30B covers mainnet
     * with any overpayment refunded. */
    ic0_call_cycles_add128(0, 30000000000ULL);
    return ic0_call_perform();
}

static int32_t begin_pubkey_call(int32_t reply_idx, int32_t reject_idx) {
    uint8_t request[256];
    uint32_t request_len = encode_pubkey_request(request);
    int32_t perform_result;
    clear_last_error();

    debug("ic_schnorr: begin_pubkey_call: encoding request");
    ic0_call_new((int32_t)(uintptr_t)MANAGEMENT_CANISTER, 0,
                 (int32_t)(uintptr_t)METHOD_PUBKEY, (int32_t)(sizeof(METHOD_PUBKEY) - 1),
                 reply_idx, 0, reject_idx, 0);
    debug("ic_schnorr: ic0_call_new done");
    ic0_call_data_append((int32_t)(uintptr_t)request, (int32_t)request_len);
    debug("ic_schnorr: ic0_call_data_append done");
    ic0_call_cycles_add128(0, 25000000000ULL);
    debug("ic_schnorr: ic0_call_cycles_add128 done, calling perform");
    perform_result = ic0_call_perform();
    if (perform_result == 0) {
        debug("ic_schnorr: ic0_call_perform returned 0 (initiated)");
    } else {
        debug("ic_schnorr: ic0_call_perform returned NONZERO (failed to initiate)");
    }
    return perform_result;
}

static void ic_schnorr_sign_reply_callback(int32_t env);
static void ic_schnorr_sign_reject_callback(int32_t env);
static void ic_schnorr_pubkey_reply_callback(int32_t env);
static void ic_schnorr_pubkey_reject_callback(int32_t env);
static void hook_sign_reply_callback(int32_t env);
static void hook_sign_reject_callback(int32_t env);
static void hook_pubkey_reply_callback(int32_t env);
static void hook_pubkey_reject_callback(int32_t env);
static void hook_cert_sign_reply_callback(int32_t env);
static void hook_cert_sign_reject_callback(int32_t env);

static ic_schnorr_callback_fn g_hook_cert_sign_reply  = &hook_cert_sign_reply_callback;
static ic_schnorr_callback_fn g_hook_cert_sign_reject = &hook_cert_sign_reject_callback;

static ic_schnorr_callback_fn g_ic_sign_reply = &ic_schnorr_sign_reply_callback;
static ic_schnorr_callback_fn g_ic_sign_reject = &ic_schnorr_sign_reject_callback;
static ic_schnorr_callback_fn g_ic_pubkey_reply = &ic_schnorr_pubkey_reply_callback;
static ic_schnorr_callback_fn g_ic_pubkey_reject = &ic_schnorr_pubkey_reject_callback;
static ic_schnorr_callback_fn g_hook_sign_reply = &hook_sign_reply_callback;
static ic_schnorr_callback_fn g_hook_sign_reject = &hook_sign_reject_callback;
static ic_schnorr_callback_fn g_hook_pubkey_reply = &hook_pubkey_reply_callback;
static ic_schnorr_callback_fn g_hook_pubkey_reject = &hook_pubkey_reject_callback;

static void ic_schnorr_sign_reply_callback(int32_t env) {
    uint8_t sig[64];
    (void)env;
    if (parse_signature_reply(sig)) {
        memcpy(g_signature, sig, sizeof(sig));
        g_signature_len = 64;
        g_sign_status = 1;
        clear_last_error();
    } else {
        set_last_error("error:signature_not_found_in_reply");
        g_sign_status = -1;
    }
}

static void ic_schnorr_sign_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:sign_rejected:");
    set_last_error(err);
    g_sign_status = -1;
}

static void ic_schnorr_pubkey_reply_callback(int32_t env) {
    (void)env;
    if (!parse_pubkey_reply()) {
        set_last_error("error:no_key_in_reply");
    } else {
        clear_last_error();
    }
}

static void ic_schnorr_pubkey_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:schnorr_rejected:");
    set_last_error(err);
    g_public_key_len = 0;
}

static void hook_sign_reply_callback(int32_t env) {
    uint8_t sig[64];
    char result[132];
    int i;
    int pos = 0;
    (void)env;
    if (!parse_signature_reply(sig)) {
        callback_reply_text("error:signature_not_found_in_reply");
        return;
    }
    memcpy(g_signature, sig, sizeof(sig));
    g_signature_len = 64;
    g_sign_status = 1;
    clear_last_error();

    memcpy(result, "sig=", 4);
    pos = 4;
    for (i = 0; i < 64; i++) {
        result[pos++] = HEX[(sig[i] >> 4) & 0xF];
        result[pos++] = HEX[sig[i] & 0xF];
    }
    result[pos] = '\0';
    callback_reply_text(result);
}

static void hook_sign_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:sign_rejected:");
    set_last_error(err);
    g_sign_status = -1;
    callback_reply_text(err);
}

/* Append a hex-encoded big-endian uint32 (SSH wire "string" length prefix)
 * to result[*pos], advancing *pos by 8 hex chars. */
static void append_hex_u32(char* result, uint32_t* pos, uint32_t value) {
    int shift;
    for (shift = 28; shift >= 0; shift -= 4) {
        result[(*pos)++] = HEX[(value >> shift) & 0xF];
    }
}

/* Append a hex-encoded byte buffer to result[*pos], advancing *pos by
 * 2*len hex chars. */
static void append_hex_bytes(char* result, uint32_t* pos, const uint8_t* bytes, uint32_t len) {
    uint32_t i;
    for (i = 0; i < len; i++) {
        result[(*pos)++] = HEX[(bytes[i] >> 4) & 0xF];
        result[(*pos)++] = HEX[bytes[i] & 0xF];
    }
}

/* Append one SSH wire "string" field (4-byte big-endian length + bytes),
 * hex-encoded, to result[*pos]. */
static void append_hex_sshstring(char* result, uint32_t* pos, const uint8_t* bytes, uint32_t len) {
    append_hex_u32(result, pos, len);
    append_hex_bytes(result, pos, bytes, len);
}

/* Cert-body variant of hook_sign_reply/reject: appends the signature to
 * the Idris2-assembled body held in g_pending_cert_body and replies the
 * WHOLE certificate (body + wire-format signature FIELD) as one hex
 * string — this is the deferred reply for joinBuilderFleet, not for
 * sshCaSign.
 *
 * The "signature" field itself is NOT the raw 64 bytes — per
 * PROTOCOL.certkeys / SshCert.Core.appendSignature, it is:
 *   sshString( sshText("ssh-ed25519") ++ sshString(sig64) )
 * i.e. one more length-prefix wrapping [algo-name-string, sig-blob-string].
 * An earlier version of this function forgot this wrapping entirely and
 * appended the 64 raw signature bytes directly — every certificate byte
 * before the signature parsed correctly (verified live via a standalone
 * SSH-wire-format decoder + cryptography.Ed25519PublicKey.verify), but
 * verification failed because sshd would never find a valid signature
 * field there. Found + fixed 2026-07-28 by decoding a real joinBuilderFleet
 * reply end-to-end and discovering the tail was 64 bytes short of what a
 * correctly-wrapped signature field requires. */
static void hook_cert_sign_reply_callback(int32_t env) {
    uint8_t sig[64];
    /* body (up to 512B -> 1024 hex) + signature field (~4+4+11+4+64=87B -> 174 hex) + margin */
    char result[SCHNORR_MAX_CERT_BODY * 2 + 256];
    uint32_t pos;
    static const char SIG_ALGO[] = "ssh-ed25519";
    uint32_t algo_len = (uint32_t)(sizeof(SIG_ALGO) - 1);
    uint32_t inner_len = 4 + algo_len + 4 + 64;  /* algo string + sig string, both length-prefixed */
    (void)env;
    if (!parse_signature_reply(sig)) {
        callback_reply_text("error:signature_not_found_in_reply");
        return;
    }
    pos = 0;
    append_hex_bytes(result, &pos, g_pending_cert_body, g_pending_cert_body_len);
    /* outer sshString wrapping the [algo-string, sig-blob-string] pair */
    append_hex_u32(result, &pos, inner_len);
    append_hex_sshstring(result, &pos, (const uint8_t*)SIG_ALGO, algo_len);
    append_hex_sshstring(result, &pos, sig, 64);
    result[pos] = '\0';
    callback_reply_text(result);
}

static void hook_cert_sign_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:cert_sign_rejected:");
    callback_reply_text(err);
}

static void hook_pubkey_reply_callback(int32_t env) {
    char result[70];
    int i, pos;
    (void)env;
    debug("ic_schnorr: hook_pubkey_reply_callback FIRED");
    if (!parse_pubkey_reply()) {
        debug("ic_schnorr: parse_pubkey_reply FAILED");
        callback_reply_text("error:no_key_in_reply");
        return;
    }
    clear_last_error();
    pos = 0;
    for (i = 0; i < (int)g_public_key_len; i++) {
        result[pos++] = HEX[(g_public_key[i] >> 4) & 0xF];
        result[pos++] = HEX[g_public_key[i] & 0xF];
    }
    result[pos] = '\0';
    callback_reply_text(result);
}

static void hook_pubkey_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    debug("ic_schnorr: hook_pubkey_reject_callback FIRED");
    format_reject_error(err, sizeof(err), "error:schnorr_rejected:");
    set_last_error(err);
    callback_reply_text(err);
}

void ic_schnorr_set_key(int64_t key_type) {
    set_key_name_from_type(key_type);
}

/* ---- Idris2-driven deferred-sign path (joinBuilderFleet) ----
 * The cert body is loaded into BOTH g_pending_cert_body (kept verbatim for
 * the reply) and g_schnorr_message (what actually gets signed) — they are
 * the same bytes, held in two buffers because begin_sign_call signs
 * g_schnorr_message unconditionally and the reply callback needs the body
 * still available AFTER g_schnorr_message may have been reused/cleared by
 * an unrelated concurrent call (a canister processes one message at a time,
 * so "concurrent" here really means "a later message before this one's
 * callback fires" — the copy makes that safe regardless). */
void ic_schnorr_clear_pending_cert_body(void) {
    g_pending_cert_body_len = 0;
    g_pending_cert_overflow = 0;
    memset(g_pending_cert_body, 0, sizeof(g_pending_cert_body));
}

void ic_schnorr_set_pending_cert_body_byte(int64_t idx, int64_t byte) {
    if (idx >= 0 && idx < SCHNORR_MAX_CERT_BODY) {
        g_pending_cert_body[(int32_t)idx] = (uint8_t)(byte & 0xFF);
        if ((int32_t)idx + 1 > (int32_t)g_pending_cert_body_len) {
            g_pending_cert_body_len = (uint32_t)((int32_t)idx + 1);
        }
    } else {
        /* Found live 2026-07-29 (carl, mfycd principal): silently dropping
         * an out-of-range byte here produced a truncated certificate that
         * LOOKED complete (correct declared field lengths, wrong actual
         * byte count) and failed only much later, at ssh-keygen -L on the
         * CLIENT. Recording the overflow lets ic_schnorr_sign_pending_cert
         * refuse to sign a body it silently mangled, rather than signing
         * a truncated message and returning a certificate that will always
         * fail verification. */
        g_pending_cert_overflow = 1;
    }
}

int64_t ic_schnorr_sign_pending_cert(void) {
    int32_t perform_result;
    uint32_t i;
    if (g_pending_cert_overflow) {
        /* The assembled body did not fit in SCHNORR_MAX_CERT_BODY — signing
         * a truncated message would produce a certificate that verifies
         * against different bytes than SshCert.Core.assembleCertBody
         * actually emitted, indistinguishable from a real successful issue
         * until the CLIENT tries to use it. Refuse instead. */
        return (int64_t)-1;
    }
    ic_schnorr_clear_message();
    for (i = 0; i < g_pending_cert_body_len; i++) {
        ic_schnorr_set_message_byte((int64_t)i, (int64_t)g_pending_cert_body[i]);
    }
    set_key_name_from_type(0);  /* production key_1 — the fleet CA, one key */
    perform_result = begin_sign_call((int32_t)(uintptr_t)g_hook_cert_sign_reply,
                                      (int32_t)(uintptr_t)g_hook_cert_sign_reject);
    if (perform_result == 0) {
        g_pending_cert_reply = 1;
    }
    return (int64_t)perform_result;
}

int32_t ic_schnorr_consume_pending_cert_reply(void) {
    int32_t v = g_pending_cert_reply;
    g_pending_cert_reply = 0;
    return v;
}

void ic_schnorr_clear_message(void) {
    g_schnorr_message_len = 0;
    memset(g_schnorr_message, 0, sizeof(g_schnorr_message));
}

void ic_schnorr_set_message_byte(int64_t idx, int64_t byte) {
    if (idx >= 0 && idx < SCHNORR_MAX_MESSAGE) {
        g_schnorr_message[(int32_t)idx] = (uint8_t)(byte & 0xFF);
        if ((int32_t)idx + 1 > (int32_t)g_schnorr_message_len) {
            g_schnorr_message_len = (uint32_t)((int32_t)idx + 1);
        }
    }
}

int64_t ic_schnorr_sign(void) {
    return (int64_t)begin_sign_call((int32_t)(uintptr_t)g_ic_sign_reply,
                                    (int32_t)(uintptr_t)g_ic_sign_reject);
}

int64_t ic_schnorr_get_status(void) { return (int64_t)g_sign_status; }
int64_t ic_schnorr_get_signature_len(void) { return (int64_t)g_signature_len; }

int64_t ic_schnorr_get_signature_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_signature_len) {
        return (int64_t)g_signature[(int32_t)index];
    }
    return 0;
}

int64_t ic_schnorr_request_pubkey(void) {
    return (int64_t)begin_pubkey_call((int32_t)(uintptr_t)g_ic_pubkey_reply,
                                      (int32_t)(uintptr_t)g_ic_pubkey_reject);
}

int64_t ic_schnorr_get_pubkey_len(void) { return (int64_t)g_public_key_len; }

int64_t ic_schnorr_get_pubkey_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_public_key_len) {
        return (int64_t)g_public_key[(int32_t)index];
    }
    return 0;
}

int64_t ic_schnorr_get_last_error_len(void) { return (int64_t)g_last_error_len; }

int64_t ic_schnorr_get_last_error_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_last_error_len) {
        return (int64_t)(uint8_t)g_last_error[(int32_t)index];
    }
    return 0;
}

/* ---- Candid-arg parse helpers for the entry functions (arg0 = text) ---- */

static uint64_t parse_leb128_ref(const uint8_t* buf, int32_t size, int32_t* offset) {
    return parse_leb128_from(buf, size, offset);
}

static int parse_candid_text_arg0(const uint8_t* arg_buf, int32_t arg_buf_size, char* out, uint32_t out_size) {
    int32_t offset = 4;
    uint64_t type_count;
    uint64_t arg_count;
    uint64_t str_len;
    if (arg_buf_size < 7) return 0;
    if (arg_buf[0] != 'D' || arg_buf[1] != 'I' || arg_buf[2] != 'D' || arg_buf[3] != 'L') return 0;
    type_count = parse_leb128_ref(arg_buf, arg_buf_size, &offset);
    while (type_count-- > 0) {
        (void)parse_leb128_ref(arg_buf, arg_buf_size, &offset);
    }
    arg_count = parse_leb128_ref(arg_buf, arg_buf_size, &offset);
    if (arg_count < 1) return 0;
    offset += 1;
    str_len = parse_leb128_ref(arg_buf, arg_buf_size, &offset);
    if (str_len >= out_size) str_len = out_size - 1;
    if (offset + (int32_t)str_len > arg_buf_size) return 0;
    memcpy(out, arg_buf + offset, (uint32_t)str_len);
    out[str_len] = '\0';
    return 1;
}

static uint8_t hex_char_to_nibble(char c) {
    if (c >= '0' && c <= '9') return (uint8_t)(c - '0');
    if (c >= 'a' && c <= 'f') return (uint8_t)(10 + c - 'a');
    if (c >= 'A' && c <= 'F') return (uint8_t)(10 + c - 'A');
    return 0;
}

/* arg0 = hex-encoded message bytes (the SSH certificate body up to and
 * including the signature-key field, per PROTOCOL.certkeys). Always signs
 * with the production key (key_1) — the fleet has exactly one CA key. */
const char* icw_schnorr_sign_message_entry(const uint8_t* arg_buf, int32_t arg_buf_size) {
    char hex[SCHNORR_MAX_MESSAGE * 2 + 1];
    int32_t hex_len;
    int32_t i;

    if (!parse_candid_text_arg0(arg_buf, arg_buf_size, hex, sizeof(hex))) {
        return "error:missing_message";
    }
    hex_len = (int32_t)strlen(hex);
    if (hex_len == 0 || (hex_len % 2) != 0) {
        return "error:message_hex_odd_length";
    }
    if (hex_len / 2 > SCHNORR_MAX_MESSAGE) {
        return "error:message_too_long";
    }

    ic_schnorr_clear_message();
    for (i = 0; i < hex_len / 2; i++) {
        uint8_t b = (uint8_t)((hex_char_to_nibble(hex[i * 2]) << 4) | hex_char_to_nibble(hex[i * 2 + 1]));
        ic_schnorr_set_message_byte(i, b);
    }
    set_key_name_from_type(0);  /* production key_1 — the fleet CA */

    if (begin_sign_call((int32_t)(uintptr_t)g_hook_sign_reply,
                        (int32_t)(uintptr_t)g_hook_sign_reject) != 0) {
        return "error:sign_call_failed";
    }
    return 0;
}

const char* icw_schnorr_get_pubkey_entry(const uint8_t* arg_buf, int32_t arg_buf_size) {
    (void)arg_buf; (void)arg_buf_size;
    set_key_name_from_type(0);  /* production key_1 — the fleet CA */
    if (begin_pubkey_call((int32_t)(uintptr_t)g_hook_pubkey_reply,
                          (int32_t)(uintptr_t)g_hook_pubkey_reject) != 0) {
        return "error:call_perform_failed";
    }
    return 0;
}
