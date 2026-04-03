#include "ic_tecdsa.h"

#include <gmp.h>
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

typedef void (*ic_tecdsa_callback_fn)(int32_t env);

static const uint8_t MANAGEMENT_CANISTER[] = {};
static const char METHOD_SIGN[] = "sign_with_ecdsa";
static const char METHOD_PUBKEY[] = "ecdsa_public_key";
static const char KEY_PRODUCTION[] = "key_1";
static const char KEY_TEST[] = "test_key_1";
static const char KEY_LOCAL[] = "dfx_test_key";
static const char HEX[] = "0123456789abcdef";

static uint8_t g_message_hash[32];
static uint8_t g_derivation_path[128];
static uint32_t g_derivation_path_len = 0;
static uint8_t g_key_name[32];
static uint32_t g_key_name_len = 0;

static uint8_t g_signature[64];
static uint32_t g_signature_len = 0;
static int32_t g_sign_status = 0;

static uint8_t g_public_key[65];
static uint32_t g_public_key_len = 0;
static char g_evm_address_hex[67];
static int32_t g_evm_address_ready = 0;

static char g_ecdsa_last_error[256] = "";
static int32_t g_ecdsa_last_error_len = 0;

static int export_mpz_be32(mpz_t value, uint8_t out[32]) {
    uint8_t tmp[32];
    size_t count = 0;
    memset(out, 0, 32);
    memset(tmp, 0, sizeof(tmp));
    mpz_export(tmp, &count, 1, 1, 1, 0, value);
    if (count > 32) return 0;
    if (count > 0) {
        memcpy(out + (32 - count), tmp, count);
    }
    return 1;
}

static int decompress_secp256k1_pubkey(const uint8_t compressed[33], uint8_t uncompressed[65]) {
    mpz_t p, x, y2, y, exp, check;
    uint8_t x_bytes[32];
    uint8_t y_bytes[32];
    int want_odd;
    int ok = 0;

    mpz_inits(p, x, y2, y, exp, check, NULL);
    mpz_set_str(p, "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", 16);
    mpz_import(x, 32, 1, 1, 1, 0, compressed + 1);

    mpz_mul(y2, x, x);
    mpz_mod(y2, y2, p);
    mpz_mul(y2, y2, x);
    mpz_add_ui(y2, y2, 7);
    mpz_mod(y2, y2, p);

    mpz_add_ui(exp, p, 1);
    mpz_tdiv_q_2exp(exp, exp, 2);
    mpz_powm(y, y2, exp, p);

    mpz_mul(check, y, y);
    mpz_mod(check, check, p);
    if (mpz_cmp(check, y2) != 0) goto cleanup;

    want_odd = (compressed[0] == 0x03);
    if ((mpz_tstbit(y, 0) != 0) != want_odd) {
        mpz_sub(y, p, y);
    }

    if (!export_mpz_be32(x, x_bytes)) goto cleanup;
    if (!export_mpz_be32(y, y_bytes)) goto cleanup;

    uncompressed[0] = 0x04;
    memcpy(uncompressed + 1, x_bytes, 32);
    memcpy(uncompressed + 33, y_bytes, 32);
    ok = 1;

cleanup:
    mpz_clears(p, x, y2, y, exp, check, NULL);
    return ok;
}

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

static uint32_t encode_leb128_signed(uint8_t* buf, int64_t value) {
    uint32_t len = 0;
    int more = 1;
    while (more) {
        uint8_t byte = (uint8_t)(value & 0x7F);
        value >>= 7;
        if ((value == 0 && (byte & 0x40) == 0) ||
            (value == -1 && (byte & 0x40) != 0)) {
            more = 0;
        } else {
            byte |= 0x80;
        }
        buf[len++] = byte;
    }
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
    g_ecdsa_last_error[0] = '\0';
    g_ecdsa_last_error_len = 0;
}

static void set_last_error(const char* msg) {
    uint32_t len = (uint32_t)strlen(msg);
    if (len >= sizeof(g_ecdsa_last_error)) len = sizeof(g_ecdsa_last_error) - 1;
    memcpy(g_ecdsa_last_error, msg, len);
    g_ecdsa_last_error[len] = '\0';
    g_ecdsa_last_error_len = (int32_t)len;
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

static uint32_t encode_sign_request(uint8_t* buf) {
    uint32_t pos = 0;

    buf[pos++] = 'D';
    buf[pos++] = 'I';
    buf[pos++] = 'D';
    buf[pos++] = 'L';

    pos += encode_leb128_unsigned(buf + pos, 4);

    buf[pos++] = 0x6D;
    pos += encode_leb128_signed(buf + pos, -19);

    buf[pos++] = 0x6C;
    pos += encode_leb128_unsigned(buf + pos, 2);
    pos += encode_leb128_unsigned(buf + pos, 0x8DF6D6D2UL & 0xFFFFFFFF);
    pos += encode_leb128_unsigned(buf + pos, 2);
    pos += encode_leb128_unsigned(buf + pos, 0xD3C5CC99UL & 0xFFFFFFFF);
    pos += encode_leb128_signed(buf + pos, -15);

    buf[pos++] = 0x6B;
    pos += encode_leb128_unsigned(buf + pos, 1);
    pos += encode_leb128_unsigned(buf + pos, 0xCFD1E33BUL & 0xFFFFFFFF);
    pos += encode_leb128_signed(buf + pos, -17);

    buf[pos++] = 0x6C;
    pos += encode_leb128_unsigned(buf + pos, 3);
    pos += encode_leb128_unsigned(buf + pos, 0x8F3AB8BAUL & 0xFFFFFFFF);
    pos += encode_leb128_unsigned(buf + pos, 0);
    pos += encode_leb128_unsigned(buf + pos, 0x931C2F2AUL & 0xFFFFFFFF);
    pos += encode_leb128_unsigned(buf + pos, 1);
    pos += encode_leb128_unsigned(buf + pos, 0xCE890A2BUL & 0xFFFFFFFF);
    pos += encode_leb128_signed(buf + pos, -19);

    pos += encode_leb128_unsigned(buf + pos, 1);
    pos += encode_leb128_unsigned(buf + pos, 3);

    {
        uint32_t path_count = g_derivation_path_len / 4;
        uint32_t i;
        pos += encode_leb128_unsigned(buf + pos, path_count);
        for (i = 0; i < path_count; i++) {
            pos += encode_leb128_unsigned(buf + pos, 4);
            memcpy(buf + pos, g_derivation_path + i * 4, 4);
            pos += 4;
        }
    }

    pos += encode_leb128_unsigned(buf + pos, 0);
    pos += encode_leb128_unsigned(buf + pos, g_key_name_len);
    memcpy(buf + pos, g_key_name, g_key_name_len);
    pos += g_key_name_len;

    pos += encode_leb128_unsigned(buf + pos, 32);
    memcpy(buf + pos, g_message_hash, 32);
    pos += 32;

    return pos;
}

static uint32_t encode_pubkey_request(uint8_t* buf) {
    static const uint8_t HEADER[] = {
        'D', 'I', 'D', 'L',
        0x06,
        0x6B, 0x01, 0x9A, 0xDE, 0xE4, 0xEA, 0x01, 0x7F,
        0x6C, 0x02, 0xCB, 0xE4, 0xFD, 0xC7, 0x04, 0x71, 0xAF, 0x99, 0xE1, 0xF2, 0x04, 0x00,
        0x6E, 0x68,
        0x6D, 0x7B,
        0x6D, 0x03,
        0x6C, 0x03, 0xBB, 0xEB, 0xAD, 0xFF, 0x03, 0x01, 0xB3, 0xC4, 0xB1, 0xF2, 0x04, 0x02, 0xAD, 0xA8, 0xB2, 0xB1, 0x05, 0x04,
        0x01, 0x05
    };
    uint32_t pos = 0;
    memcpy(buf, HEADER, sizeof(HEADER));
    pos = (uint32_t)sizeof(HEADER);

    pos += encode_leb128_unsigned(buf + pos, g_key_name_len);
    memcpy(buf + pos, g_key_name, g_key_name_len);
    pos += g_key_name_len;
    buf[pos++] = 0x00;
    buf[pos++] = 0x00;
    buf[pos++] = 0x00;
    return pos;
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

static void keccak_f1600(uint64_t st[25]) {
    static const uint64_t rc[24] = {
        0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808aULL,
        0x8000000080008000ULL, 0x000000000000808bULL, 0x0000000080000001ULL,
        0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008aULL,
        0x0000000000000088ULL, 0x0000000080008009ULL, 0x000000008000000aULL,
        0x000000008000808bULL, 0x800000000000008bULL, 0x8000000000008089ULL,
        0x8000000000008003ULL, 0x8000000000008002ULL, 0x8000000000000080ULL,
        0x000000000000800aULL, 0x800000008000000aULL, 0x8000000080008081ULL,
        0x8000000000008080ULL, 0x0000000080000001ULL, 0x8000000080008008ULL
    };
    static const int piln[24] = {10,7,11,17,18,3,5,16,8,21,24,4,15,23,19,13,12,2,20,14,22,9,6,1};
    static const int rotc[24] = {1,3,6,10,15,21,28,36,45,55,2,14,27,41,56,8,25,43,62,18,39,61,20,44};
    int round;
    for (round = 0; round < 24; round++) {
        uint64_t c[5];
        uint64_t d[5];
        uint64_t tmp;
        int i;
        for (i = 0; i < 5; i++) {
            c[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];
        }
        for (i = 0; i < 5; i++) {
            d[i] = c[(i + 4) % 5] ^ ((c[(i + 1) % 5] << 1) | (c[(i + 1) % 5] >> 63));
            st[i] ^= d[i];
            st[i + 5] ^= d[i];
            st[i + 10] ^= d[i];
            st[i + 15] ^= d[i];
            st[i + 20] ^= d[i];
        }
        tmp = st[1];
        for (i = 0; i < 24; i++) {
            uint64_t t2 = st[piln[i]];
            st[piln[i]] = (tmp << rotc[i]) | (tmp >> (64 - rotc[i]));
            tmp = t2;
        }
        for (i = 0; i < 25; i += 5) {
            uint64_t t0 = st[i];
            uint64_t t1 = st[i + 1];
            uint64_t t2 = st[i + 2];
            uint64_t t3 = st[i + 3];
            uint64_t t4 = st[i + 4];
            st[i]     = t0 ^ ((~t1) & t2);
            st[i + 1] = t1 ^ ((~t2) & t3);
            st[i + 2] = t2 ^ ((~t3) & t4);
            st[i + 3] = t3 ^ ((~t4) & t0);
            st[i + 4] = t4 ^ ((~t0) & t1);
        }
        st[0] ^= rc[round];
    }
}

static void keccak256(const uint8_t* data, uint32_t len, uint8_t out[32]) {
    uint64_t st[25];
    uint8_t last_block[136];
    uint32_t rate = 136;
    uint32_t offset = 0;
    uint32_t i;

    memset(st, 0, sizeof(st));
    while (offset + rate <= len) {
        for (i = 0; i < rate / 8; i++) {
            uint64_t block = 0;
            int b;
            for (b = 0; b < 8; b++) {
                block |= ((uint64_t)data[offset + i * 8 + (uint32_t)b]) << (b * 8);
            }
            st[i] ^= block;
        }
        keccak_f1600(st);
        offset += rate;
    }

    memset(last_block, 0, sizeof(last_block));
    memcpy(last_block, data + offset, len - offset);
    last_block[len - offset] = 0x01;
    last_block[rate - 1] |= 0x80;
    for (i = 0; i < rate / 8; i++) {
        uint64_t block = 0;
        int b;
        for (b = 0; b < 8; b++) {
            block |= ((uint64_t)last_block[i * 8 + (uint32_t)b]) << (b * 8);
        }
        st[i] ^= block;
    }
    keccak_f1600(st);

    for (i = 0; i < 4; i++) {
        int b;
        for (b = 0; b < 8; b++) {
            out[i * 8 + (uint32_t)b] = (uint8_t)(st[i] >> (b * 8));
        }
    }
}

void ic_tecdsa_derive_evm_address(void) {
    if (g_public_key_len == 65 && g_public_key[0] == 0x04) {
        uint8_t hash[32];
        int i;
        keccak256(g_public_key + 1, 64, hash);
        g_evm_address_hex[0] = '0';
        g_evm_address_hex[1] = 'x';
        for (i = 0; i < 20; i++) {
            g_evm_address_hex[2 + i * 2] = HEX[(hash[12 + i] >> 4) & 0xF];
            g_evm_address_hex[3 + i * 2] = HEX[hash[12 + i] & 0xF];
        }
        g_evm_address_hex[42] = '\0';
        g_evm_address_ready = 1;
        return;
    }
    if (g_public_key_len == 33 && (g_public_key[0] == 0x02 || g_public_key[0] == 0x03)) {
        uint8_t uncompressed[65];
        uint8_t hash[32];
        int i;
        if (!decompress_secp256k1_pubkey(g_public_key, uncompressed)) {
            g_evm_address_ready = 0;
            return;
        }
        keccak256(uncompressed + 1, 64, hash);
        g_evm_address_hex[0] = '0';
        g_evm_address_hex[1] = 'x';
        for (i = 0; i < 20; i++) {
            g_evm_address_hex[2 + i * 2] = HEX[(hash[12 + i] >> 4) & 0xF];
            g_evm_address_hex[3 + i * 2] = HEX[hash[12 + i] & 0xF];
        }
        g_evm_address_hex[42] = '\0';
        g_evm_address_ready = 1;
        return;
    }
    g_evm_address_ready = 0;
}

static int parse_signature_reply(uint8_t sig_out[64]) {
    uint8_t reply[256];
    int32_t reply_size = ic0_msg_arg_data_size();
    int32_t off;
    if (reply_size > (int32_t)sizeof(reply)) reply_size = (int32_t)sizeof(reply);
    if (reply_size <= 0) return 0;
    ic0_msg_arg_data_copy((int32_t)(uintptr_t)reply, 0, reply_size);

    if (reply_size >= 82 && reply[17] == 0x40 && 17 + 65 <= reply_size) {
        memcpy(sig_out, reply + 18, 64);
        return 1;
    }
    for (off = 0; off < reply_size - 64; off++) {
        if (reply[off] == 0x40 && off + 65 <= reply_size) {
            memcpy(sig_out, reply + off + 1, 64);
            return 1;
        }
    }
    return 0;
}

static int parse_pubkey_reply(void) {
    uint8_t reply[512];
    int32_t reply_size = ic0_msg_arg_data_size();
    int32_t offset;
    if (reply_size > (int32_t)sizeof(reply)) reply_size = (int32_t)sizeof(reply);
    if (reply_size <= 0) return 0;
    ic0_msg_arg_data_copy((int32_t)(uintptr_t)reply, 0, reply_size);

    for (offset = 0; offset < reply_size - 34; offset++) {
        if (reply[offset] == 33 && offset + 34 <= reply_size &&
            (reply[offset + 1] == 0x02 || reply[offset + 1] == 0x03)) {
            g_public_key_len = 33;
            memcpy(g_public_key, reply + offset + 1, 33);
            ic_tecdsa_derive_evm_address();
            return 1;
        }
        if (reply[offset] == 65 && offset + 66 <= reply_size && reply[offset + 1] == 0x04) {
            g_public_key_len = 65;
            memcpy(g_public_key, reply + offset + 1, 65);
            ic_tecdsa_derive_evm_address();
            return 1;
        }
    }
    return 0;
}

static int32_t begin_sign_call(int32_t reply_idx, int32_t reject_idx) {
    uint8_t request[512];
    uint32_t request_len = encode_sign_request(request);
    clear_last_error();
    g_sign_status = 0;
    g_signature_len = 0;

    ic0_call_new((int32_t)(uintptr_t)MANAGEMENT_CANISTER, 0,
                 (int32_t)(uintptr_t)METHOD_SIGN, (int32_t)(sizeof(METHOD_SIGN) - 1),
                 reply_idx, 0, reject_idx, 0);
    ic0_call_data_append((int32_t)(uintptr_t)request, (int32_t)request_len);
    ic0_call_cycles_add128(0, 25000000000ULL);
    return ic0_call_perform();
}

static int32_t begin_pubkey_call(int32_t reply_idx, int32_t reject_idx) {
    uint8_t request[256];
    uint32_t request_len = encode_pubkey_request(request);
    clear_last_error();

    ic0_call_new((int32_t)(uintptr_t)MANAGEMENT_CANISTER, 0,
                 (int32_t)(uintptr_t)METHOD_PUBKEY, (int32_t)(sizeof(METHOD_PUBKEY) - 1),
                 reply_idx, 0, reject_idx, 0);
    ic0_call_data_append((int32_t)(uintptr_t)request, (int32_t)request_len);
    ic0_call_cycles_add128(0, 25000000000ULL);
    return ic0_call_perform();
}

static void ic_tecdsa_sign_reply_callback(int32_t env);
static void ic_tecdsa_sign_reject_callback(int32_t env);
static void ic_tecdsa_pubkey_reply_callback(int32_t env);
static void ic_tecdsa_pubkey_reject_callback(int32_t env);
static void hook_sign_reply_callback(int32_t env);
static void hook_sign_reject_callback(int32_t env);
static void hook_pubkey_reply_callback(int32_t env);
static void hook_pubkey_reject_callback(int32_t env);

static ic_tecdsa_callback_fn g_ic_sign_reply = &ic_tecdsa_sign_reply_callback;
static ic_tecdsa_callback_fn g_ic_sign_reject = &ic_tecdsa_sign_reject_callback;
static ic_tecdsa_callback_fn g_ic_pubkey_reply = &ic_tecdsa_pubkey_reply_callback;
static ic_tecdsa_callback_fn g_ic_pubkey_reject = &ic_tecdsa_pubkey_reject_callback;
static ic_tecdsa_callback_fn g_hook_sign_reply = &hook_sign_reply_callback;
static ic_tecdsa_callback_fn g_hook_sign_reject = &hook_sign_reject_callback;
static ic_tecdsa_callback_fn g_hook_pubkey_reply = &hook_pubkey_reply_callback;
static ic_tecdsa_callback_fn g_hook_pubkey_reject = &hook_pubkey_reject_callback;

static void ic_tecdsa_sign_reply_callback(int32_t env) {
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

static void ic_tecdsa_sign_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:sign_rejected:");
    set_last_error(err);
    g_sign_status = -1;
}

static void ic_tecdsa_pubkey_reply_callback(int32_t env) {
    (void)env;
    if (!parse_pubkey_reply()) {
        set_last_error("error:no_key_in_reply");
    } else {
        clear_last_error();
    }
}

static void ic_tecdsa_pubkey_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:ecdsa_rejected:");
    set_last_error(err);
    g_public_key_len = 0;
    g_evm_address_ready = 0;
}

static void hook_sign_reply_callback(int32_t env) {
    uint8_t sig[64];
    char result[140];
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

    memcpy(result, "r=", 2);
    pos = 2;
    for (i = 0; i < 32; i++) {
        result[pos++] = HEX[(sig[i] >> 4) & 0xF];
        result[pos++] = HEX[sig[i] & 0xF];
    }
    result[pos++] = ',';
    result[pos++] = 's';
    result[pos++] = '=';
    for (i = 0; i < 32; i++) {
        result[pos++] = HEX[(sig[32 + i] >> 4) & 0xF];
        result[pos++] = HEX[sig[32 + i] & 0xF];
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

static void hook_pubkey_reply_callback(int32_t env) {
    (void)env;
    if (!parse_pubkey_reply()) {
        callback_reply_text("error:no_key_in_reply");
        return;
    }
    clear_last_error();
    if (ic_tecdsa_get_evm_address_len() > 0) {
        callback_reply_text(g_evm_address_hex);
    } else {
        callback_reply_text("error:address_derivation_failed");
    }
}

static void hook_pubkey_reject_callback(int32_t env) {
    char err[256];
    (void)env;
    format_reject_error(err, sizeof(err), "error:ecdsa_rejected:");
    set_last_error(err);
    callback_reply_text(err);
}

void ic_tecdsa_set_message_hash(uint32_t h0, uint32_t h1, uint32_t h2, uint32_t h3,
                                uint32_t h4, uint32_t h5, uint32_t h6, uint32_t h7) {
    uint32_t* hash32 = (uint32_t*)g_message_hash;
    hash32[0] = h0; hash32[1] = h1; hash32[2] = h2; hash32[3] = h3;
    hash32[4] = h4; hash32[5] = h5; hash32[6] = h6; hash32[7] = h7;
}

void ic_tecdsa_set_key(int64_t key_type) {
    set_key_name_from_type(key_type);
}

void ic_tecdsa_set_derivation_segment(int64_t index, uint32_t segment) {
    if (index >= 0 && index < 5) {
        uint32_t* path32 = (uint32_t*)g_derivation_path;
        path32[(int32_t)index] = segment;
        if ((uint32_t)(((int32_t)index + 1) * 4) > g_derivation_path_len) {
            g_derivation_path_len = (uint32_t)(((int32_t)index + 1) * 4);
        }
    }
}

void ic_tecdsa_clear_path(void) {
    g_derivation_path_len = 0;
    memset(g_derivation_path, 0, sizeof(g_derivation_path));
}

int64_t ic_tecdsa_sign(void) {
    return (int64_t)begin_sign_call((int32_t)(uintptr_t)g_ic_sign_reply,
                                    (int32_t)(uintptr_t)g_ic_sign_reject);
}

int64_t ic_tecdsa_get_status(void) {
    return (int64_t)g_sign_status;
}

int64_t ic_tecdsa_get_signature_len(void) {
    return (int64_t)g_signature_len;
}

int64_t ic_tecdsa_get_signature_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_signature_len) {
        return (int64_t)g_signature[(int32_t)index];
    }
    return 0;
}

int64_t ic_tecdsa_get_pubkey_len(void) {
    return (int64_t)g_public_key_len;
}

int64_t ic_tecdsa_get_pubkey_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_public_key_len) {
        return (int64_t)g_public_key[(int32_t)index];
    }
    return 0;
}

int64_t ic_tecdsa_request_pubkey(void) {
    return (int64_t)begin_pubkey_call((int32_t)(uintptr_t)g_ic_pubkey_reply,
                                      (int32_t)(uintptr_t)g_ic_pubkey_reject);
}

int64_t ic_tecdsa_get_evm_address_len(void) {
    if (!g_evm_address_ready) ic_tecdsa_derive_evm_address();
    return g_evm_address_ready ? (int64_t)strlen(g_evm_address_hex) : 0;
}

int64_t ic_tecdsa_get_evm_address_byte(int64_t index) {
    uint32_t len = (uint32_t)strlen(g_evm_address_hex);
    if (!g_evm_address_ready) return 0;
    if (index >= 0 && index < (int64_t)len) return (int64_t)(uint8_t)g_evm_address_hex[(int32_t)index];
    return 0;
}

int64_t ic_tecdsa_get_last_error_len(void) {
    return (int64_t)g_ecdsa_last_error_len;
}

int64_t ic_tecdsa_get_last_error_byte(int64_t index) {
    if (index >= 0 && index < (int64_t)g_ecdsa_last_error_len) {
        return (int64_t)(uint8_t)g_ecdsa_last_error[(int32_t)index];
    }
    return 0;
}

static int parse_candid_text_arg0(const uint8_t* arg_buf, int32_t arg_buf_size, char* out, uint32_t out_size) {
    int32_t offset = 4;
    uint64_t type_count;
    uint64_t arg_count;
    uint64_t str_len;
    if (arg_buf_size < 7) return 0;
    if (arg_buf[0] != 'D' || arg_buf[1] != 'I' || arg_buf[2] != 'D' || arg_buf[3] != 'L') return 0;
    type_count = parse_leb128_from(arg_buf, arg_buf_size, &offset);
    while (type_count-- > 0) {
        (void)parse_leb128_from(arg_buf, arg_buf_size, &offset);
    }
    arg_count = parse_leb128_from(arg_buf, arg_buf_size, &offset);
    if (arg_count < 1) return 0;
    offset += 1;
    str_len = parse_leb128_from(arg_buf, arg_buf_size, &offset);
    if (str_len >= out_size) str_len = out_size - 1;
    if (offset + (int32_t)str_len > arg_buf_size) return 0;
    memcpy(out, arg_buf + offset, (uint32_t)str_len);
    out[str_len] = '\0';
    return 1;
}

static int parse_candid_nat_arg0(const uint8_t* arg_buf, int32_t arg_buf_size, int64_t* out) {
    int32_t offset = 4;
    uint64_t type_count;
    uint64_t arg_count;
    if (arg_buf_size < 7) return 0;
    if (arg_buf[0] != 'D' || arg_buf[1] != 'I' || arg_buf[2] != 'D' || arg_buf[3] != 'L') return 0;
    type_count = parse_leb128_from(arg_buf, arg_buf_size, &offset);
    while (type_count-- > 0) {
        (void)parse_leb128_from(arg_buf, arg_buf_size, &offset);
    }
    arg_count = parse_leb128_from(arg_buf, arg_buf_size, &offset);
    if (arg_count < 1) return 0;
    offset += 1;
    *out = (int64_t)parse_leb128_from(arg_buf, arg_buf_size, &offset);
    return 1;
}

const char* icw_tecdsa_get_evm_address_entry(const uint8_t* arg_buf, int32_t arg_buf_size) {
    int64_t key_type = 0;
    if (ic_tecdsa_get_pubkey_len() > 0) {
        return g_evm_address_ready ? g_evm_address_hex : "error:address_derivation_failed";
    }
    if (ic_tecdsa_get_last_error_len() > 0) {
        return g_ecdsa_last_error;
    }
    if (!parse_candid_nat_arg0(arg_buf, arg_buf_size, &key_type)) {
        key_type = 0;
    }
    set_key_name_from_type(key_type);
    if (begin_pubkey_call((int32_t)(uintptr_t)g_hook_pubkey_reply,
                          (int32_t)(uintptr_t)g_hook_pubkey_reject) != 0) {
        return "error:call_perform_failed";
    }
    return 0;
}

static uint8_t hex_char_to_nibble(char c) {
    if (c >= '0' && c <= '9') return (uint8_t)(c - '0');
    if (c >= 'a' && c <= 'f') return (uint8_t)(10 + c - 'a');
    if (c >= 'A' && c <= 'F') return (uint8_t)(10 + c - 'A');
    return 0;
}

const char* icw_tecdsa_sign_hash_entry(const uint8_t* arg_buf, int32_t arg_buf_size) {
    char hash_hex[130];
    int i;
    const char* h;
    if (!parse_candid_text_arg0(arg_buf, arg_buf_size, hash_hex, sizeof(hash_hex))) {
        return "error:missing_hash";
    }
    h = hash_hex;
    if (h[0] == '0' && (h[1] == 'x' || h[1] == 'X')) h += 2;
    if ((int32_t)strlen(h) < 64) {
        return "error:hash_too_short";
    }
    for (i = 0; i < 32; i++) {
        g_message_hash[i] = (uint8_t)((hex_char_to_nibble(h[i * 2]) << 4) | hex_char_to_nibble(h[i * 2 + 1]));
    }
    set_key_name_from_type(1);
    g_derivation_path_len = 0;
    memset(g_derivation_path, 0, sizeof(g_derivation_path));
    if (begin_sign_call((int32_t)(uintptr_t)g_hook_sign_reply,
                        (int32_t)(uintptr_t)g_hook_sign_reject) != 0) {
        return "error:sign_call_failed";
    }
    return 0;
}
