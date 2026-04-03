#ifndef ICW_TECDSA_H
#define ICW_TECDSA_H

#include <stdint.h>

void ic_tecdsa_set_message_hash(uint32_t h0, uint32_t h1, uint32_t h2, uint32_t h3,
                                uint32_t h4, uint32_t h5, uint32_t h6, uint32_t h7);
void ic_tecdsa_set_key(int64_t key_type);
void ic_tecdsa_set_derivation_segment(int64_t index, uint32_t segment);
void ic_tecdsa_clear_path(void);
int64_t ic_tecdsa_sign(void);
int64_t ic_tecdsa_get_status(void);
int64_t ic_tecdsa_get_signature_len(void);
int64_t ic_tecdsa_get_signature_byte(int64_t index);
int64_t ic_tecdsa_get_pubkey_len(void);
int64_t ic_tecdsa_get_pubkey_byte(int64_t index);
int64_t ic_tecdsa_request_pubkey(void);
void ic_tecdsa_derive_evm_address(void);
int64_t ic_tecdsa_get_evm_address_len(void);
int64_t ic_tecdsa_get_evm_address_byte(int64_t index);
int64_t ic_tecdsa_get_last_error_len(void);
int64_t ic_tecdsa_get_last_error_byte(int64_t index);

const char* icw_tecdsa_get_evm_address_entry(const uint8_t* arg_buf, int32_t arg_buf_size);
const char* icw_tecdsa_sign_hash_entry(const uint8_t* arg_buf, int32_t arg_buf_size);

#endif
