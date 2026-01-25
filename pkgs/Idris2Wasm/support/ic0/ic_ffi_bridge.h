/*
 * IC FFI Bridge - Header
 *
 * Generic C ↔ Idris2 communication for IC canisters.
 */
#ifndef IC_FFI_BRIDGE_H
#define IC_FFI_BRIDGE_H

#include <stdint.h>

/* =============================================================================
 * FFI Bridge: Argument/Result Passing
 *
 * Idris2 FFI declarations:
 *   %foreign "C:ic_ffi_get_arg,libic0"
 *   getArg : Int -> Int
 *
 *   %foreign "C:ic_ffi_set_result,libic0"
 *   setResult : Int -> PrimIO ()
 * ============================================================================= */

/* Called from Idris2 via %foreign */
int64_t ic_ffi_get_arg(int64_t index);
void ic_ffi_set_result(int64_t value);

/* Called from C (canister_entry.c) */
void ic_ffi_c_set_arg(int32_t index, int64_t value);
int64_t ic_ffi_c_get_result(void);
void ic_ffi_reset(void);

/* =============================================================================
 * Candid Buffer: Idris2 writes Candid for C to send
 *
 * Idris2 FFI declarations:
 *   %foreign "C:ic_candid_write_byte,libic0"
 *   candidWriteByte : Int -> Int -> PrimIO ()
 *
 *   %foreign "C:ic_candid_set_len,libic0"
 *   candidSetLen : Int -> PrimIO ()
 *
 *   %foreign "C:ic_candid_clear,libic0"
 *   candidClear : PrimIO ()
 * ============================================================================= */

/* Called from Idris2 via %foreign */
void ic_candid_write_byte(int64_t index, int64_t byte);
void ic_candid_set_len(int64_t len);
void ic_candid_clear(void);

/* Called from C */
uint8_t* ic_candid_c_get_buf(void);
int32_t ic_candid_c_get_len(void);

/* =============================================================================
 * JSON Buffer: C sets JSON for Idris2 to parse
 *
 * Idris2 FFI declarations:
 *   %foreign "C:ic_json_get_len,libic0"
 *   jsonGetLen : PrimIO Int
 *
 *   %foreign "C:ic_json_get_byte,libic0"
 *   jsonGetByte : Int -> PrimIO Int
 * ============================================================================= */

/* Called from C */
void ic_json_c_set(const char* json);

/* Called from Idris2 via %foreign */
int64_t ic_json_get_len(void);
int64_t ic_json_get_byte(int64_t index);

/* =============================================================================
 * String Buffer: Idris2 writes string for C (debug, etc.)
 *
 * Idris2 FFI declarations:
 *   %foreign "C:ic_str_write_byte,libic0"
 *   strWriteByte : Int -> Int -> PrimIO ()
 *
 *   %foreign "C:ic_str_set_len,libic0"
 *   strSetLen : Int -> PrimIO ()
 * ============================================================================= */

/* Called from Idris2 via %foreign */
void ic_str_write_byte(int64_t index, int64_t byte);
void ic_str_set_len(int64_t len);

/* Called from C */
const char* ic_str_c_get(void);
int32_t ic_str_c_get_len(void);

#endif /* IC_FFI_BRIDGE_H */
