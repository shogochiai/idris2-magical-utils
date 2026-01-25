/*
 * IC0 System API declarations for Internet Computer canisters
 * These functions are provided by the IC runtime at canister execution time
 */
#ifndef IC0_H
#define IC0_H

#include <stdint.h>

/* Message API */
__attribute__((import_module("ic0"), import_name("msg_reply")))
void ic0_msg_reply(void);

__attribute__((import_module("ic0"), import_name("msg_reply_data_append")))
void ic0_msg_reply_data_append(const void* src, uint32_t size);

__attribute__((import_module("ic0"), import_name("msg_arg_data_size")))
uint32_t ic0_msg_arg_data_size(void);

__attribute__((import_module("ic0"), import_name("msg_arg_data_copy")))
void ic0_msg_arg_data_copy(void* dst, uint32_t offset, uint32_t size);

__attribute__((import_module("ic0"), import_name("msg_caller_size")))
uint32_t ic0_msg_caller_size(void);

__attribute__((import_module("ic0"), import_name("msg_caller_copy")))
void ic0_msg_caller_copy(void* dst, uint32_t offset, uint32_t size);

__attribute__((import_module("ic0"), import_name("msg_reject")))
void ic0_msg_reject(const void* src, uint32_t size);

__attribute__((import_module("ic0"), import_name("msg_reject_code")))
uint32_t ic0_msg_reject_code(void);

/* Debug API */
__attribute__((import_module("ic0"), import_name("debug_print")))
void ic0_debug_print(const void* src, uint32_t size);

__attribute__((import_module("ic0"), import_name("trap")))
void ic0_trap(const void* src, uint32_t size);

/* Canister status */
__attribute__((import_module("ic0"), import_name("canister_self_size")))
uint32_t ic0_canister_self_size(void);

__attribute__((import_module("ic0"), import_name("canister_self_copy")))
void ic0_canister_self_copy(void* dst, uint32_t offset, uint32_t size);

/* Stable memory */
__attribute__((import_module("ic0"), import_name("stable_size")))
uint32_t ic0_stable_size(void);

__attribute__((import_module("ic0"), import_name("stable_grow")))
uint32_t ic0_stable_grow(uint32_t new_pages);

__attribute__((import_module("ic0"), import_name("stable_read")))
void ic0_stable_read(void* dst, uint32_t offset, uint32_t size);

__attribute__((import_module("ic0"), import_name("stable_write")))
void ic0_stable_write(uint32_t offset, const void* src, uint32_t size);

/* Time */
__attribute__((import_module("ic0"), import_name("time")))
uint64_t ic0_time(void);

#endif /* IC0_H */
