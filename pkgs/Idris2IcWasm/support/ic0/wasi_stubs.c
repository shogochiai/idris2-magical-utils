/*
 * WASI Stubs for ICP Canister
 *
 * The IC runtime doesn't support WASI, so we stub out these functions.
 * These are needed because Emscripten's STANDALONE_WASM mode still
 * references some WASI functions for things like stdio.
 */
#include <stdint.h>

/* WASI errno values */
#define WASI_ERRNO_SUCCESS 0
#define WASI_ERRNO_BADF 8
#define WASI_ERRNO_NOSYS 52

/* fd_close - close a file descriptor (no-op on IC) */
__attribute__((export_name("fd_close")))
int32_t fd_close(int32_t fd) {
    (void)fd;
    return WASI_ERRNO_BADF;
}

/* fd_write - write to a file descriptor (no-op on IC) */
__attribute__((export_name("fd_write")))
int32_t fd_write(int32_t fd, int32_t iovs, int32_t iovs_len, int32_t nwritten) {
    (void)fd;
    (void)iovs;
    (void)iovs_len;
    /* Write 0 bytes */
    if (nwritten) {
        *((int32_t*)nwritten) = 0;
    }
    return WASI_ERRNO_SUCCESS;
}

/* fd_seek - seek in a file descriptor (no-op on IC) */
__attribute__((export_name("fd_seek")))
int32_t fd_seek(int32_t fd, int64_t offset, int32_t whence, int32_t newoffset) {
    (void)fd;
    (void)offset;
    (void)whence;
    (void)newoffset;
    return WASI_ERRNO_BADF;
}

/* fd_read - read from a file descriptor (no-op on IC) */
__attribute__((export_name("fd_read")))
int32_t fd_read(int32_t fd, int32_t iovs, int32_t iovs_len, int32_t nread) {
    (void)fd;
    (void)iovs;
    (void)iovs_len;
    if (nread) {
        *((int32_t*)nread) = 0;
    }
    return WASI_ERRNO_BADF;
}

/* environ_sizes_get - get environment variable sizes (no-op on IC) */
__attribute__((export_name("environ_sizes_get")))
int32_t environ_sizes_get(int32_t environ_count, int32_t environ_buf_size) {
    if (environ_count) *((int32_t*)environ_count) = 0;
    if (environ_buf_size) *((int32_t*)environ_buf_size) = 0;
    return WASI_ERRNO_SUCCESS;
}

/* environ_get - get environment variables (no-op on IC) */
__attribute__((export_name("environ_get")))
int32_t environ_get(int32_t environ, int32_t environ_buf) {
    (void)environ;
    (void)environ_buf;
    return WASI_ERRNO_SUCCESS;
}

/* proc_exit - exit the process (trap on IC) */
__attribute__((export_name("proc_exit")))
void proc_exit(int32_t code) {
    (void)code;
    /* IC will trap if we try to call an undefined function */
    /* For now, just return - the IC handles process lifecycle */
}
