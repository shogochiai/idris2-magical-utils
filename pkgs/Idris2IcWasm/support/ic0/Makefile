# Build libic0.dylib for macOS local testing
#
# Contains no-op stubs for sql_ffi_* (SQLite FFI).
# ic_ffi_bridge.c, ic0_stubs.c, wasi_stubs.c, canister_entry.c are excluded
# because they depend on ic0_* WASM imports unavailable on macOS.

CC ?= clang
CFLAGS = -O2 -fPIC

# macOS-compilable sources only (no ic0 WASM dependencies)
SRCS = sql_ffi_stub.c

libic0.dylib: $(SRCS)
	$(CC) $(CFLAGS) -shared -o $@ $^

clean:
	rm -f libic0.dylib

.PHONY: clean
