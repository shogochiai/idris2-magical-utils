#!/bin/bash
# Shared Idris2IcWasm canister build pipeline.
# Caller scripts set variables/functions below, then source this file.

ensure_mini_gmp() {
    if [ -f "$MINI_GMP/mini-gmp.c" ] && [ -f "$MINI_GMP/gmp.h" ]; then
        return
    fi
    echo "Preparing mini-gmp headers..."
    mkdir -p "$MINI_GMP"
    curl -sLo "$MINI_GMP/mini-gmp.c" https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.c
    curl -sLo "$MINI_GMP/mini-gmp.h" https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.h
    cat > "$MINI_GMP/gmp.h" << 'GMPEOF'
#ifndef GMP_WRAPPER_H
#define GMP_WRAPPER_H
#include "mini-gmp.h"
#include <stdarg.h>
static inline void mpz_inits(mpz_t x, ...) {
    va_list ap; va_start(ap, x); mpz_init(x);
    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_init(x);
    va_end(ap);
}
static inline void mpz_clears(mpz_t x, ...) {
    va_list ap; va_start(ap, x); mpz_clear(x);
    while ((x = va_arg(ap, mpz_ptr)) != NULL) mpz_clear(x);
    va_end(ap);
}
#endif
GMPEOF
}

ensure_refc_runtime() {
    if [ ! -f "$REFC_SRC/runtime.c" ]; then
        echo "Downloading RefC runtime sources..."
        mkdir -p "$REFC_SRC"
        for f in memoryManagement.c runtime.c stringOps.c mathFunctions.c casts.c clock.c buffer.c prim.c refc_util.c; do
            curl -sLo "$REFC_SRC/$f" "https://raw.githubusercontent.com/idris-lang/Idris2/main/support/refc/$f"
        done
        for f in runtime.h cBackend.h datatypes.h _datatypes.h refc_util.h mathFunctions.h memoryManagement.h stringOps.h casts.h clock.h buffer.h prim.h threads.h; do
            curl -sLo "$REFC_SRC/$f" "https://raw.githubusercontent.com/idris-lang/Idris2/main/support/refc/$f"
        done
        for f in idris_support.c idris_file.c idris_directory.c idris_util.c idris_support.h idris_file.h idris_directory.h idris_util.h; do
            curl -sLo "$REFC_SRC/$f" "https://raw.githubusercontent.com/idris-lang/Idris2/main/support/c/$f"
        done
    fi
}

run_named_hook() {
    local fn="$1"
    if [ -n "$fn" ] && declare -f "$fn" >/dev/null 2>&1; then
        "$fn"
    fi
}

generate_canister_entry() {
    if [ "${SKIP_GEN_ENTRY:-}" = "1" ]; then
        echo "Skipping gen-entry (SKIP_GEN_ENTRY=1)"
        CANISTER_ENTRY="${CANISTER_ENTRY:-$DEFAULT_CANISTER_ENTRY}"
        return
    fi
    if [ ${#GEN_ENTRY_ARGS[@]} -eq 0 ]; then
        CANISTER_ENTRY="${CANISTER_ENTRY:-$DEFAULT_CANISTER_ENTRY}"
        return
    fi

    local gen_exec=""
    if [ -x "$LOCAL_IDRIS2_ICWASM" ]; then
        gen_exec="$LOCAL_IDRIS2_ICWASM"
        echo "Generating canister_entry.c via local idris2-icwasm"
    elif command -v idris2-icwasm >/dev/null 2>&1; then
        gen_exec="$(command -v idris2-icwasm)"
        echo "Generating canister_entry.c via gen-entry"
    else
        echo "idris2-icwasm not found, using existing canister_entry.c"
        CANISTER_ENTRY="${CANISTER_ENTRY:-$DEFAULT_CANISTER_ENTRY}"
        return
    fi

    "$gen_exec" gen-entry "${GEN_ENTRY_ARGS[@]}"
    CANISTER_ENTRY="${CANISTER_ENTRY:-$DEFAULT_CANISTER_ENTRY}"
}

run_idris_build() {
    if [ -n "$BUILD_FN" ] && declare -f "$BUILD_FN" >/dev/null 2>&1; then
        "$BUILD_FN"
        return
    fi

    echo "Step 2: Generating C with RefC codegen..."
    cd "$SOURCE_DIR"
    CPATH="$MINI_GMP${CPATH:+:$CPATH}" idris2 --codegen refc --build-dir "$BUILD_DIR" -o "$DEFAULT_OUTPUT_NAME" Main.idr
}

find_generated_c_file() {
    if [ -n "$C_FILE_EXPECTED" ] && [ -f "$C_FILE_EXPECTED" ]; then
        C_FILE="$C_FILE_EXPECTED"
        return
    fi

    local dir candidate
    for dir in "${C_FILE_FIND_DIRS[@]}"; do
        [ -d "$dir" ] || continue
        candidate=$(find "$dir" -name "$C_FILE_PATTERN" 2>/dev/null | head -1)
        if [ -n "$candidate" ] && [ -f "$candidate" ]; then
            C_FILE="$candidate"
            return
        fi
    done

    echo "No C file generated"
    exit 1
}

inject_ffi_declarations() {
    if [ -z "$FFI_DECLARATIONS" ]; then
        return 0
    fi
    echo ">>> Step 1.5: Injecting FFI declarations"
    echo "$FFI_DECLARATIONS" > "$BUILD_DIR/ffi_header.h"
    cat "$BUILD_DIR/ffi_header.h" "$C_FILE" > "$BUILD_DIR/main_patched.c"
    mv "$BUILD_DIR/main_patched.c" "$C_FILE"
    echo "Injected FFI declarations into $C_FILE"
}

link_canister_wasm() {
    echo ">>> Step 3: C → WASM (Emscripten)"
    if [ "$ICWASM_SUPPORT" != "$IC0_SUPPORT" ]; then
        echo "Using Idris2IcWasm support: $ICWASM_SUPPORT"
    fi

    local emcc_inputs=(
        "$C_FILE"
        $REFC_C_FILES
        "$MINI_GMP/mini-gmp.c"
        "$IC0_SUPPORT/ic0_stubs.c"
    )
    if [ -n "$CANISTER_ENTRY" ] && [ -f "$CANISTER_ENTRY" ]; then
        emcc_inputs+=("$CANISTER_ENTRY")
    fi
    if [ -f "$IC0_SUPPORT/wasi_stubs.c" ]; then
        emcc_inputs+=("$IC0_SUPPORT/wasi_stubs.c")
    fi
    if [ -f "$IC0_SUPPORT/ic_ffi_bridge.c" ]; then
        emcc_inputs+=("$IC0_SUPPORT/ic_ffi_bridge.c")
    fi
    if [ -f "$ICWASM_SUPPORT/ic_tecdsa.c" ]; then
        emcc_inputs+=("$ICWASM_SUPPORT/ic_tecdsa.c")
    fi
    emcc_inputs+=("${EXTRA_C_FILES[@]}")

    local include_dirs=(
        "$MINI_GMP"
        "$REFC_INCLUDE"
        "$C_INCLUDE"
        "$IC0_SUPPORT"
    )
    if [ "$ICWASM_SUPPORT" != "$IC0_SUPPORT" ]; then
        include_dirs+=("$ICWASM_SUPPORT")
    fi
    include_dirs+=("${EXTRA_INCLUDE_DIRS[@]}")

    local force_includes=("$MINI_GMP/gmp.h")
    force_includes+=("${EXTRA_FORCE_INCLUDES[@]}")

    local emcc_flags=(
        -o "$OUTPUT_WASM"
        -s STANDALONE_WASM=1
        -s FILESYSTEM=0
        -s ERROR_ON_UNDEFINED_SYMBOLS=0
        --no-entry
        -O2
    )
    emcc_flags+=("${EXTRA_EMCC_FLAGS[@]}")

    local include_args=()
    local force_include_args=()
    local path
    for path in "${include_dirs[@]}"; do
        include_args+=(-I"$path")
    done
    for path in "${force_includes[@]}"; do
        force_include_args+=(-include "$path")
    done

    CPATH= CPLUS_INCLUDE_PATH= emcc "${emcc_inputs[@]}" \
        "${force_include_args[@]}" \
        "${include_args[@]}" \
        "${emcc_flags[@]}" \
        2>&1 || {
        echo "Emscripten compilation failed"
        exit 1
    }
}

stub_wasi_imports() {
    if [ "$ENABLE_WASI_STUB" != "1" ]; then
        return
    fi

    echo ">>> Step 4: WASI stubbing"
    if [ -f "$STUB_WASI_SCRIPT" ]; then
        bash "$STUB_WASI_SCRIPT" "$OUTPUT_WASM" "$OUTPUT_STUBBED_WASM"
        return
    fi

    if command -v wasm2wat >/dev/null 2>&1 && command -v wat2wasm >/dev/null 2>&1; then
        echo "Stubbing WASI imports..."
        wasm2wat "$OUTPUT_WASM" -o "$BUILD_DIR/$(basename "$DEFAULT_OUTPUT_NAME").wat"
        sed -i.bak '
            s/(import "wasi_snapshot_preview1" "fd_close" (func \$__wasi_fd_close (type [0-9]*)))/(func $__wasi_fd_close (param i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "fd_write" (func \$__wasi_fd_write (type [0-9]*)))/(func $__wasi_fd_write (param i32 i32 i32 i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "fd_seek" (func \$__wasi_fd_seek (type [0-9]*)))/(func $__wasi_fd_seek (param i32 i64 i32 i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "fd_read" (func \$__wasi_fd_read (type [0-9]*)))/(func $__wasi_fd_read (param i32 i32 i32 i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "environ_sizes_get" (func \$__wasi_environ_sizes_get (type [0-9]*)))/(func $__wasi_environ_sizes_get (param i32 i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "environ_get" (func \$__wasi_environ_get (type [0-9]*)))/(func $__wasi_environ_get (param i32 i32) (result i32) i32.const 0)/
            s/(import "wasi_snapshot_preview1" "proc_exit" (func \$__wasi_proc_exit (type [0-9]*)))/(func $__wasi_proc_exit (param i32))/
        ' "$BUILD_DIR/$(basename "$DEFAULT_OUTPUT_NAME").wat"

        wat2wasm "$BUILD_DIR/$(basename "$DEFAULT_OUTPUT_NAME").wat" --debug-names -o "$OUTPUT_STUBBED_WASM" || {
            echo "WASI stubbing failed, using original WASM"
            cp "$OUTPUT_WASM" "$OUTPUT_STUBBED_WASM"
        }
    else
        echo "wabt not found, skipping WASI stubbing"
        cp "$OUTPUT_WASM" "$OUTPUT_STUBBED_WASM"
    fi
}

show_build_summary() {
    local final_output="$OUTPUT_WASM"
    if [ "$ENABLE_WASI_STUB" = "1" ]; then
        final_output="$OUTPUT_STUBBED_WASM"
    fi

    echo ">>> Build complete!"
    echo ""
    echo "Output files:"
    if [ "$ENABLE_WASI_STUB" = "1" ]; then
        ls -la "$OUTPUT_WASM" "$OUTPUT_STUBBED_WASM" 2>/dev/null || ls -la "$OUTPUT_WASM"
    else
        ls -la "$OUTPUT_WASM"
    fi

    if command -v wasm-objdump >/dev/null 2>&1; then
        echo ""
        echo "=== WASM Exports ==="
        wasm-objdump -x "$final_output" 2>/dev/null | grep -E "Export\\[|^ - func" | head -30 || true
        echo ""
        echo "=== WASM Imports ==="
        wasm-objdump -x "$final_output" 2>/dev/null | grep -E "Import\\[|^ - func" | head -20 || true
    fi
}

run_build_pipeline() {
    : "${PROJECT_DIR:?PROJECT_DIR is required}"

    DEFAULT_OUTPUT_NAME="${DEFAULT_OUTPUT_NAME:-canister}"
    BUILD_LABEL="${BUILD_LABEL:-$DEFAULT_OUTPUT_NAME}"
    BUILD_DIR="${BUILD_DIR:-$PROJECT_DIR/build/$BUILD_LABEL}"
    WORKDIR="${WORKDIR:-$PROJECT_DIR}"
    IC0_SUPPORT="${IC0_SUPPORT:-$PROJECT_DIR/support/ic0}"
    ICWASM_SUPPORT="${ICWASM_SUPPORT:-$PROJECT_DIR/support/ic0}"
    MINI_GMP="${MINI_GMP:-/tmp/mini-gmp}"
    REFC_SRC="${REFC_SRC:-/tmp/refc-src}"
    LOCAL_IDRIS2_ICWASM="${LOCAL_IDRIS2_ICWASM:-$PROJECT_DIR/build/exec/idris2-icwasm}"
    DEFAULT_CANISTER_ENTRY="${DEFAULT_CANISTER_ENTRY:-$IC0_SUPPORT/canister_entry.c}"
    CANISTER_ENTRY="${CANISTER_ENTRY:-$DEFAULT_CANISTER_ENTRY}"
    OUTPUT_WASM="${OUTPUT_WASM:-$BUILD_DIR/$DEFAULT_OUTPUT_NAME.wasm}"
    OUTPUT_STUBBED_WASM="${OUTPUT_STUBBED_WASM:-$BUILD_DIR/${DEFAULT_OUTPUT_NAME}_stubbed.wasm}"
    ENABLE_WASI_STUB="${ENABLE_WASI_STUB:-0}"
    C_FILE_PATTERN="${C_FILE_PATTERN:-*.c}"
    STUB_WASI_SCRIPT="${STUB_WASI_SCRIPT:-$PROJECT_DIR/../idris2-wasm/tools/stub-wasi.sh}"

    if [ ${#C_FILE_FIND_DIRS[@]} -eq 0 ]; then
        C_FILE_FIND_DIRS=("$BUILD_DIR")
    fi

    export PATH="${PATH:-$HOME/.local/bin}"

    if [ -f "$HOME/emsdk/emsdk_env.sh" ]; then
        source "$HOME/emsdk/emsdk_env.sh" > /dev/null 2>&1
        echo "Using emsdk: $(emcc --version | head -1)"
    fi

    echo "=== Building $BUILD_LABEL to ICP Canister WASM ==="
    command -v idris2 >/dev/null 2>&1 || { echo "idris2 not found"; exit 1; }
    command -v emcc >/dev/null 2>&1 || { echo "emcc not found. Install: https://emscripten.org/docs/getting_started/downloads.html"; exit 1; }

    mkdir -p "$BUILD_DIR"
    ensure_mini_gmp
    run_named_hook "$VALIDATE_FN"

    echo ">>> Step 0b: Generating canister_entry_gen.c from can.did"
    generate_canister_entry

    echo ">>> Step 1: Idris2 → C (RefC backend)"
    cd "$WORKDIR"
    run_idris_build

    echo ">>> Step 2: Find generated C file"
    find_generated_c_file
    echo "Generated: $C_FILE"
    inject_ffi_declarations

    echo ">>> Step 2: Prepare RefC runtime"
    ensure_refc_runtime
    REFC_INCLUDE="$REFC_SRC"
    C_INCLUDE="$REFC_SRC"
    REFC_C_FILES="$REFC_SRC/runtime.c $REFC_SRC/memoryManagement.c $REFC_SRC/stringOps.c $REFC_SRC/mathFunctions.c $REFC_SRC/casts.c $REFC_SRC/prim.c $REFC_SRC/refc_util.c"

    run_named_hook "$PRE_LINK_FN"
    link_canister_wasm
    stub_wasi_imports
    show_build_summary
}

run_build_pipeline
