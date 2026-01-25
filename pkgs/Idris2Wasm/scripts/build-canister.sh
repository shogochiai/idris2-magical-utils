#!/bin/bash
# Build Idris2 to ICP Canister WASM
# Uses Emscripten in standalone WASM mode for IC compatibility
set -e

# Idris2 environment
export PATH="$HOME/.local/bin:$PATH"
export IDRIS2_PREFIX="$HOME/.local"

# Use emsdk if available
if [ -f "$HOME/emsdk/emsdk_env.sh" ]; then
    source "$HOME/emsdk/emsdk_env.sh" > /dev/null 2>&1
    echo "Using emsdk: $(emcc --version | head -1)"
fi

EXAMPLE="${1:-canister}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
EXAMPLE_DIR="$PROJECT_DIR/examples/$EXAMPLE"
BUILD_DIR="$PROJECT_DIR/build/$EXAMPLE"

echo "=== Building $EXAMPLE to ICP Canister WASM ==="

# Check dependencies
command -v idris2 >/dev/null 2>&1 || { echo "idris2 not found"; exit 1; }
command -v emcc >/dev/null 2>&1 || { echo "emcc not found. Install: https://emscripten.org/docs/getting_started/downloads.html"; exit 1; }

mkdir -p "$BUILD_DIR"

echo ">>> Step 1: Idris2 → C (RefC backend)"
cd "$EXAMPLE_DIR"
idris2 --codegen refc --build-dir "$BUILD_DIR" -o main Main.idr

echo ">>> Step 2: Find generated C file"
C_FILE=$(find "$BUILD_DIR" -name "*.c" | head -1)
if [ -z "$C_FILE" ]; then
    echo "No C file generated"
    exit 1
fi
echo "Generated: $C_FILE"

# Idris2 RefC runtime headers and sources
IDRIS2_SUPPORT="$IDRIS2_PREFIX/idris2-0.8.0/support"
REFC_SUPPORT="$IDRIS2_SUPPORT/refc"
C_SUPPORT="$IDRIS2_SUPPORT/c"
MINI_GMP="/tmp/mini-gmp"
REFC_SRC="/tmp/refc-src"
IC0_SUPPORT="$PROJECT_DIR/support/ic0"

# Download mini-gmp if not present
if [ ! -f "$MINI_GMP/mini-gmp.c" ]; then
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
fi

# Download RefC runtime source if not present
if [ ! -f "$REFC_SRC/runtime.c" ]; then
    mkdir -p "$REFC_SRC"
    for f in memoryManagement.c runtime.c stringOps.c mathFunctions.c casts.c clock.c buffer.c prim.c refc_util.c; do
        curl -sLo "$REFC_SRC/$f" "https://raw.githubusercontent.com/idris-lang/Idris2/main/support/refc/$f"
    done
    for f in idris_support.c idris_file.c idris_directory.c idris_util.c; do
        curl -sLo "$REFC_SRC/$f" "https://raw.githubusercontent.com/idris-lang/Idris2/main/support/c/$f"
    done
fi

# Minimal RefC files for canister (no file I/O to avoid WASI)
REFC_C_FILES="$REFC_SRC/runtime.c $REFC_SRC/memoryManagement.c $REFC_SRC/stringOps.c $REFC_SRC/mathFunctions.c $REFC_SRC/casts.c $REFC_SRC/prim.c $REFC_SRC/refc_util.c"

echo ">>> Step 3: Compile to standalone WASM (Emscripten)"

# Build standalone WASM using emscripten
# STANDALONE_WASM produces a .wasm file that can run without JS glue
# PURE_WASI=0 and FILESYSTEM=0 to avoid WASI imports that IC doesn't support
# First build to JS/WASM bundle (avoids WASI completely)
emcc "$C_FILE" $REFC_C_FILES "$MINI_GMP/mini-gmp.c" "$IC0_SUPPORT/canister_entry.c" \
    -I"$REFC_SUPPORT" \
    -I"$C_SUPPORT" \
    -I"$MINI_GMP" \
    -I"$IC0_SUPPORT" \
    -o "$BUILD_DIR/canister.wasm" \
    -s STANDALONE_WASM=1 \
    -s FILESYSTEM=0 \
    -s ERROR_ON_UNDEFINED_SYMBOLS=0 \
    --no-entry \
    -O2

echo ">>> Done!"
echo "Output: $BUILD_DIR/canister.wasm"
ls -la "$BUILD_DIR/canister.wasm"

# Show WASM info if wabt tools available
if command -v wasm-objdump >/dev/null 2>&1; then
    echo ""
    echo "=== WASM Exports ==="
    wasm-objdump -x "$BUILD_DIR/canister.wasm" | grep -E "Export\[|^ - func" | head -20

    echo ""
    echo "=== WASM Imports (ic0) ==="
    wasm-objdump -x "$BUILD_DIR/canister.wasm" | grep -E "Import\[|^ - func" | head -20
fi
