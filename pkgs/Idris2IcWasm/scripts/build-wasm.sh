#!/bin/bash
# Build Idris2 to WASM via RefC + Emscripten
set -e

# Idris2 environment
export PATH="$HOME/.local/bin:$PATH"
export IDRIS2_PREFIX="$HOME/.local"

EXAMPLE="${1:-hello}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
EXAMPLE_DIR="$PROJECT_DIR/examples/$EXAMPLE"
BUILD_DIR="$PROJECT_DIR/build/$EXAMPLE"

echo "=== Building $EXAMPLE to WASM ==="

# Check dependencies
command -v idris2 >/dev/null 2>&1 || { echo "idris2 not found"; exit 1; }
command -v emcc >/dev/null 2>&1 || { echo "emcc not found. Install: https://emscripten.org/docs/getting_started/downloads.html"; exit 1; }

mkdir -p "$BUILD_DIR"

echo ">>> Step 1: Idris2 → C (RefC backend)"
cd "$EXAMPLE_DIR"
idris2 --codegen refc --build-dir "$BUILD_DIR" -o main Main.idr

echo ">>> Step 2: C → WASM (Emscripten)"
# Find generated C file
C_FILE=$(find "$BUILD_DIR" -name "*.c" | head -1)
if [ -z "$C_FILE" ]; then
    echo "No C file generated"
    exit 1
fi

# Idris2 RefC runtime headers
IDRIS2_SUPPORT="$IDRIS2_PREFIX/idris2-0.8.0/support"
REFC_SUPPORT="$IDRIS2_SUPPORT/refc"
C_SUPPORT="$IDRIS2_SUPPORT/c"
MINI_GMP="/tmp/mini-gmp"
REFC_SRC="/tmp/refc-src"

# Download mini-gmp if not present
if [ ! -f "$MINI_GMP/mini-gmp.c" ]; then
    mkdir -p "$MINI_GMP"
    curl -sLo "$MINI_GMP/mini-gmp.c" https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.c
    curl -sLo "$MINI_GMP/mini-gmp.h" https://gmplib.org/repo/gmp/raw-file/tip/mini-gmp/mini-gmp.h
    # Create gmp.h wrapper with mpz_inits/mpz_clears stubs (not in mini-gmp)
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

REFC_C_FILES="$REFC_SRC/runtime.c $REFC_SRC/memoryManagement.c $REFC_SRC/stringOps.c $REFC_SRC/mathFunctions.c $REFC_SRC/casts.c $REFC_SRC/prim.c $REFC_SRC/idris_support.c $REFC_SRC/idris_file.c $REFC_SRC/refc_util.c $REFC_SRC/idris_util.c"

emcc "$C_FILE" $REFC_C_FILES "$MINI_GMP/mini-gmp.c" \
    -I"$REFC_SUPPORT" \
    -I"$C_SUPPORT" \
    -I"$MINI_GMP" \
    -o "$BUILD_DIR/main.js" \
    -s WASM=1 \
    -s SINGLE_FILE=1 \
    -s EXIT_RUNTIME=1 \
    -s ASSERTIONS=2

echo ">>> Done!"
echo "Output: $BUILD_DIR/main.js (WASM embedded)"
ls -la "$BUILD_DIR"/main.js
