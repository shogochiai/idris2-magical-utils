#!/bin/bash
# Build Idris2 examples to ICP canister WASM via the shared IcWasm pipeline.
set -e

EXAMPLE="${1:-canister}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SOURCE_DIR="$PROJECT_DIR/examples/$EXAMPLE"
BUILD_DIR="$PROJECT_DIR/build/$EXAMPLE"
COMMON_SCRIPT="$SCRIPT_DIR/lib/build-canister-common.sh"

export PATH="$HOME/.local/bin:$PATH"
export IDRIS2_PREFIX="${IDRIS2_PREFIX:-$HOME/.local}"

build_example_refc() {
    echo "Step 2: Generating C with RefC codegen..."
    cd "$SOURCE_DIR"
    CPATH="$MINI_GMP${CPATH:+:$CPATH}" idris2 --codegen refc --build-dir "$BUILD_DIR" -o main Main.idr
}

BUILD_LABEL="$EXAMPLE"
WORKDIR="$SOURCE_DIR"
IC0_SUPPORT="$PROJECT_DIR/support/ic0"
ICWASM_SUPPORT="$PROJECT_DIR/support/ic0"
DEFAULT_OUTPUT_NAME="canister"
BUILD_FN=build_example_refc
C_FILE_FIND_DIRS=("$BUILD_DIR")
C_FILE_PATTERN="*.c"
ENABLE_WASI_STUB=0

source "$COMMON_SCRIPT"
