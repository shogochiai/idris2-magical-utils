#!/bin/bash
# Thin wrapper: defer to `idris2-icwasm build-canister --config=...`.
set -e

EXAMPLE="${1:-canister}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
CONFIG_PATH="$PROJECT_DIR/examples/$EXAMPLE/canister-build.toml"
LOCAL_IDRIS2_ICWASM="$PROJECT_DIR/build/exec/idris2-icwasm"

export PATH="$HOME/.local/bin:$PATH"
export IDRIS2_PREFIX="${IDRIS2_PREFIX:-$HOME/.local}"

[ -f "$CONFIG_PATH" ] || {
    echo "Error: config not found: $CONFIG_PATH"
    exit 1
}

if [ -x "$LOCAL_IDRIS2_ICWASM" ]; then
    exec "$LOCAL_IDRIS2_ICWASM" build-canister --config="$CONFIG_PATH"
elif command -v idris2-icwasm >/dev/null 2>&1; then
    exec idris2-icwasm build-canister --config="$CONFIG_PATH"
else
    echo "Error: idris2-icwasm not found"
    exit 1
fi
