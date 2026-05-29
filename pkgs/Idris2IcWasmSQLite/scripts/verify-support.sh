#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SUPPORT_DIR="${1:-$ROOT/support/ic0}"

required=(
  "$SUPPORT_DIR/sqlite_bridge.c"
  "$SUPPORT_DIR/sqlite_bridge.h"
  "$SUPPORT_DIR/sqlite_vfs_bridge.c"
  "$SUPPORT_DIR/sqlite_vfs_bridge.h"
  "$SUPPORT_DIR/sqlite/libsqlite3.a"
  "$SUPPORT_DIR/sqlite/sqlite3.h"
  "$SUPPORT_DIR/sqlite/sqlite3ext.h"
)

for path in "${required[@]}"; do
  [[ -f "$path" ]] || { echo "missing SQLite VFS support file: $path" >&2; exit 1; }
done

grep -q 'SQLITE_OS_OTHER' "$ROOT/support/sqlite/build-flags.txt" \
  || { echo "support/sqlite/build-flags.txt must include SQLITE_OS_OTHER" >&2; exit 1; }
grep -q 'sqlite3_os_init' "$SUPPORT_DIR/sqlite_vfs_bridge.c" \
  || { echo "sqlite_vfs_bridge.c must provide sqlite3_os_init for SQLITE_OS_OTHER" >&2; exit 1; }

if [[ -f "$SUPPORT_DIR/wasi_polyfill.c" ]]; then
  echo "wasi_polyfill.c is obsolete for SQLITE_OS_OTHER VFS support" >&2
  exit 1
fi

echo "SQLite VFS support verified: $SUPPORT_DIR"
