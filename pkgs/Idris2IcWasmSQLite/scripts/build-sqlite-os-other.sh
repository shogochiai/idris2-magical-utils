#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${1:-$ROOT/support/ic0/sqlite}"
FLAGS_FILE="$ROOT/support/sqlite/build-flags.txt"

SQLITE_VERSION_NUMBER="${SQLITE_VERSION_NUMBER:-3510300}"
SQLITE_YEAR="${SQLITE_YEAR:-2026}"
SQLITE_URL="${SQLITE_URL:-https://www.sqlite.org/$SQLITE_YEAR/sqlite-amalgamation-$SQLITE_VERSION_NUMBER.zip}"

tmpdir="$(mktemp -d /tmp/idris2-icwasm-sqlite.XXXXXX)"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT

sqlite_src="${SQLITE3_C:-}"
sqlite_header="${SQLITE3_H:-}"
sqlite_ext_header="${SQLITE3EXT_H:-}"

if [[ -z "$sqlite_src" ]]; then
  if [[ -n "${SQLITE_AMALGAMATION_DIR:-}" ]]; then
    sqlite_src="$SQLITE_AMALGAMATION_DIR/sqlite3.c"
    sqlite_header="$SQLITE_AMALGAMATION_DIR/sqlite3.h"
    sqlite_ext_header="$SQLITE_AMALGAMATION_DIR/sqlite3ext.h"
  else
    archive="$tmpdir/sqlite.zip"
    curl -fsSL "$SQLITE_URL" -o "$archive"
    unzip -q "$archive" -d "$tmpdir"
    amalgamation_dir="$(find "$tmpdir" -maxdepth 1 -type d -name 'sqlite-amalgamation-*' | head -1)"
    sqlite_src="$amalgamation_dir/sqlite3.c"
    sqlite_header="$amalgamation_dir/sqlite3.h"
    sqlite_ext_header="$amalgamation_dir/sqlite3ext.h"
  fi
fi

[[ -f "$sqlite_src" ]] || { echo "sqlite3.c not found: $sqlite_src" >&2; exit 1; }
[[ -f "$sqlite_header" ]] || { echo "sqlite3.h not found: $sqlite_header" >&2; exit 1; }
[[ -f "$sqlite_ext_header" ]] || { echo "sqlite3ext.h not found: $sqlite_ext_header" >&2; exit 1; }

cc_bin="${CC:-emcc}"
ar_bin="${AR:-emar}"

flags=()
while IFS= read -r flag; do
  flag="${flag#"${flag%%[![:space:]]*}"}"
  flag="${flag%"${flag##*[![:space:]]}"}"
  [[ -n "$flag" ]] || continue
  [[ "$flag" != \#* ]] || continue
  flags+=("-D$flag")
done < "$FLAGS_FILE"

mkdir -p "$OUT_DIR"
CPATH= CPLUS_INCLUDE_PATH= "$cc_bin" \
  -Oz \
  -I "$(dirname "$sqlite_src")" \
  "${flags[@]}" \
  -c "$sqlite_src" \
  -o "$tmpdir/sqlite3.o"

"$ar_bin" crs "$OUT_DIR/libsqlite3.a" "$tmpdir/sqlite3.o"
cp "$sqlite_header" "$OUT_DIR/sqlite3.h"
cp "$sqlite_ext_header" "$OUT_DIR/sqlite3ext.h"

echo "built $OUT_DIR/libsqlite3.a with SQLITE_OS_OTHER=1"
