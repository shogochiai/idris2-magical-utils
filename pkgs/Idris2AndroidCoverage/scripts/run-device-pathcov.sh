#!/usr/bin/env bash
# run-device-pathcov.sh — REAL device path coverage. The bash here does ONLY raw I/O;
# all coverage math (exclusions, denominator ∩ numerator, the report) is computed by
# the Idris2 `device-pathcov` executable (Android.Coverage.PathCoverage) — lazy and
# etherclaw are portable Idris2, so no python/jq matcher lives here.
#
#   numerator   = path_ids the View hit while running on the device (logcat
#                 IDRIS_PATHHIT:<id>, emitted by device-pathhit-hook.js).
#   denominator = the --dumppaths-json path_ids (extracted here, exclusions applied by
#                 the Idris2 side).
#
# Usage:
#   run-device-pathcov.sh --package PKG --paths DUMPPATHS_JSON \
#       [--module-prefix SpcDaoApp] [--driver SCRIPT] [--settle N] [--serial S] [--out FILE]
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKGROOT="$(cd "$HERE/.." && pwd)"

PKG="" PATHS="" PREFIX="" DRIVER="" SETTLE=8 SERIAL="" OUTFILE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --package)       PKG="$2"; shift 2 ;;
    --paths)         PATHS="$2"; shift 2 ;;
    --module-prefix) PREFIX="$2"; shift 2 ;;
    --driver)        DRIVER="$2"; shift 2 ;;
    --settle)        SETTLE="$2"; shift 2 ;;
    --serial)        SERIAL="$2"; shift 2 ;;
    --out)           OUTFILE="$2"; shift 2 ;;
    -h|--help) echo "Usage: run-device-pathcov.sh --package PKG --paths DUMPPATHS_JSON [--module-prefix P] [--driver S] [--settle N] [--serial S] [--out FILE]"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[[ -n "$PKG" && -n "$PATHS" ]] || { echo "need --package and --paths" >&2; exit 2; }
[[ -f "$PATHS" ]] || { echo "dumppaths json not found: $PATHS" >&2; exit 2; }

# Locate the Idris2 coverage executable (built from idris2-android-coverage.ipkg).
PATHCOV_BIN="$(find "$PKGROOT/build/exec" -maxdepth 1 -type f -name device-pathcov -perm -u+x 2>/dev/null | head -1)"
[[ -x "$PATHCOV_BIN" ]] || { echo "device-pathcov not built; run: pack build idris2-android-coverage" >&2; exit 4; }

ADB=(adb); [[ -n "$SERIAL" ]] && ADB=(adb -s "$SERIAL")
"${ADB[@]}" get-state >/dev/null 2>&1 || { echo "no device" >&2; exit 3; }

# Clear logcat, launch the app, let the View mount (the hook fires as paths run).
"${ADB[@]}" logcat -c >/dev/null 2>&1 || true
"${ADB[@]}" shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1 || true
sleep "$SETTLE"
[[ -n "$DRIVER" && -x "$DRIVER" ]] && "$DRIVER" "${SERIAL:-}" || true
sleep 2

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
DENOM="$WORK/denom.txt"; HITS="$WORK/hits.txt"

# DENOMINATOR: every dumppaths path_id, one per line. Plain grep/sed (no jq).
grep -oE '"path_id"[[:space:]]*:[[:space:]]*"[^"]*"' "$PATHS" \
  | sed -E 's/.*"path_id"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/' \
  | sort -u > "$DENOM"

# NUMERATOR: distinct path_ids the device logged.
"${ADB[@]}" logcat -d 2>/dev/null \
  | sed -n 's/.*IDRIS_PATHHIT:\([^ ]*\).*/\1/p' \
  | sort -u > "$HITS"

# The Idris2 side computes coverage (exclusions, intersection, report) and sets exit.
if [[ -n "$OUTFILE" ]]; then
  "$PATHCOV_BIN" "$DENOM" "$HITS" "$PREFIX" | tee "$OUTFILE"
else
  "$PATHCOV_BIN" "$DENOM" "$HITS" "$PREFIX"
fi
