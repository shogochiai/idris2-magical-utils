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

# Clear logcat, COLD-start the app, let the View mount (the hook fires as paths
# run). force-stop first: if the app is already foregrounded, `monkey` just
# resumes the existing activity WITHOUT remounting the View, so no recordPathHit
# fires and the numerator is a false 0 (observed: manual cold start = 58 hits,
# warm resume = 0). force-stop guarantees a fresh mount every run.
"${ADB[@]}" shell am force-stop "$PKG" >/dev/null 2>&1 || true
sleep 2
"${ADB[@]}" logcat -c >/dev/null 2>&1 || true
# Launch via `am start` (deterministic activity start) rather than monkey, which
# occasionally warm-resumes without remounting. Fall back to monkey.
"${ADB[@]}" shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1 || true
# Wait for the View to actually mount: the hook logs IDRIS_PATHHIT_HOOK_INSTALLED
# once installed, and recordPathHit lines follow. Poll until the hook marker
# appears (up to ~$SETTLE+20s) so a slow cold start is not scraped too early.
# NOTE: `grep -q` closes the pipe on first match, sending SIGPIPE up to
# `adb logcat -d`; under `set -o pipefail` that both trips errexit AND can leave
# the adb client in a state where the NEXT logcat -d returns nothing (the false
# 0 that dogged this harness). Dump to a var and match with a non-short-circuit
# test instead — no pipe into a truncating consumer.
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
DENOM="$WORK/denom.txt"; HITS="$WORK/hits.txt"; RAW="$WORK/rawlog.txt"

# DENOMINATOR: every dumppaths path_id, one per line. Plain grep/sed (no jq).
grep -oE '"path_id"[[:space:]]*:[[:space:]]*"[^"]*"' "$PATHS" \
  | sed -E 's/.*"path_id"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/' \
  | sort -u > "$DENOM"

# NUMERATOR: poll the device logcat until the recordPathHit lines appear (the
# View mount that fires them is async; a slow cold start would otherwise be
# scraped too early and misread as a false 0). Each poll dumps logcat to a file
# and greps THAT file — the earlier one-liner piped `adb logcat -d | grep`
# where a SIGPIPE / errexit / locale-backref interaction under `set -euo
# pipefail` left the extraction empty even though the dump held 58-67 hits.
# Robust extraction: fixed-string prefix, cut the id up to whitespace.
: > "$HITS"
# Disable errexit/pipefail for the poll: an early iteration legitimately finds
# ZERO hits (grep exits 1), and under `set -e`/`pipefail` that first empty grep
# killed the whole harness before the View had mounted — the real cause of the
# persistent false 0 (the dump held 58-67 hits, but the script had already
# exited on the first, still-empty poll). A single all-in-one awk does the
# fixed-string match + id extraction so there is no grep-in-pipe to trip on.
set +e +o pipefail
for _try in $(seq 1 $(( SETTLE + 20 ))); do
  # Pipe adb logcat straight into awk (the form proven to recover 58-67 ids in
  # this same shell). The prior `> "$RAW"` redirect produced an empty file under
  # this script's execution context while an identical interactive command did
  # not — cause never isolated, so avoid the intermediate file entirely.
  "${ADB[@]}" logcat -d 2>/dev/null | awk '
    /IDRIS_PATHHIT:/ && !/IDRIS_PATHHIT_HOOK_INSTALLED/ {
      i = index($0, "IDRIS_PATHHIT:")
      rest = substr($0, i + length("IDRIS_PATHHIT:"))
      split(rest, a, /[ \t]/)
      if (a[1] != "") print a[1]
    }' | sort -u > "$HITS"
  if [ -s "$HITS" ]; then break; fi
  sleep 1
done
set -e -o pipefail

# The Idris2 side computes coverage (exclusions, intersection, report) and sets exit.
if [[ -n "$OUTFILE" ]]; then
  "$PATHCOV_BIN" "$DENOM" "$HITS" "$PREFIX" | tee "$OUTFILE"
else
  "$PATHCOV_BIN" "$DENOM" "$HITS" "$PREFIX"
fi
