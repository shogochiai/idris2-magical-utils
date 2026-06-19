#!/usr/bin/env bash
# run-device-pathcov.sh — REAL device path coverage for a family=android MVU app.
#
# numerator   = path ids the View hit WHILE RUNNING ON THE DEVICE, scraped from
#               logcat (the device-pathhit-hook prints IDRIS_PATHHIT:<id> per id).
# denominator = the --dumppaths-json canonical CaseTree paths of the View's own
#               functions, with exclusions applied (library/synthetic/zero-length —
#               same policy as the host step4).
#
# This is the path-coverage analogue of run-device-e2e.sh's testID check: not "is the
# element present" but "did the View's code path actually execute on the device". A
# proof-TextView host never runs the View, so its numerator is empty → 0% → fail.
#
# Usage:
#   run-device-pathcov.sh --package PKG --paths DUMPPATHS_JSON \
#       [--module-prefix SpcDaoApp]   # only count paths in the app's own modules
#       [--driver SCRIPT]             # optional: a script that taps the UI to exercise paths
#       [--settle N] [--serial S]
set -euo pipefail

PKG="" PATHS="" PREFIX="" DRIVER="" SETTLE=8 SERIAL=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --package)       PKG="$2"; shift 2 ;;
    --paths)         PATHS="$2"; shift 2 ;;
    --module-prefix) PREFIX="$2"; shift 2 ;;
    --driver)        DRIVER="$2"; shift 2 ;;
    --settle)        SETTLE="$2"; shift 2 ;;
    --serial)        SERIAL="$2"; shift 2 ;;
    -h|--help) echo "Usage: run-device-pathcov.sh --package PKG --paths DUMPPATHS_JSON [--module-prefix P] [--driver S] [--settle N] [--serial S]"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[[ -n "$PKG" && -n "$PATHS" ]] || { echo "need --package and --paths" >&2; exit 2; }
[[ -f "$PATHS" ]] || { echo "dumppaths json not found: $PATHS" >&2; exit 2; }

ADB=(adb); [[ -n "$SERIAL" ]] && ADB=(adb -s "$SERIAL")
"${ADB[@]}" get-state >/dev/null 2>&1 || { echo "no device" >&2; exit 3; }

# Clear logcat, launch the app, let the View mount (the hook fires as paths execute).
"${ADB[@]}" logcat -c >/dev/null 2>&1 || true
"${ADB[@]}" shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1 || true
sleep "$SETTLE"
# Optional driver taps the UI to exercise more View paths (vote, select, scroll…).
[[ -n "$DRIVER" && -x "$DRIVER" ]] && "$DRIVER" "${SERIAL:-}" || true
sleep 2

# NUMERATOR: distinct path ids the device logged.
HITS="$(mktemp)"
"${ADB[@]}" logcat -d 2>/dev/null \
  | sed -n 's/.*IDRIS_PATHHIT:\([^ ]*\).*/\1/p' \
  | sort -u > "$HITS"
HOOK_OK="$("${ADB[@]}" logcat -d 2>/dev/null | grep -c IDRIS_PATHHIT_HOOK_INSTALLED || true)"

# DENOMINATOR + report, computed in python against the dumppaths schema.
python3 - "$PATHS" "$HITS" "$PREFIX" "$HOOK_OK" <<'PY'
import json, sys
paths_file, hits_file, prefix, hook_ok = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
d = json.load(open(paths_file))

# Exclusions (same spirit as host step4): drop library/synthetic functions and
# zero-length record-accessor paths, so the denominator is the View's real obligations.
LIB_PREFIXES = ("Prelude.", "PrimIO.", "Builtin.", "Data.", "System.", "Libraries.")
def excluded(fn_name, p):
    if fn_name.startswith(LIB_PREFIXES): return True
    if prefix and not fn_name.startswith(prefix): return True   # only the app's own modules
    if p.get("classification") not in (None, "ReachableObligation"): return True
    if p.get("path_length", 1) == 0 and p.get("terminal_kind") == "reached_clause" and not p.get("steps"):
        # zero-length accessor-style path: keep only if it's a real clause; drop trivial ones
        pass
    return False

denom = []
for fn in d.get("functions", []):
    name = fn.get("function_name","")
    for p in fn.get("paths", []):
        if not excluded(name, p):
            denom.append(p["path_id"])
denom = sorted(set(denom))

hits = set(l.strip() for l in open(hits_file) if l.strip())
covered = [pid for pid in denom if pid in hits]
missing = [pid for pid in denom if pid not in hits]
total = len(denom)
pct = (len(covered)*100//total) if total else 0

print("# Device PATH Coverage (dumppaths denominator, on-device hit numerator)")
print("hook_installed_on_device:", "yes" if hook_ok and hook_ok!="0" else "NO (no IDRIS_PATHHIT_HOOK_INSTALLED in logcat — bundle not instrumented or hook not prepended)")
print("claim_admissible:", "True" if (total>0 and not missing) else "False")
print(f"coverage_percent: {pct}.0")
print(f"total_paths: {total}")
print(f"covered_paths: {len(covered)}")
print(f"Missing paths: {len(missing)}")
print("evidence_kind: device_dumppaths_path_coverage")
print()
print("Missing paths:")
if not missing:
    print("  (none)")
else:
    for m in missing: print("  " + m)
sys.exit(0 if (total>0 and not missing) else 1)
PY
rc=$?
rm -f "$HITS"
exit $rc
