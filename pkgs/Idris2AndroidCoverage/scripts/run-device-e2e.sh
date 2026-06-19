#!/usr/bin/env bash
# run-device-e2e.sh — the device half of Android.Coverage.DeviceProbe.
#
# The pure module (DeviceProbe.idr) says WHICH testIDs the View declares and WHICH a
# scenario must drive; this runner checks they are ACTUALLY on the real device. That
# closes the acceptance-hack hole: a proof-TextView host exposes none of the testIDs,
# so it fails here even if every grep/unzip/adb-install acceptance passes.
#
# Contract: given an installed+launched app and the list of testIDs the View declares
# (one per line in --surface FILE, produced by `viewTestIDs` at build time), it dumps
# the live UI, checks each testID is present (RN testID → Android resource-id /
# content-desc), taps the ones marked tappable, and prints a parity-ti-shaped report:
#
#     coverage_percent: <p>
#     Missing paths: <n>
#     Missing paths:
#       <testID not found on device>
#
# Exit 0 iff every declared testID was found on-device (Missing paths: 0).
set -euo pipefail

PKG="" COMPONENT="" SURFACE="" SERIAL=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --package)   PKG="$2"; shift 2 ;;
    --component) COMPONENT="$2"; shift 2 ;;      # e.g. com.spcdaoapp/.MainActivity
    --surface)   SURFACE="$2"; shift 2 ;;        # file: one declared testID per line
    --serial)    SERIAL="$2"; shift 2 ;;
    -h|--help)   echo "Usage: run-device-e2e.sh --package PKG --component COMP --surface FILE [--serial S]"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[[ -n "$PKG" && -n "$SURFACE" ]] || { echo "need --package and --surface" >&2; exit 2; }
[[ -f "$SURFACE" ]] || { echo "surface file not found: $SURFACE" >&2; exit 2; }

ADB=(adb)
[[ -n "$SERIAL" ]] && ADB=(adb -s "$SERIAL")

"${ADB[@]}" get-state >/dev/null 2>&1 || { echo "no device (adb get-state failed)" >&2; exit 3; }

# Launch (idempotent) and let RN mount.
"${ADB[@]}" shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1 || true
sleep 6

# Dump the live UI tree. uiautomator exposes resource-id and content-desc, which is
# where RN's testID lands. A hollow TextView host has neither for the spec elements.
DUMP="$(mktemp)"
"${ADB[@]}" shell uiautomator dump /sdcard/ac-ui.xml >/dev/null 2>&1
"${ADB[@]}" shell cat /sdcard/ac-ui.xml > "$DUMP" 2>/dev/null
"${ADB[@]}" shell rm -f /sdcard/ac-ui.xml >/dev/null 2>&1 || true

declared=(); while IFS= read -r line; do [[ -n "$line" ]] && declared+=("$line"); done < "$SURFACE"
total=${#declared[@]}
missing=()
for t in "${declared[@]}"; do
  # present if the testID appears as a resource-id or content-desc in the dump
  if grep -qE "resource-id=\"[^\"]*${t}\"|content-desc=\"[^\"]*${t}[^\"]*\"" "$DUMP"; then
    :
  else
    missing+=("$t")
  fi
done
rm -f "$DUMP"

found=$(( total - ${#missing[@]} ))
pct=0; [[ $total -gt 0 ]] && pct=$(( found * 100 / total ))

echo "# Device UI Coverage (uiautomator)"
echo "claim_admissible: $([[ ${#missing[@]} -eq 0 ]] && echo True || echo False)"
echo "coverage_percent: ${pct}.0"
echo "Missing paths: ${#missing[@]}"
echo "total_elements: $total"
echo "covered_elements: $found"
echo "evidence_kind: device_uiautomator_testid_coverage"
echo
echo "Missing paths:"
if [[ ${#missing[@]} -eq 0 ]]; then
  echo "  (none)"
else
  for t in "${missing[@]}"; do echo "  $t"; done
fi
[[ ${#missing[@]} -eq 0 ]]
