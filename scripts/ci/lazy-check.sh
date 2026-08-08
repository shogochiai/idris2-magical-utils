#!/bin/bash
set -euo pipefail

# Usage: lazy-check.sh <family> <pkg_path>
# Runs lazy <family> ask --steps=1,2,4 and checks stepStatuses.
# Exits 1 if any of stparity/testorphans/testandcoverage is not OK/AssumedOK.

FAMILY="$1"
PKG="$2"

echo "=== lazy $FAMILY ask --steps=1,2,4 $PKG ==="

# Capture output allowing lazy to fail (exit code != 0 is expected for gaps)
RAW=$(lazy "$FAMILY" ask --steps=1,2,4 --format json "$PKG" 2>&1 || true)
echo "$RAW"

OUTPUT=$(echo "$RAW" | grep '^{' | tail -1 || true)

if [ -z "$OUTPUT" ]; then
  echo "FAIL: $PKG - no JSON output from lazy $FAMILY ask"
  exit 1
fi

FAILED=0
for STEP in stparity testorphans testandcoverage; do
  STATUS=$(echo "$OUTPUT" | jq -r ".stepStatuses.$STEP // \"Missing\"")
  case "$STATUS" in
    OK|AssumedOK)
      echo "  $STEP: $STATUS"
      ;;
    *)
      echo "  $STEP: $STATUS  << FAIL"
      FAILED=1
      ;;
  esac
done

if [ "$FAILED" -eq 1 ]; then
  echo "FAIL: $PKG"
  echo "$OUTPUT" | jq .
  exit 1
fi

echo "OK: $PKG"
