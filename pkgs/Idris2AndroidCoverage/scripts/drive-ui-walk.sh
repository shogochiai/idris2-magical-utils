#!/usr/bin/env bash
# drive-ui-walk.sh — a --driver for run-device-pathcov.sh.
#
# Contract (run-device-pathcov.sh): `bash "$DRIVER" "$PKG" "$SERIAL"`, invoked
# SYNCHRONOUSLY after the app has mounted and before the logcat scrape, with the
# logcat buffer already cleared. Whatever paths this provokes are still in the
# buffer when the harness reads it. Exiting nonzero does not fail the run — the
# harness keeps whatever was reached.
#
# WHY THIS IS APP-AGNOSTIC. It taps what the accessibility tree says is
# clickable, so it needs no knowledge of the app's screens or testIDs. That is
# deliberate: a driver written against one app's testIDs measures that app and
# then rots when the View changes, and a rotted driver reports a low number
# rather than an error.
#
# WHY A COLD START IS NOT ENOUGH. Measured on carl 2026-08-07 with
# pkgs/Idris2LuciAndroid: launch plus settle reaches 41 of 1478 paths (2.77%).
# The View is invoked through the RN framework, so nothing beyond mount runs
# until something touches it.
set -uo pipefail

PKG="${1:?usage: drive-ui-walk.sh PKG [SERIAL]}"
SERIAL="${2:-}"
ROUNDS="${UI_WALK_ROUNDS:-3}"
SETTLE_MS="${UI_WALK_SETTLE_MS:-700}"

ADB=(adb); [[ -n "$SERIAL" ]] && ADB=(adb -s "$SERIAL")

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
UIXML="$WORK/ui.xml"

dump_ui() {
  # uiautomator dump occasionally loses the race with a mid-animation frame and
  # prints "could not get idle state"; retry rather than treat it as an empty
  # screen, which would silently end the walk.
  local i
  for i in 1 2 3; do
    if "${ADB[@]}" shell uiautomator dump /sdcard/_uiwalk.xml >/dev/null 2>&1; then
      "${ADB[@]}" shell cat /sdcard/_uiwalk.xml 2>/dev/null > "$UIXML"
      [ -s "$UIXML" ] && return 0
    fi
    sleep 1
  done
  return 1
}

# Emit "x y key" for every clickable node, centre point first. The key is the
# identity we de-duplicate on WITHIN a round; across rounds we tap again, since
# the same control can reach different code once the model has changed.
#
# The XML arrives as a FILE PATH, not on stdin. `python3 - <<'PY'` takes the
# program text from stdin, so a pipe into it is consumed by the heredoc and
# `sys.stdin.read()` returns "". Measured 2026-08-07: every round reported
# "0 clickable" against a screen that a hand-run dump showed had 14, and the run
# still finished with a well-formed number identical to the undriven one.
clickable_points() {
  python3 - "$UIXML" <<'PY'
import re, sys
xml = open(sys.argv[1]).read()
seen = set()
for node in re.findall(r'<node[^>]*/?>', xml):
    if 'clickable="true"' not in node:
        continue
    b = re.search(r'bounds="\[(\d+),(\d+)\]\[(\d+),(\d+)\]"', node)
    if not b:
        continue
    x1, y1, x2, y2 = map(int, b.groups())
    if x2 <= x1 or y2 <= y1:          # zero-area node: not tappable
        continue
    rid = re.search(r'resource-id="([^"]*)"', node)
    cd  = re.search(r'content-desc="([^"]*)"', node)
    tx  = re.search(r'text="([^"]*)"', node)
    key = (rid.group(1) if rid else '') or (cd.group(1) if cd else '') or (tx.group(1) if tx else '') or f'{x1},{y1}'
    if key in seen:
        continue
    seen.add(key)
    print((x1 + x2) // 2, (y1 + y2) // 2, key.replace(' ', '_')[:40])
PY
}

echo "[ui-walk] $PKG rounds=$ROUNDS" >&2

for round in $(seq 1 "$ROUNDS"); do
  dump_ui || { echo "[ui-walk] round $round: uiautomator dump failed" >&2; break; }
  # NOT mapfile: macOS ships bash 3.2 and the harness invokes this with plain
  # `bash`. Measured 2026-08-07 — `mapfile: command not found` followed by
  # `points: unbound variable`, and the walk did nothing while the run still
  # reported a clean 41/1478, identical to the undriven number. A driver that
  # cannot run must not look like a driver that found nothing.
  points=()
  while IFS= read -r _line; do
    [ -n "$_line" ] && points+=("$_line")
  done < <(clickable_points)
  echo "[ui-walk] round $round: ${#points[@]} clickable" >&2
  [ ${#points[@]} -eq 0 ] && break

  for p in "${points[@]}"; do
    read -r x y key <<<"$p"
    "${ADB[@]}" shell input tap "$x" "$y" >/dev/null 2>&1
    "${ADB[@]}" shell "sleep $(awk "BEGIN{print $SETTLE_MS/1000}")" >/dev/null 2>&1

    # Deliberately NO back-press per tap. An earlier version returned to the
    # enumerated screen after every tap so the remaining coordinates stayed
    # valid; that also meant no screen below the root was ever left standing
    # long enough to render, which is exactly the code a driver exists to reach.
    # Drifting off the enumerated screen costs a few taps on empty space; the
    # re-dump at the top of the next round re-enumerates wherever we ended up,
    # so drift becomes additional coverage rather than lost coverage.

    # A tap can leave the app entirely (BACK on the root, or an external
    # intent). Relaunch WITHOUT force-stop: force-stop remounts the View and
    # re-fires paths already counted — harmless to the number, since ids are
    # de-duplicated, but it spends the rest of the walk re-treading mount code.
    if ! "${ADB[@]}" shell pidof "$PKG" >/dev/null 2>&1; then
      "${ADB[@]}" shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1
      "${ADB[@]}" shell "sleep 2" >/dev/null 2>&1
    fi
  done

  # Return towards the root and scroll, so the next round enumerates rows that
  # were off-screen rather than re-walking the same visible ones.
  "${ADB[@]}" shell input keyevent 4 >/dev/null 2>&1
  "${ADB[@]}" shell "sleep 0.5" >/dev/null 2>&1
  "${ADB[@]}" shell input swipe 540 1600 540 700 300 >/dev/null 2>&1
  "${ADB[@]}" shell "sleep 0.5" >/dev/null 2>&1
done

"${ADB[@]}" shell rm -f /sdcard/_uiwalk.xml >/dev/null 2>&1
echo "[ui-walk] done" >&2
exit 0
