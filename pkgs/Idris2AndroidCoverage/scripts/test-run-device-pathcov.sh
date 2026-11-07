#!/usr/bin/env bash
# test-run-device-pathcov.sh — offline tests for run-device-pathcov.sh (REQ_ANDCOV_008, REQ_ANDCOV_009).
# A fake `adb` on PATH plays the device: its streaming `logcat` reader LOSES some IDRIS_PATHHIT lines that its
# `logcat -d` ring dump still holds (what a logd reader skip / a stream behind the stop rule looks like), so the
# test pins that the harness counts the union, stops its reader, restores the ring size, and applies
# --exclusions. Needs the built device-pathcov (pack build idris2-android-coverage); no device.
#   bash scripts/test-run-device-pathcov.sh      → exit 0 iff every check passes
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HARNESS="${HARNESS_UNDER_TEST:-$HERE/run-device-pathcov.sh}"   # override: run these checks against another copy
T="$(mktemp -d)"
trap '[ -n "${KEEP:-}" ] || rm -rf "$T"; [ -n "${KEEP:-}" ] && echo "kept $T"' EXIT
FAILS=0
check() { if eval "$2"; then echo "  PASS $1"; else echo "  FAIL $1"; FAILS=$((FAILS + 1)); fi; }

PKGROOT="$(cd "$HERE/.." && pwd)"
if ! find "$PKGROOT/build/exec" -maxdepth 1 -type f -name device-pathcov -perm -u+x 2>/dev/null | grep -q .; then
  echo "SKIP: device-pathcov not built (pack build idris2-android-coverage)"; exit 1
fi

# --- the fake device -----------------------------------------------------------------------------------------------
mkdir -p "$T/bin"
cat > "$T/bin/adb" <<'ADB'
#!/usr/bin/env bash
# fake adb. State in $FAKE_ADB_DIR. Logs every call.
D="$FAKE_ADB_DIR"
echo "$*" >> "$D/calls"
[ "${1:-}" = "-s" ] && shift 2
case "$*" in
  get-state) echo device ;;
  "shell am force-stop "*|"shell monkey "*) : ;;
  "logcat -c") : ;;
  "logcat -b main -g") echo "main: ring buffer is 256 KiB (65 KiB consumed, 18 KiB readable), max entry is 5120 B, max payload is 4068 B" ;;
  "logcat -b main -G "*) a="$*"; echo "${a##* }" >> "$D/ringsize" ;;
  "logcat -d -b main") cat "$D/ring.txt" ;;
  logcat)
    echo $$ > "$D/stream.pid"
    cat "$D/stream.txt"
    exec sleep 300 ;;    # a real stream never ends by itself
  *) echo "fake adb: unexpected: $*" >&2; exit 1 ;;
esac
ADB
chmod +x "$T/bin/adb"

line() { echo "11-07 17:47:55.900  4242  4300 I ReactNativeJS: IDRIS_PATHHIT:$1"; }
mkdir -p "$T/dev"
{
  echo "11-07 17:47:50.000  4242  4300 I ReactNativeJS: IDRIS_PATHHIT_HOOK_INSTALLED"
  for id in 'App.View.view#p0' 'App.Canister.vote#p0' 'App.View.case block in case block in voiceHome,primaryAct#p0' \
            'App.Live.ageWordsJa#p0' 'App.Me.roleBadge#p0'; do line "$id"; done
  echo "11-07 17:47:56.000  4242  4300 I ReactNativeJS: IDRIS_REPLAY_DONE 1/1"
} > "$T/dev/ring.txt"
# The stream reader saw only the first two hits (the comma/space ids and the rest never reached it).
grep -v -e 'primaryAct' -e 'ageWordsJa' -e 'roleBadge' "$T/dev/ring.txt" > "$T/dev/stream.txt"

cat > "$T/paths.json" <<'JSON'
{"functions": [{"paths": [
 {"path_id": "App.View.view#p0"}, {"path_id": "App.View.view#p1"},
 {"path_id": "App.Canister.vote#p0"}, {"path_id": "App.Canister.vote#p1"},
 {"path_id": "App.View.case block in case block in voiceHome,primaryAct#p0"},
 {"path_id": "App.Live.ageWordsJa#p0"}, {"path_id": "App.Me.roleBadge#p0"}
]}]}
JSON
printf '# comment\n\nApp.Canister.   # IO only\n' > "$T/excl.txt"

run() { FAKE_ADB_DIR="$T/dev" PATH="$T/bin:$PATH" TMPDIR="$T" bash "$HARNESS" --package app.id --paths "$T/paths.json" --settle 1 "$@"; }

# --- 1. union of stream and dump; reader stopped; ring restored ----------------------------------------------------
: > "$T/dev/calls"; : > "$T/dev/ringsize"
run > "$T/out1" 2> "$T/err1"; rc1=$?
check "008 harness exit 0 (admissible run)"            '[ "$rc1" = 0 ]'
check "008 all 5 logged ids counted (stream had 2)"      'grep -qx "paths_hit: 5" "$T/out1"'
check "008 denominator 7, nothing excluded w/o file"     'grep -qx "paths_denominator: 7" "$T/out1" && grep -qx "paths_excluded: 0" "$T/out1"'
check "008 comma+space id counted"                        '! grep -q "voiceHome,primaryAct" "$T/out1"'
check "008 stderr names the dump-only ids"                'grep -q "numerator: union 5 ids (stream 2, dump 5; only in stream 0, only in dump 3)" "$T/err1"'
spid="$(cat "$T/dev/stream.pid" 2>/dev/null)"
check "008 stream reader stopped at exit"                 '[ -n "$spid" ] && ! kill -0 "$spid" 2>/dev/null'
check "008 ring enlarged to 16M then restored to 256K"    '[ "$(tr "\n" " " < "$T/dev/ringsize")" = "16M 256K " ]'
check "008 no harness temp files left"                    '[ -z "$(find "$T" -mindepth 1 -maxdepth 1 -name "tmp.*" -print -quit)" ]'

# --- 2. --exclusions applied with prefix semantics, counted --------------------------------------------------------
run --exclusions "$T/excl.txt" > "$T/out2" 2> "$T/err2"; rc2=$?
check "009 harness exit 0 with --exclusions"              '[ "$rc2" = 0 ]'
check "009 Canister prefix excluded: denominator 5"       'grep -qx "paths_denominator: 5" "$T/out2"'
check "009 paths_excluded 2 = paths_excluded_project 2"   'grep -qx "paths_excluded: 2" "$T/out2" && grep -qx "paths_excluded_project: 2" "$T/out2"'
check "009 hit 4 (Canister hit no longer counted)"        'grep -qx "paths_hit: 4" "$T/out2"'
check "009 report names the file"                         'grep -q "^exclusions_file: $T/excl.txt (1 pattern(s))" "$T/out2"'

# --- 3. a named file that does not exist is refused before any device I/O ----------------------------------------
: > "$T/dev/calls"
run --exclusions "$T/nope.txt" > "$T/out3" 2> "$T/err3"; rc3=$?
check "009 missing exclusions file → exit 2"              '[ "$rc3" = 2 ] && grep -q "exclusions file not found" "$T/err3"'
check "009 ... and no adb call made"                      '[ ! -s "$T/dev/calls" ]'

echo "Failures: $FAILS"
[ "$FAILS" = 0 ]
