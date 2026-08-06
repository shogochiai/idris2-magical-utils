#!/bin/bash
# WASM WASI Import Stubber for IC Canisters
# Replaces WASI imports with stub functions
#
# Usage: ./stub-wasi.sh input.wasm output.wasm

set -e

INPUT="${1:?Usage: $0 input.wasm output.wasm}"
OUTPUT="${2:?Usage: $0 input.wasm output.wasm}"

# Toolchain selection: wabt if present, else binaryen.
#
# emsdk ships binaryen (wasm-dis/wasm-as) but NOT wabt, so on a host whose only
# wasm toolchain came with emscripten this script used to abort at "wasm2wat not
# found" — after emcc had already produced the unstubbed .wasm. That aborts the
# whole canister build (measured 2026-08-01, alice: `>>> Step 4: WASI stubbing` /
# `wasm2wat not found (install wabt)` and no global_registry_stubbed.wasm), for a
# dependency the build otherwise does not need. Prefer wabt to keep existing
# hosts byte-identical; fall back to binaryen rather than fail.
#
# The two disassemblers emit different WAT dialects and each needs its own
# transformer:
#   wabt:     (import "wasi…" "fd_close" (func $__wasi_fd_close (type 0)))
#   binaryen: (import "wasi…" "fd_close" (func $__wasi_fd_close (param i32) (result i32)))
# i.e. binaryen inlines the signature instead of referencing a (type N).
BACKEND=""
if command -v wasm2wat >/dev/null && command -v wat2wasm >/dev/null; then
  BACKEND=wabt
else
  # Look on PATH first, then beside emcc (emsdk's upstream/bin holds binaryen).
  DIS=$(command -v wasm-dis || true)
  ASM=$(command -v wasm-as || true)
  if [ -z "$DIS" ] || [ -z "$ASM" ]; then
    EMCC_PATH=$(command -v emcc || true)
    if [ -n "$EMCC_PATH" ]; then
      EM_BIN="$(cd "$(dirname "$(readlink "$EMCC_PATH" || echo "$EMCC_PATH")")/../bin" 2>/dev/null && pwd || true)"
      [ -x "$EM_BIN/wasm-dis" ] && DIS="$EM_BIN/wasm-dis"
      [ -x "$EM_BIN/wasm-as" ] && ASM="$EM_BIN/wasm-as"
    fi
  fi
  if [ -n "$DIS" ] && [ -n "$ASM" ]; then
    BACKEND=binaryen
  fi
fi

if [ -z "$BACKEND" ]; then
  echo "no wasm text toolchain found: need either wabt (wasm2wat/wat2wasm) or binaryen (wasm-dis/wasm-as)"
  exit 1
fi
echo ">>> WAT backend: $BACKEND"

TEMP_WAT=$(mktemp /tmp/wasm-stub-XXXXXX.wat)
TEMP_WAT2=$(mktemp /tmp/wasm-stub2-XXXXXX.wat)
TEMP_PY_BINARYEN=$(mktemp /tmp/wasm-stub-binaryen-XXXXXX.py)
trap "rm -f $TEMP_WAT $TEMP_WAT2 $TEMP_PY_BINARYEN" EXIT

echo ">>> Converting WASM to WAT..."
if [ "$BACKEND" = wabt ]; then
  wasm2wat "$INPUT" -o "$TEMP_WAT"
else
  # --all-features: the module uses bulk-memory (memory.copy). Without it the
  # round trip fails on the way back in, not here.
  "$DIS" --all-features "$INPUT" -o "$TEMP_WAT"
fi

echo ">>> Analyzing WASI imports..."
grep -E '^\s*\(import "wasi_snapshot_preview1"' "$TEMP_WAT" || echo "(no WASI imports found)"

echo ">>> Creating Python transformer..."
cat > /tmp/stub_wasi.py << 'PYEOF'
import sys
import re

wat_file = sys.argv[1]
output_file = sys.argv[2]

with open(wat_file, 'r') as f:
    content = f.read()

# Track function indices to remap
# We need to find the WASI import function indices and replace them

lines = content.split('\n')
new_lines = []
stub_funcs = []
import_count = 0
wasi_imports = {}  # func_idx -> (name, type_idx)

# First pass: identify WASI imports and env imports to stub
for i, line in enumerate(lines):
    # Match both formats:
    #   (import "wasi_snapshot_preview1" "fd_close" (func (;3;) (type 0)))
    #   (import "wasi_snapshot_preview1" "fd_close" (func $__wasi_fd_close (type 0)))
    m = re.match(r'\s*\(import "wasi_snapshot_preview1" "(\w+)" \(func (?:\(;(\d+);\)|\$(\w+)) \(type (\d+)\)\)\)', line)
    if m:
        name = m.group(1)
        func_idx = m.group(2) if m.group(2) else m.group(3)  # numeric or named
        type_idx = m.group(4)
        wasi_imports[func_idx] = (name, int(type_idx))
        print(f"  Found WASI import: {name} (func {func_idx}, type {type_idx})", file=sys.stderr)
    # Also match env imports (like emscripten_notify_memory_growth)
    m = re.match(r'\s*\(import "env" "(\w+)" \(func (?:\(;(\d+);\)|\$(\w+)) \(type (\d+)\)\)\)', line)
    if m:
        name = m.group(1)
        func_idx = m.group(2) if m.group(2) else m.group(3)
        type_idx = m.group(4)
        wasi_imports[func_idx] = ("env:" + name, int(type_idx))
        print(f"  Found env import: {name} (func {func_idx}, type {type_idx})", file=sys.stderr)

if not wasi_imports:
    print("No WASI imports to stub", file=sys.stderr)
    with open(output_file, 'w') as f:
        f.write(content)
    sys.exit(0)

# Find type definitions for WASI functions
type_defs = {}
for i, line in enumerate(lines):
    # Match: (type (;0;) (func (param i32) (result i32)))
    # Use greedy match for inner content
    m = re.search(r'\(type \(;(\d+);\) \(func ([^)]*(?:\([^)]*\))*[^)]*)\)\)', line)
    if m:
        type_idx = int(m.group(1))
        func_sig = m.group(2).strip()
        type_defs[type_idx] = func_sig
        # Debug: print found types
        if type_idx in [0, 6, 12]:
            print(f"  Type {type_idx}: [{func_sig}]", file=sys.stderr)

print(f">>> Found {len(wasi_imports)} WASI imports to stub", file=sys.stderr)

# Generate stub function bodies based on type signatures
def make_stub_body(type_sig):
    """Generate stub function body that returns appropriate defaults"""
    # Parse result type - check for any result keyword
    if 'result i32' in type_sig:
        return '(i32.const 0)'  # Return 0 (success for WASI)
    elif 'result i64' in type_sig:
        return '(i64.const 0)'
    elif 'result f32' in type_sig:
        return '(f32.const 0)'
    elif 'result f64' in type_sig:
        return '(f64.const 0)'
    return ''  # No result, empty body

# Second pass: transform the WAT
for line in lines:
    # Check if this is a WASI import to replace - handle both numeric and named formats
    m = re.match(r'(\s*)\(import "wasi_snapshot_preview1" "(\w+)" \(func (?:\(;(\d+);\)|\$(\w+)) \(type (\d+)\)\)\)', line)
    # Also match env imports
    m_env = re.match(r'(\s*)\(import "env" "(\w+)" \(func (?:\(;(\d+);\)|\$(\w+)) \(type (\d+)\)\)\)', line)
    if m_env:
        m = m_env  # Use env match
    if m:
        indent = m.group(1)
        name = m.group(2)
        func_idx_num = m.group(3)  # numeric index like (;3;)
        func_idx_name = m.group(4)  # named like $__wasi_fd_close
        type_idx = int(m.group(5))
        type_sig = type_defs.get(type_idx, '')
        stub_body = make_stub_body(type_sig)

        # Generate stub function - preserve the original function identifier format
        if func_idx_num:
            new_line = f'{indent}(func (;{func_idx_num};) (type {type_idx}) {stub_body})'
        else:
            new_line = f'{indent}(func ${func_idx_name} (type {type_idx}) {stub_body})'
        new_lines.append(new_line)
        print(f"  Stubbed: {name} -> {stub_body or '(nop)'}", file=sys.stderr)
    else:
        new_lines.append(line)

with open(output_file, 'w') as f:
    f.write('\n'.join(new_lines))

print(f">>> Wrote transformed WAT to {output_file}", file=sys.stderr)
PYEOF

cat > "$TEMP_PY_BINARYEN" << 'PYEOF'
"""Stub WASI/env imports in binaryen's WAT dialect (inline signatures).

Each matching import line becomes a local definition with the SAME name and the
SAME signature, and a body returning the neutral value, so every call site keeps
typechecking against an identical type. Nothing else in the module changes.

Unlike the wabt path there are no numeric function indices to remap: binaryen
names every function, so replacing the line in place is sufficient.
"""
import re
import sys

src, dst = sys.argv[1], sys.argv[2]
with open(src) as f:
    lines = f.read().split('\n')

# (import "MOD" "NAME" (func $ID <sig>))  — sig is the rest, minus two closers.
IMPORT = re.compile(
    r'^(\s*)\(import "(wasi_snapshot_preview1|env)" "([^"]+)" '
    r'\(func (\$[^\s)]+)(.*)\)\)\s*$')

RESULT = re.compile(r'\(result\s+([^\s)]+)')

NEUTRAL = {
    'i32': '(i32.const 0)',
    'i64': '(i64.const 0)',
    'f32': '(f32.const 0)',
    'f64': '(f64.const 0)',
}


def body_for(sig):
    m = RESULT.search(sig)
    if not m:
        return '(nop)'
    ty = m.group(1)
    if ty not in NEUTRAL:
        # An unknown result type would get a wrong-typed body and the assembler
        # would reject it with a confusing message. Say which one, and stop.
        print('unsupported result type in stub: %s' % ty, file=sys.stderr)
        sys.exit(2)
    return NEUTRAL[ty]


out = []
stubbed = {'wasi_snapshot_preview1': 0, 'env': 0}
for line in lines:
    m = IMPORT.match(line)
    if not m:
        out.append(line)
        continue
    indent, module, name, ident, sig = m.groups()
    out.append('%s(func %s%s\n%s %s\n%s)' % (indent, ident, sig, indent, body_for(sig), indent))
    stubbed[module] += 1
    # env imports are stubbed for parity with the wabt path, but printed
    # individually: a silently-zeroed env import is how a missing .c file once
    # shipped as a canister that just never replied (see canister-build.toml's
    # ic_schnorr.c note). Naming them here makes that visible in the build log.
    label = 'WASI' if module == 'wasi_snapshot_preview1' else 'env  ** check this is intentional **'
    print('  stubbed %s: %s %s' % (label, name, body_for(sig)), file=sys.stderr)

with open(dst, 'w') as f:
    f.write('\n'.join(out))

remaining = sum(l.count('"wasi_snapshot_preview1"') for l in out)
print('>>> stubbed wasi=%d env=%d; remaining wasi mentions=%d'
      % (stubbed['wasi_snapshot_preview1'], stubbed['env'], remaining), file=sys.stderr)
# Emitting a module that still imports WASI is the failure this script exists to
# prevent: the replica rejects it at install time (IC0505) with no useful
# diagnostic, long after the build reported success.
sys.exit(0 if remaining == 0 else 1)
PYEOF

echo ">>> Transforming WAT..."
if [ "$BACKEND" = wabt ]; then
  python3 /tmp/stub_wasi.py "$TEMP_WAT" "$TEMP_WAT2"
else
  python3 "$TEMP_PY_BINARYEN" "$TEMP_WAT" "$TEMP_WAT2"
fi

echo ">>> Remaining imports:"
grep -E '^\s*\(import' "$TEMP_WAT2" | head -10 || echo "(none)"

echo ">>> Converting back to WASM..."
if [ "$BACKEND" = wabt ]; then
  wat2wasm "$TEMP_WAT2" -o "$OUTPUT" 2>&1
else
  "$ASM" --all-features "$TEMP_WAT2" -o "$OUTPUT" 2>&1
fi

# Postcondition on the ARTIFACT, not on the intermediate text. Everything above
# can succeed and still ship a module the replica refuses; the only reading that
# settles it is disassembling what was actually written.
echo ">>> Verifying no WASI imports survive in the output..."
VERIFY_WAT=$(mktemp /tmp/wasm-stub-verify-XXXXXX.wat)
if [ "$BACKEND" = wabt ]; then
  wasm2wat "$OUTPUT" -o "$VERIFY_WAT"
else
  "$DIS" --all-features "$OUTPUT" -o "$VERIFY_WAT"
fi
SURVIVING=$(grep -a -c 'wasi_snapshot_preview1' "$VERIFY_WAT" || true)
rm -f "$VERIFY_WAT"
if [ "$SURVIVING" != "0" ]; then
  echo "FAIL: $SURVIVING wasi_snapshot_preview1 reference(s) remain in $OUTPUT"
  echo "      (installing this would fail with IC0505 at the replica, not here)"
  rm -f "$OUTPUT"
  exit 1
fi
echo ">>> Verified: 0 WASI imports"

echo ">>> Done!"
ls -la "$OUTPUT"

# Verify with wasm-objdump if available
if command -v wasm-objdump >/dev/null; then
    echo ""
    echo ">>> Imports in output WASM:"
    wasm-objdump -x "$OUTPUT" | grep -E "Import\[" -A20 | head -25
fi
