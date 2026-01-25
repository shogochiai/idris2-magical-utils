#!/bin/bash
# WASM WASI Import Stubber for IC Canisters
# Replaces WASI imports with stub functions
#
# Usage: ./stub-wasi.sh input.wasm output.wasm

set -e

INPUT="${1:?Usage: $0 input.wasm output.wasm}"
OUTPUT="${2:?Usage: $0 input.wasm output.wasm}"

# Check dependencies
command -v wasm2wat >/dev/null || { echo "wasm2wat not found (install wabt)"; exit 1; }
command -v wat2wasm >/dev/null || { echo "wat2wasm not found (install wabt)"; exit 1; }

TEMP_WAT=$(mktemp /tmp/wasm-stub-XXXXXX.wat)
TEMP_WAT2=$(mktemp /tmp/wasm-stub2-XXXXXX.wat)
trap "rm -f $TEMP_WAT $TEMP_WAT2" EXIT

echo ">>> Converting WASM to WAT..."
wasm2wat "$INPUT" -o "$TEMP_WAT"

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

# First pass: identify WASI imports and their types
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

echo ">>> Transforming WAT..."
python3 /tmp/stub_wasi.py "$TEMP_WAT" "$TEMP_WAT2"

echo ">>> Remaining imports:"
grep -E '^\s*\(import' "$TEMP_WAT2" | head -10 || echo "(none)"

echo ">>> Converting back to WASM..."
wat2wasm "$TEMP_WAT2" -o "$OUTPUT" 2>&1

echo ">>> Done!"
ls -la "$OUTPUT"

# Verify with wasm-objdump if available
if command -v wasm-objdump >/dev/null; then
    echo ""
    echo ">>> Imports in output WASM:"
    wasm-objdump -x "$OUTPUT" | grep -E "Import\[" -A20 | head -25
fi
