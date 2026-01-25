#!/usr/bin/env python3
"""
Proper WASI stubbing for ICP canisters.
Converts WASI imports to dummy local functions that return 0.
"""

import sys
import re

def stub_wasi(input_wat, output_wat):
    with open(input_wat, 'r') as f:
        content = f.read()

    lines = content.split('\n')
    output_lines = []
    wasi_imports = []

    # First pass: collect WASI imports and their function indices
    func_idx = 0
    for line in lines:
        # Match WASI import lines like:
        # (import "wasi_snapshot_preview1" "fd_close" (func (;3;) (type 0)))
        match = re.search(r'\(import "wasi_snapshot_preview1" "(\w+)" \(func \(;(\d+);\) \(type (\d+)\)\)\)', line)
        if match:
            func_name = match.group(1)
            func_index = int(match.group(2))
            type_index = int(match.group(3))
            wasi_imports.append({
                'name': func_name,
                'index': func_index,
                'type': type_index,
                'line': line
            })

    # Type signatures we need (from the WAT file analysis)
    type_signatures = {
        0: '(param i32) (result i32)',           # fd_close
        5: '(param i32 i32 i32 i32) (result i32)', # fd_write
        11: '(param i32 i64 i32 i32) (result i32)' # fd_seek
    }

    # Second pass: output with modifications
    stub_funcs_added = False
    for line in lines:
        # Skip WASI import lines
        if 'wasi_snapshot_preview1' in line:
            # Replace import with stub function
            match = re.search(r'\(import "wasi_snapshot_preview1" "(\w+)" \(func \(;(\d+);\) \(type (\d+)\)\)\)', line)
            if match:
                func_name = match.group(1)
                func_index = match.group(2)
                type_index = int(match.group(3))

                # Get the type signature
                sig = type_signatures.get(type_index, '(result i32)')

                # Create a stub function that returns 0
                stub_line = f'  (func (;{func_index};) (type {type_index}) {sig} i32.const 0)'
                output_lines.append(stub_line)
                print(f"Stubbed WASI import: {func_name} (func {func_index}, type {type_index})")
                continue

        output_lines.append(line)

    output_content = '\n'.join(output_lines)

    with open(output_wat, 'w') as f:
        f.write(output_content)

    print(f"Wrote stubbed WAT to {output_wat}")
    return len(wasi_imports)

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <input.wat> <output.wat>")
        sys.exit(1)

    input_wat = sys.argv[1]
    output_wat = sys.argv[2]

    count = stub_wasi(input_wat, output_wat)
    print(f"Stubbed {count} WASI imports")
