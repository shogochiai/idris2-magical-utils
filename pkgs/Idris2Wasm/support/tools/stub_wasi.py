#!/usr/bin/env python3
"""
WASI Import Stubber for IC WASM

Replaces WASI imports with stub functions that return 0.
Parses type definitions from WAT to generate correct signatures.
"""
import sys
import re

def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input.wat> <output.wat>", file=sys.stderr)
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    with open(input_file) as f:
        content = f.read()

    # Parse type definitions: (type (;N;) (func ...))
    # Process line by line to handle nested parentheses correctly
    types = {}
    for line in content.split('\n'):
        line = line.strip()
        m = re.match(r'\(type \(;(\d+);\) \(func(.*)\)\)', line)
        if m:
            tid, sig = m.groups()
            types[int(tid)] = sig.strip()

    # Process lines
    lines = content.split('\n')
    output = []

    # Track function index for imports
    func_idx = 0

    for line in lines:
        # Match WASI imports with named functions: (func $name (type N))
        m = re.search(r'\(import "wasi_snapshot_preview1" "(\w+)" \(func \$(\w+) \(type (\d+)\)\)\)', line)
        if m:
            name, func_name, tid = m.groups()
            sig = types.get(int(tid), '')

            # Generate stub: return appropriate zero value based on result type
            if '(result i32)' in sig or sig.endswith('result i32'):
                ret = 'i32.const 0'
            elif '(result i64)' in sig:
                ret = 'i64.const 0'
            elif '(result f32)' in sig:
                ret = 'f32.const 0'
            elif '(result f64)' in sig:
                ret = 'f64.const 0'
            else:
                ret = ''

            # Keep function name for debugging
            output.append(f'  (func ${func_name} (type {tid}) {sig} {ret})')
            func_idx += 1
            continue

        # Match WASI imports with index only: (func (;N;) (type M))
        m = re.search(r'\(import "wasi_snapshot_preview1" "(\w+)" \(func \(;(\d+);\) \(type (\d+)\)\)\)', line)
        if m:
            name, idx, tid = m.groups()
            sig = types.get(int(tid), '')

            # Generate stub: return appropriate zero value based on result type
            if '(result i32)' in sig or sig.endswith('result i32'):
                ret = 'i32.const 0'
            elif '(result i64)' in sig:
                ret = 'i64.const 0'
            elif '(result f32)' in sig:
                ret = 'f32.const 0'
            elif '(result f64)' in sig:
                ret = 'f64.const 0'
            else:
                ret = ''

            output.append(f'  (func (;{idx};) (type {tid}) {sig} {ret})')
        else:
            output.append(line)

    with open(output_file, 'w') as f:
        f.write('\n'.join(output))

if __name__ == '__main__':
    main()
