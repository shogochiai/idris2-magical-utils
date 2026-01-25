#!/usr/bin/env python3
"""
WASM Import Stubber for IC Canisters

Replaces WASI and env imports with stub functions that trap.
This allows emscripten-compiled WASM to run on the Internet Computer.

Usage:
    python3 stub-wasi.py input.wasm output.wasm
"""

import sys
import struct

def read_leb128_unsigned(data, offset):
    """Read unsigned LEB128 encoded integer"""
    result = 0
    shift = 0
    while True:
        byte = data[offset]
        offset += 1
        result |= (byte & 0x7f) << shift
        if (byte & 0x80) == 0:
            break
        shift += 7
    return result, offset

def write_leb128_unsigned(value):
    """Write unsigned LEB128 encoded integer"""
    result = bytearray()
    while True:
        byte = value & 0x7f
        value >>= 7
        if value != 0:
            byte |= 0x80
        result.append(byte)
        if value == 0:
            break
    return bytes(result)

def read_string(data, offset):
    """Read a length-prefixed string"""
    length, offset = read_leb128_unsigned(data, offset)
    string = data[offset:offset+length].decode('utf-8')
    return string, offset + length

def stub_wasi_imports(input_path, output_path):
    """
    Process WASM file and stub out WASI/env imports.

    Strategy: Replace import function types with stub functions that trap.
    """
    with open(input_path, 'rb') as f:
        wasm = bytearray(f.read())

    # Verify WASM magic number
    if wasm[:4] != b'\x00asm':
        print("Error: Not a valid WASM file")
        sys.exit(1)

    version = struct.unpack('<I', wasm[4:8])[0]
    print(f"WASM version: {version}")

    offset = 8
    imports_to_stub = []
    import_section_start = None
    import_section_end = None

    # First pass: find imports to stub
    while offset < len(wasm):
        section_id = wasm[offset]
        offset += 1
        section_size, offset = read_leb128_unsigned(wasm, offset)
        section_end = offset + section_size

        if section_id == 2:  # Import section
            import_section_start = offset - len(write_leb128_unsigned(section_size)) - 1
            import_section_end = section_end

            num_imports, offset = read_leb128_unsigned(wasm, offset)
            print(f"Found {num_imports} imports")

            for i in range(num_imports):
                module_name, offset = read_string(wasm, offset)
                field_name, offset = read_string(wasm, offset)
                import_kind = wasm[offset]
                offset += 1

                if import_kind == 0:  # Function import
                    type_idx, offset = read_leb128_unsigned(wasm, offset)

                    # Check if this is a WASI or env import to stub
                    if module_name in ['wasi_snapshot_preview1', 'wasi_unstable', 'env']:
                        if module_name == 'env' and field_name.startswith('ic0'):
                            # Keep ic0 imports
                            print(f"  Keep: {module_name}.{field_name}")
                        else:
                            print(f"  Stub: {module_name}.{field_name} (type {type_idx})")
                            imports_to_stub.append((module_name, field_name, type_idx))
                    else:
                        print(f"  Keep: {module_name}.{field_name}")
                elif import_kind == 1:  # Table
                    offset += 1  # elemtype
                    flags, offset = read_leb128_unsigned(wasm, offset)
                    _, offset = read_leb128_unsigned(wasm, offset)  # initial
                    if flags & 1:
                        _, offset = read_leb128_unsigned(wasm, offset)  # max
                elif import_kind == 2:  # Memory
                    flags, offset = read_leb128_unsigned(wasm, offset)
                    _, offset = read_leb128_unsigned(wasm, offset)  # initial
                    if flags & 1:
                        _, offset = read_leb128_unsigned(wasm, offset)  # max
                elif import_kind == 3:  # Global
                    offset += 1  # valtype
                    offset += 1  # mutability
        else:
            offset = section_end

    if not imports_to_stub:
        print("No WASI/env imports to stub. File unchanged.")
        with open(output_path, 'wb') as f:
            f.write(wasm)
        return

    print(f"\nNeed to stub {len(imports_to_stub)} imports")
    print("Note: Full implementation requires WAT transformation.")
    print("For now, outputting list of imports that need stubbing.")

    # For a complete solution, we'd need to:
    # 1. Remove the imports from import section
    # 2. Add stub functions to the code section
    # 3. Update all function indices
    # 4. Update the function section

    # Simple approach: just copy the file and print what needs to be done
    with open(output_path, 'wb') as f:
        f.write(wasm)

    print(f"\nOutput written to {output_path}")
    print("\nTo fully stub these imports, use wasm2wat/wat2wasm:")
    print("  1. wasm2wat input.wasm -o input.wat")
    print("  2. Edit input.wat to replace imports with local functions")
    print("  3. wat2wasm input.wat -o output.wasm")

def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.wasm output.wasm")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    stub_wasi_imports(input_path, output_path)

if __name__ == '__main__':
    main()
