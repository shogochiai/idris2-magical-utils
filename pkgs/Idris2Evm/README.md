# idris2-evm

Pure Idris2 EVM Interpreter with coverage collection.

## Overview

`idris2-evm` is a pure Idris2 EVM bytecode interpreter.

When compiled with Idris2's `--coverage` flag, the Chez Scheme profiler tracks which interpreter branches are executed, providing semantic coverage of EVM/Yul code.

**Key Features:**
- Pure Idris2 implementation (no external dependencies at runtime)
- Full EVM opcode support (arithmetic, memory, storage, control flow)
- Disassembler for bytecode analysis
- Coverage collection via Chez Scheme profiler

## Requirements

- [Idris2](https://github.com/idris-lang/Idris2) with Chez Scheme backend
- [pack](https://github.com/stefan-hoeck/idris2-pack) - Idris2 package manager

## Installation

```bash
# Clone the repository
git clone https://github.com/shogochiai/idris2-evm
cd idris2-evm

# Build with pack
pack build idris2-evm

# Or install globally
pack install-app idris2-evm
```

## Usage

```bash
idris2-evm-run [options] <bytecode-file>
idris2-evm-run --bytecode 0x6001... [options]
```

### Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `-v, --version` | Show version |
| `--verbose` | Verbose output (show each step) |
| `-d, --disassemble` | Disassemble bytecode and exit |
| `--gas <limit>` | Gas limit (default: 1000000) |
| `--calldata <hex>` | Calldata as hex (e.g., 0x371303c0) |
| `--bytecode <hex>` | Bytecode as hex (alternative to file) |

### Examples

```bash
# Execute bytecode file with calldata
idris2-evm-run --calldata 0x371303c0 contract.bin

# Execute inline bytecode (increment storage slot 0)
idris2-evm-run --bytecode 0x60016000540160005500

# Disassemble bytecode
idris2-evm-run -d contract.bin
```

## Supported Opcodes

- **Arithmetic:** ADD, MUL, SUB, DIV, MOD, EXP, etc.
- **Comparison:** LT, GT, EQ, ISZERO, etc.
- **Bitwise:** AND, OR, XOR, NOT, SHL, SHR, etc.
- **Memory:** MLOAD, MSTORE, MSTORE8
- **Storage:** SLOAD, SSTORE
- **Control:** JUMP, JUMPI, JUMPDEST, STOP, RETURN, REVERT
- **Stack:** POP, PUSH0-PUSH32, DUP1-DUP16, SWAP1-SWAP16
- **Environment:** CALLER, CALLVALUE, CALLDATALOAD, etc.
- **Block:** NUMBER, TIMESTAMP, CHAINID, etc.

## Architecture

```
src/
├── Main.idr           # CLI entry point and argument parsing
└── EVM/
    ├── Interpreter.idr # EVM interpreter (step execution)
    ├── Opcodes.idr     # Opcode definitions and decoding
    ├── Bytecode.idr    # Bytecode parsing and disassembly
    ├── Word256.idr     # 256-bit word arithmetic
    ├── Stack.idr       # EVM stack operations
    ├── Memory.idr      # EVM memory model
    └── Storage.idr     # EVM storage model
```

## Coverage Collection

### Interpreter Branch Coverage

To collect interpreter branch coverage (which EVM opcodes were executed), rebuild with `--coverage`:

```bash
idris2 --cg chez --coverage src/Main.idr -o idris2-evm-run
```

The Chez `.ssi` files will contain hit counts for each branch in the interpreter.

### Function-Level Coverage (with idris2-yul)

For Idris2 function-level coverage of compiled contracts, use with `idris2-evm-coverage`:

```
Idris2 Source → idris2-yul (@source comments) → EVM bytecode
                                                      ↓
                           idris2-evm-coverage ← EVM trace (hit PCs)
                                   ↓
                         Idris2 Function Coverage Report
```

See [idris2-evm-coverage](https://github.com/shogochiai/idris2-evm-coverage) for details.

## Development

### Dependencies

- `base` - Idris2 standard library
- `contrib` - Idris2 contrib library

### Building

```bash
pack build idris2-evm
```

### Testing

```bash
# Run inline bytecode test
pack run idris2-evm -- --bytecode 0x60016000540160005500

# Disassemble test contract
pack run idris2-evm -- -d test/Counter.yul
```

## License

MIT

## Authors

LazyEvm Team
