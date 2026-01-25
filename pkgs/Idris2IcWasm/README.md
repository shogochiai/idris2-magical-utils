# idris2-wasm

Idris2 to WebAssembly compilation pipeline with Source Map support for ICP canisters.

## Pipeline

```
Idris2 (.idr)
    │
    ▼ idris2 --codegen refc
C code (.c)  ──────────────────┐
    │                          │ Parse RefC comments
    ▼ emcc -gsource-map        ▼
WASM (.wasm) + C→WASM map    Idris2→C map
    │                          │
    ▼ WASI stubbing            │
Stubbed WASM                   │
    │                          │
    ▼ Chain maps ◄─────────────┘
Final WASM + Idris2→WASM Source Map
```

## Prerequisites

- [Idris2](https://github.com/idris-lang/Idris2) with RefC backend
- [Emscripten](https://emscripten.org/) via emsdk (4.0.22+)
- [dfx](https://internetcomputer.org/docs/current/developer-docs/setup/install) (for IC deployment)
- [wabt](https://github.com/WebAssembly/wabt) (optional, for WASI stubbing)

## Quick Start

### 1. Build the CLI tool

```bash
cd idris2-wasm
idris2 --build idris2-wasm.ipkg
```

### 2. Build your canister

```bash
# From your project directory
idris2-wasm build

# Or with options
idris2-wasm build --project-dir ./my-project --name my_canister
```

### 3. Output files

```
your-project/build/
├── exec/your_canister.c     # RefC generated C (with source comments)
├── idris2-c.map             # Idris2 → C Source Map
├── your_canister.wasm       # Raw WASM
├── your_canister.wasm.map   # C → WASM Source Map (emscripten)
└── your_canister_stubbed.wasm  # Final WASM for IC
```

### 4. Deploy to IC

```bash
dfx start --background
dfx deploy your_canister

# Call methods
dfx canister call your_canister greet
```

## Source Maps

### What are Source Maps?

Source Maps allow debugging tools to map compiled code back to original source. The `idris2-wasm` pipeline generates:

| File | Maps From | Maps To |
|------|-----------|---------|
| `idris2-c.map` | C line numbers | Idris2 source locations |
| `*.wasm.map` | WASM addresses | C line numbers |

### RefC Comment Format

The Idris2 RefC backend embeds source location comments in generated C:

```c
Value *Main_greet(Value * var_0)
{
    idris2_removeReference(var_0);
    return NULL;                    // Main:49:8--49:20
}
```

Format: `// Module:startLine:startCol--endLine:endCol`

### Using Source Maps

**With Node.js source-map library:**

```javascript
const { SourceMapConsumer } = require('source-map');
const fs = require('fs');

const map = JSON.parse(fs.readFileSync('build/idris2-c.map'));
const consumer = await new SourceMapConsumer(map);

// Find Idris2 source for C line 100
const pos = consumer.originalPositionFor({ line: 100, column: 0 });
console.log(pos);
// { source: 'Main.idr', line: 42, column: 8, name: null }
```

**For code coverage (idris2-dfx-coverage):**

```bash
# 1. Instrument WASM
ic-wasm instrument your_canister_stubbed.wasm -o instrumented.wasm

# 2. Run tests and collect traces
dfx canister call ...

# 3. Map WASM addresses → Idris2 lines using idris2-c.map
```

## CLI Reference

```
idris2-wasm build [OPTIONS]

OPTIONS:
  --project-dir DIR    Project directory (default: current)
  --name NAME          Canister name (default: from .ipkg)
  --main MODULE        Main module path (default: src/Main.idr)
  -p, --package PKG    Additional packages
  -h, --help           Show help
```

## Project Structure

```
idris2-wasm/
├── src/
│   ├── CLI.idr                      # Command-line interface
│   └── WasmBuilder/
│       ├── WasmBuilder.idr          # Build pipeline orchestration
│       ├── SourceMap/
│       │   ├── VLQ.idr              # Base64 VLQ encoder/decoder
│       │   ├── SourceMap.idr        # RefC parser & Source Map V3
│       │   ├── VLQTests.idr         # VLQ unit tests
│       │   └── SourceMapTests.idr   # Source Map unit tests
│       ├── IC0/
│       │   ├── FFI.idr              # C ↔ Idris2 bridge
│       │   ├── Call.idr             # Inter-canister calls
│       │   └── Stable.idr           # Stable memory
│       └── Tests/
│           └── AllTests.idr         # Integration tests
├── support/
│   └── ic0/
│       ├── ic0_stubs.c              # IC0 system API wrappers
│       ├── canister_entry.c         # Canister entry points
│       ├── wasi_stubs.c             # WASI stub implementations
│       └── ic_ffi_bridge.c          # FFI bridge implementation
├── examples/
│   ├── hello/Main.idr               # Hello World
│   └── canister/Main.idr            # ICP canister example
└── idris2-wasm.ipkg                 # Package configuration
```

## Build Pipeline Details

### Step 1: Idris2 → C (RefC)

```bash
idris2 --codegen refc --build your-project.ipkg
```

Generates C code with embedded source location comments.

### Step 2: Prepare Runtime

Downloads RefC runtime and mini-gmp (cached in `/tmp/`).

### Step 3: C → WASM (Emscripten)

```bash
emcc your_canister.c \
  -s STANDALONE_WASM=1 \
  -s FILESYSTEM=0 \
  -s ERROR_ON_UNDEFINED_SYMBOLS=0 \
  --no-entry \
  -g -gsource-map \
  -O2
```

Key flags:
- `-g -gsource-map`: Generate C→WASM source map
- `STANDALONE_WASM=1`: No JavaScript glue
- `--no-entry`: No main function (IC calls exports)

### Step 4: WASI Stubbing

Replaces unsupported WASI imports with stubs:
- `fd_close`, `fd_write`, `fd_seek` → return 0

### Step 5: Source Map Generation

Parses RefC comments and generates `idris2-c.map` (Source Map V3 format).

## Source Map V3 Format

```json
{
  "version": 3,
  "file": "your_canister.c",
  "sourceRoot": "",
  "sources": ["Main.idr", "Lib.idr"],
  "names": [],
  "mappings": "AAAA;AACA;AAEA,OAAO..."
}
```

The `mappings` field uses Base64 VLQ encoding to compactly represent:
- Generated line/column → Original line/column mappings

## Running Tests

```bash
# Build package
idris2 --build idris2-wasm.ipkg

# Run VLQ tests (18 tests)
echo ':exec printLn runVLQTests' | idris2 --find-ipkg src/WasmBuilder/SourceMap/VLQTests.idr
# Output: (18, 0)

# Run Source Map tests (9 tests)
echo ':exec printLn runSourceMapTests' | idris2 --find-ipkg src/WasmBuilder/SourceMap/SourceMapTests.idr
# Output: (9, 0)
```

## Roadmap

- [x] Idris2 → C → WASM pipeline
- [x] IC0 canister support
- [x] WASI import stubbing
- [x] Source Map generation (Idris2 → C)
- [x] Emscripten source map integration
- [ ] Full source map chaining (Idris2 → WASM)
- [ ] Candid encoding support
- [ ] Code coverage integration (idris2-dfx-coverage)

## Known Limitations

- Source map chaining is simplified (uses Idris2→C map directly)
- Returns raw bytes instead of Candid-encoded data
- Limited IC0 API surface (expanding)

## License

MIT
