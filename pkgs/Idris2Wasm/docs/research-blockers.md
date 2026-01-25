# Research Questions: Idris2 → ICP Canister Blockers

## Root Problem
**Emscripten 3.1.6 adds WASI/env imports that IC doesn't support**

```
Unwanted imports:
├── wasi_snapshot_preview1.proc_exit
├── wasi_snapshot_preview1.fd_close
├── wasi_snapshot_preview1.fd_write
├── wasi_snapshot_preview1.fd_seek
├── env.abort
├── env.emscripten_memcpy_big
├── env.emscripten_resize_heap
└── env.setTempRet0
```

---

## Research Tree

### Branch 1: Emscripten Configuration
> Can we configure emscripten to not emit WASI/env imports?

1.1 **Latest emscripten (emsdk)**
- Does emscripten 3.1.50+ have better standalone WASM support?
- What flags remove WASI dependencies?
  - `-s PURE_WASI=0`?
  - `-s ENVIRONMENT='web'`?
  - `-s MINIMAL_RUNTIME=1`?

1.2 **Memory management flags**
- Can we use `-s ALLOW_MEMORY_GROWTH=0` to avoid `emscripten_resize_heap`?
- Does `-s TOTAL_MEMORY=X` eliminate heap growth imports?

1.3 **Exit handling**
- Can `-s EXIT_RUNTIME=0` remove `proc_exit`?
- What about `-s NO_EXIT_RUNTIME=1`?

1.4 **File I/O elimination**
- Does `-s FILESYSTEM=0` actually remove fd_* imports?
- Which RefC source files trigger fd_write/fd_seek?
  - `idris_file.c`?
  - `idris_support.c`?

---

### Branch 2: Alternative Toolchains
> Can we use a different C → WASM compiler?

2.1 **wasi-sdk**
- Does wasi-sdk produce cleaner WASM?
- Can we stub WASI imports post-build?
- Tool: `wasi-stub` from wasmtime?

2.2 **Clang + wasm-ld directly**
- Target: `wasm32-unknown-unknown`
- Requires: Full libc stubs
- How to provide pthread stubs?
- How to provide stdio stubs?

2.3 **Zig as C compiler**
- `zig cc --target=wasm32-freestanding`
- Does Zig have better wasm32 support without WASI?

---

### Branch 3: WASM Post-Processing
> Can we modify the WASM after compilation?

3.1 **ic-wasm tool**
- Does `ic-wasm shrink` remove unused imports?
- Can ic-wasm stub imports?
- Feature request: import stubbing?

3.2 **wasm-opt (binaryen)**
- `--remove-unused-module-elements`?
- `--strip-producers`?
- Custom pass to stub imports?

3.3 **Custom WASM transformer**
- Use walrus (Rust) or wasm-tools
- Replace import section entries
- Add stub functions for WASI imports

3.4 **wabt tools**
- `wasm2wat` → manual edit → `wat2wasm`
- Script to auto-replace imports with stubs

---

### Branch 4: Idris2 Runtime Modifications
> Can we modify Idris2's RefC runtime for IC?

4.1 **Minimal RefC runtime**
- Which files are actually needed?
  - `runtime.c` ✓
  - `memoryManagement.c` ✓
  - `stringOps.c` ✓
  - `mathFunctions.c` ✓
  - `casts.c` ✓
  - `prim.c` ✓
  - `idris_file.c` ❌ (triggers fd_*)
  - `idris_support.c` ❓

4.2 **IC-specific RefC fork**
- Remove file I/O
- Remove threading (pthread)
- Replace malloc with IC stable memory?
- Use ic0 for debug output instead of printf

4.3 **Custom Idris2 backend**
- Write IC-native backend
- Direct WASM generation (like Chez scheme backend)
- Skip C intermediate step

---

### Branch 5: IC Runtime Extensions
> Can IC provide WASI/env compatibility?

5.1 **IC WASI support**
- Is WASI support planned for IC?
- DFINITY forum discussions?
- Roadmap items?

5.2 **Custom IC imports**
- Can we request IC to add env.abort stub?
- Minimal emscripten compat layer?

---

## Priority Matrix

| Branch | Effort | Impact | Priority |
|--------|--------|--------|----------|
| 1.1 Latest emsdk | Low | High | **P0** |
| 3.3 Custom transformer | Medium | High | **P1** |
| 4.1 Minimal RefC | Medium | High | **P1** |
| 2.2 Clang direct | High | High | P2 |
| 4.2 IC-specific fork | High | High | P2 |
| 2.3 Zig compiler | Medium | Medium | P3 |

---

## Immediate Action Items

1. **Install emsdk on build server**
   ```bash
   git clone https://github.com/emscripten-core/emsdk.git
   cd emsdk && ./emsdk install latest && ./emsdk activate latest
   ```

2. **Test latest emscripten flags**
   ```bash
   emcc ... -s STANDALONE_WASM=1 -s PURE_WASI=0 -s FILESYSTEM=0 \
            -s MINIMAL_RUNTIME=1 -s ENVIRONMENT='web'
   ```

3. **Write WASM import stubber**
   - Input: canister.wasm with WASI imports
   - Output: canister.wasm with stub functions
   - Stub behavior: trap or no-op

4. **Test minimal RefC subset**
   - Remove idris_file.c, idris_directory.c
   - Check which functions are actually called
