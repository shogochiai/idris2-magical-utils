# Idris2 IC SQLite VFS Port

This is the port boundary for using `humandebri/ic-sqlite-vfs` from Idris2
canisters.

The old `IcWasm.SQLite` path opens an in-memory SQLite database, serializes the
whole image during `pre_upgrade`, and deserializes the whole image during
`post_upgrade`. That is simple, but every persistence step scales with the full
database size.

The VFS path opens SQLite on a stable-memory backed `/main.db`. SQLite page
reads and writes go through `sqlite3_io_methods`; update calls collect dirty
pages in a heap overlay and commit only the changed logical pages plus the page
map. This matches the upstream direct path:

```text
SQLite pager -> sqlite3_vfs/icstable -> selected stable MemoryId
```

## Idris2 Surface

`IcWasm.SQLiteVFS` adds:

- `StableVFSConfig`: stable SQLite `memoryId` plus `schemaVersion`
- `SqliteVFSHandle`: proof that the VFS was initialized and opened
- `withSqliteUpdate`: one synchronous update-message transaction
- `withSqliteQuery`: read-only query-message access
- `sqlExecVFS`, `sqlQueryVFS`, `sqlPrepareVFS`: handle-checked wrappers over the
  existing SQLite statement API
- `sqliteVFSStats`: page/byte counters exposed by the backend

The Idris2 API deliberately keeps update transactions synchronous. Do not cross
an IC `await` boundary inside `withSqliteUpdate`.

## C/Wasm Backend Contract

Canister builds must provide these symbols:

```c
int64_t sql_vfs_ffi_init(int64_t memory_id);
int64_t sql_vfs_ffi_open_read_write(void);
int64_t sql_vfs_ffi_open_read_only(void);
int64_t sql_vfs_ffi_begin_update(void);
int64_t sql_vfs_ffi_commit_update(void);
int64_t sql_vfs_ffi_rollback_update(void);
int64_t sql_vfs_ffi_close(void);
int64_t sql_vfs_ffi_memory_id(void);
int64_t sql_vfs_ffi_page_count(void);
int64_t sql_vfs_ffi_allocated_bytes(void);
int64_t sql_vfs_ffi_active_bytes(void);
```

`sql_vfs_ffi_open_read_write` is expected to open the SQLite write connection
after `sql_vfs_ffi_begin_update` has started the heap overlay. Its SQL
transaction handling may live in the backend, but the final stable page-map
publish must happen only from `sql_vfs_ffi_commit_update`, after SQLite has
accepted the transaction commit.

Local RefC tests link `support/ic0/sql_vfs_ffi_stub.c`. The real canister
backend lives in `pkgs/Idris2IcWasmSQLite/support/ic0/sqlite_vfs_bridge.c`.
It implements the direct VFS, heap overlay, dirty-page append, page table, and
final-superblock publish contract. It does not yet use the upstream Rust crate's
MemoryManager-compatible byte layout.

The backend uses `SQLITE_OS_OTHER=1`, `SQLITE_THREADSAFE=0`, no WAL, and no
loadable extensions. `sqlite_vfs_bridge.c` provides `sqlite3_os_init` and
registers only the `icstable` VFS, so the SQLite support path does not link a
WASI fd/read/write/seek polyfill.

The backend should continue toward the upstream pieces in this order:

1. `sqlite3_vfs` and `sqlite3_io_methods` callbacks for `/main.db` and heap temp
   files.
2. Stable superblock encoding at virtual offset `0..64KiB`.
3. Segmented page table: root table plus 256-page segment tables.
4. Heap write overlay and atomic commit: write dirty pages/page tables first,
   then publish with the final superblock write.
5. Import/export and checksum refresh utilities for migration and operations.

## TheWorld Integration Shape

`idris2-icwasm gen-entry --sql-vfs-memory-id=N` now owns the SQLite lifecycle at
the generated canister-entry layer. In that mode it emits VFS declarations,
opens `/main.db` read-write for init, post-upgrade, update methods, and timer
callbacks, opens read-only for query methods, commits write overlays after the
Idris entrypoint returns, and does not emit `sqlite_stable_save/load`.

`pkgs/Idris2TheWorld/canister-build.toml` uses this path with
`sql_vfs_memory_id = 1`. This keeps the existing Idris SQL call sites close to
their current shape while replacing the persistence backend. Remaining work is
operational rather than a schema rewrite:

1. Keep the Idris call sites close to their current SQL shape.
2. Move code that wants explicit Idris-side handles to `StableVFSConfig` and
   `IcWasm.SQLiteVFS`; generated canister entry already brackets public calls.
3. Keep `sqlite_stable_save` out of `pre_upgrade`; the VFS image already
   lives in stable memory.
4. Add a one-time importer that reads the old serialized snapshot and writes it
   into the VFS image.
5. Update the economics docs after measuring TheWorld workloads, because the
   upstream 6.5x to 7.7x result is from a KV benchmark and should not be copied
   as a final TheWorld claim without workload-specific data.

The expected economics improvement is strongest where the old flow repeatedly
serializes or deserializes a growing database. The VFS path makes persistence
proportional to touched pages and page-map writes, so instance/release/economics
controllers should stop paying full-image persistence cost on every upgrade or
snapshot-style save.
