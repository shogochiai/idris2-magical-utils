# Idris2 IC SQLite VFS support

This directory owns the C runtime used by `Idris2IcWasm` canister builds when a
package enables the stable-memory SQLite VFS lifecycle.

- `sqlite_vfs_bridge.*` is the active VFS backend. It registers the `icstable`
  SQLite VFS and commits dirty pages into IC stable memory.
- `sqlite_bridge.*` is the shared SQLite C API bridge used by Idris call sites.
- `sqlite/libsqlite3.a` is built with `SQLITE_OS_OTHER=1`; `sqlite_vfs_bridge.c`
  provides `sqlite3_os_init()` and registers only the `icstable` VFS.
- `wasi_polyfill.c` is intentionally absent. SQLite must not depend on WASI
  fd/read/write/seek or wasi2ic for storage.
- `legacy/sqlite_stable.*` is the old serialize/deserialize snapshot bridge. It
  is kept for one-time migration and compatibility, not for the normal VFS
  runtime lifecycle.

The current VFS backend follows the direct-VFS, heap-overlay, append-and-publish
shape of `humandebri/ic-sqlite-vfs`, but it is not byte-layout compatible with
the upstream Rust MemoryManager implementation yet.
