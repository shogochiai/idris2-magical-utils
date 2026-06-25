// Path-coverage runtime for the RefC→WASM backend.
//
// The forked Idris2 compiler, when run with `--dumppathshits`, injects a call to
// `prim__recordPathHit "<fn>#p<n>"` at every canonical CaseTree leaf. The RefC
// backend (src/Compiler/RefC/RefC.idr, AExtPrim handler) lowers that to a call to
// `idris2_recordPathHit(<pathId char*>)`. The recorded string is the SAME path-id
// the dumppaths-json denominator uses, so the numerator is an identity join — no
// source map, ordinal alignment, or span matching is needed.
//
// This mirrors the ES backend's `globalThis.__idris2_recordPathHit(pathId)` hook
// and the Scheme backend's RecordPathHit extern: same instrumentation, traced in
// the real execution environment (here: a WASM canister on a dfx replica). The
// recorded ids are dumped, comma-separated, by `__dfxcov_format_path_hits()` which
// the generated canister query (`__get_path_hits`) replies with.
//
// Implementation notes:
//  - WASM canisters have no malloc churn budget for per-hit allocation, so we use a
//    fixed-capacity open-addressing string set keyed by the path-id pointer's
//    *content* (FNV-1a hash). Capacity is generous; on overflow we stop recording
//    NEW ids (already-recorded ids still count) and set a saturation flag so the
//    collector can report truncation rather than silently undercount.
//  - We store the id strings by copying into a static arena (the compiler passes
//    interned string literals, but we must not assume their lifetime across the
//    canister call boundary).

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#ifndef IDRIS2_PATHCOV_CAPACITY
#define IDRIS2_PATHCOV_CAPACITY 16384
#endif

#ifndef IDRIS2_PATHCOV_ARENA_BYTES
#define IDRIS2_PATHCOV_ARENA_BYTES (1 << 21) // 2 MiB of path-id text
#endif

// Open-addressing set: slot holds a pointer into the arena (NULL = empty).
static const char *idris2_pathcov_slots[IDRIS2_PATHCOV_CAPACITY] = {0};
static uint32_t idris2_pathcov_count = 0;
static uint32_t idris2_pathcov_saturated = 0;

static char idris2_pathcov_arena[IDRIS2_PATHCOV_ARENA_BYTES];
static size_t idris2_pathcov_arena_used = 0;

static uint32_t idris2_pathcov_hash(const char *s) {
  // FNV-1a 32-bit
  uint32_t h = 2166136261u;
  for (const unsigned char *p = (const unsigned char *)s; *p; ++p) {
    h ^= (uint32_t)(*p);
    h *= 16777619u;
  }
  return h;
}

static const char *idris2_pathcov_intern(const char *s, size_t len) {
  if (idris2_pathcov_arena_used + len + 1 > IDRIS2_PATHCOV_ARENA_BYTES) {
    idris2_pathcov_saturated = 1;
    return NULL;
  }
  char *dst = idris2_pathcov_arena + idris2_pathcov_arena_used;
  memcpy(dst, s, len);
  dst[len] = '\0';
  idris2_pathcov_arena_used += len + 1;
  return dst;
}

// Record a path-id hit (idempotent per distinct id). Declared to return void*
// so the RefC-generated `Idris2_Value * v = idris2_recordPathHit(...);` assignment
// type-checks; the returned NULL is bound to a let and immediately discarded.
void *idris2_recordPathHit(const char *pathId) {
  if (pathId == NULL) return NULL;
  uint32_t mask = IDRIS2_PATHCOV_CAPACITY - 1; // capacity must be power of two
  // If capacity is not a power of two, fall back to modulo.
  uint32_t cap = IDRIS2_PATHCOV_CAPACITY;
  uint32_t h = idris2_pathcov_hash(pathId);
  uint32_t idx = ((cap & (cap - 1)) == 0) ? (h & mask) : (h % cap);
  for (uint32_t probe = 0; probe < cap; ++probe) {
    const char *cur = idris2_pathcov_slots[idx];
    if (cur == NULL) {
      // Empty slot → first time we see this id. Intern + store.
      size_t len = strlen(pathId);
      const char *stored = idris2_pathcov_intern(pathId, len);
      if (stored == NULL) {
        // arena full; count as saturation but do not store.
        return NULL;
      }
      idris2_pathcov_slots[idx] = stored;
      idris2_pathcov_count += 1;
      return NULL;
    }
    if (strcmp(cur, pathId) == 0) {
      // already recorded; idempotent.
      return NULL;
    }
    idx = (idx + 1) % cap;
  }
  // Table full of distinct ids → saturated.
  idris2_pathcov_saturated = 1;
  return NULL;
}

uint32_t __dfxcov_path_hit_count(void) { return idris2_pathcov_count; }
uint32_t __dfxcov_path_hit_saturated(void) { return idris2_pathcov_saturated; }

void __dfxcov_reset_path_hits(void) {
  memset(idris2_pathcov_slots, 0, sizeof(idris2_pathcov_slots));
  idris2_pathcov_count = 0;
  idris2_pathcov_saturated = 0;
  idris2_pathcov_arena_used = 0;
}

// Semicolon-separated dump of all recorded path-ids. The buffer is static; callers
// (the canister query) must copy/reply before the next call. If the join output
// would exceed the buffer, the dump is truncated and the trailing marker
// ";__TRUNCATED__" is appended so the collector can flag it (no silent loss).
//
// SEPARATOR = ';' (NOT ','): path-ids for `where`-bound / lifted helpers embed a
// comma in their name (e.g. "…balancedObject,go#p0", "Leb128.encodeSigned,go#p1").
// A comma separator collided with those commas, so the collector split one id into
// two and dropped the "…,go#pN" tail — those where-helper paths could NEVER join
// and showed as permanently-missing. ';' never appears inside an Idris path-id and
// (unlike '\n') survives dfx's candid `text` reply rendering verbatim.
#ifndef IDRIS2_PATHCOV_DUMP_BYTES
#define IDRIS2_PATHCOV_DUMP_BYTES (1 << 22) // 4 MiB reply text
#endif
static char idris2_pathcov_dump[IDRIS2_PATHCOV_DUMP_BYTES];

const char *__dfxcov_format_path_hits(void) {
  size_t pos = 0;
  idris2_pathcov_dump[0] = '\0';
  int first = 1;
  for (uint32_t i = 0; i < IDRIS2_PATHCOV_CAPACITY; ++i) {
    const char *cur = idris2_pathcov_slots[i];
    if (cur == NULL) continue;
    size_t len = strlen(cur);
    size_t need = len + (first ? 0 : 1);
    if (pos + need + 1 >= sizeof(idris2_pathcov_dump) - 16) {
      const char *mark = ";__TRUNCATED__";
      size_t mlen = strlen(mark);
      memcpy(idris2_pathcov_dump + pos, mark, mlen);
      pos += mlen;
      idris2_pathcov_dump[pos] = '\0';
      return idris2_pathcov_dump;
    }
    if (!first) idris2_pathcov_dump[pos++] = ';';
    memcpy(idris2_pathcov_dump + pos, cur, len);
    pos += len;
    idris2_pathcov_dump[pos] = '\0';
    first = 0;
  }
  return idris2_pathcov_dump;
}
