#!/bin/bash
# Checked fetching of third-party C sources into a shared /tmp cache.
#
# WHY THIS FILE EXISTS AT ALL, rather than the two helpers living beside their
# callers: this repo has already paid twice for guarding the SAME /tmp cache from
# two places. `1e8631ec` (alice, 2026-08-08) hardened build-wasm.sh's mini-gmp
# guard and its own commit message names the problem — "the correct form was
# already in this repo, one directory over". Copying the corrected form a third
# time would reproduce exactly the divergence that message complained about, so
# both callers source this instead.
#
# WHAT THE EXISTENCE GUARDS COULD NOT SEE (measured 2026-08-18, carl).
# `1e8631ec` fixed the EXISTENCE dimension and enumerated the states it covered:
#
#     state on disk            guard says       truth
#     empty                    prepare          correct
#     partial (only .c)        SKIP             wrong  <- what it fixed
#     complete                 skip             correct
#
# There is a fourth state that table does not have a row for, and it is the one
# that was on disk: EVERY FILE PRESENT, ONE OF THEM TRUNCATED. `/tmp/mini-gmp/
# mini-gmp.c` had sat at 3865 lines / 77519 bytes since 2026-08-15 16:38 against
# 4666 lines / 92344 bytes upstream, ending mid-identifier — its last bytes are
# `mpz_ab` followed by spaces and no newline. All three files existed, so the
# hardened guard skipped preparation just as the old one did, and emcc failed
# with `mini-gmp.c:3863:1: note: to match this '{'` / `3 errors generated`.
#
# WHAT IT COST. The symptom surfaced three layers up as
# `EvidenceViolation: fixture output carries no v2 raw counts (pre-v2 or tampered
# instrument)`, i.e. dfx step4 CONTRACT_FAIL with `paths_denominator: 0`. That
# message names the INSTRUMENT for a truncated INPUT, and two machines spent
# hours on a "the dfx instrument is out of step with v2" hypothesis. Same shape
# as the GMP-vs-tier confusion `1e8631ec` describes: the failure is reported by
# whoever notices it last.
#
# THE OTHER HALF IS `curl -sLo`. No `-f`, so an HTTP error page is written to the
# destination as though it were source; no `--retry`, so a single dropped
# connection is permanent. Fetching to `<dst>.part` and renaming only after the
# completeness check means a failed fetch leaves NO file — which the guard then
# re-attempts on the next run — instead of leaving a bad file it trusts forever.

# Is this file a complete C source?
#
# Deliberately NOT a pinned hash: upstream is `tip`, whose content legitimately
# changes, so a hash would fail honestly-updated files and teach everyone to
# delete the check. The property that actually distinguishes the measured failure
# is that a complete text file is non-empty and ends with a newline; a transfer
# cut mid-stream does not. Verified against five inputs — the real truncated
# artefact, the pristine 4666-line file, an empty file, a minimal well-formed
# file, and an absent path.
_c_source_complete() {
    [ -s "$1" ] || return 1
    [ "$(tail -c 1 "$1" | wc -l | tr -d ' ')" = "1" ]
}

# Fetch $1 to $2, or leave $2 untouched/absent and fail loudly.
_fetch_c_source() {
    local url="$1" dst="$2" tmp="$2.part"
    rm -f "$tmp"
    if ! curl -fsSL --retry 3 --retry-delay 1 -o "$tmp" "$url"; then
        echo "ERROR: download failed (curl): $url" >&2
        rm -f "$tmp"
        return 1
    fi
    if ! _c_source_complete "$tmp"; then
        echo "ERROR: downloaded file is empty or truncated: $url" >&2
        echo "       (kept nothing; a partial file here fails a later build with" >&2
        echo "        a compiler error that names the wrong component)" >&2
        rm -f "$tmp"
        return 1
    fi
    mv "$tmp" "$dst"
}
