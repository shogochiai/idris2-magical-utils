#!/usr/bin/env bash
# verify-evm-backend.sh — the gate to run BEFORE trusting a single-runner EVM
# coverage number. A coverage % is only meaningful if the backend that produced
# it is trustworthy; this bundles the three checks that establish that:
#
#   1. conformance-revm.sh  — ABSOLUTE: revm-run matches the EVM spec on
#                             hand-computed vectors (it is not itself miscompiling
#                             LT/GT/SSTORE/RETURN/REVERT).
#   2. diff-runners.sh      — RELATIVE: revm-run and the pure-Idris idris2-evm-run
#                             fire the SAME path markers on the real contract
#                             (the two independent EVM implementations agree).
#   3. e2e-lifecycle-oracle — VALUE: the contract's full lifecycle reaches the
#                             right OBSERVABLE STATE on-chain (approved+executed),
#                             which branch-reachability coverage cannot see.
#
# (1)+(2) trust the engine; (3) trusts the compiled contract. Coverage is only
# as honest as all three passing.
#
# Usage:
#   verify-evm-backend.sh <runtime-bytecode> [<calldata-or-selector> ...]
#       runtime-bytecode : the dispatch runtime (e.g. /tmp/runtime-dispatch.bin)
#       extra args       : selectors to cross-check in diff-runners (default: a
#                          few common ones; pass your contract's real selectors)
#
# Env:
#   E2E_CALLS   : happy-path calls file for the lifecycle oracle (skips (3) if unset
#                 and no default is found)
#   SKIP_E2E=1  : run only the engine checks (1+2), e.g. for a contract with no
#                 lifecycle oracle yet.
#
# Exit 0 = all enabled checks pass (coverage is trustworthy). Non-zero = a backend
# or contract problem; do NOT trust the coverage number until it is resolved.

set -u
here="$(cd "$(dirname "$0")" && pwd)"
BYTECODE="${1:-/tmp/runtime-dispatch.bin}"; shift || true
SELECTORS=("$@")
[ "${#SELECTORS[@]}" -gt 0 ] || SELECTORS=(997072f7 c04f0123)

[ -f "$BYTECODE" ] || { echo "verify: bytecode not found: $BYTECODE" >&2; exit 2; }

rc=0
section() { echo; echo "=== $* ==="; }

# 1. ABSOLUTE conformance (engine vs EVM spec).
section "1/3 conformance: revm-run vs EVM spec (absolute)"
if bash "$here/conformance-revm.sh"; then :; else echo "  → conformance FAILED"; rc=1; fi

# 2. RELATIVE differential (revm vs pure-Idris on this contract).
section "2/3 differential: revm-run vs idris2-evm-run agree (relative)"
diff_fail=0
for sel in "${SELECTORS[@]}"; do
  if bash "$here/diff-runners.sh" "$BYTECODE" "$sel" | grep -q '^PASS'; then
    echo "  selector $sel: agree"
  else
    echo "  selector $sel: DISAGREE"; diff_fail=1
  fi
done
[ "$diff_fail" -eq 0 ] || { echo "  → differential FAILED"; rc=1; }

# 3. VALUE oracle (contract lifecycle reaches correct on-chain state).
if [ "${SKIP_E2E:-0}" = "1" ]; then
  section "3/3 value oracle: SKIPPED (SKIP_E2E=1)"
else
  # The lifecycle oracle lives with the contract (etherclaw), not here. Allow an
  # explicit override, else search common checkout layouts next to this repo.
  e2e="${E2E_ORACLE:-}"
  for cand in \
      "$here/../../../../etherclaw/pkgs/Idris2TextDao/scripts/e2e-lifecycle-oracle.sh" \
      "$HOME/code/etherclaw/pkgs/Idris2TextDao/scripts/e2e-lifecycle-oracle.sh"; do
    [ -n "$e2e" ] && break
    [ -f "$cand" ] && e2e="$cand"
  done
  if [ -n "${e2e:-}" ] && [ -f "$e2e" ]; then
    section "3/3 value oracle: lifecycle reaches approved+executed"
    if E2E_CALLS="${E2E_CALLS:-}" bash "$e2e" "$BYTECODE" ${E2E_CALLS:+"$E2E_CALLS"}; then :; else
      echo "  → value oracle FAILED"; rc=1; fi
  else
    section "3/3 value oracle: no e2e-lifecycle-oracle.sh found — SKIPPED"
    echo "  (add one per contract; set SKIP_E2E=1 to silence)"
  fi
fi

echo
if [ "$rc" -eq 0 ]; then
  echo "verify-evm-backend: ALL CHECKS PASS — coverage from this backend is trustworthy."
else
  echo "verify-evm-backend: FAILED — do NOT trust the coverage number until fixed."
fi
exit "$rc"
