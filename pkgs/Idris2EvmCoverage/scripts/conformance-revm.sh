#!/usr/bin/env bash
# conformance-revm.sh — pin revm-run's EVM behaviour against HAND-COMPUTED
# vectors. The differential harness (diff-runners.sh) proves the two backends
# AGREE; it cannot prove either is CORRECT (both could share a bug). This script
# nails the absolute correctness of the revm-run side with small EVM programs
# whose RETURN value / LOG topics are computable by hand. If revm-run ever drifts
# (a revm upgrade, a wiring change in how we deploy/call/capture), one of these
# fails. Combined with diff-runners.sh: agree (relative) + correct (absolute).
#
# Tests the actual CLI contract the coverage pipeline depends on:
#   - return data        (REVM_SHOW_RETURN → "Return: 0x..")
#   - LOG topics          ("=== Logs ===" → "[0] 0x..")
#   - SUCCESS/REVERT      ("Result: ..")
#   - storage             (REVM_DUMP_STORAGE → "slot 0x.. = 0x..")
#
# Exit 0 = all vectors pass. Exit 1 = a vector failed (revm-run is wrong/drifted).

set -u
REVM="${REVM_RUN:-revm-run}"
command -v "$REVM" >/dev/null 2>&1 || { echo "conformance: revm-run not found" >&2; exit 2; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
pass=0; fail=0

# run <name> <bytecode-hex> <extra-env> <expected-substr> [<grep-filter>]
check() {
  local name="$1" code="$2" env="$3" expect="$4" filt="${5:-.}"
  printf '%s' "$code" > "$tmp/code.bin"
  local out
  out=$(env $env "$REVM" --bytecode "$(cat "$tmp/code.bin")" --gas 10000000 2>&1 | grep -E "$filt")
  if grep -qF "$expect" <<<"$out"; then
    echo "PASS  $name"; pass=$((pass+1))
  else
    echo "FAIL  $name"; echo "      expected substring: $expect"
    echo "      got:"; sed 's/^/        /' <<<"$out"; fail=$((fail+1))
  fi
}

# 1. RETURN of a constant. PUSH1 0x2a; PUSH1 0; MSTORE; PUSH1 0x20; PUSH1 0; RETURN
#    → returns 32-byte word 0x..2a.
check "return constant 42" \
  "602a60005260206000f3" "REVM_SHOW_RETURN=1" \
  "Return: 0x000000000000000000000000000000000000000000000000000000000000002a" \
  "Return:"

# 2. ADD then RETURN. PUSH1 2; PUSH1 3; ADD(=5); PUSH1 0; MSTORE; PUSH1 32; PUSH1 0; RETURN
check "return 2+3=5" \
  "600260030160005260206000f3" "REVM_SHOW_RETURN=1" \
  "Return: 0x0000000000000000000000000000000000000000000000000000000000000005" \
  "Return:"

# 3. LT correctness (the operator the mangling bug inverted): 2 < 3 = 1.
#    PUSH1 3; PUSH1 2; LT(2<3 → top<second? EVM LT is a<b with a=2nd-from-top)…
#    EVM LT: pops a (top) then b; pushes (b < a)? No — LT computes s[0] < s[1]
#    with s[0] the top. We push 3 then 2, so top=2,next=3 → 2<3 = 1.
check "LT 2<3 = 1" \
  "600360021060005260206000f3" "REVM_SHOW_RETURN=1" \
  "Return: 0x0000000000000000000000000000000000000000000000000000000000000001" \
  "Return:"

# 4. GT correctness: with top=2,next=3 → 2>3 = 0 (so LT and GT are NOT the same —
#    the conformance counterpart to the < vs > codegen bug).
check "GT 2>3 = 0" \
  "600360021160005260206000f3" "REVM_SHOW_RETURN=1" \
  "Return: 0x0000000000000000000000000000000000000000000000000000000000000000" \
  "Return:"

# 5. LOG1 fires the exact topic. PUSH1 0x42(topic); PUSH1 0; PUSH1 0; LOG1; STOP
check "LOG1 topic 0x42" \
  "604260006000a100" "" \
  "[0] 0x0000000000000000000000000000000000000000000000000000000000000042" \
  "\[0\]"

# 6. SSTORE then the slot is observable. PUSH1 7; PUSH1 0; SSTORE; STOP.
#    Storage is only COMMITTED (and thus dumpable) via --calls-file, since single
#    --bytecode mode uses transact() without commit. The bytecode runs on any
#    call, so a one-line calls-file with a dummy calldata triggers + commits it.
sstore_check() {
  printf '00\n' > "$tmp/one.calls"
  local out
  out=$(REVM_DUMP_STORAGE=1 "$REVM" --calls-file "$tmp/one.calls" \
          --bytecode "600760005500" --gas 1000000 2>&1 | grep -E "slot 0x")
  if grep -qF "slot 0x0 = 0x7" <<<"$out"; then
    echo "PASS  SSTORE slot0 = 7"; pass=$((pass+1))
  else
    echo "FAIL  SSTORE slot0 = 7"; echo "      got:"; sed 's/^/        /' <<<"$out"; fail=$((fail+1))
  fi
}
sstore_check

# 7. REVERT is reported as REVERT, not SUCCESS. PUSH1 0; PUSH1 0; REVERT
check "REVERT reported" \
  "60006000fd" "" \
  "Result: REVERT" \
  "Result:"

echo "----"
echo "conformance: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
