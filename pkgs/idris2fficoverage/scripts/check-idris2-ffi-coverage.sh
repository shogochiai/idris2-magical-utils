#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/exec/idris2-ffi-cov"

if [[ ! -x "$BIN" ]]; then
  (cd "$ROOT" && pack build idris2-ffi-coverage >/dev/null)
fi

repo="$(mktemp -d -t idris2-ffi-cov.XXXXXX)"
full_trace=""
bad_trace=""
wrong_context_trace=""
missing_evidence_trace=""
unsafe_trust_trace=""
trap 'rm -rf "$repo" "$full_trace" "$bad_trace" "$wrong_context_trace" "$missing_evidence_trace" "$unsafe_trust_trace"' EXIT

cat >"$repo/SPEC.toml" <<'EOF'
[definitions]
prefix = "REQ_INT"

[[spec_area]]
name = "Integration paths"

[[spec]]
id = "${prefix}_001"
title = "proof bundle"
invariant = "integration/governedci/proof/bundle"
EOF

run_json() {
  local out="$1"
  shift
  set +e
  "$BIN" "$@" --format json >"$out"
  local rc=$?
  set -e
  return "$rc"
}

assert_json() {
  local file="$1"
  local expr="$2"
  python3 - "$file" "$expr" <<'PY'
import json
import sys
d = json.load(open(sys.argv[1]))
if not eval(sys.argv[2], {"__builtins__": {}}, {"d": d}):
    print(json.dumps(d, indent=2))
    raise SystemExit(1)
PY
}

run_json /tmp/idris2-ffi-cov-123.json ask "$repo" --steps=1,3
assert_json /tmp/idris2-ffi-cov-123.json 'd["clean"] is True and d["gapCount"] == 0'

full_trace="$(mktemp -t idris2-ffi-cov-full.XXXXXX.jsonl)"
paths_file="$(mktemp -t idris2-ffi-cov-paths.XXXXXX.txt)"
"$BIN" dump-paths > "$paths_file"
python3 - "$paths_file" "$full_trace" <<'PY'
import json
import sys

paths_file = sys.argv[1]
out = sys.argv[2]
evidence = {
    "integration/governedci/public-verify/submit-request": ["execution-request"],
    "integration/governedci/public-verify/query-receipt": ["execution-receipt"],
    "integration/governedci/worker/health": ["worker-selftest-receipt"],
    "integration/governedci/step3/gate": ["step3-receipt"],
    "integration/governedci/proof/bundle": ["quality-bundle-json", "quality-bundle-junit"],
    "integration/governedsource/source-capsule/materialize": ["materialization-receipt"],
    "integration/governedreview/private-review/create-request": ["review-request"],
    "integration/governedpublication/github/project-check": ["projection-receipt"],
    "integration/global-registry/upgrade/preflight": ["preflight-report"],
    "integration/global-registry/upgrade/apply": ["upgrade-receipt", "post-upgrade-preflight"],
}
with open(out, "w") as f:
    for op_id in [line.strip() for line in open(paths_file) if line.strip()]:
        f.write(json.dumps({
            "schema": "etherclaw.integration.trace.v1",
            "opId": op_id,
            "outcome": "pass",
            "evidenceIds": evidence[op_id],
            "context": {"prNumber": 1, "network": "local", "canister": "test-canister"},
        }, separators=(",", ":")) + "\n")
PY
rm -f "$paths_file"

run_json /tmp/idris2-ffi-cov-step4-full.json ask "$repo" --steps=4 --trace "$full_trace" --expect-pr 1 --expect-network local --expect-canister test-canister
assert_json /tmp/idris2-ffi-cov-step4-full.json 'd["clean"] is True and d["gapCount"] == 0 and d["coverage_percent"] == 100.0'

bad_trace="$(mktemp -t idris2-ffi-cov-fail.XXXXXX.jsonl)"
cat >"$bad_trace" <<'EOF'
{"schema":"etherclaw.integration.trace.v1","opId":"integration/governedci/proof/bundle","outcome":"fail","evidenceIds":["quality-bundle-json","quality-bundle-junit"],"context":{"prNumber":1,"network":"local","canister":"test-canister"}}
EOF
if run_json /tmp/idris2-ffi-cov-step4-fail.json ask "$repo" --steps=4 --trace "$bad_trace" --expect-pr 1 --expect-network local --expect-canister test-canister; then
  echo "failed trace event was accepted" >&2
  exit 1
fi
assert_json /tmp/idris2-ffi-cov-step4-fail.json 'd["clean"] is False and d["coverage_percent"] == 0.0'

wrong_context_trace="$(mktemp -t idris2-ffi-cov-context.XXXXXX.jsonl)"
cat >"$wrong_context_trace" <<'EOF'
{"schema":"etherclaw.integration.trace.v1","opId":"integration/governedci/proof/bundle","outcome":"pass","evidenceIds":["quality-bundle-json","quality-bundle-junit"],"context":{"prNumber":2,"network":"local","canister":"test-canister"}}
EOF
if run_json /tmp/idris2-ffi-cov-step4-context.json ask "$repo" --steps=4 --trace "$wrong_context_trace" --expect-pr 1 --expect-network local --expect-canister test-canister; then
  echo "wrong-context trace event was accepted" >&2
  exit 1
fi
assert_json /tmp/idris2-ffi-cov-step4-context.json 'd["clean"] is False and d["coverage_percent"] == 0.0'

missing_evidence_trace="$(mktemp -t idris2-ffi-cov-evidence.XXXXXX.jsonl)"
cat >"$missing_evidence_trace" <<'EOF'
{"schema":"etherclaw.integration.trace.v1","opId":"integration/governedci/proof/bundle","outcome":"pass","evidenceIds":[],"command":"mentions quality-bundle-json and quality-bundle-junit outside evidenceIds","context":{"prNumber":1,"network":"local","canister":"test-canister"}}
EOF
if run_json /tmp/idris2-ffi-cov-step4-evidence.json ask "$repo" --steps=4 --trace "$missing_evidence_trace" --expect-pr 1 --expect-network local --expect-canister test-canister; then
  echo "missing evidenceIds trace event was accepted" >&2
  exit 1
fi
assert_json /tmp/idris2-ffi-cov-step4-evidence.json 'd["clean"] is False and d["coverage_percent"] == 0.0'

unsafe_trust_trace="$(mktemp -t idris2-ffi-cov-unsafe-trust.XXXXXX.jsonl)"
cat >"$unsafe_trust_trace" <<'EOF'
{"schema":"etherclaw.integration.trace.v1","opId":"integration/governedci/proof/bundle","trust":"unsafe-diagnostic","outcome":"pass","evidenceIds":["quality-bundle-json","quality-bundle-junit"],"context":{"prNumber":1,"network":"local","canister":"test-canister"}}
EOF
if run_json /tmp/idris2-ffi-cov-step4-unsafe-trust.json ask "$repo" --steps=4 --trace "$unsafe_trust_trace" --expect-pr 1 --expect-network local --expect-canister test-canister; then
  echo "unsafe-diagnostic trace event was accepted" >&2
  exit 1
fi
assert_json /tmp/idris2-ffi-cov-step4-unsafe-trust.json 'd["clean"] is False and d["coverage_percent"] == 0.0'

echo '{"status":"clean","checks":["step123-clean","full-trace-clean","failed-event-rejected","wrong-context-rejected","missing-evidence-rejected","unsafe-trust-rejected"]}'
