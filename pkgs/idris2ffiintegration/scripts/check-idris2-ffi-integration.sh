#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/exec/idris2-ffi-integration"

if [[ ! -x "$BIN" ]]; then
  (cd "$ROOT" && idris2 --build idris2-ffi-integration.ipkg >/dev/null)
fi

self_json="$(mktemp -t idris2-ffi-integration-self.XXXXXX.json)"
registry_json="$(mktemp -t idris2-ffi-integration-registry.XXXXXX.json)"
trap 'rm -f "$self_json" "$registry_json"' EXIT

"$BIN" self-check --format json >"$self_json"
"$BIN" dump-registry --format json >"$registry_json"

python3 - "$self_json" "$registry_json" <<'PY'
import json
import sys

self_check = json.load(open(sys.argv[1]))
registry = json.load(open(sys.argv[2]))

assert self_check["status"] == "clean", self_check
assert self_check["registrySize"] == 10, self_check

ops = registry["registry"]
assert len(ops) == 10, registry
assert all(op["trust"] == "proof-grade" for op in ops), registry

by_id = {op["pathId"]: op for op in ops}
assert by_id["integration/governedci/proof/bundle"]["risk"] == "local-only"
assert by_id["integration/global-registry/upgrade/apply"]["risk"] == "destructive"
assert by_id["integration/global-registry/upgrade/apply"]["idempotency"] == "global-registry-upgrade"

print(json.dumps({
    "status": "clean",
    "checks": [
        "self-check-clean",
        "registry-size-10",
        "all-default-ops-proof-grade",
        "destructive-op-has-idempotency"
    ],
}))
PY

