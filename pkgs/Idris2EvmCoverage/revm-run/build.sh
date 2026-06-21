#!/usr/bin/env bash
# Build revm-run and install it on PATH so the EVM coverage pipeline
# (EvmCoverage.YulInstrumentor.resolveEvmRunner) picks it up automatically.
#
# revm-run is a drop-in for `idris2-evm-run --trace`: same CLI contract, same
# stdout (a ProfileFlush LOG1 event), but executes with revm in-process — fast
# enough that the EVM-execution step no longer hits the 20s timeout the
# pure-Idris interpreter stalled on.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$here"

cargo build --release

dest="${1:-$HOME/.local/bin}"
mkdir -p "$dest"
cp target/release/revm-run "$dest/revm-run"
echo "installed: $dest/revm-run"
command -v revm-run >/dev/null 2>&1 && echo "on PATH ✓" || echo "WARN: $dest not on PATH"
