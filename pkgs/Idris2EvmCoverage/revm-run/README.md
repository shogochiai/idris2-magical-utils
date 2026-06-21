# revm-run

A fast, in-process drop-in for `idris2-evm-run --trace` in the EVM coverage
pipeline (`EvmCoverage.YulInstrumentor`).

## Why

EVM path coverage is **self-reported by the instrumented bytecode**: the
instrumentor injects per-branch counters and a `__profile_flush()` that emits a
`ProfileFlush(uint256[])` **LOG1** event. The coverage tool just runs the
bytecode and reads that log. Any EIP-correct EVM can surface it.

The previous runner, `idris2-evm-run`, is a pure-Idris2 interpreter chosen for
"Idris2 で完結" purity, not speed — and it stalls past the 20s timeout on real
instrumented contracts, yielding 0% / FAIL. `revm-run` runs the same bytecode
with **revm** (the engine behind foundry/anvil/reth) in-process: milliseconds,
EIP-accurate gas, byte-identical ProfileFlush output.

## Contract (identical to idris2-evm-run)

```
revm-run --trace <bytecode-file> --gas <N>
```
- `<bytecode-file>`: runtime bytecode as trimmed hex (with or without `0x`)
- prints captured logs in the exact textual shape the trace parser expects:
  `Log #i:` / `Topics:` / `[j] 0x<topic>` / `Data: B bytes` / `0x<hexdata>` /
  `Result: ...`

## Build / install

```
./build.sh            # builds release + installs to ~/.local/bin
```

`resolveEvmRunner` selects: `EVM_COV_RUNNER` env override > `revm-run` if on PATH
> `idris2-evm-run` fallback.
