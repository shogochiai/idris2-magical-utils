# Backend Execution Standard

This package complements `Idris2CoverageStandardization`.

It standardizes execution-facing value representations for backend hot paths.
Coverage answers "what obligation was exercised?".
Execution standardization answers "what runtime representation is safe to use while exercising it?".

## Backend profiles

- IC WASM: machine-integer wrappers for pages, bytes, offsets, timestamps, cycles, balances, IDs, limits.
- EVM: machine-integer wrappers for wei, gas, block numbers, nonce, timestamps, storage slots.
- Web: instrumentation-aware wrappers for coverage byte offsets, lengths, and runtime timestamps.

## Migration rule

Do not store large byte counts, timestamps, block numbers, balances, cycles, or pagination limits as `Nat` in backend hot paths.
Use explicit backend wrappers instead.

## Current migrations

- `TheWorld.Storages.StableMemory`: stable size / page arithmetic now uses `StableBytes` and `StablePageCount`.
- `MmntCanister.Core` / `MmntCanister.CanisterMain`: DB page offset and query limit now use `StablePageCount` and `IcQueryLimit`.

## Remaining work

- TheWorld domain records still use raw `Nat` for many timestamps, balances, and identifiers.
- Mmnt URI IDs, timestamps, tally counts, and proposal identifiers still need full wrapper migration.
- EVM and Web coverage stacks should consume these backend profiles when they expose performance-sensitive instrumentation values.
