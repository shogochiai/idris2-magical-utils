# Idris2ExecutionStandardization

`Idris2ExecutionStandardization` fixes one layer below coverage.

It standardizes the execution-facing value vocabulary that backend-sensitive
Idris programs should use when they target:

- IC WASM / local ICP replica
- EVM / Yul codegen
- Web / JavaScript runtime coverage tooling

The goal is not "one universal primitive type".
The goal is:

- a shared theory of backend cost models
- backend-specific machine-integer wrappers for hot execution paths
- a migration target for application code such as `TheWorld` and `Mmnt`

## Current modules

- `Execution.Standardization.Types`
- `Execution.Standardization.Model`
- `Execution.Standardization.IcWasm`
- `Execution.Standardization.Evm`
- `Execution.Standardization.Web`

## Design rule

Large counters, timestamps, bytes, pages, gas, balances, and offsets must not
default to `Nat` in backend hot paths.

They should use backend-aware wrappers with explicit semantics instead.
