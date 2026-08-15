# ScriptDryRun: the auditor-facing dry-run plan (SDR)

`Subcontract.Core.ScriptDryRun` maps a `Script` free monad
(`pkgs/Idris2Subcontract/src/Subcontract/Core/Script.idr`) to the sequence of
EVM transactions it would send, **without any IO, RPC, or signing**. The output
is a pure, deterministic `DryRunPlan`:

- `txs` — the ordered list of `PlannedTx` the script intends to send
- `logs` — the messages accumulated by `logScript` / `recordValue`
- `asserts` — the `(message, Bool)` results accumulated by `assertScript`

This dry-run output is the **release artifact an auditor approves**: today
`luci`'s `AtomicRelease` carries no evm family artifact, so an audit vote can
only gate a merge. `planScript` produces the artifact a reviewer can read —
"this release sends exactly these transactions" — as the first pure stage of a
tx pipeline. Its six requirements (SPEC: `pkgs/Idris2Subcontract/src/
Subcontract/Core/ScriptDryRun/SPEC.toml`, prefix `SDR`) are:

- **SDR_PLAN_001** — `deploy bytecode` yields exactly one `PlannedTx` with
  `to = Nothing` (contract creation) and `data_` = the initcode.
- **SDR_PLAN_002** — `call target sel args` yields one `PlannedTx` with
  `to = Just target` and `data_` starting with the 4-byte selector.
- **SDR_PLAN_003** — `logScript` and `recordValue` append to `logs` and yield
  no `PlannedTx`.
- **SDR_PLAN_004** — the `txs` order preserves command order across mixed
  commands.
- **SDR_PLAN_005** — `send to amount` yields one `PlannedTx` with
  `to = Just to`, `value = amount`, and empty `data_`.
- **SDR_PLAN_006** — `assertScript` appends to `asserts` and yields no
  `PlannedTx`.

Because command values are not known off-chain, `planScript` threads a
type-correct placeholder value through each continuation (address 0, value 0,
no record); later commands are still planned, so the auditor sees the full
intended tx sequence. See also the package-local copy at
`pkgs/Idris2Subcontract/docs/contributors/script-dry-run.md`.
