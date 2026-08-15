# ScriptDryRun: the auditor-facing dry-run plan

`Subcontract.Core.ScriptDryRun` maps a `Script` free monad
(`Subcontract.Core.Script`) to the sequence of EVM transactions it would send,
**without any IO, RPC, or signing**. The output is a pure, deterministic
`DryRunPlan`:

- `txs` — the ordered list of `PlannedTx` the script intends to send
- `logs` — the messages accumulated by `logScript` / `recordValue`
- `asserts` — the `(message, Bool)` results accumulated by `assertScript`

This dry-run output is not a developer convenience: it is the **release
artifact an auditor approves**. Today `luci`'s `AtomicRelease` carries no evm
family artifact, so an audit vote can only gate a merge. `planScript` produces
the artifact a reviewer can read — "this release sends these exact
transactions" — as the first pure stage of a tx pipeline.

## Why a pure planner first

The `Script` free monad has only one eliminator today, `prettyScript`, which
drops continuations and cannot produce transactions. Before any RPC call or
signature can be added, the script must first be lowered to a transactable
form. That lowering is `planScript`; it inspects only the AST, so it is
deterministic and trivially testable. RPC and signing are deliberately out of
scope for this stage.

## The six requirements

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
no record); later commands are still planned, and the resulting plan shows
the full intended tx sequence for review.

## Usage

```idris
import Subcontract.Core.Script
import Subcontract.Core.ScriptDryRun

plan : DryRunPlan
plan = planScript (do
  proxy <- deploy ProxyBytecode
  send proxy.address 1)
```

## Next steps (out of scope here)

With `planScript` in place, the following stages can be layered on top:
selector-to-calldata ABI encoding for static and dynamic types, calldata
validation against the target's `Sig`, a simulator for the placeholder values,
and finally an RPC/signing layer — each still pure until the final broadcast
step.
