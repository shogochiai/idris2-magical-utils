# idris2-dfx-coverage

ICP canister coverage analysis library for the Idris2 CDK backend.

Current strongest profile:

- branch-level semantic test obligation coverage on the materialized runtime denominator
- function-level semantic test obligation coverage remains available as an advisory/static layer

In practice this means:

- the denominator is the set of branch obligations that actually materialize in the instrumented IC WASM runtime
- the numerator is the set of runtime branch probe hits that map back to those same branch obligations
- `claim_admissible=True` only when that branch-level runtime denominator/numerator pair is coherent

This is the path consumed by `lazy dfx ask --steps=4`.

## Authoritative Coverage Pipeline

**This is the canonical design. Do not introduce shortcut parsers or bypass modules.**

```
Stage 1: Static semantic surface
┌──────────────────────────────────────────────────────┐
│  DumpcasesParser                                      │
│  idris2 --dumpcases → FuncCases                       │
│  • Canonical branches = reachable semantic obligations │
│  • NonCanonical (Impossible) branches = excluded      │
│  → Type-system-aware: only counts reachable branches   │
└──────────────────────────────────────────────────────┘
                        ↓
Stage 2: Runtime mapping surface
┌──────────────────────────────────────────────────────┐
│  WasmMapper/NameSection + WasmMapper/WasmBranchParser │
│  + SourceMap/BranchProbeMap                           │
│  • NameSection: WASM func index → Idris2 QName        │
│  • WasmBranchParser: br_if, br_table, if/else → branch points │
│  • BranchProbeMap: probe index → function-local branch ordinal │
│  → Bridges gap between entry-mode profiling and       │
│    internal branch coverage                           │
└──────────────────────────────────────────────────────┘
                        ↓
Stage 3: Runtime observation
┌──────────────────────────────────────────────────────┐
│  IcWasm/ProfilingParser + __get_branch_probes         │
│  • ic-wasm __get_profiling → func_id + cycles         │
│  • __get_branch_probes → branch probe hit indices     │
│  → Parsed by ProfilingParser.parseProfilingOutput     │
│  → Converted to WasmTraceEntry via toWasmTraces       │
└──────────────────────────────────────────────────────┘
                        ↓
            WasmCoverage.runWasmCoverageFromProfiling
┌──────────────────────────────────────────────────────┐
│  Combines Stage 1 + 2 + 3                             │
│  → Function-level measurement                         │
│  → Materialized branch-level measurement              │
│  → Branch-level admissibility on runtime denominator  │
│  → High impact targets                                │
│  → WasmCoverageResult                                 │
└──────────────────────────────────────────────────────┘
```

### Why This Design Exists

ic-wasm `__get_profiling` in **entry mode** only records export function calls
(`canister_update_*`, `canister_query_*`), NOT internal Idris function calls.
The SourceMap (Stage 2) bridges this gap by mapping WASM function indices to
Idris2 qualified names, so we know which Idris functions correspond to which
WASM functions without needing full tracing support.

For branch-level claims, the extra ingredient is branch probe instrumentation.
`idris2-icwasm` now emits a probe map and `Idris2DfxCoverage` reads runtime
probe hits back through `__get_branch_probes`. That is what makes the current
branch-level runtime denominator/numerator pair possible.

## Current Practical Result

The current DFX path is no longer only a function-level fallback.

On `TheWorld`, `lazy dfx ask --steps=4` now reaches:

- `Materialized branches: 25`
- `Branches hit: 25/25 (100%)`
- `Claim admissible (runtime branch-level): True`

This is intentionally stated on the materialized runtime denominator, not on a
purely static canonical branch set.

### Consumer API (`LazyDfx` Step 4)

```idris
import DfxCoverage.IcWasm.ProfilingParser     -- parseProfilingOutput, toWasmTraces
import DfxCoverage.CodeCoverage.CodeCoverageAnalyzer  -- analyzeCodeCoverage, defaultConfig
import DfxCoverage.CodeCoverage.CodeCoverageResult    -- CodeCoverageResult, FuncCoverageInfo
import DfxCoverage.WasmMapper.WasmFunc        -- Idris2QName, Show instance
import DfxCoverage.WasmTrace.TraceEntry       -- WasmTraceEntry

-- Usage:
let profResult = parseProfilingOutput profilingContent
let wasmTraces = toWasmTraces profResult
let config = defaultConfig wasmPath
Right ccResult <- analyzeCodeCoverage config wasmTraces
-- ccResult : CodeCoverageResult
```

### Deleted: FunctionCoverage.idr

`DfxCoverage.FunctionCoverage` was removed (2026-03-13). It was a broken
shortcut parser that:

- split profiling output by `;` instead of `{`, causing 0 entries parsed
- used `isIdrisFunctionName` heuristic that passed C runtime functions
- duplicated types already properly defined in `CodeCoverageResult.idr`

**Do not recreate it.** Use `CodeCoverageAnalyzer.analyzeCodeCoverage` instead.

## Parser Disambiguation (エージェント向け)

このパッケージには複数のパーサーがあるが、**それぞれ入力形式が異なる。重複ではない。**
新しいパーサーを追加する前に、既存のものが目的に合うか必ず確認すること。

| モジュール | 入力 | 出力 | 用途 |
|---|---|---|---|
| `IcWasm/ProfilingParser` | `__get_profiling` の Candid `(vec { record { N:int32; N:int64 }; ... })` | `ProfilingResult` → `List WasmTraceEntry` (via `toWasmTraces`) | **IC canister プロファイリングデータ** |
| `WasmTrace/TraceParser` | wasmtime perf 形式 (`funcIdx\|pc\|opcode\|depth`) or JSON | `List WasmTraceEntry` | **ファイルからのトレース読み込み** (CodeCoverageAnalyzer 内部) |
| `IcWasm/IcpPublicNameParser` | ic-wasm が追加する `icp:public name` カスタムセクション (hex bytes) | `IcpFuncNames` (funcId → name map) | **instrumented WASM の関数名解決** |
| `WasmMapper/NameSection` | WASM 標準 name section (`wasm-objdump -x` or `wasm-tools print`) | `FuncMappingTable` | **Stage 2: WASM func index → Idris2 QName** |
| `DumpcasesParser` | `idris2 --dumpcases` 出力 | `List FuncCases` (Canonical/NonCanonical分類) | **Stage 1: 型レベルブランチ分析** |

**ありがちな間違い:**
- `__get_profiling` の出力を `TraceParser` に渡す → **NG** (形式が違う。`ProfilingParser` を使う)
- `ProfilingParser` の結果から直接カバレッジを計算する独自モジュールを作る → **NG** (`CodeCoverageAnalyzer.analyzeCodeCoverage` を使う)
- WASM 関数名を取得するために新しいパーサーを書く → **NG** (`NameSection.buildMappingTableFromWasm` を使う)

## High Impact Targets

DumpcasesParser の `getHighImpactTargetsWithCoverage` は、dumpcases の型レベルブランチ分析と
CodeCoverageAnalyzer のプロファイリングデータを突合して、最も改善効果の高い関数を特定する。

### Severity heuristic

```
severity = totalBranches / executedCount
```

- `executedCount = 0` (未テスト) → `severity = Inf` (最優先)
- `executedCount > 0` (一部テスト済み) → 比率が大きいほど優先度高
- 例: 13 branches / 2 executed = 6.5 (まだ11ブランチ未カバー)

### データフロー

```
Stage 1 (dumpcases)                Stage 3 (profiling)
FuncCases                          CodeCoverageResult
  .funcName = "IpFork.Core.topFork"    .functionDetails → FuncCoverageInfo
  .totalBranches = 13                    .idris2Name = Just "IpFork.Core.topFork"
                                         .status = FuncCovered 2
          ↓                                         ↓
    getHighImpactTargetsWithCoverage(n, coverageLookup, funcs)
          ↓
    HighImpactTarget
      .funcName = "IpFork.Core.topFork"
      .branchCount = 13
      .executedCount = 2
      .severity = 6.5
```

### `coverageLookup` construction

`buildCoverageLookup` が `CodeCoverageResult.functionDetails` から
Idris2 qualified name → hit count のマップを構築する。
dumpcases の `FuncCases.funcName` (例: `IpFork.Core.topFork`) と
`Idris2QName` の `Show` instance (例: `IpFork.Core.topFork`) が一致する前提。

### API

```idris
-- 旧: ブランチ数のみでソート (executed は常に 0)
getHighImpactTargets : Nat -> List FuncCases -> List HighImpactTarget

-- 新: プロファイリングデータで severity 計算
getHighImpactTargetsWithCoverage : Nat -> (String -> Nat) -> List FuncCases -> List HighImpactTarget
```

### 表示例

```
High Impact Targets (by severity = branches/executed):
  IpFork.Core.topFork: 13 branches, executed=0, severity=Inf
  GToken.Core.parseProposalStatus: 11 branches, executed=2, severity=5.5
  GToken.Core.show: 10 branches, executed=3, severity=3.33
```

## Module Structure

```
src/DfxCoverage/
├── Idris2Coverage.idr            # High-level API
├── CandidParser.idr              # Parse .did files
├── CanisterCall.idr              # Execute dfx canister calls
├── CoverageAnalyzer.idr          # Candid method coverage gap analysis
├── Exclusions.idr                # Exclusion patterns
├── DumpcasesParser.idr           # Parse idris2 --dumpcases output (Stage 1)
├── IcWasm/
│   ├── Instrumenter.idr          # ic-wasm instrument wrapper
│   ├── ProfilingParser.idr       # __get_profiling Candid 出力 → ProfilingResult (Stage 3)
│   ├── IcpPublicNameParser.idr   # icp:public name セクション → IcpFuncNames
│   └── HttpOutcallDetector.idr   # Detect HTTP outcall capability
├── WasmMapper/                   # SourceMap layer (Stage 2)
│   ├── WasmFunc.idr              # WASM function types, Idris2QName, FuncMappingTable
│   ├── NameSection.idr           # WASM 標準 name section → FuncMappingTable
│   └── WasmBranchParser.idr      # WASM branch point analysis (br_if, br_table, if)
├── WasmTrace/
│   ├── TraceEntry.idr            # WasmTraceEntry, FuncHitCount, WasmBranchPoint (型定義のみ)
│   └── TraceParser.idr           # wasmtime perf/JSON ファイル → List WasmTraceEntry
├── CodeCoverage/                 # Legacy integrated function coverage
│   ├── CodeCoverageResult.idr    # Result types: CodeCoverageResult, FuncCoverageInfo, etc.
│   └── CodeCoverageAnalyzer.idr  # Main entry: analyzeCodeCoverage
└── Ic0Mock/
    ├── Ic0Stubs.idr              # IC0 system API stubs
    └── MockContext.idr            # Mock context for testing
```

## Runtime Notes

### Instrumentation cost

Branch probes are runtime instrumentation. They increase execution cost and
should be treated as coverage-build machinery, not production-build defaults.

The intended split is:

- production canister artifact: no branch probe instrumentation
- coverage/staging artifact: branch probes enabled

### Materialized denominator

DFX does not currently claim branch coverage over every static branch emitted by
dumpcases. It claims branch coverage over the subset that materializes in the
instrumented runtime and can be mapped back to stable branch obligation IDs
within the current backend.

That is why the report distinguishes:

- static semantic analysis
- materialized branch measurement
- `claim_admissible`

## ic-wasm Profiling

```bash
# 1. Instrument
ic-wasm xxx_stubbed.wasm -o xxx_instrumented.wasm instrument --stub-wasi

# 2. Deploy
dfx canister install xxx --wasm xxx_instrumented.wasm

# 3. Get traces (DO NOT call __toggle_tracing — it disables tracing!)
dfx canister call xxx __toggle_entry '()'
dfx canister call xxx runTests '()'
dfx canister call xxx '__get_profiling' '(0 : nat32)'
```

## Dependencies

- `base`, `contrib`
- `idris2-coverage-core` (shared coverage types)
- `idris2-icwasm` (IC FFI types)

## Building

```bash
pack build idris2-dfx-coverage.ipkg
```
