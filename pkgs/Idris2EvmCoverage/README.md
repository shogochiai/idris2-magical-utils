# idris2-evm-coverage

EVM coverage collection and reporting for Idris2 smart contracts.

## Overview

`idris2-evm-coverage` provides branch-aware semantic coverage for Idris2 contracts compiled to EVM via the `idris2-yul` backend. It combines `--dumpcases` branch classification with runtime observations and reports conservative branch-level semantic obligations.

## Coverage Pipeline

```
Idris2 Source → idris2-yul → Yul (@source comments) → solc → EVM Bytecode
                                    ↓
                              asm-json (PC→Yul offset)
                                    ↓
EVM Trace (hit PCs) → PC→Yul→@source → Idris2 Function Coverage
```

## Features

- **Branch-Level Semantic Obligations**: Canonical, excluded, bug, optimizer-artifact, and unknown branch classes
- **PC to Function Mapping**: Maps EVM program counters to Idris2 functions
- **Source Location Tracking**: Uses `@source` comments from idris2-yul
- **Exclusion Patterns**: Filters out stdlib (Prelude, Data) and compiler-generated code
- **High Impact Branch Targets**: Prioritized next functions to test based on uncovered branch counts
- **Function Branch Summaries**: Per-function uncovered branch counts and bug/unknown buckets
- **Claim Admissibility**: Explicit `coverage_model`, `unknown_policy`, and `claim_admissible`

## Requirements

- [idris2-yul](https://github.com/shogochiai/idris2-yul) - Idris2 to EVM compiler
- [solc](https://docs.soliditylang.org/) - Solidity compiler (for asm-json)
- [foundry](https://getfoundry.sh/) - Anvil/Cast for EVM execution with tracing

## Installation

```bash
pack build idris2-evm-coverage
```

## Usage

### Quick Start

```bash
# 1. Compile contract with idris2-yul
idris2-yul --cg yul --build mycontract.ipkg

# 2. Run coverage collection
idris2-evm-cov build/exec/mycontract.yul -o coverage.json

# Optional: force static analysis to use a forked Idris2 that supports
# structured case-tree export.
IDRIS2_BIN=/path/to/forked/idris2 \
IDRIS2_PACKAGE_PATH=/path/to/idris2-packages \
idris2-evm-cov --dumpcases .
```

### Manual Pipeline

```bash
# 1. Generate asm-json for PC mapping
solc --strict-assembly --asm-json contract.yul > asm.json

# 2. Start anvil with tracing
anvil --code-size-limit 100000 --steps-tracing &

# 3. Deploy and execute
BYTECODE=$(solc --strict-assembly --bin contract.yul | tail -1)
cast send --create "0x$BYTECODE" --rpc-url http://localhost:8545

# 4. Get execution trace
curl -X POST http://localhost:8545 \
  -d '{"method":"debug_traceTransaction","params":["0x...txhash...",{}]}'

# 5. Map hit PCs to functions
python3 scripts/map_coverage.py --trace trace.json --asm asm.json --yul contract.yul
```

## Exclusion Patterns

The following are excluded from coverage denominator (based on `idris2-coverage/exclusions/base.txt`):

| Pattern | Description |
|---------|-------------|
| `{csegen:*}` | Compiler-generated functions |
| `Prelude.*` | Standard library |
| `Data.*`, `System.*` | Core libraries |
| `prim__*` | Primitives |
| `<generated>.*` | Runtime helpers |

## Output Formats

### JSON
```json
{
  "coverage": {
    "canonical_total": 81,
    "canonical_hit": 17,
    "percent": 21.0
  },
  "execution": {
    "profile": "evm",
    "coverage_model": "semantic test obligation coverage (branch-level)",
    "unknown_policy": "block_claim",
    "claim_admissible": true
  },
  "measurement": {
    "denominator_ids": ["Main.vote:0"],
    "covered_ids": ["Main.vote:0"],
    "excluded_ids": [],
    "unknown_ids": []
  },
  "high_impact_targets": [
    {"function": "TextDAO.Functions.Tally.castVote", "branch_count": 3, "severity": "Inf", "kind": "untested_canonical"}
  ],
  "function_summaries": [
    {"function": "TextDAO.Functions.Tally.castVote", "canonical_branches": 3, "hit_branches": 1}
  ],
  "gaps": []
}
```

### Text
```text
## Branch Classification

Coverage: 21.0%
Execution Profile: evm
Coverage Model: semantic test obligation coverage (branch-level)
Unknown Policy: block_claim
Claim Admissible: True

## Top Branch Targets
  - TextDAO.Functions.Tally.castVote (3 branches, Inf)

## Function Branch Summaries
  - TextDAO.Functions.Tally.castVote: 1/3, uncovered=2, bugs=0, unknown=0
```

## Semantics

The denominator is the set of canonical branch obligations extracted from
`--dumpcases`.

The report also tracks:

- excluded branches, such as logically unreachable `No clauses` cases
- user-admitted partial gaps, such as `Unhandled input`
- optimizer artifacts
- unknown classifications

`claim_admissible` indicates whether the current report can be presented as a
strong semantic coverage claim under the current downstream provenance model.

This package is ahead of Chez/DFX/Web in branch-level UX, but it is still a
downstream approximation until Idris2 exposes stable provenance-tagged
obligation IDs upstream.

## Architecture

```
src/
├── EvmCoverage/
│   ├── Types.idr           # Core types (BranchClass, CoverageGap)
│   ├── Aggregator.idr      # Coverage aggregation logic
│   ├── ProfileParserFSM.idr # O(n) FSM-based HTML parser
│   ├── SourceMap.idr       # PC → Idris2 function mapping
│   ├── Report.idr          # JSON/Markdown report generation
│   └── UnifiedRunner.idr   # End-to-end runner
└── Main.idr
```

## Performance

The profile HTML parser uses a finite state machine (FSM) for O(n) single-pass parsing:

| Parser | Algorithm | 10k spans | 50k spans |
|--------|-----------|-----------|-----------|
| V0 (extractSpans) | O(n²) substr | 21,000 ms | ~8 min |
| V1 (linear) | O(n²) substr copies | 29,000 ms | ~12 min |
| **V2 (FSM)** | **O(n) single-pass** | **14 ms** | **90 ms** |

The FSM approach (`ProfileParserFSM.extractHitsFSM`):
- Single `unpack` String → List Char conversion
- State machine carries accumulated values through transitions
- Minimal extraction (line number, hit count only)
- No intermediate string allocations

Benchmarks: `idris2-evm-cov --bench-v2 <html_file>`

## Related Projects

- [idris2-yul](https://github.com/shogochiai/idris2-yul) - Idris2 to EVM compiler
- [idris2-evm](https://github.com/shogochiai/idris2-evm) - Pure Idris2 EVM interpreter
- [idris2-coverage](https://github.com/5yasuyu/idris2-coverage) - Idris2 branch coverage

## License

MIT
