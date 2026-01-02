# idris2-evm-coverage

EVM coverage collection and reporting for Idris2 smart contracts.

## Overview

`idris2-evm-coverage` provides function-level code coverage for Idris2 contracts compiled to EVM via the `idris2-yul` backend. It maps EVM execution traces back to Idris2 source functions.

## Coverage Pipeline

```
Idris2 Source → idris2-yul → Yul (@source comments) → solc → EVM Bytecode
                                    ↓
                              asm-json (PC→Yul offset)
                                    ↓
EVM Trace (hit PCs) → PC→Yul→@source → Idris2 Function Coverage
```

## Features

- **PC to Function Mapping**: Maps EVM program counters to Idris2 functions
- **Source Location Tracking**: Uses `@source` comments from idris2-yul
- **Exclusion Patterns**: Filters out stdlib (Prelude, Data) and compiler-generated code
- **Module-level Reports**: Coverage breakdown by Idris2 module

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
    "total": 81,
    "hit": 17,
    "percent": 21.0
  },
  "modules": {
    "TextDAO.Functions.Tally": {"hit": 7, "total": 20},
    "TextDAO.Storages.Schema": {"hit": 10, "total": 37}
  }
}
```

### Markdown
```markdown
## Coverage: 21.0% (17/81)

| Module | Coverage |
|--------|----------|
| TextDAO.Functions.Tally | 35% (7/20) |
| TextDAO.Storages.Schema | 27% (10/37) |
```

## Architecture

```
src/
├── EvmCoverage/
│   ├── Types.idr       # Core types (BranchClass, CoverageGap)
│   ├── Aggregator.idr  # Coverage aggregation logic
│   ├── SourceMap.idr   # PC → Idris2 function mapping
│   ├── Report.idr      # JSON/Markdown report generation
│   └── UnifiedRunner.idr # End-to-end runner
└── Main.idr
```

## Related Projects

- [idris2-yul](https://github.com/shogochiai/idris2-yul) - Idris2 to EVM compiler
- [idris2-evm](https://github.com/shogochiai/idris2-evm) - Pure Idris2 EVM interpreter
- [idris2-coverage](https://github.com/5yasuyu/idris2-coverage) - Idris2 branch coverage

## License

MIT
