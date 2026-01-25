# idris2-dfx-coverage

ICP Canister coverage analysis library for Idris2 CDK backend.

## Overview

This library provides tools for analyzing test coverage of ICP (Internet Computer Protocol) canisters. It integrates with the `lazy dfx ask` command to provide Step 4 coverage analysis.

## Architecture

### ICP Coverage Infrastructure

```
LazyDfx Step4 (Consumer)
    └── idris2-dfx-coverage (Library API)
          ├── DumpcasesParser ............... Static branch analysis
          ├── ProfilingParser ............... Runtime function coverage
          ├── CanisterCall .................. Candid method testing
          ├── CoverageAnalyzer .............. Coverage gap detection
          └── IcWasm.Instrumenter ........... WASM instrumentation
                └── ic-wasm instrument adds:
                      __get_profiling (query)
                      __get_cycles (query)
```

### Coverage Types

| Type | Tool | Output |
|------|------|--------|
| **Candid Method Coverage** | CanisterCall + CoverageAnalyzer | Methods tested via dfx call |
| **Type-Level Branch Analysis** | DumpcasesParser | Static case tree analysis from `idris2 --dumpcases` |
| **Runtime Function Coverage** | IcWasm.Instrumenter + ProfilingParser | Functions executed via `__get_profiling` |

### Module Structure

```
idris2-dfx-coverage
├── CandidParser.idr              # Parse .did files
├── CanisterCall.idr              # Execute dfx canister calls
├── CoverageAnalyzer.idr          # Coverage gap analysis
├── Exclusions.idr                # Exclusion patterns
├── DumpcasesParser.idr           # Parse idris2 --dumpcases output
├── Idris2Coverage.idr            # High-level coverage API
├── IcWasm/
│   ├── Instrumenter.idr          # ic-wasm instrument wrapper
│   ├── ProfilingParser.idr       # Parse __get_profiling output
│   ├── IcpPublicNameParser.idr   # Parse function name mapping
│   └── HttpOutcallDetector.idr   # Detect HTTP outcall capability
├── WasmMapper/
│   ├── WasmFunc.idr              # WASM function mapping
│   ├── NameSection.idr           # Name section parsing
│   └── WasmBranchParser.idr      # WASM branch analysis
├── CodeCoverage/
│   ├── CodeCoverageResult.idr    # Coverage result types
│   └── CodeCoverageAnalyzer.idr  # Code-level coverage
└── Ic0Mock/
    ├── Ic0Stubs.idr              # IC0 system API stubs
    └── MockContext.idr           # Mock context for testing
```

### HighImpactTarget (Shared Type)

`HighImpactTarget` is a shared type from `idris2-coverage-core` that provides a unified representation for coverage gaps across backends.

```idris
-- From Coverage.Core.HighImpact
record HighImpactTarget where
  constructor MkHighImpactTarget
  kind          : HighImpactKind   -- UntestedCanonical | BugUnhandledInput | UnknownCrash
  funcName      : String
  moduleName    : String
  branchCount   : Nat
  executedCount : Nat
  severity      : Double           -- branchCount/executedCount ratio (Inf if 0)
  note          : String

-- Convert FuncCases from dumpcases to HighImpactTarget
import DfxCoverage.DumpcasesParser
targets <- getHighImpactTargets 10 funcCases  -- Top 10 targets
```

This enables cross-backend tooling (ICP, EVM, Chez) to share the same coverage analysis format.

## Modules

### CandidParser

Parses Candid interface files (`.did`) to extract:
- Method names
- Method signatures (args/returns)
- Method modes (query/update/oneway)

```idris
-- Parse a .did file
ci <- readCandidFile "can.did"
let methods = getMethodNames ci
```

### CanisterCall

Executes `dfx canister call` commands and captures results:

```idris
let opts = { defaultCallOptions | canisterId = "abc-123", network = "local" }
result <- callMethod opts "getVersion" ""
```

### CoverageAnalyzer

Analyzes coverage by comparing:
- Methods defined in Candid interface
- Methods actually tested via canister calls

```idris
let result = analyzeCoverage candidInterface callRecords exclusions
putStrLn $ "Coverage: " ++ show result.coveragePercent ++ "%"
```

### Exclusions

Defines patterns for excluding methods from coverage requirements:

```idris
let excl = prefixPattern "debug_" "Debug methods excluded"
let isExcl = isMethodExcluded [excl] "debug_print"  -- Just "Debug methods excluded"
```

## Usage with lazy dfx ask

This library is used by `LazyDfx` package's Step 4:

```bash
lazy dfx ask /path/to/project --steps=4
```

Step 4 will:
1. Parse the project's `.did` file
2. Start local IC network (if needed)
3. Deploy canister (if needed)
4. Execute canister calls for each method
5. Report coverage gaps

## Dependencies

- `base`
- `contrib`

## Building

```bash
pack build idris2-dfx-coverage.ipkg
```

## Related Projects

- `lazy/pkgs/LazyDfx` - The lazy dfx ask implementation
- `idris2-evm-coverage` - Similar coverage library for EVM/Yul
- `idris2-cdk` - Idris2 CDK for ICP canister development
