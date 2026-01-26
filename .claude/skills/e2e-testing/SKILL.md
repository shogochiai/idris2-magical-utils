---
name: e2e-testing
description: End-to-End testing infrastructure for Idris2 projects - cross-chain (EVM/ICP), coverage, and CI integration
triggers:
  - E2E
  - end-to-end
  - integration test
  - cross-chain test
  - anvil
  - dfx replica
  - CI test
  - e2e-tests
---

# E2E Testing Infrastructure

## Overview

E2E testing infrastructure for Idris2 projects, supporting:
- **EVM testing** via Anvil (Foundry) + Cast
- **ICP testing** via DFX local replica
- **Cross-chain testing** combining both

## Package Structure

### Shared Harness (idris2-magical-utils)
```
/Users/bob/code/idris2-magical-utils/pkgs/Idris2E2eHarness/
├── idris2-e2e-harness.ipkg
└── src/
    ├── Harness.idr           # Re-exports all modules
    └── Harness/
        ├── Types.idr         # Common types (ProcessResult, ChainConfig, etc.)
        ├── Evm.idr           # Cast command builders
        ├── Icp.idr           # DFX command builders
        ├── Anvil.idr         # Anvil management
        ├── Replica.idr       # DFX replica management
        └── Runner.idr        # Test orchestration
```

### Project-Specific Test Packages

**A-Life (cross-chain):**
```
/Users/bob/a-life/pkgs/E2eTests/
├── e2e-tests.ipkg
└── src/
    ├── E2eTests.idr
    └── E2eTests/
        ├── Config.idr
        └── OufOuc.idr       # OUF→OUC event detection
```

**Lazy (toolchain):**
```
/Users/bob/code/lazy/pkgs/E2eTests/
├── e2e-tests.ipkg
└── src/
    ├── E2eTests.idr
    └── E2eTests/
        ├── Config.idr
        └── Coverage.idr     # Coverage pipeline tests
```

## Using the Harness

### Import
```idris
import Harness
-- or individual modules:
import Harness.Evm
import Harness.Icp
import Harness.Runner
```

### Build Test Steps
```idris
myTestSteps : List TestStep
myTestSteps =
  [ -- Deploy EVM contract
    deployContractStep "MyContract" "src/MyContract.sol:MyContract" rpcUrl privateKey []

    -- Deploy ICP canister
  , deployCanisterStep "my-canister" "my_canister" Nothing

    -- Send EVM transaction
  , evmTxStep "Call function" contractAddr "myFunction(uint256)" ["42"] rpcUrl privateKey

    -- Call canister method
  , canisterCallStep "Query state" "my_canister" "getState" "()"

    -- Wait for sync
  , waitStep 5

    -- Assert result
  , assertStep "Verify output" "dfx canister call my_canister verify '()' | grep -q 'true'"
  ]
```

### Generate Shell Script
```idris
main : IO ()
main = do
  let script = formatAsScript "My E2E Test" myTestSteps
  putStrLn script
```

## CI Integration

E2E tests run as the **CI final gate**, not as part of `lazy core ask`.

### GitHub Actions Example
```yaml
name: E2E Tests

on:
  push:
    branches: [main]
  pull_request:

jobs:
  e2e:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Idris2
        uses: idris-community/setup-idris@v1
        with:
          idris-version: '0.7.0'

      - name: Install Foundry
        uses: foundry-rs/foundry-toolchain@v1

      - name: Install DFX
        run: |
          sh -ci "$(curl -fsSL https://internetcomputer.org/install.sh)"
          dfx --version

      - name: Build E2E Tests
        run: |
          cd pkgs/E2eTests
          idris2 --build e2e-tests.ipkg

      - name: Run E2E Tests
        run: |
          cd pkgs/E2eTests
          ./build/exec/e2e-tests --generate > run-e2e.sh
          chmod +x run-e2e.sh
          ./run-e2e.sh
```

### Local Execution
```bash
cd pkgs/E2eTests

# Build
idris2 --build e2e-tests.ipkg

# Generate and run
./build/exec/e2e-tests --generate > run-e2e.sh
chmod +x run-e2e.sh
./run-e2e.sh
```

## Test Patterns

### Pattern 1: EVM-only
```idris
evmTest : List TestStep
evmTest =
  [ MkTestStep "Start Anvil" "anvil --port 8545 &" True Nothing
  , waitStep 2
  , deployContractStep "Token" "Token.sol:Token" "http://localhost:8545" key []
  , evmTxStep "Mint" tokenAddr "mint(address,uint256)" [addr, "1000"] url key
  , assertStep "Check balance" "cast call ... | grep -q 1000"
  ]
```

### Pattern 2: ICP-only
```idris
icpTest : List TestStep
icpTest =
  [ MkTestStep "Start replica" "dfx start --clean --background" True Nothing
  , waitStep 3
  , deployCanisterStep "backend" "backend" Nothing
  , canisterCallStep "Init" "backend" "init" "()"
  , assertStep "Check state" "dfx canister call backend getState '()' | grep -q expected"
  ]
```

### Pattern 3: Cross-chain
```idris
crossChainTest : List TestStep
crossChainTest =
  [ -- Start both environments
    MkTestStep "Start Anvil" "anvil --fork-url https://mainnet.base.org &" True Nothing
  , MkTestStep "Start replica" "dfx start --clean --background" True Nothing
  , waitStep 3

    -- Deploy to both chains
  , deployContractStep "Bridge" "Bridge.sol" evmUrl key []
  , deployCanisterStep "indexer" "indexer" Nothing

    -- Trigger cross-chain event
  , evmTxStep "Emit event" bridgeAddr "emitEvent(bytes)" ["0x..."] evmUrl key
  , waitStep 5

    -- Verify ICP received event
  , canisterCallStep "Fetch logs" "indexer" "fetchLogs" "()"
  , waitStep 3
  , assertStep "Event received" "dfx canister call indexer getEventCount '()' | grep -v '(0'"
  ]
```

## Design Principles

1. **E2E is NOT `lazy` responsibility**: `lazy core ask` focuses on coverage analysis, not E2E execution
2. **Harness in magical-utils**: Shared infrastructure for all Idris2 projects
3. **Project-specific E2eTests/**: Each monorepo has its own test cases
4. **CI final gate**: E2E runs after unit tests, before deploy
5. **Script generation**: Idris2 generates shell scripts for portability

## Troubleshooting

### Anvil connection refused
```bash
# Wait for startup
until cast block-number --rpc-url http://localhost:8545 2>/dev/null; do sleep 1; done
```

### DFX replica timeout
```bash
# Increase wait time or check dfx.json
dfx ping || (dfx stop && dfx start --clean --background)
```

### Cross-chain event not detected
1. Verify event topic matches (keccak256 of signature)
2. Check chain ID in canister configuration
3. Confirm block range includes transaction block
