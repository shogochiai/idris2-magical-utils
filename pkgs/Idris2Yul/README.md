# idris2-yul

Idris2 to EVM backend via Yul intermediate language.

## Status: Active Development

Compiles Idris2 to EVM bytecode via Yul. Multiple example contracts are functional including Counter, ERC20, BondingCurve, and TextDAO.

## Architecture

```
Idris2 Source → ANF IR → Yul AST → Yul Source → solc → EVM Bytecode
```

### Compilation Pipeline

1. **Idris2 Frontend**: Parse and typecheck Idris2 code
2. **ANF Transform**: Convert to A-Normal Form (all arguments are variables)
3. **Yul Codegen**: Generate Yul from ANF
4. **Solc Backend**: Use Solidity compiler to produce EVM bytecode

## Key Features

- **Quantitative Type Theory (QTT)**: Idris2's multiplicity annotations enable:
  - Zero-cost abstractions (erased at runtime)
  - Linear types for safe asset management

- **Yul Target**: Outputs Yul instead of raw EVM bytecode:
  - Easier debugging
  - Solc handles stack management
  - Access to optimizer

- **Coverage Mapping**: `@source` comments in Yul output enable:
  - PC → Yul → Idris2 function tracing
  - Function-level coverage reports
  - Integration with `idris2-evm-coverage`

## Project Structure

```
src/
├── Compiler/EVM/
│   ├── Yul.idr        # Backend entry point (Codegen implementation)
│   ├── YulIR.idr      # Yul AST types, pretty printer, @source comments
│   ├── Codegen.idr    # ANF → Yul compilation, source location tracking
│   ├── Memory.idr     # Bump allocator and data layout
│   ├── Primitives.idr # Idris primitives → EVM opcodes
│   ├── Foreign.idr    # EVM FFI (%foreign "evm:...")
│   └── ABI.idr        # Ethereum ABI encoding/decoding
└── Main.idr           # Compiler entry point
```

## Coverage Integration

The compiler emits `// @source:` comments before each function in Yul output:

```yul
// @source: TextDAO.Functions.Members.isMember
function TextDAO_Functions_Members_u_isMember(v1, v0) -> result {
  ...
}
```

This enables the coverage pipeline:

```
PC (execution trace) → solc --asm-json → Yul byte offset → @source → Idris2 function
```

### Usage with idris2-evm-coverage

```bash
# 1. Compile with idris2-yul
idris2-yul --cg yul --build mycontract.ipkg

# 2. Generate asm-json mapping
solc --strict-assembly --asm-json build/exec/mycontract.yul > asm.json

# 3. Run with trace collection (anvil + cast)
anvil --code-size-limit 100000 --steps-tracing &
# ... deploy and call contract ...
curl -X POST localhost:8545 -d '{"method":"debug_traceTransaction",...}'

# 4. Map PCs to Idris2 functions for coverage report
```

## Usage

### Building

```bash
# Build the custom compiler
pack build idris2-yul

# Or with idris2 directly
idris2 --build idris2-yul.ipkg
```

### Compiling Contracts

```bash
# Quick build with default EVM version (osaka)
./scripts/build-contract.sh examples/Counter.idr

# Specify EVM version for hardfork compatibility
./scripts/build-contract.sh examples/Counter.idr osaka    # Fusaka (December 2025)
./scripts/build-contract.sh examples/Counter.idr cancun   # Dencun (March 2024)
./scripts/build-contract.sh examples/Counter.idr shanghai # Shanghai (April 2023)

# Manual steps
./build/exec/idris2-yul examples/Counter.idr -o Counter.yul
solc --strict-assembly --evm-version osaka --bin build/exec/Counter.yul.yul
```

### Testing Contracts

```bash
# Run the test suite
./test/test_counter.sh
```

## Example Contracts

| Contract | Description |
|----------|-------------|
| `Counter.idr` | Simple counter with increment/decrement |
| `ERC20.idr` | Standard ERC20 token implementation |
| `BondingCurve.idr` | Token with bonding curve pricing |
| `LinearToken.idr` | Linear types for safe token transfers |
| `Dictionary.idr` | Key-value storage pattern |
| `Proxy.idr` / `ProxyCreator.idr` | Upgradeable proxy pattern |
| `FeatureToggle.idr` | Feature flag management |
| `Receive.idr` | ETH receive handling |
| `TextDAO_*.idr` | DAO with proposal/voting/tally |

## EVM Version Compatibility

Target specific Ethereum hardforks via `--evm-version` in solc:

| Version   | Hardfork      | Date           | Key Features                    |
|-----------|---------------|----------------|---------------------------------|
| `osaka`   | Fusaka        | December 2025  | PeerDAS, EOF (default)          |
| `prague`  | Pectra        | May 2025       | EIP-7702 account abstraction    |
| `cancun`  | Dencun        | March 2024     | TLOAD/TSTORE, blobs             |
| `shanghai`| Shanghai      | April 2023     | Withdrawals, PUSH0              |
| `paris`   | The Merge     | September 2022 | Proof of Stake                  |
| `london`  | London        | August 2021    | EIP-1559, BASEFEE               |

Default is `osaka` (Fusaka). For L2s, check their EVM compatibility - many still use `cancun` or `shanghai`.

## FFI for EVM Operations

```idris
-- Storage operations
%foreign "evm:sstore"
prim__sstore : Bits256 -> Bits256 -> PrimIO ()

%foreign "evm:sload"
prim__sload : Bits256 -> PrimIO Bits256

-- Blockchain context
%foreign "evm:caller"
prim__caller : PrimIO Bits256

%foreign "evm:callvalue"
prim__callvalue : PrimIO Bits256

-- External calls
%foreign "evm:call"
prim__call : Bits256 -> Bits256 -> Bits256 -> Bits256 ->
             Bits256 -> Bits256 -> Bits256 -> PrimIO Bits256
```

## Memory Model

Uses a bump allocator (no garbage collection):

- Free memory pointer at `0x40` (Solidity convention)
- Objects are tagged unions: `[tag: 32 bytes][field1][field2]...`
- Closures: `[funcId][arity][captured1][captured2]...`

## Limitations

Current limitations (planned improvements):

- [ ] No garbage collection (bump allocator only)
- [ ] Limited String support
- [ ] No floating point (EVM doesn't support)
- [ ] Partial closure application needs work
- [ ] Dynamic arrays need ABI encoding

## References

- [Safer Smart Contracts through Type-Driven Development](https://publications.lib.chalmers.se/records/fulltext/234939/234939.pdf) - Pettersson & Edström, 2016
- [Idris2 Backend Cookbook](https://idris2.readthedocs.io/en/latest/backends/backend-cookbook.html)
- [Yul Language Specification](https://docs.soliditylang.org/en/latest/yul.html)
- [EVM Opcodes Reference](https://www.evm.codes/)

## License

MIT
