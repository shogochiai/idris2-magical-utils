# Troubleshooting

Common issues when integrating idris2-subcontract into your project.

## EVM.Primitives Ambiguity

If your project defines its own EVM FFI wrappers (e.g., in a Schema.idr):

```idris
-- Your Schema.idr
%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

export sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)
```

You'll get ambiguous elaboration errors because `Subcontract.Core.Entry` re-exports `EVM.Primitives`:

```
Error: Ambiguous elaboration. Possible results:
    YourProject.Schema.sload ...
    EVM.Primitives.sload ...
```

**Fix**: Add `%hide` directives after your imports:

```idris
import public Subcontract.Core.Entry
import YourProject.Schema

%hide EVM.Primitives.sload
%hide EVM.Primitives.sstore
%hide EVM.Primitives.mstore
%hide EVM.Primitives.keccak256
```

## Address Type Conflict

`Subcontract.Core.ABI.Decoder.Address` is a record type for decoded addresses:

```idris
record Address where
  constructor MkAddress
  addrValue : Integer
```

If you define a simple address alias with the same name, you'll get conflicts:

```idris
-- BAD: conflicts with Decoder.Address
Address : Type
Address = Integer
```

**Fix**: Use a different name for your address alias:

```idris
-- GOOD: no conflict
EvmAddr : Type
EvmAddr = Integer
```

## ABI Decoder Record Accessors

Qualified imports don't work with record field accessors like `addrValue`, `bytes32Value`, `uint256Value`:

```idris
-- BAD: ABI.addrValue won't resolve
import Subcontract.Core.ABI.Decoder as ABI

entry = do
  addr <- ABI.runDecoder ABI.decodeAddress
  result <- myFunction (ABI.addrValue addr)  -- Error: Undefined name ABI.addrValue
```

**Fix**: Use unqualified import:

```idris
-- GOOD: unqualified import works with record accessors
import Subcontract.Core.ABI.Decoder

entry = do
  addr <- runDecoder decodeAddress
  result <- myFunction (addrValue addr)  -- Works!
```

## Build Fails with "evm:* specifier not accepted"

```
Error: The given specifier '["evm:sload"]' was not accepted by any backend.
```

This is expected. Standard Idris2 (Chez Scheme backend) doesn't support `%foreign "evm:*"` FFI specifiers.

**Solution**: Use the idris2-yul compiler pipeline for actual EVM bytecode generation. The standard `pack build` / `pack typecheck` is only useful for type-checking during development.

See [idris2-yul documentation](https://github.com/AnoTensora/idris2-yul) for build instructions.
