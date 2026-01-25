# idris2-cdk Documentation

## Architecture

```
ICP.IC0 (FFI primitives)
    ↓
ICP.API (safe wrappers)
    ↓
Application code
```

## Type Classes

### Candidable
Serialize/deserialize Idris2 types to Candid:
- `candidType` - type descriptor
- `toCandid` - encode value
- `fromCandid` - decode value

### Storable
Store/retrieve from stable memory:
- `toBytes` - serialize
- `fromBytes` - deserialize
- `fixedSize` - optional fixed size

## References

- [IC Interface Spec](https://internetcomputer.org/docs/current/references/ic-interface-spec)
- [Candid Spec](https://github.com/dfinity/candid/blob/master/spec/Candid.md)
- [Stable Memory](https://internetcomputer.org/docs/current/developer-docs/memory/stable-memory)
