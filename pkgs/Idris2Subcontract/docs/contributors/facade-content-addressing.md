# Facade content addressing (FCD_CANON_/FCD_HASH_/FCD_ADDR_)

The ERC-7546 facade makes the proxy's call surface visible, but its address is
deployment-time churn. Content addressing turns that around: a facade's address
becomes a **pure function of its source** — the same signature set always
yields the same shape hash and the same CREATE2 address, so a facade that is
already deployed and verified needs no further action.

## Canonical signature set — FCD_CANON_001 / FCD_CANON_002

`facadeCanonical : List FacadeFn -> String` renders each `FacadeFn` as one line

```
name(argTypes)->(retTypes):mutability
```

- argument types, return types, and mutability are all part of the line
  (FCD_CANON_001) — the explorer's display changes when any of them change
- the lines are **sorted ascending** and joined with newlines, so a reordered
  signature set canonicalizes identically (FCD_CANON_002)

## Shape hash — FCD_HASH_001..003

`facadeShapeHash : List FacadeFn -> String` is the lowercase-hex Keccak256 of
`facadeCanonical` (FCD_HASH_001), via `Subcontract.Core.ABI.Keccak.keccak256Hex`.
Because return types (FCD_HASH_002) and mutability (FCD_HASH_003) are part of
the canonical form, a facade that differs in either hashes differently.

## Deterministic CREATE2 address — FCD_ADDR_001

`facadeCreate2Address : Integer -> String -> String -> Integer` returns the
low 20 bytes of the EIP-1014 preimage hash

```
keccak256(0xff ++ factory(20B) ++ salt(32B) ++ initCodeHash(32B))
```

with `factory` big-endian-encoded and `salt` / `initCodeHash` hex-decoded and
left-padded to 32 bytes. A facade's address is therefore fixed by its source:
deploy once, verify once, and reuse the same address for every identical
signature set.

## Tests

`Facade/Tests/AllTests.idr` registers `test_FCD_CANON_001`, `test_FCD_CANON_002`,
`test_FCD_HASH_001`, `test_FCD_HASH_002`, `test_FCD_HASH_003`, and
`test_FCD_ADDR_001` against these ids, including the canonical EIP-1014 example
vector for the CREATE2 address.
