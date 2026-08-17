# ERC-7546 Facade (FCD) — a verified contract for a Dictionary-dispersed call surface

The ERC-7546 proxy dispatches every selector through a Dictionary, so an
explorer cannot show the proxy's call surface — it only sees the proxy
contract with no ABI of its own. `Subcontract.Standards.ERC7546.Facade`
solves that by rendering one *verified* Solidity contract that bundles every
`FacadeFn` into a single explorer-visible source. This document is the doc
layer of the FCD spec-test parity chain: every `[[spec]]` id declared in the
co-located
`src/Subcontract/Standards/ERC7546/Facade/SPEC.toml` is cited here, each in a
real sentence about its requirement.

## Why a facade

ERC-7546 keeps per-selector implementations in the Dictionary and forwards
calls from the proxy. Because no single contract holds all the selectors,
verification tools and block explorers cannot list what the proxy can be
called with. `facadeSolidity : String -> List FacadeFn -> String` takes the
contract name and the list of functions the proxy exposes, and renders one
self-contained Solidity source that an explorer can verify and display. Each
`FacadeFn` pairs an existing `Subcontract.Core.ABI.Sig` (`MkSig "name" args
rets`, whose `abiTypeStr` yields the Solidity type name) with a
`StateMutability` of `View`, `Nonpayable`, or `Payable`.

## Contract Envelope

- **FCD_ENV_001** — The output begins with an SPDX license identifier line, so the rendered source is a properly licensed artifact an explorer accepts.
- **FCD_ENV_002** — The output declares `pragma solidity ^0.8.20;`, pinning the language version the verified contract is meant to compile with.
- **FCD_ENV_003** — The output wraps every rendered function inside `contract <name> { ... }` using the given name, so the facade is one named, verifiable contract.

## Function Rendering

- **FCD_FN_001** — Each `FacadeFn` renders exactly one `function <name>(<types>)` declaration whose argument list is Solidity types only, with no parameter names, so the ABI shown to the explorer is exact.
- **FCD_FN_002** — A `FacadeFn` with `mut == View` renders the function as `external view`.
- **FCD_FN_003** — A `FacadeFn` with `mut == Payable` renders the function as `external payable`.
- **FCD_FN_004** — A `FacadeFn` with `mut == Nonpayable` renders plain `external` with no extra mutability keyword.
- **FCD_FN_005** — Every emitted function has an empty body, because the facade exists to surface the call surface, not to reimplement the Dictionary's logic.

## Return Clause

- **FCD_RET_001** — A `FacadeFn` with `rets == []` emits no `returns` clause.
- **FCD_RET_002** — A `FacadeFn` with exactly one ret emits `returns (<type>)`.
- **FCD_RET_003** — A `FacadeFn` with multiple rets emits `returns (<t1>, <t2>)` with the Solidity types in order.

## Usage

```idris
import Subcontract.Standards.ERC7546.Facade
import Subcontract.Core.ABI.Sig

transferFn : FacadeFn
transferFn = MkFacadeFn (MkSig "transfer" [TAddress, TUint256] [TBool]) Nonpayable

balanceOfFn : FacadeFn
balanceOfFn = MkFacadeFn (MkSig "balanceOf" [TAddress] [TUint256]) View

facadeSolidity "MyERC7546Proxy" [transferFn, balanceOfFn]
```

The result is a single verified Solidity source describing every function the
proxy exposes, ready for an explorer.
