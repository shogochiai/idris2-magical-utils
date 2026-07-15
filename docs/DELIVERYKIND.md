# DeliveryKind & CoverageFamily — the two enums everything hangs off

> **Read this first.** It is the map. The other three docs zoom in:
> - [TOTALITY_ANCHOR.md](./TOTALITY_ANCHOR.md) — *why* these are enums and not strings (the design discipline).
> - [PLUGIN_ARCHITECTURE.md](./PLUGIN_ARCHITECTURE.md) — *how* an outside fork adds a backend without editing core.
> - [forkers/README.md](./forkers/README.md) — the step-by-step for a forker.
> - [exclusion-policy-unification.md](./exclusion-policy-unification.md) — the deep history of the honesty problem these enums solve.
> - [SCENARIO_AND_REFINEMENT_FUZZING.md](./SCENARIO_AND_REFINEMENT_FUZZING.md) — the *next* layer: writing state-carrying coverage as a typed scenario, and reaching un-covered guard arms by synthesising a refinement witness instead of hand-grinding calldata.

---

## 1. Concept — what problem these enums exist for

EtherClaw delivers one logical product across **different runtimes**: an EVM contract,
an ICP canister, a web bundle, a signed Android APK, a CLI binary. Path coverage must
mean the same thing — "every branch the product can take is either executed or honestly
excluded" — on *all* of them. But the runtimes differ in ways that matter to coverage:

| concern | EVM | ICP/WASM | Web/node | Android | Core/Chez |
|---|---|---|---|---|---|
| how a branch-hit is observed | `LOG1` topic | WASM profiling slot | `globalThis.__idris2_recordPathHit` | pre-collected device id-file | RefC counter |
| what opens an effect "hole" | a Yul opcode | `ic0.call_new` | `child_process` | RN native bridge | `popen2` |
| can the test harness run that hole? | revm runs precompiles ✅ | needs a replica ❌ | needs node child ❌ | needs device ❌ | real process ❌ |

If every place that branches on "which runtime" used a `String`, a missed case would
silently default — and a silent default in coverage is a **dishonest denominator** (the
exact bug class catalogued in [exclusion-policy-unification.md](./exclusion-policy-unification.md)).
So "which runtime" is a closed enum, matched exhaustively. Add a runtime → the compiler
lists every site that must now say something about it.

---

## 2. Type — the two enums and how they relate

There are **two** enums, deliberately separate, both in `pkgs/Idris2DeliveryKind/`.

### `DeliveryKind` — *what the product is shipped as* (the 3-layer architecture)
`pkgs/Idris2DeliveryKind/src/Idris2/DeliveryKind.idr`

```idris
data DeliveryKind
  = EVM                        -- idris2-evm: logic layer (state machines / contracts)
  | ICP (Maybe CyclesConfig)   -- idris2-icwasm: persistence layer (cycles optional)
  | CLI                        -- bin/xxx: frontend layer (scripts / CLI)
  | Web                        -- idris2-dom-mvc: frontend layer (CLI's browser twin)
  | Android                    -- idris2-react-native: phone layer (MVU → signed APK)
  | IOS                        -- idris2-react-native: phone layer (MVU → signed IPA)
  | Humanoid (Maybe HumanoidConfig)  -- embodiment layer (robot)
  | Core                       -- a pure Idris2 library
```

This is the **product/architecture** axis — the `family=` in `ETHERCLAW.toml`, the
3-layer pattern (EVM logic / ICP persistence / frontend). It answers *"what do we build
and ship?"*

### `CoverageFamily` — *how coverage is measured for that delivery*
`pkgs/Idris2DeliveryKind/src/Idris2/CoverageFamily.idr`

```idris
data CoverageFamily
  = EvmHash        -- forked --dumppaths-json denominator; FNV-1a topic join (key = Integer)
  | DfxWasm        -- forked --dumppaths-json denominator; string-identity join
  | WebMVU         -- forked --dumppaths-json (--cg node); covers Web + iOS MVU
  | AndroidDevice  -- pre-collected ON-DEVICE id-file; the ONLY honest Android coverage
  | CoreLib        -- forked --dumppaths-json denominator; CLI / pure-library coverage
  | Humanoid       -- embodiment layer; NO runnable backend yet (declared-unimplemented)
```

This is the **measurement** axis — which denominator source, which numerator-join
strategy, which observation medium. It answers *"how do we count branches honestly here?"*

### Why two enums and not one

They are **not** 1:1, and collapsing them would lose information:
- `Web` and `IOS` (two DeliveryKinds) both measure as **`WebMVU`** — they share the
  MVU-over-node coverage mechanism. One coverage family, two delivery kinds.
- **`Android` measures ONLY as `AndroidDevice`.** Android coverage is honest *only* when
  it comes from a real-device run (the pre-collected on-device id-file). It is deliberately
  **not** routed through `WebMVU`'s node re-execution: re-running the MVU logic under node
  is *not* running the APK on a phone, so it would over-claim Android coverage from a
  surrogate runtime. So Android is its own coverage family precisely to refuse that
  substitution — `AndroidDevice` is the single source of Android numerator/denominator.

So the relation is **many-to-one-or-its-own** (Web/iOS→WebMVU; Android→AndroidDevice;
each native family→its own), and each enum stays the single source of truth for its own
axis. This separation is itself an instance of the Totality-anchor discipline
(see [TOTALITY_ANCHOR.md](./TOTALITY_ANCHOR.md) §"don't merge distinct concerns") — the
memory note calls it *"anchor, not merge."*

---

## 3. Where this lives (impl sites you'll actually touch)

| you want to… | go to |
|---|---|
| add/inspect a delivery kind | `pkgs/Idris2DeliveryKind/src/Idris2/DeliveryKind.idr` |
| add/inspect a coverage family | `pkgs/Idris2DeliveryKind/src/Idris2/CoverageFamily.idr` |
| see a family's per-backend effect-boundary policy | `pkgs/Idris2CoverageCore/src/Coverage/Boundary/Canonical.idr` (`boundarySpecsFor`) |
| see the obligation/exclusion vocabulary | `pkgs/Idris2CoverageStandardization/src/Coverage/Standardization/Types.idr` |
| run a family's coverage | `lazy <evm|dfx|web|android|core> ask <target> --steps=4` |

The canonical family table that ties `CoverageFamily` to its effect boundaries is
`boundarySpecsFor : CoverageFamily -> List EffectBoundarySpec` — a total match over every
ctor, no catch-all. That is the join point between this doc and the next two:
`CoverageFamily` (the enum here) × `EffectBoundary` (the holes) = the cells a forker fills.
