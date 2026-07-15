||| Canonical EffectBoundary policy per coverage family — the official table the
||| Idris2 fork ("we") commit to, Totality-anchored over CoverageFamily. The SAME
||| hole differs by backend (what cc opens it) and by runner (whether it is
||| harness-unexecutable), so the policy is keyed by CoverageFamily.
|||
||| PLUGGABLE BY DESIGN: this table covers the canonical families only. The boundary
||| policy a coverage consumer actually uses is `boundarySpecsFor` BELOW unioned with
||| any plugin-provided `List EffectBoundarySpec` (an etherclaw fork ships its own for
||| a new backend without editing this file; the upstream may later fold a good one in).
||| `EffectBoundarySpec` is open data (familyTag is a String) precisely so forks are
||| not gated by this closed enum.
|||
||| TOTALITY ANCHOR: `boundarySpecsFor` matches every CoverageFamily constructor with
||| no `_` catch-all. Adding a canonical family forces its boundary set to be stated
||| here; adding an EffectBoundary ctor forces the spec rows that use it to compile.
||| Two axes (family × boundary) — increasing either mechanically demands the cells.
module Coverage.Boundary.Canonical

import Coverage.Standardization.Types
import Idris2.CoverageFamily

%default total

||| The official boundary specs for one canonical coverage family. Each row says:
||| on THIS backend, a %foreign whose cc contains one of `ccSubstrings` opens
||| `boundary`, and that hole is `excludable` (harness-unexecutable) for this runner.
public export
boundarySpecsFor : CoverageFamily -> List EffectBoundarySpec
boundarySpecsFor CoreLib =
  -- Chez/RefC harness: real process / network / fs are all unexecutable.
  [ MkEffectBoundarySpec "core" ProcessSpawn   ["popen2", "system"]        True
  , MkEffectBoundarySpec "core" NetworkOutcall ["curl", "http"]            True
  , MkEffectBoundarySpec "core" FileSystemIO   ["idris2_openFile", "fopen"] True
  ]
boundarySpecsFor DfxWasm =
  -- WASM canister probe: process/network none; a canister call needs a replica.
  [ MkEffectBoundarySpec "dfx" ProcessSpawn   ["popen2", "system"]   True
  , MkEffectBoundarySpec "dfx" CanisterCall   ["ic0.call_new", "ic0_call"] True
  , MkEffectBoundarySpec "dfx" NetworkOutcall ["http_request"]       True
  ]
boundarySpecsFor WebMVU =
  -- node/Hermes harness: a different process/network/fs surface than Chez.
  [ MkEffectBoundarySpec "web" ProcessSpawn   ["child_process", "node:spawn"] True
  , MkEffectBoundarySpec "web" NetworkOutcall ["fetch", "XMLHttpRequest"]     True
  , MkEffectBoundarySpec "web" FileSystemIO   ["node:fs", "react-native"]     True
  ]
boundarySpecsFor AndroidDevice =
  -- Pre-collected on-device id-file: native bridge (RN FFI) is unexecutable in
  -- the harness; covered paths come from the device run, not a re-execution.
  [ MkEffectBoundarySpec "android-device" ProcessSpawn  ["react-native", "rn:"] True
  , MkEffectBoundarySpec "android-device" CanisterCall  ["@dfinity/agent"]      True
  ]
boundarySpecsFor EvmHash =
  -- revm harness: EVM has NO process/network/fs. Its "boundaries" are opcodes,
  -- and many are RUNNABLE under revm → NOT excludable (they stay in the denominator).
  -- staticcall to a precompile (ecrecover/sha256) is executed by revm itself.
  [ MkEffectBoundarySpec "evm" CanisterCall ["staticcall", "call"] False  -- runnable in revm
  ]
boundarySpecsFor Humanoid =
  -- Declared-unimplemented embodiment layer: no runnable backend yet, so it opens
  -- no harness-measurable boundary. Stated explicitly (Totality), not skipped.
  []
