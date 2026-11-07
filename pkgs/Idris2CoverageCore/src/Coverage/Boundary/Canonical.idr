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
  -- Concurrency prims of base's System.Concurrency (mutex / thread) and the libc `sleep`: the Chez harness EXECUTES them on every
  -- run (a mutex is created and released in-process, a thread is forked and joined, sleep returns), so reaching one opens no harness
  -- hole. excludable=False resolves them to PureComputation: the path stays in the DENOMINATOR and needs a real hit (stricter than
  -- the claim-blocking Unknown it was before). pkgs/LuciBFF, 2026-10-27: 294 paths were Unknown for exactly these four ccs.
  -- Second measurement (same day): idrnet_errno (libidris2_support's errno read after a socket call) and the in-memory buffer prims
  -- (blodwen-new-buffer / -buffer-setint32: setsockopt's argument) were the last 25 Unknown paths, all in LuciBFF.Main's HTTP server. A
  -- localhost socket and a buffer are executable by the Chez harness too: they stay in the denominator and need a test that hits them.
  -- Third (2026-10-28, main 96a47448): `C:setsockopt,libc 6` of LuciBFF.Main.setReuseAddr was the one Unknown left (claim_admissible
  -- False); the same localhost socket, the same rule.
  , MkEffectBoundarySpec "core" PureComputation ["blodwen-make-mutex", "blodwen-mutex", "blodwen-thread", "collect_safe:sleep", "idrnet_errno", "blodwen-new-buffer", "blodwen-buffer", "setsockopt"] False
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
  -- The EVM opcode prims themselves (`evm:sload`, `evm:sstore`, `evm:caller`,
  -- `evm:timestamp`, …) are the instruction set revm executes on every run:
  -- reaching one opens no harness hole, so a path through them stays in the
  -- denominator. Keyed on the `evm:` cc prefix (a compiler fact), so a new
  -- opcode binding is captured automatically without per-symbol enumeration.
  , MkEffectBoundarySpec "evm" PureComputation ["evm:"] False
  ]
boundarySpecsFor Humanoid =
  -- Declared-unimplemented embodiment layer: no runnable backend yet, so it opens
  -- no harness-measurable boundary. Stated explicitly (Totality), not skipped.
  []
boundarySpecsFor Esp32Device =
  -- Declared-unimplemented device layer: the ESP-IDF FFI holes (GPIO/I2S/Wi-Fi)
  -- get their specs together with the on-device collector, not guessed ahead.
  []
