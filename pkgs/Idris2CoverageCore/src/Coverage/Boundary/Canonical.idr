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
  -- Fourth (2026-11-08, Henry #4518): LuciBFF.Host.Part1's Chez timing prims (get-thread-id, current-time 'time-thread,
  -- sstats-gc-real, collections, bytes-allocated; added 11/7-11/8 for the call-start timing) read the process's own clock and
  -- counters. The Chez harness executes them on every run, so they open no hole: PureComputation, in the denominator.
  , MkEffectBoundarySpec "core" PureComputation ["blodwen-make-mutex", "blodwen-mutex", "blodwen-thread", "collect_safe:sleep", "idrnet_errno", "blodwen-new-buffer", "blodwen-buffer", "setsockopt"
                                                  , "get-thread-id", "time-thread", "sstats-gc-real", "(collections)", "bytes-allocated"] False
  -- Fifth (2026-11-09, LuciBFF step4 at luci worktree SUBAGENT-1794173654, measured independently of the fourth row's box): 1900
  -- paths UnknownClassification, claim_admissible False across every per-SpecId run. The taint roots are ccs LuciBFF gained
  -- 2026-10-29..11-08, all executed in-process by the Chez harness on every run, the same rule as the rows above:
  --   * `scheme:(lambda ...)` (Host.Part1 stageClock/threadIdNow/threadCpuMs 22c95f17, collect-trip-bytes, JsonFast string-ref
  --     43322f64): a scheme: cc IS the backend's own code, never a harness hole — keyed on the `scheme:` prefix exactly as
  --     EvmHash keys on `evm:` (this subsumes the fourth row's five per-symbol keys and covers the next scheme lambda too).
  --   * `C__collect_safe:poll,libc 6` (Net.waitReadable 16753d10): the collect-safe poll on a localhost socket, same as sleep.
  --   * `C:malloc/memcpy/free,libc 6` (Net 383bcc42: the pollfd in malloc'd memory) and `C:strdup,libc 6` (Proc): in-process libc.
  , MkEffectBoundarySpec "core" PureComputation ["scheme:", "collect_safe:poll", "C:malloc", "C:memcpy", "C:free", "C:strdup"] False
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
