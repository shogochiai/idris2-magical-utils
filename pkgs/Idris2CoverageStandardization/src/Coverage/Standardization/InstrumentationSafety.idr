||| The axiom of PRODUCTION instrumentation safety: why a family's path-coverage
||| instrumentation does not affect the shipped artifact.
|||
||| Path coverage works by injecting `prim__recordPathHit` at every CaseTree leaf
||| (the forked compiler's `--dumppathshits`). Each backend lowers that probe to its
||| own medium (RefC pathcov.c, Yul `log1`, ES `globalThis` hook, a hits file). For
||| coverage to be HONEST, the shipped production artifact must be provably
||| unaffected — and today each family justifies that by a DIFFERENT mechanism in a
||| prose comment, with nothing forcing the claim. This type makes the justification
||| a value every coverage backend must surface, the second axiom alongside
||| `SoundnessEnvelope.exclusionRequiresSemanticUnreachability` (exclusion needs a
||| semantic reason; production-safety needs a build/runtime reason).
|||
||| DEPENDENCY DISCIPLINE: `base` ONLY — it is pure data (no Path/Step/runtime
||| reference), so it lives with the other soundness vocabulary in Standardization.
module Coverage.Standardization.InstrumentationSafety

%default total

||| Why the production deliverable is unaffected by coverage instrumentation. Both
||| arms carry a mandatory human-written reason (the contract test rejects "").
public export
data InstrumentationSafety
  = ||| The probe is ABSENT from the production artifact: instrumentation is opt-in
    ||| (a separate forked build / env flag), so the shipped binary never contains it.
    ||| (evm: opt-in `EVM_PATHCOV_YUL`; dfx: coverage-fork-only; core/Chez: separate
    ||| `--dumppathshits` build.)
    AbsentByBuildSeparation String
  | ||| The probe is PRESENT in the bundle but provably inert: it is a no-op unless a
    ||| runtime hook is installed by the harness. (web/ES: `globalThis.__idris2_recordPathHit`
    ||| is undefined in prod; android-device: logcat lines are inert unless the device
    ||| harness reads them.)
    NoOpWithoutHook String

||| The mandatory rationale carried by either arm.
public export
safetyReason : InstrumentationSafety -> String
safetyReason (AbsentByBuildSeparation r) = r
safetyReason (NoOpWithoutHook r)         = r

public export
Eq InstrumentationSafety where
  (AbsentByBuildSeparation a) == (AbsentByBuildSeparation b) = a == b
  (NoOpWithoutHook a)         == (NoOpWithoutHook b)         = a == b
  _                           == _                           = False

public export
Show InstrumentationSafety where
  show (AbsentByBuildSeparation r) = "AbsentByBuildSeparation " ++ show r
  show (NoOpWithoutHook r)         = "NoOpWithoutHook " ++ show r
