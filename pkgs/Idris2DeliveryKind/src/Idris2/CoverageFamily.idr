||| The COVERAGE axis (AXIS B): the distinct *mechanisms* by which a deliverable's
||| path obligations are measured for parity-ti Step 4 coverage.
|||
||| This is deliberately a SEPARATE sum from `DeliveryKind` (the delivery axis A):
|||   - Some deliveries share one coverage family: `Web` and `IOS` both measure as
|||     `WebMVU` (the same forked-dumppaths Msg-branch coverage under node).
|||   - `Android` measures ONLY as `AndroidDevice` — a real on-device run (flat id-file
|||     denominator + logcat numerator). It is deliberately NOT folded into `WebMVU`:
|||     re-running MVU logic under node is a surrogate runtime, not the APK on a phone,
|||     so it would over-claim Android coverage. Android keeps its own family to refuse
|||     that substitution.
|||
||| Why a sum and not a string tag: a TOTAL dispatch over `CoverageFamily` forces
||| every family to declare its coverage story (Denominator / Numerator / Chunk /
||| Exclusions, or an explicit declared-unimplemented). Adding a constructor makes the
||| dispatch non-total → a compile error, so a new family can NEVER be silently
||| coverage-less (the historical `lazyCoverageFamily (Humanoid _) = Nothing` escape).
||| The `CoverageFamily` ⇄ tag pair preserves the established string tags for any
||| caller that still keys on the wire form.
|||
||| DEPENDENCY DISCIPLINE: `base` ONLY (lives beside `DeliveryKind`), so the bridge
||| `coverageFamilyOf : DeliveryKind -> CoverageFamily` can sit with its source type
||| and every layer can depend on the axis without pulling in coverage internals.
||| Coverage families carry NO payloads — layer-specific config (cycles, robot) is on
||| the delivery axis; the coverage axis only needs the mechanism identity.
module Idris2.CoverageFamily

%default total

||| The coverage mechanisms. Each constructor names a distinct (denominator source,
||| numerator join, chunking, exclusion) shape — see the per-family notes.
public export
data CoverageFamily
  = EvmHash        -- forked --dumppaths-json denominator; FNV-1a topic join (key = Integer)
  | DfxWasm        -- forked --dumppaths-json denominator; string-identity join (3 strategies, all terminate in pathId)
  | WebMVU         -- forked --dumppaths-json (--cg node) denominator; string-identity join; covers Web + iOS
  | AndroidDevice  -- real on-device id-file denominator; the ONLY honest Android coverage (no node surrogate)
  | CoreLib        -- forked --dumppaths-json denominator; string-identity join; CLI / pure-library coverage
  | Humanoid       -- embodiment layer; NO runnable backend yet (declared-unimplemented with a rationale)

public export
Eq CoverageFamily where
  EvmHash       == EvmHash       = True
  DfxWasm       == DfxWasm       = True
  WebMVU        == WebMVU        = True
  AndroidDevice == AndroidDevice = True
  CoreLib       == CoreLib       = True
  Humanoid      == Humanoid      = True
  _             == _             = False

||| The established wire/string tag for each coverage family. Total: adding a
||| constructor forces an arm here. `web` keeps its historical tag (Web/Android-MVU/
||| iOS all measure as web); the on-device family gets the distinct `android-device`.
public export
coverageFamilyTag : CoverageFamily -> String
coverageFamilyTag EvmHash       = "evm"
coverageFamilyTag DfxWasm       = "dfx"
coverageFamilyTag WebMVU        = "web"
coverageFamilyTag AndroidDevice = "android-device"
coverageFamilyTag CoreLib       = "core"
coverageFamilyTag Humanoid      = "humanoid"

||| Parse a coverage-family tag back to its `CoverageFamily`. Round-trips with
||| `coverageFamilyTag` over `allCoverageFamilies` (see the contract test). Unknown →
||| `CoreLib` (a safe, pure fallback, matching `deliveryKindFromString`'s policy).
public export
coverageFamilyFromString : String -> CoverageFamily
coverageFamilyFromString "evm"            = EvmHash
coverageFamilyFromString "dfx"            = DfxWasm
coverageFamilyFromString "web"            = WebMVU
coverageFamilyFromString "android-device" = AndroidDevice
coverageFamilyFromString "humanoid"       = Humanoid
coverageFamilyFromString _                = CoreLib   -- "core" + unknown → CoreLib

public export
Show CoverageFamily where
  show = coverageFamilyTag

||| How a family scopes its NUMERATOR to a single SpecId (the per-SpecId Step 4
||| a save-driven local QEQ needs). This is the totality anchor that was missing:
||| local QEQ is `step1 ∧ … ∧ step4(q) ∧ step5` PER SpecId q, so step4 must be
||| restrictable to the paths that q's tests exercise. Adding a family forces an
||| arm here, so a new family's per-SpecId story can never be silently absent.
public export
data SpecScopeMethod
  = ||| The forked compiler attributes each hit to the enterTest label; the
    ||| runner keeps only hits whose label matches the SpecId
    ||| (IDRIS2COV_SPEC_FILTER + System.Coverage.enterTest). Works for any
    ||| backend whose test HARNESS runs test_<SpecId>_… under enterTest —
    ||| core/evm/dfx/web all execute their suite in-process/in-replica/in-node.
    EnterTestLabel
  | ||| The numerator comes from a device RUN of the app, not a test suite, so
    ||| there is no per-test enterTest boundary to filter on. Whole-target only
    ||| UNTIL a device test-runner drives `test_<SpecId>_…` on-device and the
    ||| hook forwards the enterTest label through logcat. Named — not silently
    ||| EnterTestLabel — so this gap is visible in the type, not a false pass.
    DeviceRunWholeTargetOnly
  | ||| No runnable numerator at all (Humanoid): per-SpecId is vacuous.
    NoNumerator

public export
Eq SpecScopeMethod where
  EnterTestLabel           == EnterTestLabel           = True
  DeviceRunWholeTargetOnly == DeviceRunWholeTargetOnly = True
  NoNumerator              == NoNumerator              = True
  _                        == _                        = False

||| The per-SpecId numerator method for each family. Total: a new CoverageFamily
||| forces its per-SpecId answer here. `AndroidDevice` is
||| DeviceRunWholeTargetOnly — the honest statement that its device numerator is
||| not yet SpecId-scoped (needs an on-device test-runner + enterTest-over-logcat
||| forwarding), so local QEQ over an android SpecId must treat step4 as
||| whole-target, NOT claim a false per-SpecId pass.
public export
specScopeMethod : CoverageFamily -> SpecScopeMethod
specScopeMethod EvmHash       = EnterTestLabel
specScopeMethod DfxWasm       = EnterTestLabel
specScopeMethod WebMVU        = EnterTestLabel
specScopeMethod AndroidDevice = DeviceRunWholeTargetOnly
specScopeMethod CoreLib       = EnterTestLabel
specScopeMethod Humanoid      = NoNumerator

||| Does this family support a genuine per-SpecId Step 4 numerator? A local QEQ
||| gate MUST consult this: for a family that does NOT (android device today),
||| a per-SpecId step4 is UNSCOPED — treat it as whole-target evidence, never as
||| a SpecId-scoped pass. This is the anchor local QEQ was missing.
public export
supportsPerSpecIdNumerator : CoverageFamily -> Bool
supportsPerSpecIdNumerator f = specScopeMethod f == EnterTestLabel

||| Every coverage family — for exhaustiveness-style iteration in the round-trip and
||| no-silent-gap contract tests, and any UI that enumerates coverage mechanisms.
public export
allCoverageFamilies : List CoverageFamily
allCoverageFamilies = [EvmHash, DfxWasm, WebMVU, AndroidDevice, CoreLib, Humanoid]
