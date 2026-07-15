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

||| Every coverage family — for exhaustiveness-style iteration in the round-trip and
||| no-silent-gap contract tests, and any UI that enumerates coverage mechanisms.
public export
allCoverageFamilies : List CoverageFamily
allCoverageFamilies = [EvmHash, DfxWasm, WebMVU, AndroidDevice, CoreLib, Humanoid]
