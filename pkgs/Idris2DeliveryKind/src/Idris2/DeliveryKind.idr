||| The canonical `DeliveryKind` — "what a deliverable is delivered AS, and which
||| architectural layer it lives in": a contract, a canister, a web bundle, a signed
||| APK, a CLI binary, a robot bundle, a pure library.
|||
||| ★Two axes, one word disambiguated. EtherClaw historically called this `family`, but
||| "family" actually names TWO different axes that were conflated:
|||   - DELIVERY axis (this type): what is built / deployed / released / preflighted —
|||     evm / dfx / web / android / humanoid / core. This is the AXIS A that
|||     build/deploy/release/preflight/website导线 all dispatch on.
|||   - COVERAGE axis (kept as "family" in the parity tooling): the unit a parity-st /
|||     parity-ti measurement covers — the delivery kinds PLUS `integration`
|||     (system-of-systems coverage of the whole instance). This is AXIS B.
||| `integration` belongs ONLY to the coverage axis (there is nothing to build/deploy/
||| preflight for it), so it is NOT a DeliveryKind. Renaming away from "family" makes
||| that distinction structural: a function that takes a DeliveryKind cannot be handed
||| `integration`, so "should integration be in preflight/release?" answers itself —
||| no, those take a DeliveryKind. The coverage projection `coverageFamilyOf` is the
||| one-way bridge DeliveryKind → coverage-family string (android→"web", cli→"core").
|||
||| The point of a SUM with payloads on their own constructors: a delivery's
||| layer-specific settings live ONLY on its constructor (ICP→cycles, Humanoid→robot
||| config) — impossible with a flat enum. A new delivery kind adds a constructor and
||| the compiler then points at every total function that must handle it.
|||
||| DEPENDENCY DISCIPLINE: `base` ONLY, so the app, the canister, and etherclaw can all
||| depend on it. It does NOT know about coverage observation layers (that mapping is
||| one-way on the etherclaw side), so coverage-standardization stays unaware of it.
module Idris2.DeliveryKind

import public Idris2.CoverageFamily

%default total

-- =============================================================================
-- Layer-specific payloads
-- =============================================================================

||| ICP-layer-only cycles config (no other delivery has this).
public export
record CyclesConfig where
  constructor MkCyclesConfig
  threshold  : Nat     -- refill-trigger balance (cycles)
  refillFrom : String  -- "treasury" (from DAO treasury) | "manual"

public export
Eq CyclesConfig where
  a == b = a.threshold == b.threshold && a.refillFrom == b.refillFrom

||| Humanoid-layer-only config (the embodiment delivery's layer-specific settings).
||| Small for now — it proves the extension axis (a new layer adds a constructor + its
||| payload) and will grow (actuator profile, safety envelope, assigned physical unit).
public export
record HumanoidConfig where
  constructor MkHumanoidConfig
  unitId   : String   -- the assigned physical robot unit
  platform : String   -- the humanoid platform / SDK identifier

public export
Eq HumanoidConfig where
  a == b = a.unitId == b.unitId && a.platform == b.platform

-- =============================================================================
-- The canonical DeliveryKind
-- =============================================================================

||| What a deliverable is delivered AS / which architectural layer it lives in. A
||| delivery's layer-specific settings ride on its own constructor (ICP→cycles,
||| Humanoid→config). The constructor names are the established short tags.
public export
data DeliveryKind
  = EVM                        -- idris2-evm: logic layer (state machines / contracts)
  | ICP (Maybe CyclesConfig)   -- idris2-icwasm: persistence layer (cycles optional)
  | CLI                        -- bin/xxx: frontend layer (scripts / CLI)
  | Web                        -- idris2-dom-mvc: frontend layer (CLI's browser twin)
  | Android                    -- idris2-react-native: phone layer (MVU → signed APK)
  | IOS                        -- idris2-react-native: phone layer (MVU → signed IPA)
  | Humanoid (Maybe HumanoidConfig)  -- embodiment layer (robot; config optional)
  | Core                       -- a pure Idris2 library
  | Fork                       -- a forked EXTERNAL (possibly non-Idris2) upstream; bypasses the Idris2-specific verification chain by design; "release" means a merged upstream PR/MR, not GovSrc activation

public export
Eq DeliveryKind where
  EVM          == EVM          = True
  (ICP _)      == (ICP _)      = True
  CLI          == CLI          = True
  Web          == Web          = True
  Android      == Android      = True
  IOS          == IOS          = True
  (Humanoid _) == (Humanoid _) = True
  Core         == Core         = True
  Fork         == Fork         = True
  _            == _            = False

-- =============================================================================
-- The wire/string projection — the ONE place delivery ⇄ string is defined
-- =============================================================================

||| The COVERAGE family this delivery is measured under — the AXIS A → AXIS B map,
||| now landing on the typed `CoverageFamily` (not a string), so a TOTAL dispatch
||| over the result forces every family to declare its coverage story. A parity run
||| keys on this: Android & iOS fold to `WebMVU` (the pure MVU layer, no forked
||| compiler), CLI to `CoreLib`. Lossy by design (Web/Android/iOS → WebMVU), which is
||| why it is SEPARATE from the lossless `deliveryTag`. The on-device `AndroidDevice`
||| family is reached explicitly by the device runner, not via this default map (an
||| Android delivery's DEFAULT measurement is the pure MVU layer). `integration` is
||| NOT producible here — it is a coverage unit with no delivery.
public export
coverageFamilyOf : DeliveryKind -> CoverageFamily
coverageFamilyOf EVM          = EvmHash
coverageFamilyOf (ICP _)      = DfxWasm
coverageFamilyOf CLI          = CoreLib
coverageFamilyOf Web          = WebMVU
coverageFamilyOf Android      = WebMVU      -- MVU dumppaths path coverage over the pure layer
coverageFamilyOf IOS          = WebMVU      -- same MVU layer as Android/Web → same coverage
coverageFamilyOf (Humanoid _) = Humanoid
coverageFamilyOf Core         = CoreLib
coverageFamilyOf Fork         = ExternalFork  -- no Idris2 dumppaths coverage — verification bypassed by design

||| The COVERAGE-family wire tag for a delivery (the string callers that historically
||| used `coverageFamilyOf : … -> String` use this). Composes the typed map with the
||| family's tag, so the "android→web / cli→core" folding lives in ONE place.
public export
coverageFamilyTagOf : DeliveryKind -> String
coverageFamilyTagOf = coverageFamilyTag . coverageFamilyOf

||| The lossless DELIVERY tag (distinct from coverageFamilyOf where coverage collapses
||| layers). android/cli/humanoid keep their OWN tag even though coverage folds
||| android→web / cli→core. This is what the deploy bundle and the website derivation
||| key on (DownloadApk vs InstallCli vs …), so it must NOT be folded. (Historically
||| `familyTag`.)
public export
deliveryTag : DeliveryKind -> String
deliveryTag EVM          = "evm"
deliveryTag (ICP _)      = "dfx"
deliveryTag CLI          = "cli"
deliveryTag Web          = "web"
deliveryTag Android      = "android"
deliveryTag IOS          = "ios"
deliveryTag (Humanoid _) = "humanoid"
deliveryTag Core         = "core"
deliveryTag Fork         = "fork"

public export
Show DeliveryKind where
  show = deliveryTag

||| Parse a TOML/wire delivery string back to a DeliveryKind (payloads default to
||| Nothing — the config is filled separately). Unknown → Core (a safe, build-only
||| fallback). `deliveryTag` round-trips here; `coverageFamilyOf` does not (it is
||| lossy). (Historically `layerFamilyFromString`.)
public export
deliveryKindFromString : String -> DeliveryKind
deliveryKindFromString "evm"      = EVM
deliveryKindFromString "dfx"      = ICP Nothing
deliveryKindFromString "cli"      = CLI
deliveryKindFromString "web"      = Web
deliveryKindFromString "android"  = Android
deliveryKindFromString "ios"      = IOS
deliveryKindFromString "humanoid" = Humanoid Nothing
deliveryKindFromString "fork"     = Fork
deliveryKindFromString _          = Core   -- "core" + unknown → Core

||| Every delivery kind (payload-free representatives) — for UIs that enumerate
||| deliveries (the app's release-wiring picker) and exhaustiveness-style iteration.
public export
allDeliveryKinds : List DeliveryKind
allDeliveryKinds = [EVM, ICP Nothing, CLI, Web, Android, IOS, Humanoid Nothing, Core, Fork]
