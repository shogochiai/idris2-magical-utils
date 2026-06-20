||| The CANONICAL `LayerFamily` — EtherClaw's single source of truth for "which
||| architectural layer / toolchain a deliverable belongs to".
|||
||| WHY this lives in idris2-magical-utils (not in etherclaw): the same family concept
||| was duplicated in FOUR places — EtherClaw.InstanceConfig.LayerFamily,
||| EtherClaw.ParityCheck.ProjectDetection.ProjectFamily,
||| EtherClaw.DerivationGraph.Family, and Idris2EtherClawAndroid.Model.DeployFamily —
||| so adding a family meant editing 8–12 sites in lockstep. The app (idris2-react-
||| native) and the canister (GlobalRegistry) could not see etherclaw's enum, so they
||| each kept their own copy. This package is the ONE definition all of them depend on.
|||
||| DESIGN (CLAUDE.md's 3-layer frame, encoded in the type):
|||  * EVM       — logic layer (state machines / contracts)
|||  * ICP       — persistence layer; carries its layer-specific CyclesConfig payload
|||  * CLI / Web / Android — interchangeable FRONTEND layer (same MVU surface)
|||  * Humanoid  — embodiment layer; carries its layer-specific HumanoidConfig payload
|||  * Core      — a pure library, below the layers
|||
||| The point of a SUM type (vs a flat enum): a family's layer-SPECIFIC settings live
||| ONLY on that family's constructor (ICP's cycles, Humanoid's embodiment config), so
||| the type itself guarantees "this setting exists iff this layer" — impossible with a
||| flat enum + optional fields. This is also the extension axis: a new embodiment
||| family (e.g. another robot platform) adds a constructor with its own payload and the
||| compiler then points at every total function that must handle it.
|||
||| DEPENDENCY DISCIPLINE: this module depends on `base` ONLY, so the app, the canister,
||| and etherclaw can all depend on it without pulling a heavy graph. It deliberately
||| does NOT know about coverage (ObservationLayer/Granularity) — the family→coverage
||| mapping lives one-way on the etherclaw side, so coverage-standardization stays
||| unaware of families (no two-way coupling).
module Idris2.LayerFamily

%default total

-- =============================================================================
-- Layer-specific payloads
-- =============================================================================

||| ICP-layer-only cycles config (no other family has this).
public export
record CyclesConfig where
  constructor MkCyclesConfig
  threshold  : Nat     -- refill-trigger balance (cycles)
  refillFrom : String  -- "treasury" (from DAO treasury) | "manual"

public export
Eq CyclesConfig where
  a == b = a.threshold == b.threshold && a.refillFrom == b.refillFrom

||| Humanoid-layer-only config (the embodiment family's layer-specific settings). Kept
||| deliberately small for now — it exists to PROVE the extension axis (a new layer adds
||| a constructor + its payload) and will grow (actuator profile, safety envelope, the
||| assigned physical unit id) as the humanoid family is built out.
public export
record HumanoidConfig where
  constructor MkHumanoidConfig
  unitId   : String   -- the assigned physical robot unit
  platform : String   -- the humanoid platform / SDK identifier

public export
Eq HumanoidConfig where
  a == b = a.unitId == b.unitId && a.platform == b.platform

-- =============================================================================
-- The canonical LayerFamily
-- =============================================================================

||| Which architectural layer / toolchain a deliverable belongs to. A family's
||| layer-specific settings ride on its own constructor (ICP→cycles, Humanoid→config).
public export
data LayerFamily
  = EVM                        -- idris2-evm: logic layer (state machines / contracts)
  | ICP (Maybe CyclesConfig)   -- idris2-icwasm: persistence layer (cycles optional)
  | CLI                        -- bin/xxx: frontend layer (scripts / CLI)
  | Web                        -- idris2-dom-mvc: frontend layer (CLI's browser twin)
  | Android                    -- idris2-react-native: phone layer (MVU → signed APK)
  | Humanoid (Maybe HumanoidConfig)  -- embodiment layer (robot; config optional)
  | Core                       -- a pure Idris2 library

public export
Eq LayerFamily where
  EVM        == EVM        = True
  (ICP _)    == (ICP _)    = True
  CLI        == CLI        = True
  Web        == Web        = True
  Android    == Android    = True
  (Humanoid _) == (Humanoid _) = True
  Core       == Core       = True
  _          == _          = False

-- =============================================================================
-- The wire/string projection — the ONE place family ⇄ string is defined
-- =============================================================================

||| The `lazy <family> ask` argument string AND the deploy-bundle wire tag. This is the
||| single canonical projection family → string; the canister (which keeps families as
||| String tags for a minimal `base`-only wasm) receives exactly this, so the truth is
||| one even though the canister never imports the type.
|||
||| NOTE the non-1:1 maps (kept, not flattened): Android → "web" because its coverage is
||| MVU Msg-branch parity (the same as web; no forked compiler) — only its build/release
||| (idris2-rn → APK) differs. CLI → "core" likewise. Humanoid → "humanoid" (its own
||| coverage/release surface). These maps are the reason a sum type with names beats a
||| flat list: "why does android map to web" stays legible at the definition site.
public export
familyName : LayerFamily -> String
familyName EVM          = "evm"
familyName (ICP _)      = "dfx"
familyName CLI          = "core"
familyName Web          = "web"
familyName Android      = "web"       -- MVU Msg-branch coverage parity (no forked compiler)
familyName (Humanoid _) = "humanoid"
familyName Core          = "core"

||| The build/release toolchain tag (distinct from familyName where coverage collapses
||| layers). android/cli/humanoid keep their OWN tag here even though familyName folds
||| android→web / cli→core for coverage. This is what the deploy bundle and the website
||| derivation key on (DownloadApk vs InstallCli vs …), so it must NOT be folded.
public export
familyTag : LayerFamily -> String
familyTag EVM          = "evm"
familyTag (ICP _)      = "dfx"
familyTag CLI          = "cli"
familyTag Web          = "web"
familyTag Android      = "android"
familyTag (Humanoid _) = "humanoid"
familyTag Core          = "core"

public export
Show LayerFamily where
  show = familyName

||| Parse a TOML/wire family string back to a LayerFamily (payloads default to Nothing —
||| the config is filled separately, not carried in the bare family word). Unknown →
||| Core (a safe, build-only fallback). `familyTag` round-trips here; `familyName` does
||| not (it is lossy by design — "web" could be Web or Android).
public export
layerFamilyFromString : String -> LayerFamily
layerFamilyFromString "evm"      = EVM
layerFamilyFromString "dfx"      = ICP Nothing
layerFamilyFromString "cli"      = CLI
layerFamilyFromString "web"      = Web
layerFamilyFromString "android"  = Android
layerFamilyFromString "humanoid" = Humanoid Nothing
layerFamilyFromString _          = Core   -- "core" + unknown → Core

||| Every family value (payload-free representatives) — for UIs that enumerate families
||| (the app's release-wiring picker) and for exhaustiveness-style iteration. A new
||| constructor here is a deliberate, single edit the rest of the system keys off.
public export
allLayerFamilies : List LayerFamily
allLayerFamilies = [EVM, ICP Nothing, CLI, Web, Android, Humanoid Nothing, Core]
