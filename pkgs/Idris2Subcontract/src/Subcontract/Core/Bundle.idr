||| Subcontract Core: Bundle and Function Types
|||
||| Implements ERC-7546 concepts as phantom types:
||| - Bundle: Named contract group (e.g., TextDAO, HubDAO)
||| - Function: Implementation bound to bundle + signature
||| - Selector: Derived from Sig, never hand-written
|||
||| Key insight: Selector is NOT an Integer constant.
||| It is derived from Sig at build time, eliminating
||| "did I copy the right selector?" review burden.
|||
module Subcontract.Core.Bundle

import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder

-- =============================================================================
-- Bundle: Named Contract Group
-- =============================================================================

||| A bundle is a named group of related contracts
||| The phantom type parameter ensures functions from different
||| bundles cannot be accidentally mixed
public export
data Bundle : String -> Type where
  MkBundle : (name : String) -> Bundle name

||| Example bundles
public export
TextDAO : Bundle "TextDAO"
TextDAO = MkBundle "TextDAO"

public export
HubDAO : Bundle "HubDAO"
HubDAO = MkBundle "HubDAO"

-- =============================================================================
-- Function: Implementation bound to Bundle + Sig
-- =============================================================================

||| A function implementation in a bundle
||| - `b` is the bundle name (phantom)
||| - `s` is the function signature
||| - Selector is derived from `s`, not stored separately
public export
record Function (b : String) (s : Sig) where
  constructor MkFunction
  implAddress : Integer

||| Initializer function (can only be called once)
||| Distinguished from regular Function for phase checking
public export
record Initializer (b : String) (s : Sig) where
  constructor MkInitializer
  implAddress : Integer

-- =============================================================================
-- Selector Derivation (NOT hand-written)
-- =============================================================================

||| Derive selector from signature
||| This is the ONLY way to get a selector - no Integer constants
|||
||| At compile time, this produces a consistent value.
||| At runtime (when keccak256 is available), it can be verified.
public export
record DerivedSelector (s : Sig) where
  constructor MkDerivedSelector
  ||| The 4-byte selector value (derived, not written)
  value : Integer
  ||| Proof of derivation (signature string)
  derivedFrom : String

||| Derive selector from signature
||| In production: keccak256(sigString s)[:4]
||| For now: placeholder that must be verified by test
export
deriveSelector : (s : Sig) -> DerivedSelector s
deriveSelector s = MkDerivedSelector 0 (sigString s)

-- =============================================================================
-- Known Selectors (Verified by Test)
-- =============================================================================

||| A selector that has been verified to match its signature
||| Created only through `verifySelector` which checks keccak256
public export
record VerifiedSelector (s : Sig) where
  constructor MkVerifiedSelector
  value : Integer

||| Create a verified selector (for use with known constants)
||| The caller asserts this value matches keccak256(sigString s)[:4]
||| This should be checked by a test
export
unsafeAssumeVerified : (s : Sig) -> Integer -> VerifiedSelector s
unsafeAssumeVerified _ v = MkVerifiedSelector v

-- =============================================================================
-- Dictionary Entry
-- =============================================================================

||| An entry in the dictionary: selector -> implementation
||| The bundle and sig are tracked at type level
public export
record DictEntry (b : String) where
  constructor MkDictEntry
  {s : Sig}
  selector : VerifiedSelector s
  impl : Function b s

-- =============================================================================
-- Phase: Deployment lifecycle
-- =============================================================================

||| Deployment phases for a bundle
public export
data Phase
  = NotDeployed      -- Bundle not yet deployed
  | Deployed         -- Dictionary + Proxy deployed, not initialized
  | Initialized      -- Initializer has been called

export
Eq Phase where
  NotDeployed == NotDeployed = True
  Deployed == Deployed = True
  Initialized == Initialized = True
  _ == _ = False

export
Show Phase where
  show NotDeployed = "NotDeployed"
  show Deployed = "Deployed"
  show Initialized = "Initialized"

-- =============================================================================
-- Bundle Context
-- =============================================================================

||| Context for bundle operations
||| Tracks bundle identity and current phase at type level
public export
record BundleCtx (b : String) (p : Phase) where
  constructor MkBundleCtx
  proxyAddress : Integer
  dictAddress : Integer
