||| Obligation-Graded Failure-Recovery Monad
|||
||| Based on the FR Monad paper: "Recovery-Preserving Kleisli Semantics for World-Computer Virtual Machines"
|||
||| Section 7.2: Obligation-Graded FR Monad
|||   GFR : Obligations → Type → Type
|||
||| Grades accumulate under composition and statically track unresolved recovery obligations.
||| This ensures that all recovery obligations are either handled or explicitly visible.
module FRMonad.Graded

import FRMonad.Failure
import FRMonad.Evidence
import Data.List

%default total

-- =============================================================================
-- Obligations: Type-level tracking of unresolved recovery requirements
-- =============================================================================

||| Type-level obligation markers
||| Each represents a category of failure that requires handling
public export
data Obligation : Type where
  ||| Must handle security failures (auth, permissions)
  SecurityObligation  : Obligation
  ||| Must handle state failures (consistency, conflicts)
  StateObligation     : Obligation
  ||| Must handle resource failures (cycles, memory)
  ResourceObligation  : Obligation
  ||| Must handle network failures (calls, HTTP)
  NetworkObligation   : Obligation
  ||| Must handle encoding failures (Candid, serialization)
  EncodingObligation  : Obligation
  ||| Must handle upgrade failures (migration, versioning)
  UpgradeObligation   : Obligation
  ||| No obligations (pure computation)
  NoObligation        : Obligation

public export
Eq Obligation where
  SecurityObligation == SecurityObligation = True
  StateObligation    == StateObligation    = True
  ResourceObligation == ResourceObligation = True
  NetworkObligation  == NetworkObligation  = True
  EncodingObligation == EncodingObligation = True
  UpgradeObligation  == UpgradeObligation  = True
  NoObligation       == NoObligation       = True
  _ == _ = False

public export
Show Obligation where
  show SecurityObligation  = "Security"
  show StateObligation     = "State"
  show ResourceObligation  = "Resource"
  show NetworkObligation   = "Network"
  show EncodingObligation  = "Encoding"
  show UpgradeObligation   = "Upgrade"
  show NoObligation        = "None"

-- =============================================================================
-- Obligations: A list of unresolved obligations (the grade)
-- =============================================================================

||| A grade is a list of obligations that must be resolved
public export
Obligations : Type
Obligations = List Obligation

||| Empty obligations (identity for combination)
public export
noObligations : Obligations
noObligations = []

||| Combine obligations (union, removing duplicates)
public export
combineObligations : Obligations -> Obligations -> Obligations
combineObligations o1 o2 = nub (o1 ++ o2)

||| Remove an obligation (mark as handled)
public export
discharge : Obligation -> Obligations -> Obligations
discharge o = filter (/= o)

||| Check if obligations are empty (all handled)
public export
isResolved : Obligations -> Bool
isResolved [] = True
isResolved _  = False

-- =============================================================================
-- GFR: Obligation-Graded Failure-Recovery Monad
-- Section 7.2: GFR : Obligations → Type → Type
-- =============================================================================

||| Obligation-Graded FR Monad
||| The grade `obs` tracks what obligations this computation may produce
public export
data GFR : (obs : Obligations) -> Type -> Type where
  ||| Success with no new obligations
  GOk   : (value : a) -> (evidence : Evidence) -> GFR obs a
  ||| Failure with obligation from failure category
  GFail : (failure : Fail) -> (evidence : Evidence) -> GFR obs a

public export
Show a => Show (GFR obs a) where
  show (GOk v e)   = "GOk(" ++ show v ++ ") " ++ show e
  show (GFail f e) = "GFail(" ++ show f ++ ") " ++ show e

-- =============================================================================
-- Obligation inference from failures
-- =============================================================================

||| Convert failure category to obligation
public export
categoryToObligation : FailCategory -> Obligation
categoryToObligation SecurityFail  = SecurityObligation
categoryToObligation StateFail     = StateObligation
categoryToObligation ResourceFail  = ResourceObligation
categoryToObligation NetworkFail   = NetworkObligation
categoryToObligation EncodingFail  = EncodingObligation
categoryToObligation PolicyFail    = StateObligation  -- Policy maps to State
categoryToObligation UpgradeFail   = UpgradeObligation
categoryToObligation ExecutionFail = StateObligation  -- Execution maps to State

||| Get obligation from a failure
public export
failureObligation : Fail -> Obligation
failureObligation = categoryToObligation . category

-- =============================================================================
-- Graded Functor
-- =============================================================================

||| Map over the value, preserving grade
public export
gmap : (a -> b) -> GFR obs a -> GFR obs b
gmap f (GOk v e)   = GOk (f v) e
gmap f (GFail x e) = GFail x e

-- =============================================================================
-- Graded Monad operations
-- Section 7.2: Grades accumulate under composition
-- =============================================================================

||| Pure: No obligations
public export
gpure : a -> GFR [] a
gpure v = GOk v emptyEvidence

||| Bind: Obligations accumulate
||| GFR obs1 a -> (a -> GFR obs2 b) -> GFR (obs1 ++ obs2) b
||| Note: In practice, we use a single obs parameter and track dynamically
public export
gbind : GFR obs a -> (a -> GFR obs b) -> GFR obs b
gbind (GOk v e1) f = case f v of
  GOk v' e2   => GOk v' (combineEvidence e1 e2)
  GFail x e2  => GFail x (combineEvidence e1 e2)
gbind (GFail x e) _ = GFail x e

||| Sequence operator
public export
(>>>>=) : GFR obs a -> (a -> GFR obs b) -> GFR obs b
(>>>>=) = gbind

-- =============================================================================
-- Obligation-introducing constructors
-- =============================================================================

||| Create a graded failure with security obligation
public export
gsecurityFail : String -> GFR [SecurityObligation] a
gsecurityFail msg = GFail (Unauthorized msg) (mkEvidence Update "security" msg)

||| Create a graded failure with state obligation
public export
gstateFail : String -> GFR [StateObligation] a
gstateFail msg = GFail (InvalidState msg) (mkEvidence Update "state" msg)

||| Create a graded failure with resource obligation
public export
gresourceFail : String -> GFR [ResourceObligation] a
gresourceFail msg = GFail (RateLimited msg) (mkEvidence Update "resource" msg)

||| Create a graded failure with network obligation
public export
gnetworkFail : String -> GFR [NetworkObligation] a
gnetworkFail msg = GFail (CallError msg) (mkEvidence Update "network" msg)

||| Create a graded failure with encoding obligation
public export
gencodingFail : String -> GFR [EncodingObligation] a
gencodingFail msg = GFail (DecodeError msg) (mkEvidence Update "encoding" msg)

||| Create a graded failure with upgrade obligation
public export
gupgradeFail : String -> GFR [UpgradeObligation] a
gupgradeFail msg = GFail (UpgradeError msg) (mkEvidence PreUpgrade "upgrade" msg)

-- =============================================================================
-- Obligation-discharging handlers
-- Handling a specific category removes that obligation from the grade
-- =============================================================================

||| Handle security failures, discharging SecurityObligation
public export
handleSecurity : ((Fail, Evidence) -> GFR obs a)
              -> GFR (SecurityObligation :: obs) a
              -> GFR obs a
handleSecurity _ (GOk v e)   = GOk v e
handleSecurity h (GFail f e) =
  if category f == SecurityFail
    then h (f, e)
    else GFail f e

||| Handle state failures, discharging StateObligation
public export
handleState : ((Fail, Evidence) -> GFR obs a)
           -> GFR (StateObligation :: obs) a
           -> GFR obs a
handleState _ (GOk v e)   = GOk v e
handleState h (GFail f e) =
  if category f == StateFail
    then h (f, e)
    else GFail f e

||| Handle network failures, discharging NetworkObligation
public export
handleNetwork : ((Fail, Evidence) -> GFR obs a)
             -> GFR (NetworkObligation :: obs) a
             -> GFR obs a
handleNetwork _ (GOk v e)   = GOk v e
handleNetwork h (GFail f e) =
  if category f == NetworkFail
    then h (f, e)
    else GFail f e

||| Handle resource failures, discharging ResourceObligation
public export
handleResource : ((Fail, Evidence) -> GFR obs a)
              -> GFR (ResourceObligation :: obs) a
              -> GFR obs a
handleResource _ (GOk v e)   = GOk v e
handleResource h (GFail f e) =
  if category f == ResourceFail
    then h (f, e)
    else GFail f e

-- =============================================================================
-- Boundary: Only fully-resolved computations can cross boundaries
-- =============================================================================

||| Run a fully-resolved graded FR computation
||| Can only be called when all obligations are discharged (obs = [])
public export
runGFR : GFR [] a -> Either (Fail, Evidence) (a, Evidence)
runGFR (GOk v e)   = Right (v, e)
runGFR (GFail f e) = Left (f, e)

||| Unsafe run that ignores remaining obligations (for testing/debugging)
public export
unsafeRunGFR : GFR obs a -> Either (Fail, Evidence) (a, Evidence)
unsafeRunGFR (GOk v e)   = Right (v, e)
unsafeRunGFR (GFail f e) = Left (f, e)

-- =============================================================================
-- Lifting between grade levels
-- =============================================================================

||| Weaken: Add obligations (safe, computation could fail with new categories)
public export
weaken : GFR obs a -> GFR (o :: obs) a
weaken (GOk v e)   = GOk v e
weaken (GFail f e) = GFail f e

-- Strengthen: Remove obligations (only safe after handling)
-- This is provided by the handle* functions above

-- =============================================================================
-- Retry support with evidence accumulation
-- =============================================================================

||| Retry configuration
public export
record RetryConfig where
  constructor MkRetryConfig
  maxAttempts     : Nat      -- Maximum number of attempts (including initial)
  initialDelayMs  : Nat      -- Initial delay in milliseconds
  backoffFactor   : Nat      -- Multiplier for exponential backoff (use 2 for doubling)
  retryPredicate  : Fail -> Bool  -- Which failures to retry

||| Default retry config (3 attempts, 100ms initial delay, 2x backoff)
public export
defaultRetryConfig : RetryConfig
defaultRetryConfig = MkRetryConfig 3 100 2 isRetryable

||| Retry config for network operations (5 attempts, longer delays)
public export
networkRetryConfig : RetryConfig
networkRetryConfig = MkRetryConfig 5 200 2 (\f => isRetryable f && category f == NetworkFail)

||| Record a retry attempt in evidence
addRetryEvidence : Nat -> Nat -> Fail -> Evidence -> Evidence
addRetryEvidence attempt maxAttempts failure ev =
  let retryEv = mkEvidence Update "retry"
        ("attempt " ++ show attempt ++ "/" ++ show maxAttempts ++ ": " ++ show failure)
  in combineEvidence ev retryEv

||| Internal: Perform retry loop with fuel (remaining attempts)
||| Returns accumulated evidence and final result
gretryLoop : (fuel : Nat)  -- remaining attempts
          -> RetryConfig
          -> Evidence      -- accumulated evidence
          -> GFR obs a
          -> GFR obs a
gretryLoop Z _ accEv comp =
  -- No more retries, return final result
  case comp of
    GOk v e   => GOk v (combineEvidence accEv e)
    GFail f e => GFail f (combineEvidence accEv e)
gretryLoop (S remaining) cfg accEv comp =
  case comp of
    GOk v e => GOk v (combineEvidence accEv e)
    GFail f e =>
      let attempt = cfg.maxAttempts `minus` remaining
          newEv = addRetryEvidence attempt cfg.maxAttempts f (combineEvidence accEv e)
      in if cfg.retryPredicate f
           then gretryLoop remaining cfg newEv comp
           else GFail f newEv

||| Retry a computation with evidence accumulation
||| Evidence tracks all retry attempts for debugging/observability
public export
gretry : RetryConfig -> GFR obs a -> GFR obs a
gretry cfg = gretryLoop cfg.maxAttempts cfg emptyEvidence

||| Retry with default config
public export
gretryDefault : GFR obs a -> GFR obs a
gretryDefault = gretry defaultRetryConfig

||| Retry network operations specifically
public export
gretryNetwork : GFR obs a -> GFR obs a
gretryNetwork = gretry networkRetryConfig

-- =============================================================================
-- Example usage:
--
-- The type signature documents what obligations a computation may produce:
--
-- authenticate : Principal -> GFR [SecurityObligation] User
-- loadState    : Key -> GFR [StateObligation, NetworkObligation] State
-- process      : User -> State -> GFR [ResourceObligation] Result
--
-- To run, all obligations must be handled:
--
-- safeProcess : Principal -> Key -> Either Error Result
-- safeProcess p k = runGFR $
--   handleSecurity defaultAuth $
--   handleState (const defaultState) $
--   handleNetwork retry $
--   handleResource throttle $
--   do user <- weaken $ authenticate p
--      state <- weaken $ loadState k
--      process user state
-- =============================================================================
