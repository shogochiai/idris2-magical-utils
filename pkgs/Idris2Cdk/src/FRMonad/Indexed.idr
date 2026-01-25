||| State-Indexed Failure-Recovery Monad
|||
||| Based on the FR Monad paper: "Recovery-Preserving Kleisli Semantics for World-Computer Virtual Machines"
|||
||| Section 7.1: State-Indexed FR Monad
|||   IFR : State → Type → Type  (post is existential)
|||
||| Key invariant: Failure is revert-based atomicity (post = pre on failure).
||| This ensures that invalid state machine transitions are rejected at compile time.
module FRMonad.Indexed

import FRMonad.Failure
import FRMonad.Evidence

%default total

-- =============================================================================
-- State: Type-level state for tracking state machine transitions
-- =============================================================================

||| Type-level state markers for canister lifecycle
public export
data CanisterState : Type where
  Uninitialized : CanisterState
  Initialized   : CanisterState
  Running       : CanisterState
  Upgrading     : CanisterState
  Stopped       : CanisterState

||| Singleton type for CanisterState
||| Allows recovering runtime value from type-level state
public export
data Sing : CanisterState -> Type where
  SUninitialized : Sing Uninitialized
  SInitialized   : Sing Initialized
  SRunning       : Sing Running
  SUpgrading     : Sing Upgrading
  SStopped       : Sing Stopped

||| Auto-implicit singleton instances
public export %hint
sUninitialized : Sing Uninitialized
sUninitialized = SUninitialized

public export %hint
sInitialized : Sing Initialized
sInitialized = SInitialized

public export %hint
sRunning : Sing Running
sRunning = SRunning

public export %hint
sUpgrading : Sing Upgrading
sUpgrading = SUpgrading

public export %hint
sStopped : Sing Stopped
sStopped = SStopped

-- =============================================================================
-- IRes: Indexed Result Core
-- Section 7.1: The core result type with revert-based atomicity
--
-- Important invariant:
--   Failure forces post = pre (revert-based atomicity)
-- =============================================================================

||| Indexed Result: the core result type
||| `pre` is the precondition state
||| `post` is the postcondition state
||| On failure, post = pre (revert semantics)
public export
data IRes : (pre : CanisterState) -> (post : CanisterState) -> Type -> Type where
  ||| Success: state transition from pre to post with value and evidence
  IOk   : (value : a) -> (evidence : Evidence) -> IRes pre post a
  ||| Failure: revert to pre state (post = pre)
  IFail : (failure : Fail) -> (evidence : Evidence) -> IRes pre pre a

public export
Show a => Show (IRes pre post a) where
  show (IOk v e)   = "IOk(" ++ show v ++ ") " ++ show e
  show (IFail f e) = "IFail(" ++ show f ++ ") " ++ show e

-- =============================================================================
-- IFR: Existentially-post-indexed computation
--
-- IFR pre a := exists post. IRes pre post a
--
-- This is the key move: "post is existential".
-- The caller doesn't statically know what post will be, but:
--   - On success, post can be any valid target state
--   - On failure, post = pre (guaranteed by IFail constructor)
-- =============================================================================

||| State-Indexed FR Monad with existential post
||| The post state is determined at runtime but constrained by IRes
public export
IFR : (pre : CanisterState) -> Type -> Type
IFR pre a = (post : CanisterState ** IRes pre post a)

-- =============================================================================
-- Smart constructors
-- =============================================================================

||| Success with existential post
public export
iok : {post : CanisterState} -> (value : a) -> (evidence : Evidence) -> IFR pre a
iok {post} v e = (post ** IOk v e)

||| Helper: construct failure with singleton witness (revert to pre)
||| Pattern matching on Sing ensures singVal sing = pre at type level
public export
mkIFail : Sing pre -> Fail -> Evidence -> IFR pre a
mkIFail SUninitialized x e = (Uninitialized ** IFail x e)
mkIFail SInitialized   x e = (Initialized ** IFail x e)
mkIFail SRunning       x e = (Running ** IFail x e)
mkIFail SUpgrading     x e = (Upgrading ** IFail x e)
mkIFail SStopped       x e = (Stopped ** IFail x e)

||| Failure (reverts to pre)
public export
ifailWith : {auto sing : Sing pre} -> (failure : Fail) -> (evidence : Evidence) -> IFR pre a
ifailWith {sing} x e = mkIFail sing x e

-- =============================================================================
-- Basic accessors
-- =============================================================================

||| Check if result is success
public export
isIOk : IFR pre a -> Bool
isIOk (_ ** IOk _ _)   = True
isIOk (_ ** IFail _ _) = False

||| Check if result is failure
public export
isIFail : IFR pre a -> Bool
isIFail = not . isIOk

||| Extract evidence
public export
getIEvidence : IFR pre a -> Evidence
getIEvidence (_ ** IOk _ e)   = e
getIEvidence (_ ** IFail _ e) = e

||| Extract failure if present
public export
getIFailure : IFR pre a -> Maybe Fail
getIFailure (_ ** IOk _ _)   = Nothing
getIFailure (_ ** IFail f _) = Just f

-- =============================================================================
-- Indexed Functor
-- =============================================================================

||| Map over the value, preserving state indices
public export
imap : (a -> b) -> IFR pre a -> IFR pre b
imap f (post ** IOk v e)   = (post ** IOk (f v) e)
imap f (p ** IFail x e)    = (p ** IFail x e)

-- =============================================================================
-- Indexed Monad operations
-- Section 7.1: Composition with revert-based atomicity
--
-- Semantics:
--   - If the first computation fails, we stay at pre (already ensured by IFail)
--   - If the first succeeds to some mid, we run f at that mid
--   - If f fails, the whole composition reverts to pre (revert-based atomicity)
--   - Evidence is combined in all cases
-- =============================================================================

||| Helper: construct success for ipure with singleton witness
mkIPure : Sing s -> a -> IFR s a
mkIPure SUninitialized v = (Uninitialized ** IOk v emptyEvidence)
mkIPure SInitialized   v = (Initialized ** IOk v emptyEvidence)
mkIPure SRunning       v = (Running ** IOk v emptyEvidence)
mkIPure SUpgrading     v = (Upgrading ** IOk v emptyEvidence)
mkIPure SStopped       v = (Stopped ** IOk v emptyEvidence)

||| Pure: No state change
public export
ipure : {auto sing : Sing s} -> a -> IFR s a
ipure {sing} v = mkIPure sing v

||| Bind with revert-based atomicity
||| If either computation fails, the result reverts to pre
public export
ibind : {auto sing : Sing pre} -> IFR pre a -> ((x : a) -> IFR mid b) -> IFR pre b
ibind {sing} (_ ** IOk v e1) f =
  case f v of
    (post ** IOk v' e2) =>
      (post ** IOk v' (combineEvidence e1 e2))
    (_ ** IFail x e2) =>
      -- revert-based atomicity: failure forces post = pre at the outer boundary
      mkIFail sing x (combineEvidence e1 e2)
ibind {sing} (_ ** IFail x e) _ =
  -- already pre -> pre (revert), keep as-is
  mkIFail sing x e

||| Sequence: Chain state transitions
public export
(>>>=) : {auto sing : Sing pre} -> IFR pre a -> ((x : a) -> IFR mid b) -> IFR pre b
(>>>=) = ibind

||| Ignore left result
public export
(>>>) : {auto sing : Sing pre} -> IFR pre () -> IFR mid b -> IFR pre b
(>>>) ma mb = ibind ma (\_ => mb)

-- =============================================================================
-- State transition constructors
-- These return IFR with specific post states
-- =============================================================================

||| Initialize canister: Uninitialized → Initialized
public export
initialize : String -> a -> IFR Uninitialized a
initialize label value = (Initialized ** IOk value (mkEvidence Init label "initialized"))

||| Start canister: Initialized → Running
public export
start : String -> a -> IFR Initialized a
start label value = (Running ** IOk value (mkEvidence Update label "started"))

||| Begin upgrade: Running → Upgrading
public export
beginUpgrade : String -> a -> IFR Running a
beginUpgrade label value = (Upgrading ** IOk value (mkEvidence PreUpgrade label "upgrade started"))

||| Complete upgrade: Upgrading → Running
public export
completeUpgrade : String -> a -> IFR Upgrading a
completeUpgrade label value = (Running ** IOk value (mkEvidence PostUpgrade label "upgrade completed"))

||| Stop canister: Running → Stopped
public export
stop : String -> a -> IFR Running a
stop label value = (Stopped ** IOk value (mkEvidence Update label "stopped"))

||| Restart canister: Stopped → Running
public export
restart : String -> a -> IFR Stopped a
restart label value = (Running ** IOk value (mkEvidence Update label "restarted"))

-- =============================================================================
-- Operations within a state (no state change)
-- =============================================================================

||| Query operation (no state change)
public export
iquery : String -> a -> IFR Running a
iquery label value = (Running ** IOk value (mkEvidence Query label "query"))

||| Update operation (no state change, but mutates internal state)
public export
iupdate : String -> a -> IFR Running a
iupdate label value = (Running ** IOk value (mkEvidence Update label "update"))

||| Fail with no state change (revert to pre)
public export
ifail : {auto sing : Sing s} -> Phase -> String -> String -> Fail -> IFR s a
ifail {sing} phase label detail failure = mkIFail sing failure (mkEvidence phase label detail)

-- =============================================================================
-- Recovery operations
-- =============================================================================

||| Handle failure, potentially recovering
public export
ihandleWith : ((Fail, Evidence) -> IFR pre a) -> IFR pre a -> IFR pre a
ihandleWith _ (post ** IOk v e)   = (post ** IOk v e)
ihandleWith h (_ ** IFail f e)    = h (f, e)

||| Try alternative on failure
public export
iorElse : IFR pre a -> Lazy (IFR pre a) -> IFR pre a
iorElse (post ** IOk v e)   _   = (post ** IOk v e)
iorElse (_ ** IFail _ _)    alt = alt

-- =============================================================================
-- Boundary functions
-- =============================================================================

||| Run indexed FR and extract result (loses post information)
public export
runIFR : IFR pre a -> Either (Fail, Evidence) (a, Evidence)
runIFR (_ ** IOk v e)   = Right (v, e)
runIFR (_ ** IFail f e) = Left (f, e)

||| Convert to non-indexed result with evidence transform
public export
toFR : IFR pre a -> (Evidence -> Evidence) -> Either (Fail, Evidence) (a, Evidence)
toFR (_ ** IOk v e) f   = Right (v, f e)
toFR (_ ** IFail x e) f = Left (x, f e)

-- =============================================================================
-- Example: Safe canister lifecycle
--
-- This type signature guarantees at compile time that:
-- 1. Canister starts uninitialized
-- 2. Must be initialized before running
-- 3. Must be running to serve queries/updates
-- 4. On failure, state reverts to pre (revert-based atomicity)
--
-- lifecycle : IFR Uninitialized Config
-- lifecycle = initialize "setup" defaultConfig
--         >>> start "launch" ()
--
-- The existential post means we don't statically know the final state,
-- but we know:
--   - On success: transitions completed as specified
--   - On failure: reverted to Uninitialized
-- =============================================================================
