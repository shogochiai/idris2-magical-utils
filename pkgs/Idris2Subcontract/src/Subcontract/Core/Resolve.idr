||| Subcontract Core: Resolve (Recovery Procedures)
|||
||| MVP-1: Failures are not terminal - they can be resolved.
|||
||| Key insight: "Fail and revert" is not the only option.
||| Different conflicts may warrant different resolutions:
||| - Retry (idempotent operations)
||| - Compensate (rollback via inverse operation)
||| - Escalate (require human intervention)
||| - Abort (truly unrecoverable)
module Subcontract.Core.Resolve

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence
import public Subcontract.Core.Outcome

%default total

-- =============================================================================
-- Resolution: What to Do After Failure
-- =============================================================================

||| Resolution decision for a failure
public export
data Resolution : Type -> Type where
  ||| Successfully recovered with value
  Recovered : a -> Resolution a
  ||| Escalate to higher layer (human, governance, etc)
  Escalate : Conflict -> Evidence -> Resolution a
  ||| Abort - unrecoverable, but classified
  Abort : Conflict -> Evidence -> Resolution a
  ||| Retry - operation is idempotent, can try again
  Retry : Nat -> Resolution a  -- Nat = suggested retry count

||| Check if resolution is successful
public export
isRecovered : Resolution a -> Bool
isRecovered (Recovered _) = True
isRecovered _ = False

||| Extract recovered value
public export
fromResolution : a -> Resolution a -> a
fromResolution _ (Recovered x) = x
fromResolution def _ = def

-- =============================================================================
-- Resolver Interface
-- =============================================================================

||| Interface for conflict-specific resolution strategies
public export
interface Resolver (c : Conflict) where
  ||| Attempt to resolve this conflict type
  resolve : Evidence -> Resolution a
  ||| Can this conflict be retried?
  canRetry : Bool
  ||| Can this conflict be compensated?
  canCompensate : Bool

-- =============================================================================
-- Default Resolution Strategies
-- =============================================================================

||| Default resolver: abort on all conflicts
public export
defaultResolve : Conflict -> Evidence -> Resolution a
defaultResolve c e = Abort c e

||| Retry resolver: suggest retry for idempotent operations
public export
retryResolve : Nat -> Conflict -> Evidence -> Resolution a
retryResolve maxRetries c e = 
  if canRetryConflict c
    then Retry maxRetries
    else Abort c e
  where
    canRetryConflict : Conflict -> Bool
    canRetryConflict GasExhausted = True
    canRetryConflict Revert = False  -- Depends on cause
    canRetryConflict _ = False

||| Escalation resolver: escalate security-critical conflicts
public export
escalateResolve : Conflict -> Evidence -> Resolution a
escalateResolve c e =
  if shouldEscalate c
    then Escalate c e
    else Abort c e
  where
    shouldEscalate : Conflict -> Bool
    shouldEscalate Reentrancy = True
    shouldEscalate AuthViolation = True
    shouldEscalate CEIViolation = True
    shouldEscalate StorageAlias = True
    shouldEscalate _ = False

-- =============================================================================
-- Resolution Chain
-- =============================================================================

||| Chain of resolution strategies
public export
data ResolutionChain : Type -> Type where
  ||| End of chain - use default
  DefaultRes : ResolutionChain a
  ||| Try this strategy, then continue if not recovered
  TryRes : (Conflict -> Evidence -> Resolution a) 
        -> ResolutionChain a 
        -> ResolutionChain a

||| Run resolution chain
public export
runChain : ResolutionChain a -> Conflict -> Evidence -> Resolution a
runChain DefaultRes c e = defaultResolve c e
runChain (TryRes f rest) c e = 
  case f c e of
    Recovered x => Recovered x
    _ => runChain rest c e

||| Standard resolution chain
public export
standardChain : ResolutionChain a
standardChain = TryRes retryResolve (TryRes escalateResolve DefaultRes)
  where
    retryResolve : Conflict -> Evidence -> Resolution a
    retryResolve c e = Resolve.retryResolve 3 c e
    escalateResolve : Conflict -> Evidence -> Resolution a
    escalateResolve = Resolve.escalateResolve

-- =============================================================================
-- Outcome + Resolution Integration
-- =============================================================================

||| Resolve an outcome using a resolution function
public export
resolveOutcome : (Conflict -> Evidence -> Resolution a) -> Outcome a -> Resolution a
resolveOutcome _ (Ok x) = Recovered x
resolveOutcome f (Fail c e) = f c e

||| Resolve with standard chain
public export
resolveStandard : Outcome a -> Resolution a
resolveStandard = resolveOutcome (runChain standardChain)

||| Convert resolution back to outcome (for chaining)
public export
resolutionToOutcome : Resolution a -> Outcome a
resolutionToOutcome (Recovered x) = Ok x
resolutionToOutcome (Escalate c e) = Fail c e
resolutionToOutcome (Abort c e) = Fail c e
resolutionToOutcome (Retry _) = Fail Revert (tagEvidence "retry requested")

-- =============================================================================
-- Compensation (Inverse Operations)
-- =============================================================================

||| A compensable operation with its inverse
public export
record Compensable (a : Type) where
  constructor MkCompensable
  ||| The forward operation
  doOp : IO (Outcome a)
  ||| The inverse/rollback operation
  undoOp : a -> IO (Outcome ())

||| Run compensable operation, undo on failure
public export
runCompensable : Compensable a -> IO (Outcome a)
runCompensable comp = do
  result <- comp.doOp
  case result of
    Ok x => pure (Ok x)
    Fail c e => pure (Fail c e)

||| Sequence compensable operations with rollback
public export
sequenceCompensable : List (Compensable a) -> IO (Outcome (List a))
sequenceCompensable [] = pure (Ok [])
sequenceCompensable (c :: cs) = do
  result <- c.doOp
  case result of
    Ok x => do
      rest <- sequenceCompensable cs
      case rest of
        Ok xs => pure (Ok (x :: xs))
        Fail cf ef => do
          -- Rollback this operation
          _ <- c.undoOp x
          pure (Fail cf ef)
    Fail cf ef => pure (Fail cf ef)

-- =============================================================================
-- Resolution Logging
-- =============================================================================

||| Log entry for resolution attempt
public export
record ResolutionLog where
  constructor MkResLog
  conflict : Conflict
  evidence : Evidence
  attempted : String  -- Resolution type attempted
  success : Bool

||| Create log entry
public export
logResolution : Conflict -> Evidence -> String -> Bool -> ResolutionLog
logResolution = MkResLog
