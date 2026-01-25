||| Subcontract Core: Idempotence and Compensation
|||
||| MVP-4: Meta-indicators for safe composition.
|||
||| Key insight: Not all operations are equal for recovery:
||| - Idempotent: Can safely retry (GET, check balance)
||| - Compensable: Has inverse operation (transfer has inverse transfer)
||| - Terminal: No undo possible (burn, selfdestruct)
|||
||| Solidity: All operations treated equally
||| Idris2: Composability properties are type-level
module Subcontract.Core.Idempotent

import public Subcontract.Core.Outcome
import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence

%default total

-- =============================================================================
-- Idempotence: Safe to Retry
-- =============================================================================

||| Interface for idempotent operations.
||| An operation is idempotent if f(f(x)) = f(x).
public export
interface Idempotent (op : Type) where
  ||| Check if operation is idempotent
  isIdempotent : op -> Bool
  ||| Maximum safe retry count (0 = infinite)
  maxRetries : op -> Nat

||| Marker for idempotent operations
public export
data IdempotentOp : Type -> Type where
  MkIdempotent : (maxRetry : Nat) -> a -> IdempotentOp a

||| Extract operation from idempotent wrapper
public export
getOp : IdempotentOp a -> a
getOp (MkIdempotent _ x) = x

||| Get max retries
public export
getMaxRetry : IdempotentOp a -> Nat
getMaxRetry (MkIdempotent n _) = n

||| Run idempotent operation with automatic retry
public export
runIdempotent : IdempotentOp (IO (Outcome a)) -> IO (Outcome a)
runIdempotent (MkIdempotent maxRetry op) = go maxRetry
  where
    go : Nat -> IO (Outcome a)
    go Z = op
    go (S n) = do
      result <- op
      case result of
        Ok x => pure (Ok x)
        Fail GasExhausted _ => go n  -- Retry on gas
        Fail c e => pure (Fail c e)  -- Don't retry other failures

-- =============================================================================
-- Compensation: Has Inverse
-- =============================================================================

||| A compensable operation with its inverse.
||| If forward fails after partial execution, inverse can undo.
public export
record Compensable (a : Type) where
  constructor MkCompensable
  ||| The forward operation
  forward : IO (Outcome a)
  ||| The inverse/undo operation (takes result of forward)
  inverse : a -> IO (Outcome ())
  ||| Description for logging
  description : String

||| Run compensable operation
public export
runCompensable : Compensable a -> IO (Outcome a)
runCompensable c = c.forward

||| Compensate (run inverse)
public export
compensate : Compensable a -> a -> IO (Outcome ())
compensate c x = c.inverse x

||| A completed operation with its result and compensator
public export
record CompletedOp where
  constructor MkCompleted
  {0 resultType : Type}
  result : resultType
  comp : resultType -> IO (Outcome ())

||| Sequence compensables with automatic rollback on failure
public export
sequenceWithRollback : List (a ** Compensable a) -> IO (Outcome ())
sequenceWithRollback ops = go ops []
  where
    rollback : List CompletedOp -> IO ()
    rollback [] = pure ()
    rollback (MkCompleted res comp :: rest) = do
      _ <- comp res
      rollback rest

    go : List (a ** Compensable a) -> List CompletedOp -> IO (Outcome ())
    go [] _ = pure (Ok ())
    go ((_ ** c) :: rest) done = do
      result <- runCompensable c
      case result of
        Ok x => go rest (MkCompleted x c.inverse :: done)
        Fail cf ef => do
          -- Rollback all completed operations
          rollback done
          pure (Fail cf ef)

-- =============================================================================
-- Terminal Operations: No Undo
-- =============================================================================

||| Marker for terminal operations (cannot be undone)
public export
data TerminalOp : Type -> Type where
  MkTerminal : String -> a -> TerminalOp a

||| Get operation from terminal wrapper
public export
getTerminalOp : TerminalOp a -> a
getTerminalOp (MkTerminal _ x) = x

||| Get reason for terminality
public export
terminalReason : TerminalOp a -> String
terminalReason (MkTerminal r _) = r

-- =============================================================================
-- Operation Classification
-- =============================================================================

||| Classification of operations for recovery purposes
public export
data OpClass : Type where
  ||| Safe to retry any number of times
  Idem : OpClass
  ||| Has inverse operation
  Comp : OpClass
  ||| Cannot be undone
  Term : OpClass

||| Classify an operation
public export
interface Classified (op : Type) where
  classify : op -> OpClass

-- =============================================================================
-- Bounded Loops (Termination)
-- =============================================================================

||| A loop that is guaranteed to terminate
public export
record BoundedLoop (a : Type) where
  constructor MkBoundedLoop
  ||| Maximum iterations
  maxIter : Nat
  ||| Loop body (returns Nothing to continue, Just to stop)
  body : Nat -> IO (Maybe a)

||| Run bounded loop
public export
runBounded : BoundedLoop a -> IO (Maybe a)
runBounded loop = go loop.maxIter
  where
    go : Nat -> IO (Maybe a)
    go Z = pure Nothing  -- Max iterations reached
    go (S n) = do
      result <- loop.body (loop.maxIter `minus` n)
      case result of
        Just x => pure (Just x)
        Nothing => go n

||| Create bounded loop from predicate
public export
boundedWhile : Nat -> (Nat -> IO Bool) -> IO () -> BoundedLoop ()
boundedWhile maxIter pred body = MkBoundedLoop maxIter (\i => do
  continue <- pred i
  if continue
    then body >> pure Nothing
    else pure (Just ()))

-- =============================================================================
-- Saga Pattern (Distributed Compensation)
-- =============================================================================

||| A saga step with forward and compensating action
public export
record SagaStep (a : Type) where
  constructor MkStep
  stepName : String
  action : IO (Outcome a)
  compensation : a -> IO (Outcome ())

||| A saga is a sequence of compensable steps
public export
Saga : Type -> Type
Saga a = List (b ** SagaStep b)

||| Run saga with automatic rollback
public export
runSaga : Saga a -> IO (Outcome ())
runSaga steps = go steps []
  where
    rollback : List (b ** (b, SagaStep b)) -> IO ()
    rollback [] = pure ()
    rollback ((b ** (x, step)) :: rest) = do
      _ <- step.compensation x
      rollback rest
    
    go : Saga a -> List (b ** (b, SagaStep b)) -> IO (Outcome ())
    go [] _ = pure (Ok ())
    go ((b ** step) :: rest) done = do
      result <- step.action
      case result of
        Ok x => go rest ((b ** (x, step)) :: done)
        Fail c e => do
          rollback done
          pure (Fail c (addTag ("saga failed at: " ++ step.stepName) e))

-- =============================================================================
-- Integration with Outcome
-- =============================================================================

||| Mark outcome as from idempotent operation
public export
idempotentOutcome : Outcome a -> (Outcome a, Bool)
idempotentOutcome o = (o, True)

||| Check if operation can be retried based on conflict
public export
canRetry : Conflict -> Bool
canRetry GasExhausted = True
canRetry Revert = False  -- Generic revert - don't retry
canRetry DecodeError = False
canRetry _ = False

||| Retry outcome if allowed
public export
retryIf : Nat -> IO (Outcome a) -> IO (Outcome a)
retryIf Z op = op
retryIf (S n) op = do
  result <- op
  case result of
    Ok x => pure (Ok x)
    Fail c e => 
      if canRetry c
        then retryIf n op
        else pure (Fail c e)
