||| Failure-Recovery Monad Core
|||
||| Based on the FR Monad paper: "Recovery-Preserving Kleisli Semantics for World-Computer Virtual Machines"
|||
||| Section 2: R := (V × E) ∪ (F × E) - the result type
||| Section 3: FR morphisms f : S → (S × R)
||| Section 4: Recovery-Aware Kleisli Composition
||| Section 5: The Failure-Recovery Monad
|||
||| Key principle: composability = existence of recovery-preserving morphisms
||| Monad laws are interpreted as semantic invariants ensuring that
||| refactoring and modularization preserve recovery obligations.
module FRMonad.Core

import public FRMonad.Failure
import public FRMonad.Evidence

%default total

-- =============================================================================
-- FR: The Failure-Recovery Result Type
-- R := (V × E) ∪ (F × E) from Section 2
-- =============================================================================

||| Failure-Recovery result type
||| All computations return either success with evidence or failure with evidence
||| This is R := (V × E) ∪ (F × E) from the paper
public export
data FR : Type -> Type where
  ||| Success case: (V × E)
  Ok   : (value : a) -> (evidence : Evidence) -> FR a
  ||| Failure case: (F × E)
  Fail : (failure : Fail) -> (evidence : Evidence) -> FR a

public export
Show a => Show (FR a) where
  show (Ok v e)   = "Ok(" ++ show v ++ ") " ++ show e
  show (Fail f e) = "Fail(" ++ show f ++ ") " ++ show e

-- =============================================================================
-- Basic accessors
-- =============================================================================

||| Check if result is success
public export
isOk : FR a -> Bool
isOk (Ok _ _)   = True
isOk (Fail _ _) = False

||| Check if result is failure
public export
isFail : FR a -> Bool
isFail = not . isOk

||| Extract value or default
public export
fromOk : a -> FR a -> a
fromOk _ (Ok v _)   = v
fromOk d (Fail _ _) = d

||| Extract evidence
public export
getEvidence : FR a -> Evidence
getEvidence (Ok _ e)   = e
getEvidence (Fail _ e) = e

||| Extract failure if present
public export
getFailure : FR a -> Maybe Fail
getFailure (Ok _ _)   = Nothing
getFailure (Fail f _) = Just f

-- =============================================================================
-- Functor, Applicative, Monad instances
-- Section 5: Monad Laws as Semantic Invariants
-- - Unit laws: pure computations introduce no recovery obligations
-- - Associativity: refactoring does not change recovery meaning
-- =============================================================================

public export
Functor FR where
  map f (Ok v e)   = Ok (f v) e
  map f (Fail x e) = Fail x e

public export
Applicative FR where
  pure v = Ok v emptyEvidence
  (Ok f e1) <*> (Ok v e2)   = Ok (f v) (combineEvidence e1 e2)
  (Ok _ e1) <*> (Fail x e2) = Fail x (combineEvidence e1 e2)
  (Fail x e) <*> _          = Fail x e

||| Monad instance implementing Recovery-Aware Kleisli Composition (Section 4)
||| 1. Evidence accumulates via ⊕
||| 2. Failures propagate unless handled
||| 3. No failure is erased without recovery
public export
Monad FR where
  (Ok v e1) >>= f = case f v of
    Ok v' e2   => Ok v' (combineEvidence e1 e2)
    Fail x e2  => Fail x (combineEvidence e1 e2)
  (Fail x e) >>= _ = Fail x e

-- =============================================================================
-- Smart Constructors
-- =============================================================================

||| Create success result with evidence
public export
ok : Phase -> String -> String -> a -> FR a
ok phase label detail value = Ok value (mkEvidence phase label detail)

||| Create failure result with evidence
public export
fail : Phase -> String -> String -> Fail -> FR a
fail phase label detail failure = Fail failure (mkEvidence phase label detail)

||| Create conflict failure (common in optimistic upgrader)
public export
conflict : Phase -> String -> String -> FR a
conflict phase label detail = fail phase label detail (Conflict detail)

||| Create unauthorized failure
public export
unauthorized : Phase -> String -> String -> FR a
unauthorized phase label detail = fail phase label detail (Unauthorized detail)

||| Create not found failure
public export
notFound : Phase -> String -> String -> FR a
notFound phase label detail = fail phase label detail (NotFound detail)

||| Create validation error
public export
validationError : Phase -> String -> String -> FR a
validationError phase label detail = fail phase label detail (ValidationError detail)

||| Create internal error
public export
internal : Phase -> String -> String -> FR a
internal phase label detail = fail phase label detail (Internal detail)

-- =============================================================================
-- Handler Types (Section 3.1: Recovery Interfaces)
-- =============================================================================

||| Handler type: transforms failures into recovery actions
||| Resolve_b : (F × E) → P(A_b) from Section 3.1
public export
Handler : Type -> Type -> Type
Handler a b = (Fail, Evidence) -> FR b

||| Recovery type: attempts to produce a value from failure info
public export
Recovery : Type -> Type
Recovery a = (Fail, Evidence) -> Maybe a

-- =============================================================================
-- Recovery Combinators (Section 6: Composability)
-- A system is composable iff required recovery-preserving morphisms exist
-- =============================================================================

||| Apply handler to failure, pass through success
public export
handleWith : Handler a a -> FR a -> FR a
handleWith _ (Ok v e)      = Ok v e
handleWith h (Fail f e)    = h (f, e)

||| Try alternative on failure
public export
orElse : FR a -> Lazy (FR a) -> FR a
orElse (Ok v e)   _   = Ok v e
orElse (Fail _ _) alt = alt

||| Infix version of orElse
public export
(<|>) : FR a -> Lazy (FR a) -> FR a
(<|>) = orElse

||| Try to recover from failure
public export
tryRecover : Recovery a -> FR a -> FR a
tryRecover recover (Fail f e) = case recover (f, e) of
  Just v  => Ok v (addTag "recovered" e)
  Nothing => Fail f e
tryRecover _ ok = ok

||| Catch specific failure type and attempt recovery
public export
catchFail : (Fail -> Bool) -> Handler a a -> FR a -> FR a
catchFail pred handler (Fail f e) = if pred f then handler (f, e) else Fail f e
catchFail _ _ ok = ok

||| Catch failures by category
public export
catchCategory : FailCategory -> Handler a a -> FR a -> FR a
catchCategory cat = catchFail (\f => category f == cat)

||| Catch failures by severity
public export
catchSeverity : Severity -> Handler a a -> FR a -> FR a
catchSeverity sev = catchFail (\f => severity f == sev)

||| Catch retryable failures
public export
catchRetryable : Handler a a -> FR a -> FR a
catchRetryable = catchFail isRetryable

-- =============================================================================
-- Guard Combinators
-- =============================================================================

||| Require condition to be true, or fail with given failure
public export
require : Bool -> Fail -> Evidence -> FR ()
require True  _ e = Ok () e
require False f e = Fail f e

||| Require Maybe to be Just, or fail
public export
requireJust : Maybe a -> Fail -> Evidence -> FR a
requireJust (Just v) _ e = Ok v e
requireJust Nothing  f e = Fail f e

||| Require Either to be Right, or fail with Left message
public export
requireRight : Either String a -> (String -> Fail) -> Evidence -> FR a
requireRight (Right v) _ e   = Ok v e
requireRight (Left msg) f e  = Fail (f msg) e

||| Guard that fails if condition is false
public export
guard : Phase -> String -> Bool -> Fail -> FR ()
guard phase label cond failure =
  if cond
    then Ok () (mkEvidence phase label "guard passed")
    else Fail failure (mkEvidence phase label "guard failed")

||| Assert ownership/caller
public export
assertCaller : Phase -> String -> String -> String -> FR ()
assertCaller phase label expected actual =
  if expected == actual
    then Ok () (mkEvidence phase label "caller verified")
    else Fail (PrincipalMismatch $ "expected " ++ expected ++ ", got " ++ actual)
              (mkEvidence phase label "caller mismatch")

-- =============================================================================
-- Sequence Combinators
-- =============================================================================

||| Sequence a list of FR computations, stopping at first failure
public export
sequence : List (FR a) -> FR (List a)
sequence [] = Ok [] emptyEvidence
sequence (x :: xs) = do
  v  <- x
  vs <- FRMonad.Core.sequence xs
  pure (v :: vs)

||| Traverse with FR effect
public export
traverse : (a -> FR b) -> List a -> FR (List b)
traverse f xs = FRMonad.Core.sequence (map f xs)

||| For each element, apply function, collect results
public export
forM : List a -> (a -> FR b) -> FR (List b)
forM = flip FRMonad.Core.traverse

||| Partition results into successes and failures
public export
partition : List (FR a) -> (List (a, Evidence), List (Fail, Evidence))
partition [] = ([], [])
partition (x :: xs) =
  let (oks, fails) = partition xs
  in case x of
    Ok v e   => ((v, e) :: oks, fails)
    Fail f e => (oks, (f, e) :: fails)

||| Try first success from a list
public export
firstSuccess : List (FR a) -> FR a
firstSuccess [] = Fail (NotFound "no alternatives") emptyEvidence
firstSuccess [x] = x
firstSuccess (x :: xs) = x <|> firstSuccess xs

||| Try all and return first success, or last failure
public export
tryAll : List (Lazy (FR a)) -> FR a
tryAll [] = Fail (NotFound "no alternatives") emptyEvidence
tryAll [x] = x
tryAll (x :: xs) = x <|> tryAll xs

-- =============================================================================
-- Boundary Functions (Section 3.1: Boundaries and Recovery Interfaces)
-- A system is recovery-closed at boundary b if failures are either
-- resolved internally or explicitly exported.
-- =============================================================================

||| Boundary function: enforce recovery closure at phase boundary
||| Failures that escape the boundary are converted to trap/reject
public export
boundary : Phase -> FR a -> Either (Fail, Evidence) (a, Evidence)
boundary _ (Ok v e)   = Right (v, e)
boundary _ (Fail f e) = Left (f, e)

||| Run FR and get result or error message
public export
runFR : FR a -> Either String a
runFR (Ok v _)   = Right v
runFR (Fail f _) = Left (show f)

||| Run FR and get full result with evidence
public export
runFRWithEvidence : FR a -> Either (Fail, Evidence) (a, Evidence)
runFRWithEvidence (Ok v e)   = Right (v, e)
runFRWithEvidence (Fail f e) = Left (f, e)

-- =============================================================================
-- Evidence manipulation
-- =============================================================================

||| Map FR result, preserving evidence on success
public export
mapOk : (a -> b) -> FR a -> FR b
mapOk f (Ok v e)   = Ok (f v) e
mapOk _ (Fail x e) = Fail x e

||| Add tag to evidence in FR
public export
tag : String -> FR a -> FR a
tag t (Ok v e)   = Ok v (addTag t e)
tag t (Fail f e) = Fail f (addTag t e)

||| Update evidence in FR
public export
withEvidence : (Evidence -> Evidence) -> FR a -> FR a
withEvidence f (Ok v e)   = Ok v (f e)
withEvidence f (Fail x e) = Fail x (f e)

||| Set phase in evidence
public export
inPhase : Phase -> FR a -> FR a
inPhase p = withEvidence (\e => { phase := p } e)

-- =============================================================================
-- Conversion helpers
-- =============================================================================

||| Convert Maybe to FR with custom failure
public export
fromMaybe : Fail -> Evidence -> Maybe a -> FR a
fromMaybe _ e (Just v) = Ok v e
fromMaybe f e Nothing  = Fail f e

||| Convert Either to FR
public export
fromEither : (String -> Fail) -> Evidence -> Either String a -> FR a
fromEither _ e (Right v)  = Ok v e
fromEither f e (Left msg) = Fail (f msg) e

||| Convert FR to Maybe (losing evidence and error info)
public export
toMaybe : FR a -> Maybe a
toMaybe (Ok v _)   = Just v
toMaybe (Fail _ _) = Nothing

||| Convert FR to Either String
public export
toEither : FR a -> Either String a
toEither (Ok v _)   = Right v
toEither (Fail f _) = Left (show f)

-- =============================================================================
-- ICP-specific entry point helpers
-- =============================================================================

||| Wrap a query operation with proper evidence
public export
queryOp : String -> FR a -> FR a
queryOp label = inPhase Query . tag ("query:" ++ label)

||| Wrap an update operation with proper evidence
public export
updateOp : String -> FR a -> FR a
updateOp label = inPhase Update . tag ("update:" ++ label)

||| Wrap an init operation with proper evidence
public export
initOp : String -> FR a -> FR a
initOp label = inPhase Init . tag ("init:" ++ label)

-- =============================================================================
-- Output helpers
-- =============================================================================

||| Run FR computation and convert to text response (for canister interface)
public export
runFRToText : Show a => FR a -> String
runFRToText (Ok v e)   = "OK: " ++ show v
runFRToText (Fail f e) = "ERROR: " ++ show f ++ "\n" ++ renderEvidence e

||| Run FR computation returning JSON-like response
public export
runFRToJson : Show a => FR a -> String
runFRToJson (Ok v e) =
  "{\"status\":\"ok\",\"value\":" ++ show v ++
  ",\"evidence\":{\"phase\":\"" ++ show e.phase ++ "\",\"label\":\"" ++ e.label ++ "\"}}"
runFRToJson (Fail f e) =
  "{\"status\":\"error\",\"error\":\"" ++ show f ++
  "\",\"severity\":\"" ++ show (severity f) ++
  "\",\"category\":\"" ++ show (category f) ++ "\"}"
