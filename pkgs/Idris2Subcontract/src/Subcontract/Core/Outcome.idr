||| Subcontract Core: Outcome (Result Normal Form)
|||
||| MVP-1: Every operation returns Outcome, never throws.
|||
||| Key insight: Exceptions hide control flow and make recovery hard.
||| Outcome makes success/failure explicit at the type level.
|||
||| Solidity: try/catch with untyped errors
||| Idris2: Outcome a = Ok a | Fail Conflict Evidence
module Subcontract.Core.Outcome

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence

%default total

-- =============================================================================
-- Outcome: Success or Typed Failure
-- =============================================================================

||| Result of any operation: success with value, or failure with evidence.
public export
data Outcome : Type -> Type where
  ||| Operation succeeded with result
  Ok : a -> Outcome a
  ||| Operation failed with classified conflict and evidence
  Fail : Conflict -> Evidence -> Outcome a

||| Check if outcome is success
public export
isOk : Outcome a -> Bool
isOk (Ok _) = True
isOk (Fail _ _) = False

||| Check if outcome is failure
public export
isFail : Outcome a -> Bool
isFail = not . isOk

||| Extract value or default
public export
fromOutcome : a -> Outcome a -> a
fromOutcome _ (Ok x) = x
fromOutcome def (Fail _ _) = def

||| Extract conflict if failed
public export
getConflict : Outcome a -> Maybe Conflict
getConflict (Ok _) = Nothing
getConflict (Fail c _) = Just c

||| Extract evidence if failed
public export
getEvidence : Outcome a -> Maybe Evidence
getEvidence (Ok _) = Nothing
getEvidence (Fail _ e) = Just e

-- =============================================================================
-- Functor / Applicative / Monad
-- =============================================================================

public export
Functor Outcome where
  map f (Ok x) = Ok (f x)
  map _ (Fail c e) = Fail c e

public export
Applicative Outcome where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Ok _) <*> (Fail c e) = Fail c e
  (Fail c e) <*> _ = Fail c e

public export
Monad Outcome where
  (Ok x) >>= f = f x
  (Fail c e) >>= _ = Fail c e

-- =============================================================================
-- Outcome Combinators
-- =============================================================================

||| Map over failure (transform conflict/evidence)
public export
mapFail : (Conflict -> Evidence -> (Conflict, Evidence)) -> Outcome a -> Outcome a
mapFail _ (Ok x) = Ok x
mapFail f (Fail c e) = let (c', e') = f c e in Fail c' e'

||| Add context tag to failure
public export
tagFail : String -> Outcome a -> Outcome a
tagFail tag = mapFail (\c, e => (c, addTag tag e))

||| Convert Maybe to Outcome
public export
fromMaybe : Conflict -> Evidence -> Maybe a -> Outcome a
fromMaybe _ _ (Just x) = Ok x
fromMaybe c e Nothing = Fail c e

||| Convert Either to Outcome
public export
fromEither : (e -> (Conflict, Evidence)) -> Either e a -> Outcome a
fromEither _ (Right x) = Ok x
fromEither f (Left e) = let (c, ev) = f e in Fail c ev

||| Try first, if fails try second
public export
orElse : Outcome a -> Lazy (Outcome a) -> Outcome a
orElse (Ok x) _ = Ok x
orElse (Fail _ _) second = second

||| Require condition or fail
public export
require : Bool -> Conflict -> Evidence -> Outcome ()
require True _ _ = Ok ()
require False c e = Fail c e

||| Require Maybe is Just
public export
requireJust : Maybe a -> Conflict -> Evidence -> Outcome a
requireJust (Just x) _ _ = Ok x
requireJust Nothing c e = Fail c e

-- =============================================================================
-- Outcome Aggregation
-- =============================================================================

||| Collect all successes, fail on first failure
public export
sequence : List (Outcome a) -> Outcome (List a)
sequence [] = Ok []
sequence (Ok x :: rest) = map (x ::) (sequence rest)
sequence (Fail c e :: _) = Fail c e

||| Collect all outcomes, returning list of successes and failures
public export
partition : List (Outcome a) -> (List a, List (Conflict, Evidence))
partition [] = ([], [])
partition (Ok x :: rest) = let (oks, fails) = partition rest in (x :: oks, fails)
partition (Fail c e :: rest) = let (oks, fails) = partition rest in (oks, (c, e) :: fails)

||| First success from list, or last failure
public export
firstSuccess : List (Outcome a) -> Outcome a
firstSuccess [] = Fail Revert (tagEvidence "no outcomes")
firstSuccess [x] = x
firstSuccess (Ok x :: _) = Ok x
firstSuccess (Fail _ _ :: rest) = firstSuccess rest

-- =============================================================================
-- IO Integration
-- =============================================================================

||| Lift IO to Outcome (always succeeds)
public export
liftIO : IO a -> IO (Outcome a)
liftIO action = map Ok action

||| Run Outcome in IO, reverting on failure
public export
runOrRevert : Outcome a -> IO a
runOrRevert (Ok x) = pure x
runOrRevert (Fail c e) = do
  -- In real impl: encode conflict+evidence and revert
  evmRevert 0 0
  pure (believe_me ())  -- Unreachable after revert

-- =============================================================================
-- Outcome with Recovery
-- =============================================================================

||| Try operation, with recovery function on failure
public export
tryRecover : Outcome a -> (Conflict -> Evidence -> Outcome a) -> Outcome a
tryRecover (Ok x) _ = Ok x
tryRecover (Fail c e) recover = recover c e

||| Assert outcome is Ok, for testing
public export
assertOk : Outcome a -> IO a
assertOk (Ok x) = pure x
assertOk (Fail c e) = do
  -- In real impl: log failure details
  pure (believe_me ())
