||| Subcontract Core: EvmM (Indexed Monad for EVM Execution)
|||
||| MVP-3: Track effects and phases at the type level.
|||
||| Key insight: EVM execution has implicit state and ordering.
||| - Phase: PreCheck -> EffectsDone -> ExternalDone -> Final
||| - Effects: Which operations are allowed
||| - CEI: Checks-Effects-Interactions pattern enforcement
|||
||| Solidity: All ordering is implicit, CEI is "hope and audit"
||| Idris2: Phase transitions are type-checked, CEI is enforced
module Subcontract.Core.EvmM

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence
import public Subcontract.Core.Outcome
import public Subcontract.Core.EntryCtx
import public Subcontract.Core.Storable

%default total

-- =============================================================================
-- Phase: Execution Ordering
-- =============================================================================

||| Execution phases for CEI pattern enforcement
public export
data Phase : Type where
  ||| Checks phase: validations, requires, reads
  PreCheck : Phase
  ||| Effects phase: storage writes, state changes
  EffectsDone : Phase
  ||| Interactions phase: external calls
  ExternalDone : Phase
  ||| Finalization: return/cleanup
  Final : Phase

||| Phase equality
public export
Eq Phase where
  PreCheck == PreCheck = True
  EffectsDone == EffectsDone = True
  ExternalDone == ExternalDone = True
  Final == Final = True
  _ == _ = False

||| Phase ordering (for CEI enforcement)
public export
phaseOrder : Phase -> Nat
phaseOrder PreCheck = 0
phaseOrder EffectsDone = 1
phaseOrder ExternalDone = 2
phaseOrder Final = 3

||| Check if phase transition is valid (forward only)
public export
validTransition : Phase -> Phase -> Bool
validTransition from to = phaseOrder from <= phaseOrder to

-- =============================================================================
-- Effect Row: Allowed Operations
-- =============================================================================

||| Set of allowed effects
public export
record EffectRow where
  constructor MkEffRow
  ||| Storage read/write allowed
  storage : Bool
  ||| External calls allowed
  externalCall : Bool
  ||| Event emission allowed
  logging : Bool
  ||| Revert allowed
  revertOp : Bool
  ||| Create/Create2 allowed
  create : Bool
  ||| Selfdestruct allowed
  selfdestruct : Bool
  ||| ETH transfer allowed
  transfer : Bool

||| Full effect row (all operations allowed)
public export
fullEffects : EffectRow
fullEffects = MkEffRow True True True True True True True

||| No effects (pure computation)
public export
noEffects : EffectRow
noEffects = MkEffRow False False False False False False False

||| Storage only effects
public export
storageEffects : EffectRow
storageEffects = MkEffRow True False True False False False False

||| Read-only effects
public export
readOnlyEffects : EffectRow
readOnlyEffects = MkEffRow False False False False False False False

||| Merge two effect rows (intersection - both must allow)
public export
mergeEffects : EffectRow -> EffectRow -> EffectRow
mergeEffects e1 e2 = MkEffRow
  (e1.storage && e2.storage)
  (e1.externalCall && e2.externalCall)
  (e1.logging && e2.logging)
  (e1.revertOp && e2.revertOp)
  (e1.create && e2.create)
  (e1.selfdestruct && e2.selfdestruct)
  (e1.transfer && e2.transfer)

-- =============================================================================
-- Execution Environment
-- =============================================================================

||| EVM execution environment
public export
record EvmEnv where
  constructor MkEvmEnv
  ||| Entry context (how we got here)
  entryCtx : EntryCtx
  ||| Current phase
  phase : Phase
  ||| Allowed effects
  effRow : EffectRow
  ||| Message sender
  msgSender : Bits256
  ||| Message value
  msgValue : Bits256
  ||| Current contract address
  selfAddr : Bits256
  ||| Call depth
  depth : Nat

||| Create default environment for direct call
public export
defaultEnv : Bits256 -> Bits256 -> Bits256 -> EvmEnv
defaultEnv sender value self = MkEvmEnv
  { entryCtx = DirectCall
  , phase = PreCheck
  , effRow = fullEffects
  , msgSender = sender
  , msgValue = value
  , selfAddr = self
  , depth = 0
  }

-- =============================================================================
-- EvmM: Indexed Monad
-- =============================================================================

||| Indexed monad for EVM execution.
||| Tracks phase transitions at the type level.
||| pre: phase before operation
||| post: phase after operation
public export
data EvmM : (pre : Phase) -> (post : Phase) -> (a : Type) -> Type where
  MkEvmM : (EvmEnv -> IO (Outcome a, EvmEnv)) -> EvmM pre post a

||| Run EvmM with environment
public export
runEvmM : EvmEnv -> EvmM pre post a -> IO (Outcome a, EvmEnv)
runEvmM env (MkEvmM f) = f env

||| Run and extract just the outcome
public export
evalEvmM : EvmEnv -> EvmM pre post a -> IO (Outcome a)
evalEvmM env m = map fst (runEvmM env m)

-- =============================================================================
-- EvmM Combinators
-- =============================================================================

||| Pure value (no phase change)
public export
pure' : a -> EvmM p p a
pure' x = MkEvmM (\env => pure (Ok x, env))

||| Fail with conflict
public export
fail' : Conflict -> Evidence -> EvmM p p a
fail' c e = MkEvmM (\env => pure (Fail c e, env))

||| Sequence (phase must match)
public export
bind' : EvmM p q a -> (a -> EvmM q r b) -> EvmM p r b
bind' (MkEvmM f) g = MkEvmM (\env => do
  (result, env') <- f env
  case result of
    Ok x => let MkEvmM h = g x in h env'
    Fail c e => pure (Fail c e, env'))

||| Map over result
public export
map' : (a -> b) -> EvmM p q a -> EvmM p q b
map' f m = bind' m (\x => pure' (f x))

||| Get current environment
public export
getEnv : EvmM p p EvmEnv
getEnv = MkEvmM (\env => pure (Ok env, env))

||| Modify environment
public export
modifyEnv : (EvmEnv -> EvmEnv) -> EvmM p p ()
modifyEnv f = MkEvmM (\env => pure (Ok (), f env))

-- =============================================================================
-- Phase Transitions
-- =============================================================================

||| Transition to EffectsDone phase
public export
beginEffects : EvmM PreCheck EffectsDone ()
beginEffects = MkEvmM (\env => pure (Ok (), { phase := EffectsDone } env))

||| Transition to ExternalDone phase
public export
beginExternal : EvmM EffectsDone ExternalDone ()
beginExternal = MkEvmM (\env => pure (Ok (), { phase := ExternalDone } env))

||| Transition to Final phase
public export
finalize : EvmM ExternalDone Final ()
finalize = MkEvmM (\env => pure (Ok (), { phase := Final } env))

-- =============================================================================
-- Effect-Checked Operations
-- =============================================================================

||| Require (check phase)
public export
require'' : Bool -> String -> EvmM PreCheck PreCheck ()
require'' True _ = pure' ()
require'' False msg = fail' Revert (tagEvidence msg)

||| Storage read (allowed in any phase)
public export
sload' : Bits256 -> EvmM p p Bits256
sload' slot = MkEvmM (\env =>
  if env.effRow.storage
    then do
      val <- sload slot
      pure (Ok val, env)
    else pure (Fail UnsafeEntryContext (tagsEvidence ["sload", "storage forbidden"]), env))

||| Storage write (must be in PreCheck or EffectsDone)
public export
sstore' : Bits256 -> Bits256 -> EvmM PreCheck EffectsDone ()
sstore' slot val = MkEvmM (\env =>
  if env.effRow.storage
    then do
      sstore slot val
      pure (Ok (), { phase := EffectsDone } env)
    else pure (Fail UnsafeEntryContext (tagsEvidence ["sstore", "storage forbidden"]), env))

||| External call (must be in EffectsDone, transitions to ExternalDone)
public export
call' : Bits256 -> Bits256 -> Bits256 -> EvmM EffectsDone ExternalDone Bool
call' target value gas = MkEvmM (\env =>
  if env.effRow.externalCall
    then do
      success <- call gas target value 0 0 0 0
      pure (Ok (success /= 0), { phase := ExternalDone } env)
    else pure (Fail ExternalCallForbidden (tagsEvidence ["call", "external forbidden"]), env))

||| Log event (allowed in most phases)
public export
log0' : Bits256 -> Bits256 -> EvmM p p ()
log0' offset size = MkEvmM (\env =>
  if env.effRow.logging
    then do
      log0 offset size
      pure (Ok (), env)
    else pure (Fail UnsafeEntryContext (tagsEvidence ["log", "logging forbidden"]), env))

-- =============================================================================
-- CEI Pattern Enforcement
-- =============================================================================

||| A complete CEI-safe transaction
||| Type signature enforces: PreCheck -> EffectsDone -> ExternalDone -> Final
public export
CEITransaction : Type -> Type
CEITransaction a = EvmM PreCheck Final a

||| Build a CEI transaction
public export
ceiTransaction : EvmM PreCheck EffectsDone ()      -- checks + effects
              -> EvmM EffectsDone ExternalDone ()  -- interactions
              -> EvmM ExternalDone Final a          -- finalization
              -> CEITransaction a
ceiTransaction checks interactions finalization = 
  bind' checks (\_ => bind' interactions (\_ => finalization))

-- =============================================================================
-- Entry Context Integration
-- =============================================================================

||| Set effect row based on entry context
public export
applyContextPolicy : EntryCtx -> EvmM p p ()
applyContextPolicy ctx = modifyEnv (\env =>
  let policy = policyOf ctx in
  { effRow := MkEffRow
      policy.allowStorage
      policy.allowExternal
      policy.allowLog
      True  -- revert always allowed
      policy.allowCreate
      policy.allowSelfdestruct
      policy.allowTransfer
  } env)

||| Guard operation by entry context
public export
guardByContext : EvmM p q a -> EvmM p q a
guardByContext (MkEvmM f) = MkEvmM (\env =>
  case checkStorage env.entryCtx of
    Fail c e => pure (Fail c e, env)
    Ok () => f env)

-- =============================================================================
-- Convenience Aliases
-- =============================================================================

||| Type alias for pre-check phase operations
public export
Check : Type -> Type
Check = EvmM PreCheck PreCheck

||| Type alias for effect phase operations
public export
Effect : Type -> Type
Effect = EvmM PreCheck EffectsDone

||| Type alias for interaction phase operations
public export
Interact : Type -> Type
Interact = EvmM EffectsDone ExternalDone
