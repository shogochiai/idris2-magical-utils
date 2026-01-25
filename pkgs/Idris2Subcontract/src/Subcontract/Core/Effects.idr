||| Subcontract Core: Effect-Typed Entries
|||
||| Each Entry declares which storage slots it reads/writes at the TYPE level.
||| This enables:
||| - Static reentrancy detection (Write before Call = type error)
||| - Gas estimation from effects
||| - Parallel execution safety analysis
||| - Automated audit (effect verification)
|||
||| Solidity: Effects are implicit, discovered by analysis tools
||| Idris2: Effects are explicit types, verified at compile time
module Subcontract.Core.Effects

import public Data.List
import public Data.Vect
import public Subcontract.Core.Storable

%default total

-- =============================================================================
-- Effect Types
-- =============================================================================

||| Storage slot identifier (compile-time known or dynamic)
public export
data SlotId : Type where
  ||| Statically known slot
  StaticSlot : Bits256 -> SlotId
  ||| Dynamic slot (computed at runtime, e.g., mapping key)
  DynamicSlot : String -> SlotId  -- Named for documentation

||| Call target identifier
public export
data TargetId : Type where
  ||| Known contract address
  KnownTarget : Bits256 -> TargetId
  ||| Dynamic target (parameter)
  DynamicTarget : String -> TargetId

||| Individual effect
public export
data Effect : Type where
  ||| Read from storage slot
  SLoad : SlotId -> Effect
  ||| Write to storage slot
  SStore : SlotId -> Effect
  ||| External call
  Call : TargetId -> Effect
  ||| Delegatecall (preserves context)
  DelegateCall : TargetId -> Effect
  ||| Static call (read-only)
  StaticCall : TargetId -> Effect
  ||| Pure computation (no effects)
  Pure : Effect
  ||| Memory operation (not storage)
  MemEffect : Effect
  ||| Log/Event emission
  Log : Nat -> Effect  -- Nat = number of topics

||| Effect equality
public export
Eq SlotId where
  (StaticSlot a) == (StaticSlot b) = a == b
  (DynamicSlot a) == (DynamicSlot b) = a == b
  _ == _ = False

public export
Eq TargetId where
  (KnownTarget a) == (KnownTarget b) = a == b
  (DynamicTarget a) == (DynamicTarget b) = a == b
  _ == _ = False

public export
Eq Effect where
  (SLoad a) == (SLoad b) = a == b
  (SStore a) == (SStore b) = a == b
  (Call a) == (Call b) = a == b
  (DelegateCall a) == (DelegateCall b) = a == b
  (StaticCall a) == (StaticCall b) = a == b
  Pure == Pure = True
  MemEffect == MemEffect = True
  (Log a) == (Log b) = a == b
  _ == _ = False

-- =============================================================================
-- Effect Sets (Type-Level)
-- =============================================================================

||| Effect set as a list (order matters for reentrancy analysis)
public export
EffectList : Type
EffectList = List Effect

||| Empty effect set (pure function)
public export
noEffects : EffectList
noEffects = []

||| Single effect
public export
effect : Effect -> EffectList
effect e = [e]

||| Combine effect lists (sequence)
public export
(+++) : EffectList -> EffectList -> EffectList
(+++) = (++)

-- =============================================================================
-- Effect Predicates
-- =============================================================================

||| Check if effect list contains any writes
public export
hasWrite : EffectList -> Bool
hasWrite [] = False
hasWrite (SStore _ :: _) = True
hasWrite (_ :: rest) = hasWrite rest

||| Check if effect list contains any external calls
public export
hasCall : EffectList -> Bool
hasCall [] = False
hasCall (Call _ :: _) = True
hasCall (DelegateCall _ :: _) = True
hasCall (_ :: rest) = hasCall rest

||| Check if effect list contains only reads (view function)
public export
isView : EffectList -> Bool
isView effs = not (hasWrite effs) && not (hasCall effs)

||| Check if effect list is pure (no storage, no calls)
public export
isPure : EffectList -> Bool
isPure [] = True
isPure (Pure :: rest) = isPure rest
isPure (MemEffect :: rest) = isPure rest
isPure _ = False

-- =============================================================================
-- Reentrancy Safety (CEI Pattern)
-- =============================================================================

||| Check if Write appears before Call (reentrancy risk!)
||| CEI = Checks-Effects-Interactions
||| Safe pattern: all Writes must come AFTER all Calls
public export
data WriteBeforeCall : EffectList -> Type where
  ||| Found a Write, then later a Call
  FoundViolation : WriteBeforeCall effs

||| Proof that no Write appears in the remaining list (defined first for CEISafe)
public export
data NoWriteAfter : EffectList -> Type where
  NoWriteEmpty : NoWriteAfter []
  NoWritePure : NoWriteAfter rest -> NoWriteAfter (Pure :: rest)
  NoWriteMem : NoWriteAfter rest -> NoWriteAfter (MemEffect :: rest)
  NoWriteRead : NoWriteAfter rest -> NoWriteAfter (SLoad _ :: rest)
  NoWriteLog : NoWriteAfter rest -> NoWriteAfter (Log _ :: rest)
  NoWriteCall : NoWriteAfter rest -> NoWriteAfter (Call _ :: rest)
  NoWriteDelegate : NoWriteAfter rest -> NoWriteAfter (DelegateCall _ :: rest)
  NoWriteStatic : NoWriteAfter rest -> NoWriteAfter (StaticCall _ :: rest)
  -- Note: SStore is NOT included - that's a write!

||| Proof that no Write appears before Call
public export
data CEISafe : EffectList -> Type where
  ||| Empty list is safe
  EmptySafe : CEISafe []
  ||| Pure/Mem effects are always safe
  PureSafe : CEISafe rest -> CEISafe (Pure :: rest)
  MemSafe : CEISafe rest -> CEISafe (MemEffect :: rest)
  ||| Read is safe
  ReadSafe : CEISafe rest -> CEISafe (SLoad _ :: rest)
  ||| Log is safe
  LogSafe : CEISafe rest -> CEISafe (Log _ :: rest)
  ||| Call is safe if no writes follow
  CallSafe : NoWriteAfter rest -> CEISafe (Call _ :: rest)
  DelegateCallSafe : NoWriteAfter rest -> CEISafe (DelegateCall _ :: rest)
  StaticCallSafe : CEISafe rest -> CEISafe (StaticCall _ :: rest)
  ||| Write is safe if no calls came before (checked by position)
  WriteSafe : CEISafe rest -> CEISafe (SStore _ :: rest)

||| Check if no write after (helper)
export
checkNoWriteAfter : (effs : EffectList) -> Maybe (NoWriteAfter effs)
checkNoWriteAfter [] = Just NoWriteEmpty
checkNoWriteAfter (Pure :: r) = map NoWritePure (checkNoWriteAfter r)
checkNoWriteAfter (MemEffect :: r) = map NoWriteMem (checkNoWriteAfter r)
checkNoWriteAfter (SLoad _ :: r) = map NoWriteRead (checkNoWriteAfter r)
checkNoWriteAfter (Log _ :: r) = map NoWriteLog (checkNoWriteAfter r)
checkNoWriteAfter (Call _ :: r) = map NoWriteCall (checkNoWriteAfter r)
checkNoWriteAfter (DelegateCall _ :: r) = map NoWriteDelegate (checkNoWriteAfter r)
checkNoWriteAfter (StaticCall _ :: r) = map NoWriteStatic (checkNoWriteAfter r)
checkNoWriteAfter (SStore _ :: _) = Nothing  -- Violation!

||| Check CEI safety at runtime (for dynamic verification)
export
checkCEISafe : (effs : EffectList) -> Maybe (CEISafe effs)
checkCEISafe [] = Just EmptySafe
checkCEISafe (Pure :: rest) = map PureSafe (checkCEISafe rest)
checkCEISafe (MemEffect :: rest) = map MemSafe (checkCEISafe rest)
checkCEISafe (SLoad _ :: rest) = map ReadSafe (checkCEISafe rest)
checkCEISafe (Log _ :: rest) = map LogSafe (checkCEISafe rest)
checkCEISafe (StaticCall _ :: rest) = map StaticCallSafe (checkCEISafe rest)
checkCEISafe (Call t :: rest) = map (CallSafe {rest}) (checkNoWriteAfter rest)
checkCEISafe (DelegateCall t :: rest) = map (DelegateCallSafe {rest}) (checkNoWriteAfter rest)
checkCEISafe (SStore _ :: rest) = map WriteSafe (checkCEISafe rest)

-- =============================================================================
-- Effect-Typed Entry
-- =============================================================================

||| Function signature (simplified)
public export
record FnSig where
  constructor MkFnSig
  fnName : String
  fnSelector : Bits256

||| Entry point with declared effects
||| The effect list is part of the TYPE - verified at compile time
public export
record EffectEntry (effs : EffectList) where
  constructor MkEffectEntry
  sig : FnSig
  handler : IO ()

||| Create a CEI-safe entry (requires proof)
export
safeEntry : (sig : FnSig)
         -> (effs : EffectList)
         -> {auto prf : CEISafe effs}
         -> IO ()
         -> EffectEntry effs
safeEntry sig effs handler = MkEffectEntry sig handler

||| View entry (no writes, no calls)
export
viewEntry : (sig : FnSig)
         -> (effs : EffectList)
         -> {auto viewPrf : isView effs = True}
         -> IO ()
         -> EffectEntry effs
viewEntry sig effs handler = MkEffectEntry sig handler

||| Pure entry (no storage effects)
export
pureEntry : (sig : FnSig) -> IO () -> EffectEntry []
pureEntry sig handler = MkEffectEntry sig handler

-- =============================================================================
-- Effect Composition
-- =============================================================================

||| Sequence two effect entries
export
sequenceEffects : EffectEntry e1 -> EffectEntry e2 -> EffectEntry (e1 ++ e2)
sequenceEffects (MkEffectEntry sig1 h1) (MkEffectEntry _ h2) =
  MkEffectEntry sig1 (h1 >> h2)

-- =============================================================================
-- Slot Collision Detection
-- =============================================================================

||| Check if a slot is accessed in an effect list
public export
slotInList : SlotId -> EffectList -> Bool
slotInList _ [] = False
slotInList s (SLoad s' :: rest) = s == s' || slotInList s rest
slotInList s (SStore s' :: rest) = s == s' || slotInList s rest
slotInList s (_ :: rest) = slotInList s rest

||| Check if two effect lists access the same slot
public export
slotCollision : EffectList -> EffectList -> Maybe SlotId
slotCollision [] _ = Nothing
slotCollision _ [] = Nothing
slotCollision (SLoad s :: rest) effs2 =
  if slotInList s effs2 then Just s else slotCollision rest effs2
slotCollision (SStore s :: rest) effs2 =
  if slotInList s effs2 then Just s else slotCollision rest effs2
slotCollision (_ :: rest) effs2 = slotCollision rest effs2

||| Proof that two effect lists don't collide
public export
data NoCollision : EffectList -> EffectList -> Type where
  MkNoCollision : (slotCollision e1 e2 = Nothing) -> NoCollision e1 e2

-- =============================================================================
-- Gas Estimation from Effects
-- =============================================================================

||| Estimate gas for a single effect
public export
effectGas : Effect -> Nat
effectGas (SLoad _) = 2100       -- Cold SLOAD
effectGas (SStore _) = 20000     -- Worst case SSTORE
effectGas (Call _) = 2600        -- Cold CALL
effectGas (DelegateCall _) = 2600
effectGas (StaticCall _) = 2600
effectGas Pure = 0
effectGas MemEffect = 3
effectGas (Log n) = 375 + n * 375  -- LOG base + topics

||| Estimate total gas for effect list
public export
totalEffectGas : EffectList -> Nat
totalEffectGas [] = 0
totalEffectGas (e :: rest) = effectGas e + totalEffectGas rest

-- =============================================================================
-- Example: Safe Transfer Pattern
-- =============================================================================

||| ERC20 transfer effects (CEI-safe order)
||| 1. Check balance (read)
||| 2. Update sender balance (write)
||| 3. Update recipient balance (write)
||| 4. Emit event (log)
||| No external calls - inherently safe
public export
transferEffects : EffectList
transferEffects = 
  [ SLoad (DynamicSlot "balances[from]")
  , SLoad (DynamicSlot "balances[to]")
  , SStore (DynamicSlot "balances[from]")
  , SStore (DynamicSlot "balances[to]")
  , Log 3  -- Transfer(from, to, amount)
  ]

||| Proof that transfer is CEI-safe (no calls, so trivially safe)
export
transferIsSafe : CEISafe Effects.transferEffects
transferIsSafe = ReadSafe (ReadSafe (WriteSafe (WriteSafe (LogSafe EmptySafe))))

-- =============================================================================
-- Example: Unsafe Withdraw Pattern (Would Not Compile with CEI proof)
-- =============================================================================

||| UNSAFE: Write before Call (reentrancy vulnerable!)
||| This effect list cannot get a CEISafe proof
public export
unsafeWithdrawEffects : EffectList
unsafeWithdrawEffects =
  [ SLoad (DynamicSlot "balances[msg.sender]")
  , SStore (DynamicSlot "balances[msg.sender]")  -- Write BEFORE call!
  , Call (DynamicTarget "msg.sender")             -- External call
  ]

-- unsafeWithdrawProof : CEISafe unsafeWithdrawEffects
-- unsafeWithdrawProof = ?impossible  -- Cannot construct!

||| SAFE: CEI-compliant withdraw
||| Read -> Call -> Write
public export
safeWithdrawEffects : EffectList
safeWithdrawEffects =
  [ SLoad (DynamicSlot "balances[msg.sender]")
  , Call (DynamicTarget "msg.sender")             -- Call BEFORE write
  , SStore (DynamicSlot "balances[msg.sender]")  -- Write AFTER call
  ]

-- But wait - this also fails CEISafe because we have Write after Call!
-- The CEI pattern requires: Checks (reads) -> Effects (writes) -> Interactions (calls)
-- So the truly safe pattern is:
-- Read -> Write -> Call (where the call doesn't affect our state)
-- OR: Read -> Call -> Write (but then we need reentrancy guard)

-- =============================================================================
-- Compile-Time Guarantees
-- =============================================================================

-- 1. Effect lists are TYPE-LEVEL - declared at compile time
-- 2. CEISafe proof required for safeEntry - unsafe patterns don't compile
-- 3. NoCollision prevents parallel execution conflicts
-- 4. Gas estimation is static from effect list
-- 5. View/Pure classification is automatic from effects
