||| Subcontract Core: Failure-Recovery Bridge
|||
||| Bridges FR theory types (Conflict, Evidence, Outcome) to EVM semantics.
|||
||| Key insight from FR theory:
||| - EVM logs are LOST on revert, so evidence must go in revert data
||| - Success evidence can go in return data or logs
||| - This module provides the EVM-specific serialization layer
|||
||| FR Theory Mapping:
||| - F (failure surface) = Conflict
||| - E (evidence) = Evidence
||| - R (result) = Outcome
||| - b (boundary) = EntryCtx
||| - H_b (handler) = Resolve functions
module Subcontract.Core.FR

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence
import public Subcontract.Core.Outcome
import public Subcontract.Core.EntryCtx
import public Subcontract.Core.Storable
import public Data.Vect

%default total

-- =============================================================================
-- FR Error Selector (Custom Error ABI)
-- =============================================================================

||| Custom error selector for FR failures
||| error FRFailure(uint8 conflictCode, bytes32 digest, bytes32[] tags)
||| Selector = keccak256("FRFailure(uint8,bytes32,bytes32[])")[:4]
public export
frFailureSelector : Bits256
frFailureSelector = 0x46524641  -- "FRFA" as placeholder, compute real selector

-- =============================================================================
-- Conflict -> uint8 Encoding
-- =============================================================================

||| Encode Conflict as uint8 for ABI
public export
encodeConflict : Conflict -> Bits256
encodeConflict Revert = 0
encodeConflict Reentrancy = 1
encodeConflict AuthViolation = 2
encodeConflict CEIViolation = 3
encodeConflict EpochMismatch = 4
encodeConflict InitAlready = 5
encodeConflict NotInitialized = 6
encodeConflict StorageAlias = 7
encodeConflict ExternalCallForbidden = 8
encodeConflict UnsafeEntryContext = 9
encodeConflict GasExhausted = 10
encodeConflict DecodeError = 11
encodeConflict ArithmeticError = 12
encodeConflict UpgradeNotAllowed = 13
encodeConflict RollbackNotAllowed = 14
encodeConflict AssetAlreadySpent = 15
encodeConflict AssetNotOwned = 16
encodeConflict InvariantViolation = 17
encodeConflict InvalidTransition = 18
encodeConflict ProofMissing = 19

||| Decode uint8 back to Conflict
public export
decodeConflict : Bits256 -> Conflict
decodeConflict 0 = Revert
decodeConflict 1 = Reentrancy
decodeConflict 2 = AuthViolation
decodeConflict 3 = CEIViolation
decodeConflict 4 = EpochMismatch
decodeConflict 5 = InitAlready
decodeConflict 6 = NotInitialized
decodeConflict 7 = StorageAlias
decodeConflict 8 = ExternalCallForbidden
decodeConflict 9 = UnsafeEntryContext
decodeConflict 10 = GasExhausted
decodeConflict 11 = DecodeError
decodeConflict 12 = ArithmeticError
decodeConflict 13 = UpgradeNotAllowed
decodeConflict 14 = RollbackNotAllowed
decodeConflict 15 = AssetAlreadySpent
decodeConflict 16 = AssetNotOwned
decodeConflict 17 = InvariantViolation
decodeConflict 18 = InvalidTransition
decodeConflict 19 = ProofMissing
decodeConflict _ = Revert  -- Unknown codes map to Revert

-- =============================================================================
-- Evidence -> Revert Data Encoding
-- =============================================================================

||| Minimum evidence encoding for revert data
||| Layout: [4 bytes selector][32 bytes conflict][32 bytes digest]
||| Total: 68 bytes minimum
public export
minimalRevertDataSize : Bits256
minimalRevertDataSize = 68

||| Write FR failure to memory and return (offset, size)
||| Uses scratch space starting at 0x00
public export
encodeFailureToMemory : Conflict -> Evidence -> IO (Bits256, Bits256)
encodeFailureToMemory c e = do
  -- Write selector at 0x00 (left-aligned: multiply by 2^224)
  -- frFailureSelector * 2^224 = frFailureSelector * 0x100000000000000000000000000000000000000000000000000000000
  mstore 0x00 (frFailureSelector * 0x100000000000000000000000000000000000000000000000000000000)
  -- Write conflict code at 0x04
  mstore 0x04 (encodeConflict c)
  -- Write evidence digest at 0x24
  mstore 0x24 e.digest
  -- For minimal encoding, we stop here (68 bytes)
  -- Full encoding would include tags array
  pure (0x00, minimalRevertDataSize)

||| Revert with classified failure and evidence
||| This is the FR-compliant replacement for `evmRevert 0 0`
public export
revertWithFailure : Conflict -> Evidence -> IO ()
revertWithFailure c e = do
  (offset, size) <- encodeFailureToMemory c e
  evmRevert offset size

||| Revert with just conflict (auto-generates minimal evidence)
public export
revertConflict : Conflict -> IO ()
revertConflict c = revertWithFailure c (tagEvidence (conflictTag c))

||| Revert from Outcome (if Fail)
public export
revertOutcome : Outcome a -> IO ()
revertOutcome (Ok _) = pure ()  -- Should not be called on Ok
revertOutcome (Fail c e) = revertWithFailure c e

-- =============================================================================
-- FR-Aware Guards (replace bare evmRevert 0 0)
-- =============================================================================

||| FR-aware require: returns Outcome instead of reverting
public export
requireFR : Bool -> Conflict -> String -> Outcome ()
requireFR True _ _ = Ok ()
requireFR False c msg = Fail c (tagEvidence msg)

||| FR-aware require with immediate revert
public export
requireFR' : Bool -> Conflict -> String -> IO ()
requireFR' True _ _ = pure ()
requireFR' False c msg = revertWithFailure c (tagEvidence msg)

||| FR-aware require for Maybe
public export
requireJustFR : Maybe a -> Conflict -> String -> Outcome a
requireJustFR (Just x) _ _ = Ok x
requireJustFR Nothing c msg = Fail c (tagEvidence msg)

||| Check condition and return classified failure
public export
checkFR : Bool -> Conflict -> Evidence -> IO (Outcome ())
checkFR True _ _ = pure (Ok ())
checkFR False c e = pure (Fail c e)

-- =============================================================================
-- FR-Aware Entry Point Wrapper
-- =============================================================================

||| Run an FR computation and handle the outcome
||| Success: return normally
||| Failure: revert with classified failure + evidence
public export
runFR : IO (Outcome a) -> (a -> IO ()) -> IO ()
runFR computation onSuccess = do
  result <- computation
  case result of
    Ok x => onSuccess x
    Fail c e => revertWithFailure c e

||| Run FR computation, return value on success, revert on failure
public export
runFRReturn : Storable a => IO (Outcome a) -> IO ()
runFRReturn computation = do
  result <- computation
  case result of
    Ok x => do
      -- Encode return value
      let slots = toSlots x
      -- Write to memory and return
      writeSlots 0x00 slots
      evmReturn 0x00 (32 * cast (slotCount {a}))
    Fail c e => revertWithFailure c e
  where
    writeSlots : Bits256 -> Vect m Bits256 -> IO ()
    writeSlots _ [] = pure ()
    writeSlots off (v :: vs) = do
      mstore off v
      writeSlots (off + 32) vs

-- =============================================================================
-- Boundary-Aware FR (EntryCtx integration)
-- =============================================================================

||| Create evidence with entry context information
public export
ctxEvidence : EntryCtx -> String -> Evidence
ctxEvidence ctx msg = tagsEvidence [ctxName ctx, msg]

||| FR-aware context check
public export
checkContextFR : EntryCtx -> (EntryCtx -> Outcome ()) -> IO (Outcome ())
checkContextFR ctx check = pure (check ctx)

||| Guard storage access by entry context
public export
guardStorageFR : EntryCtx -> IO (Outcome ())
guardStorageFR ctx = pure (checkStorage ctx)

||| Guard external call by entry context
public export
guardExternalFR : EntryCtx -> IO (Outcome ())
guardExternalFR ctx = pure (checkExternal ctx)

-- =============================================================================
-- FR Dispatch (replaces dispatch with FR semantics)
-- =============================================================================

||| Evidence for selector-related failures
public export
selectorEvidence : Bits256 -> Evidence
selectorEvidence sel = MkEvidence sel ["selector", "not found"] [] [] []

||| Evidence for decode failures
public export
decodeEvidence : Bits256 -> String -> Evidence
decodeEvidence sel msg = MkEvidence sel ["decode", msg] [] [] []

||| Result of selector lookup
public export
data SelectorResult : Type where
  Found : IO () -> SelectorResult
  NotFound : Bits256 -> SelectorResult

||| FR-aware dispatch: unknown selector becomes classified failure
public export
dispatchFR : Bits256 -> List (Bits256, IO ()) -> IO ()
dispatchFR sel handlers = go handlers
  where
    go : List (Bits256, IO ()) -> IO ()
    go [] = revertWithFailure DecodeError (selectorEvidence sel)
    go ((s, h) :: rest) = if s == sel then h else go rest

-- =============================================================================
-- FR Monad Helpers
-- =============================================================================

||| Lift IO to FR computation (always succeeds)
public export
liftFR : IO a -> IO (Outcome a)
liftFR action = map Ok action

||| Sequence FR computations
public export
bindFR : IO (Outcome a) -> (a -> IO (Outcome b)) -> IO (Outcome b)
bindFR ma f = do
  result <- ma
  case result of
    Ok x => f x
    Fail c e => pure (Fail c e)

||| Combine two FR computations (both must succeed)
public export
andFR : IO (Outcome a) -> IO (Outcome b) -> IO (Outcome (a, b))
andFR ma mb = bindFR ma (\x => bindFR mb (\y => pure (Ok (x, y))))

-- =============================================================================
-- FR Theory Validation Helpers
-- =============================================================================

||| Check if a failure is recoverable (can be retried)
public export
isRecoverableFR : Conflict -> Bool
isRecoverableFR GasExhausted = True
isRecoverableFR _ = False

||| Check if a failure should be escalated (security-critical)
public export
shouldEscalateFR : Conflict -> Bool
shouldEscalateFR Reentrancy = True
shouldEscalateFR AuthViolation = True
shouldEscalateFR CEIViolation = True
shouldEscalateFR StorageAlias = True
shouldEscalateFR _ = False

||| FR implication 1: Composability = morphism existence
||| A handler is composable if it returns classified failures
public export
interface FRComposable (handler : Type) where
  ||| All failure modes are classified
  failureSurface : List Conflict
  ||| Evidence is always produced
  producesEvidence : Bool

