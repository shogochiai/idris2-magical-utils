||| FRMonad Test Suite
|||
||| Tests for FRMonad.Core, FRMonad.Failure, FRMonad.Evidence, FRMonad.Indexed
module FRMonad.Tests.AllTests

import FRMonad.Core
import FRMonad.Failure
import FRMonad.Evidence
import FRMonad.Indexed
import Data.List

%default total

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

public export
record TestDef where
  constructor MkTest
  testId    : String
  testDesc  : String
  testFunc  : IO Bool

public export
test : String -> String -> IO Bool -> TestDef
test = MkTest

runTest : TestDef -> IO Bool
runTest t = t.testFunc

countPassed : List Bool -> Nat
countPassed = length . filter id

export
runTestSuite : String -> List TestDef -> IO ()
runTestSuite name tests = do
    results <- traverse runTest tests
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Passed: " ++ show (countPassed results) ++ "/" ++ show (length results)
    for_ (zip tests results) $ \(t, r) =>
      putStrLn $ (if r then "[PASS] " else "[FAIL] ") ++ t.testId ++ ": " ++ t.testDesc

-- =============================================================================
-- FRMonad.Core Tests
-- =============================================================================

-- Monad Laws
test_left_identity : IO Bool
test_left_identity = do
  let f : Int -> FR Int = \x => Ok (x * 2) emptyEvidence
  let lhs = pure 42 >>= f
  let rhs = f 42
  pure $ fromOk 0 lhs == fromOk 0 rhs

test_right_identity : IO Bool
test_right_identity = pure $ fromOk 0 (Ok 42 emptyEvidence >>= pure) == 42

test_associativity : IO Bool
test_associativity = do
  let m : FR Int = Ok 10 emptyEvidence
  let f : Int -> FR Int = \x => Ok (x + 5) emptyEvidence
  let g : Int -> FR Int = \x => Ok (x * 2) emptyEvidence
  pure $ fromOk 0 ((m >>= f) >>= g) == fromOk 0 (m >>= (\x => f x >>= g))

-- Basic Operations
test_isOk : IO Bool
test_isOk = pure $ isOk (Ok 42 emptyEvidence) && not (isOk (the (FR Int) (Fail (NotFound "x") emptyEvidence)))

test_isFail : IO Bool
test_isFail = pure $ isFail (the (FR Int) (Fail (NotFound "x") emptyEvidence)) && not (isFail (Ok 42 emptyEvidence))

test_fromOk : IO Bool
test_fromOk = pure $ fromOk 0 (Ok 42 emptyEvidence) == 42 && fromOk 0 (the (FR Int) (Fail (NotFound "x") emptyEvidence)) == 0

-- Recovery Combinators
test_orElse : IO Bool
test_orElse = do
  let ok : FR Int = Ok 42 emptyEvidence
  let fail : FR Int = Fail (NotFound "x") emptyEvidence
  let alt : FR Int = Ok 99 emptyEvidence
  pure $ fromOk 0 (ok `orElse` alt) == 42 && fromOk 0 (fail `orElse` alt) == 99

test_handleWith : IO Bool
test_handleWith = do
  let handler : Handler Int Int = \_ => Ok 99 emptyEvidence
  let ok : FR Int = Ok 42 emptyEvidence
  let fail : FR Int = Fail (NotFound "x") emptyEvidence
  pure $ fromOk 0 (handleWith handler ok) == 42 && fromOk 0 (handleWith handler fail) == 99

-- Guard Combinators
test_require : IO Bool
test_require = do
  let ev = mkEvidence Update "test" "ok"
  pure $ isOk (require True (NotFound "x") ev) && isFail (require False (NotFound "x") ev)

test_guard : IO Bool
test_guard = pure $ isOk (guard Update "check" True (Unauthorized "x")) && isFail (guard Update "check" False (Unauthorized "x"))

-- Sequence Combinators
test_sequence : IO Bool
test_sequence = do
  let oks : List (FR Int) = [Ok 1 emptyEvidence, Ok 2 emptyEvidence, Ok 3 emptyEvidence]
  let withFail : List (FR Int) = [Ok 1 emptyEvidence, Fail (NotFound "x") emptyEvidence]
  pure $ fromOk [] (FRMonad.Core.sequence oks) == [1,2,3] && isFail (FRMonad.Core.sequence withFail)

test_firstSuccess : IO Bool
test_firstSuccess = do
  let xs : List (FR Int) = [Fail (NotFound "a") emptyEvidence, Ok 42 emptyEvidence]
  pure $ fromOk 0 (firstSuccess xs) == 42

-- Conversions
test_toMaybe : IO Bool
test_toMaybe = pure $ toMaybe (Ok 42 emptyEvidence) == Just 42 && toMaybe (the (FR Int) (Fail (NotFound "x") emptyEvidence)) == Nothing

test_toEither : IO Bool
test_toEither = do
  let ok = toEither (Ok 42 emptyEvidence)
  let fail = toEither (the (FR Int) (Fail (NotFound "x") emptyEvidence))
  pure $ (case ok of Right _ => True; _ => False) && (case fail of Left _ => True; _ => False)

-- =============================================================================
-- FRMonad.Failure Tests
-- =============================================================================

test_severity_critical : IO Bool
test_severity_critical = pure $ severity (SysInvariant "x") == Critical && severity (StableMemError "x") == Critical

test_severity_high : IO Bool
test_severity_high = pure $ severity (Trap "x") == High && severity (Unauthorized "x") == High

test_severity_medium : IO Bool
test_severity_medium = pure $ severity (Conflict "x") == Medium && severity (CallError "x") == Medium

test_severity_low : IO Bool
test_severity_low = pure $ severity (NotFound "x") == Low && severity (ValidationError "x") == Low

test_category : IO Bool
test_category = pure $
  category (Unauthorized "x") == SecurityFail &&
  category (Conflict "x") == StateFail &&
  category (CallError "x") == NetworkFail &&
  category (DecodeError "x") == EncodingFail

test_isRetryable : IO Bool
test_isRetryable = pure $
  isRetryable (CallError "x") && isRetryable (Conflict "x") &&
  not (isRetryable (SysInvariant "x")) && not (isRetryable (Unauthorized "x"))

test_isUserRecoverable : IO Bool
test_isUserRecoverable = pure $
  isUserRecoverable (ValidationError "x") && isUserRecoverable (NotFound "x") &&
  not (isUserRecoverable (SysInvariant "x"))

test_severity_order : IO Bool
test_severity_order = pure $ Critical > High && High > Medium && Medium > Low && Low > Info

-- =============================================================================
-- FRMonad.Evidence Tests
-- =============================================================================

test_emptyEvidence : IO Bool
test_emptyEvidence = pure $ emptyEvidence.phase == Query && emptyEvidence.label == ""

test_mkEvidence : IO Bool
test_mkEvidence = do
  let e = mkEvidence Update "op" "detail"
  pure $ e.phase == Update && e.label == "op" && e.detail == "detail"

test_addTag : IO Bool
test_addTag = pure $ hasTag "test" (addTag "test" emptyEvidence)

test_combineEvidence : IO Bool
test_combineEvidence = do
  let e1 = addTag "a" emptyEvidence
  let e2 = addTag "b" emptyEvidence
  let combined = combineEvidence e1 e2
  pure $ hasTag "a" combined && hasTag "b" combined

test_cyclesConsumed : IO Bool
test_cyclesConsumed = pure $ cyclesConsumed (withCycles 1000 750 emptyEvidence) == 250

test_callCount : IO Bool
test_callCount = do
  let call = MkCallRecord "c" "m" 100 True 0
  pure $ callCount (recordCall call emptyEvidence) == 1

-- =============================================================================
-- FRMonad.Indexed Tests
-- =============================================================================

test_isIOk : IO Bool
test_isIOk = do
  let ok : IFR Running Int = iok {post=Running} 42 emptyEvidence
  let fail : IFR Running Int = ifailWith (NotFound "x") emptyEvidence
  pure $ isIOk ok && not (isIOk fail)

test_initialize : IO Bool
test_initialize = do
  let result : IFR Uninitialized String = initialize "setup" "config"
  case result of
    (Initialized ** IOk v _) => pure $ v == "config"
    _ => pure False

test_start : IO Bool
test_start = do
  let result : IFR Initialized String = start "launch" "ready"
  case result of
    (Running ** IOk v _) => pure $ v == "ready"
    _ => pure False

test_iquery : IO Bool
test_iquery = do
  let result : IFR Running Int = iquery "get" 100
  case result of
    (Running ** IOk v _) => pure $ v == 100
    _ => pure False

test_ibind : IO Bool
test_ibind = do
  let m : IFR Running Int = iquery "first" 10
  let result : IFR Running Int = ibind {mid=Running} m (\x => iquery "second" (x * 2))
  case result of
    (_ ** IOk v _) => pure $ v == 20
    _ => pure False

test_ihandleWith : IO Bool
test_ihandleWith = do
  let ok : IFR Running Int = iquery "get" 42
  let fail : IFR Running Int = ifail Update "op" "err" (NotFound "x")
  let handler = \_ => iquery "fallback" 99
  pure $ isIOk (ihandleWith handler ok) && isIOk (ihandleWith handler fail)

test_runIFR : IO Bool
test_runIFR = do
  let ok : IFR Running Int = iquery "get" 42
  let fail : IFR Running Int = ifail Update "op" "err" (NotFound "x")
  pure $ (case runIFR ok of Right _ => True; _ => False) && (case runIFR fail of Left _ => True; _ => False)

-- =============================================================================
-- Test Collection
-- =============================================================================

public export
allTests : List TestDef
allTests =
  [ -- Core: Monad Laws
    test "FR_CORE_001" "Left identity" test_left_identity
  , test "FR_CORE_002" "Right identity" test_right_identity
  , test "FR_CORE_003" "Associativity" test_associativity
  -- Core: Basic Ops
  , test "FR_CORE_004" "isOk" test_isOk
  , test "FR_CORE_005" "isFail" test_isFail
  , test "FR_CORE_006" "fromOk" test_fromOk
  -- Core: Recovery
  , test "FR_CORE_007" "orElse" test_orElse
  , test "FR_CORE_008" "handleWith" test_handleWith
  -- Core: Guards
  , test "FR_CORE_009" "require" test_require
  , test "FR_CORE_010" "guard" test_guard
  -- Core: Sequence
  , test "FR_CORE_011" "sequence" test_sequence
  , test "FR_CORE_012" "firstSuccess" test_firstSuccess
  -- Core: Conversions
  , test "FR_CORE_013" "toMaybe" test_toMaybe
  , test "FR_CORE_014" "toEither" test_toEither
  -- Failure: Severity
  , test "FR_FAIL_001" "severity critical" test_severity_critical
  , test "FR_FAIL_002" "severity high" test_severity_high
  , test "FR_FAIL_003" "severity medium" test_severity_medium
  , test "FR_FAIL_004" "severity low" test_severity_low
  -- Failure: Category
  , test "FR_FAIL_005" "category" test_category
  -- Failure: Recovery guidance
  , test "FR_FAIL_006" "isRetryable" test_isRetryable
  , test "FR_FAIL_007" "isUserRecoverable" test_isUserRecoverable
  , test "FR_FAIL_008" "severity order" test_severity_order
  -- Evidence
  , test "FR_EV_001" "emptyEvidence" test_emptyEvidence
  , test "FR_EV_002" "mkEvidence" test_mkEvidence
  , test "FR_EV_003" "addTag" test_addTag
  , test "FR_EV_004" "combineEvidence" test_combineEvidence
  , test "FR_EV_005" "cyclesConsumed" test_cyclesConsumed
  , test "FR_EV_006" "callCount" test_callCount
  -- Indexed
  , test "FR_IDX_001" "isIOk" test_isIOk
  , test "FR_IDX_002" "initialize" test_initialize
  , test "FR_IDX_003" "start" test_start
  , test "FR_IDX_004" "iquery" test_iquery
  , test "FR_IDX_005" "ibind" test_ibind
  , test "FR_IDX_006" "ihandleWith" test_ihandleWith
  , test "FR_IDX_007" "runIFR" test_runIFR
  ]

export
runAllTests : IO ()
runAllTests = runTestSuite "FRMonad" allTests

main : IO ()
main = runAllTests
