||| Coverage Core Test Suite
|||
||| Tests for idris2-coverage-core shared types and functions.
module Tests.AllTests

import Data.List

import Coverage.Core.Types
import Coverage.Core.Result
import Coverage.Core.RuntimeHit
import Coverage.Core.ObligationMap
import Coverage.Core.HighImpact
import Coverage.Classification.BranchClass
import Coverage.Standardization.Types

%default covering

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

||| Test definition
public export
record TestDef where
  constructor MkTestDef
  testId      : String
  description : String
  run         : () -> Bool

||| Create test
test : String -> String -> (() -> Bool) -> TestDef
test tid desc fn = MkTestDef tid desc fn

-- =============================================================================
-- Types Module Tests
-- =============================================================================

test_TYPES_001 : () -> Bool
test_TYPES_001 () =
  -- AltType equality
  ConCase == ConCase && ConstCase == ConstCase && ConCase /= ConstCase

test_TYPES_002 : () -> Bool
test_TYPES_002 () =
  -- CaseAlt construction
  let alt = MkCaseAlt ConCase (Just "MkPair") Nothing
  in alt.altType == ConCase && alt.constructorName == Just "MkPair"

test_TYPES_003 : () -> Bool
test_TYPES_003 () =
  -- CaseExpr target count with default
  let alt1 = MkCaseAlt ConCase (Just "True") Nothing
      alt2 = MkCaseAlt ConCase (Just "False") Nothing
      ce = MkCaseExpr [alt1, alt2] True
  in caseTargetCount ce == 3  -- 2 alts + 1 default

test_TYPES_004 : () -> Bool
test_TYPES_004 () =
  -- CaseExpr target count without default
  let alt1 = MkCaseAlt ConCase (Just "Z") Nothing
      alt2 = MkCaseAlt ConCase (Just "S") Nothing
      ce = MkCaseExpr [alt1, alt2] False
  in caseTargetCount ce == 2

test_TYPES_005 : () -> Bool
test_TYPES_005 () =
  -- FuncCases total branches
  let alt1 = MkCaseAlt ConCase (Just "Z") Nothing
      alt2 = MkCaseAlt ConCase (Just "S") Nothing
      ce1 = MkCaseExpr [alt1, alt2] True       -- 3 targets
      ce2 = MkCaseExpr [alt1] False            -- 1 target
      totalBr = calcTotalBranches [ce1, ce2]
  in totalBr == 4

test_TYPES_006 : () -> Bool
test_TYPES_006 () =
  -- BranchId equality
  let b1 = MkBranchId "Main" "foo" 0 0
      b2 = MkBranchId "Main" "foo" 0 0
      b3 = MkBranchId "Main" "foo" 0 1
  in b1 == b2 && b1 /= b3

test_TYPES_007 : () -> Bool
test_TYPES_007 () =
  -- getModuleName extraction
  getModuleName "Prelude.Types.null" == "Prelude.Types"

test_TYPES_008 : () -> Bool
test_TYPES_008 () =
  -- getSimpleFuncName extraction
  getSimpleFuncName "Prelude.Types.null" == "null"

test_TYPES_009 : () -> Bool
test_TYPES_009 () =
  -- parseFullName
  let (mod, fn) = parseFullName "Main.dispatch"
  in mod == "Main" && fn == "dispatch"

test_TYPES_010 : () -> Bool
test_TYPES_010 () =
  -- Single name (no module)
  getModuleName "main" == "" && getSimpleFuncName "main" == "main"

-- =============================================================================
-- Result Module Tests
-- =============================================================================

test_RESULT_001 : () -> Bool
test_RESULT_001 () =
  -- buildCoverageResult basic
  let targets = ["foo", "bar", "baz"]
      executed = ["foo", "bar"]
      result = buildCoverageResult targets executed
  in result.coveredCount == 2 && result.totalCount == 3 && length result.uncoveredFunctions == 1

test_RESULT_002 : () -> Bool
test_RESULT_002 () =
  -- emptyCoverageResult
  let r = emptyCoverageResult
  in r.totalCount == 0 && r.coveragePercent == 100.0

test_RESULT_003 : () -> Bool
test_RESULT_003 () =
  -- getHighImpactTargets
  let result = buildCoverageResult ["a", "b", "c"] ["a"]
  in getHighImpactTargets result 10 == ["b", "c"]

test_RESULT_004 : () -> Bool
test_RESULT_004 () =
  -- meetsCoverageThreshold
  let result = buildCoverageResult ["a", "b"] ["a", "b"]  -- 100%
  in meetsCoverageThreshold result 90.0

test_RESULT_005 : () -> Bool
test_RESULT_005 () =
  -- meetsCoverageThreshold failure
  let result = buildCoverageResult ["a", "b"] ["a"]  -- 50%
  in not (meetsCoverageThreshold result 80.0)

test_RESULT_006 : () -> Bool
test_RESULT_006 () =
  -- mergeCoverageResults OR aggregation
  let r1 = buildCoverageResult ["a", "b"] ["a"]
      r2 = buildCoverageResult ["a", "b", "c"] ["b", "c"]
      merged = mergeCoverageResults [r1, r2]
  in merged.totalCount == 3 && merged.coveredCount == 3

test_RESULT_007 : () -> Bool
test_RESULT_007 () =
  -- coverageSeverity thresholds
  coverageSeverity 40.0 == Critical &&
  coverageSeverity 60.0 == High &&
  coverageSeverity 80.0 == Medium &&
  coverageSeverity 95.0 == Low

test_RESULT_008 : () -> Bool
test_RESULT_008 () =
  -- CoverageSeverity ordering
  Critical > High && High > Medium && Medium > Low

-- =============================================================================
-- RuntimeHit Module Tests
-- =============================================================================

test_RUNTIME_001 : () -> Bool
test_RUNTIME_001 () =
  -- FunctionRuntimeHit construction
  let hit = MkFunctionRuntimeHit "Main.foo" "Main_foo" 4 3 10 8
  in hit.funcName == "Main.foo" && hit.canonicalCount == 4

test_RUNTIME_002 : () -> Bool
test_RUNTIME_002 () =
  -- functionRuntimeCoveragePercent
  let hit = MkFunctionRuntimeHit "f" "f" 4 2 10 5
  in functionRuntimeCoveragePercent hit == 50.0

test_RUNTIME_003 : () -> Bool
test_RUNTIME_003 () =
  -- isFullyCovered true
  let hit = MkFunctionRuntimeHit "f" "f" 4 4 10 10
  in isFullyCovered hit

test_RUNTIME_004 : () -> Bool
test_RUNTIME_004 () =
  -- isFullyCovered false
  let hit = MkFunctionRuntimeHit "f" "f" 4 2 10 5
  in not (isFullyCovered hit)

test_RUNTIME_005 : () -> Bool
test_RUNTIME_005 () =
  -- uncoveredCount
  let hit = MkFunctionRuntimeHit "f" "f" 10 3 20 10
  in uncoveredCount hit == 7

test_RUNTIME_006 : () -> Bool
test_RUNTIME_006 () =
  -- uncoveredCount when all covered
  let hit = MkFunctionRuntimeHit "f" "f" 5 10 20 20
  in uncoveredCount hit == 0

test_RUNTIME_007 : () -> Bool
test_RUNTIME_007 () =
  let om = buildObligationMap
             [ ("Main.foo", [runtimeFunctionName "Main.foo", generatedFunctionName "Main_foo"])
             , ("Main.bar", [runtimeFunctionName "Main.bar"])
             ]
      resolved = resolveRuntimeUnits om [generatedFunctionName "Main_foo"]
  in resolved == ["Main.foo"]

test_RUNTIME_008 : () -> Bool
test_RUNTIME_008 () =
  let obligations =
        [ MkCoverageObligation "Main.foo" ElaboratedCaseTree FunctionLevel "foo" Nothing ReachableObligation
        , MkCoverageObligation "Main.artifact" ElaboratedCaseTree FunctionLevel "artifact" Nothing CompilerInsertedArtifact
        ]
      om = buildFunctionObligationMap obligations (\ob =>
             [runtimeFunctionName ob.obligationId, generatedFunctionName (getSimpleFuncName ob.obligationId)]
           )
      covered = resolveCoveredDenominatorIds obligations om
                  [generatedFunctionName "foo", generatedFunctionName "artifact"]
  in covered == ["Main.foo"]

-- =============================================================================
-- HighImpact Module Tests
-- =============================================================================

test_HI_001 : () -> Bool
test_HI_001 () =
  -- HighImpactKind equality
  HIT_UntestedCanonical == HIT_UntestedCanonical &&
  HIT_BugUnhandledInput /= HIT_UnknownCrash

test_HI_002 : () -> Bool
test_HI_002 () =
  -- kindPriority ordering
  kindPriority HIT_BugUnhandledInput < kindPriority HIT_UnknownCrash &&
  kindPriority HIT_UnknownCrash < kindPriority HIT_UntestedCanonical

test_HI_003 : () -> Bool
test_HI_003 () =
  -- severityRatio with nonzero executed
  let sev = severityRatio 10 5
  in sev == 2.0

test_HI_004 : () -> Bool
test_HI_004 () =
  -- severityRatio with zero executed (infinity)
  let sev = severityRatio 10 0
  in sev > 1.0e308

test_HI_005 : () -> Bool
test_HI_005 () =
  -- mkUntestedTarget
  let t = mkUntestedTarget "Main.foo" "Main" 10 5
  in t.kind == HIT_UntestedCanonical && t.branchCount == 10

test_HI_006 : () -> Bool
test_HI_006 () =
  -- mkBugTarget
  let t = mkBugTarget "Main.bar" "Main" 5
  in t.kind == HIT_BugUnhandledInput && t.severity > 1.0e308

test_HI_007 : () -> Bool
test_HI_007 () =
  -- mkUnknownCrashTarget
  let t = mkUnknownCrashTarget "Main.crash" "Main" 3
  in t.kind == HIT_UnknownCrash

test_HI_008 : () -> Bool
test_HI_008 () =
  -- targetFromRuntimeHit with gaps
  let hit = MkFunctionRuntimeHit "Main.foo" "Main_foo" 10 5 20 10
      mTarget = targetFromRuntimeHit hit
  in case mTarget of
       Just t => t.funcName == "Main.foo" && t.branchCount == 10
       Nothing => False

test_HI_009 : () -> Bool
test_HI_009 () =
  -- targetFromRuntimeHit fully covered -> Nothing
  let hit = MkFunctionRuntimeHit "Main.foo" "Main_foo" 10 10 20 20
      mTarget = targetFromRuntimeHit hit
  in case mTarget of
       Nothing => True
       Just _ => False

test_HI_010 : () -> Bool
test_HI_010 () =
  -- sortTargets by severity
  let t1 = mkUntestedTarget "f1" "M" 10 5   -- severity 2.0
      t2 = mkUntestedTarget "f2" "M" 10 2   -- severity 5.0
      t3 = mkUntestedTarget "f3" "M" 10 10  -- severity 1.0
      sorted = sortTargets [t1, t2, t3]
  in case sorted of
       [a, b, c] => a.funcName == "f2" && c.funcName == "f3"
       _ => False

test_HI_011 : () -> Bool
test_HI_011 () =
  -- topKTargets
  let targets = map (\n => mkUntestedTarget ("f" ++ show n) "M" 10 (cast n)) [1..5]
      top2 = topKTargets 2 targets
  in length top2 == 2

test_HI_012 : () -> Bool
test_HI_012 () =
  -- targetsFromRuntimeHits
  let hits = [ MkFunctionRuntimeHit "f1" "f1" 10 5 20 10   -- has gaps
             , MkFunctionRuntimeHit "f2" "f2" 10 10 20 20  -- fully covered
             , MkFunctionRuntimeHit "f3" "f3" 8 3 16 8     -- has gaps
             ]
      targets = targetsFromRuntimeHits hits
  in length targets == 2

test_HI_013 : () -> Bool
test_HI_013 () =
  -- showSeverity with regular value
  showSeverity 3.5 == "3.5"

test_HI_014 : () -> Bool
test_HI_014 () =
  -- showSeverity with Inf
  showSeverity severityInfinity == "Inf"

-- =============================================================================
-- BranchClass / Standardization Tests
-- =============================================================================

test_BRANCH_001 : () -> Bool
test_BRANCH_001 () =
  branchClassToObligationClass BCCanonical == ReachableObligation
  && branchClassToObligationClass BCExcludedNoClauses == LogicallyUnreachable
  && branchClassToObligationClass BCBugUnhandledInput == UserAdmittedPartialGap

test_BRANCH_002 : () -> Bool
test_BRANCH_002 () =
  isCountedInDenominator BCCanonical
  && not (isCountedInDenominator BCExcludedNoClauses)
  && not (isCountedInDenominator (BCUnknownCrash "mystery"))

test_BRANCH_003 : () -> Bool
test_BRANCH_003 () =
  let canonical = MkClassifiedBranch (MkBranchId "Main" "safe" 0 0) BCCanonical "concase"
      unknown = MkClassifiedBranch (MkBranchId "Main" "risky" 0 1) (BCUnknownCrash "mystery") "crash"
  in isStandardCoverageClaimAdmissible [canonical]
     && not (isStandardCoverageClaimAdmissible [canonical, unknown])

test_BRANCH_004 : () -> Bool
test_BRANCH_004 () =
  let canonical = MkClassifiedBranch (MkBranchId "Main" "safe" 0 0) BCCanonical "concase"
      partialBranch = MkClassifiedBranch (MkBranchId "Main" "safe" 0 1) BCBugUnhandledInput "crash"
      fn = MkStaticFunctionAnalysis "Main.safe" [canonical, partialBranch]
      obligation = staticFunctionToCoverageObligation fn
  in functionObligationClass fn.branches == UserAdmittedPartialGap
     && obligation.granularity == FunctionLevel
     && isStandardFunctionCoverageClaimAdmissible [fn]

test_BRANCH_005 : () -> Bool
test_BRANCH_005 () =
  let unreachable = MkClassifiedBranch (MkBranchId "Main" "voidFn" 0 0) BCExcludedNoClauses "impossible"
      unknownBranch = MkClassifiedBranch (MkBranchId "Main" "unknownFn" 0 1) (BCUnknownCrash "mystery") "crash"
      excludedFn = MkStaticFunctionAnalysis "Main.voidFn" [unreachable]
      unknownFn = MkStaticFunctionAnalysis "Main.unknownFn" [unknownBranch]
  in functionObligationClass excludedFn.branches == LogicallyUnreachable
     && isStandardFunctionCoverageClaimAdmissible [excludedFn]
     && not (isStandardFunctionCoverageClaimAdmissible [excludedFn, unknownFn])

-- =============================================================================
-- All Tests
-- =============================================================================

||| All tests
export
allTests : List TestDef
allTests =
  -- Types
  [ test "TYPES_001" "AltType equality" test_TYPES_001
  , test "TYPES_002" "CaseAlt construction" test_TYPES_002
  , test "TYPES_003" "CaseExpr target count with default" test_TYPES_003
  , test "TYPES_004" "CaseExpr target count without default" test_TYPES_004
  , test "TYPES_005" "calcTotalBranches" test_TYPES_005
  , test "TYPES_006" "BranchId equality" test_TYPES_006
  , test "TYPES_007" "getModuleName" test_TYPES_007
  , test "TYPES_008" "getSimpleFuncName" test_TYPES_008
  , test "TYPES_009" "parseFullName" test_TYPES_009
  , test "TYPES_010" "single name handling" test_TYPES_010

  -- Result
  , test "RESULT_001" "buildCoverageResult basic" test_RESULT_001
  , test "RESULT_002" "emptyCoverageResult" test_RESULT_002
  , test "RESULT_003" "getHighImpactTargets" test_RESULT_003
  , test "RESULT_004" "meetsCoverageThreshold success" test_RESULT_004
  , test "RESULT_005" "meetsCoverageThreshold failure" test_RESULT_005
  , test "RESULT_006" "mergeCoverageResults OR" test_RESULT_006
  , test "RESULT_007" "coverageSeverity thresholds" test_RESULT_007
  , test "RESULT_008" "CoverageSeverity ordering" test_RESULT_008

  -- RuntimeHit
  , test "RUNTIME_001" "FunctionRuntimeHit construction" test_RUNTIME_001
  , test "RUNTIME_002" "functionRuntimeCoveragePercent" test_RUNTIME_002
  , test "RUNTIME_003" "isFullyCovered true" test_RUNTIME_003
  , test "RUNTIME_004" "isFullyCovered false" test_RUNTIME_004
  , test "RUNTIME_005" "uncoveredCount with gaps" test_RUNTIME_005
  , test "RUNTIME_006" "uncoveredCount fully covered" test_RUNTIME_006
  , test "RUNTIME_007" "ObligationMap resolves generated names to obligation IDs" test_RUNTIME_007
  , test "RUNTIME_008" "ObligationMap only reports denominator-covered IDs" test_RUNTIME_008

  -- HighImpact
  , test "HI_001" "HighImpactKind equality" test_HI_001
  , test "HI_002" "kindPriority ordering" test_HI_002
  , test "HI_003" "severityRatio nonzero" test_HI_003
  , test "HI_004" "severityRatio infinity" test_HI_004
  , test "HI_005" "mkUntestedTarget" test_HI_005
  , test "HI_006" "mkBugTarget" test_HI_006
  , test "HI_007" "mkUnknownCrashTarget" test_HI_007
  , test "HI_008" "targetFromRuntimeHit with gaps" test_HI_008
  , test "HI_009" "targetFromRuntimeHit fully covered" test_HI_009
  , test "HI_010" "sortTargets by severity" test_HI_010
  , test "HI_011" "topKTargets" test_HI_011
  , test "HI_012" "targetsFromRuntimeHits" test_HI_012
  , test "HI_013" "showSeverity regular" test_HI_013
  , test "HI_014" "showSeverity Inf" test_HI_014

  -- BranchClass / Standardization
  , test "BRANCH_001" "BranchClass to obligation class mapping" test_BRANCH_001
  , test "BRANCH_002" "Denominator semantics follow standardization" test_BRANCH_002
  , test "BRANCH_003" "Unknown branches block admissible claim" test_BRANCH_003
  , test "BRANCH_004" "Function obligations aggregate partial branches" test_BRANCH_004
  , test "BRANCH_005" "Function claims block on unknown branches" test_BRANCH_005
  ]

||| Run all tests - pure version
export
runAllTestsPure : (Nat, Nat)
runAllTestsPure =
  let results = map (\t => t.run ()) allTests
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)

||| Run all tests with IO output (for lazy core ask compatibility)
export
runAllTests : IO ()
runAllTests = do
  let (passed, failed) = runAllTestsPure
  putStrLn $ "Tests: " ++ show passed ++ "/" ++ show (passed + failed) ++ " passed"
  if failed == 0
    then putStrLn "All tests PASSED!"
    else putStrLn $ show failed ++ " tests FAILED"
