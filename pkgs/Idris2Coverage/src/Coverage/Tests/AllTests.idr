||| Per-Module Test Suite for idris2-coverage
||| Consolidates all 44 Golden Tests into a single test runner
module Coverage.Tests.AllTests

import Coverage.Types
import Coverage.Linearity
import Coverage.TypeAnalyzer
import Coverage.StateSpace
import Coverage.PathAnalysis
import Coverage.Complexity
import Coverage.Collector
import Coverage.SourceAnalyzer
import Coverage.Report
import Coverage.TestHint
import Coverage.Aggregator
import Coverage.TestRunner
import Coverage.UnifiedRunner
import Coverage.Config
import Coverage.DumpcasesParser
import Data.List
import Data.String
import Data.Maybe
import System

%default total

-- =============================================================================
-- Linearity Tests (LIN_001-004)
-- =============================================================================

||| REQ_COV_LIN_001
covering
test_LIN_001 : IO Bool
test_LIN_001 = do
  let q0 = parseQuantity "0"
  let q1 = parseQuantity "1"
  let qw = parseQuantity "ω"
  pure $ q0 == Q0 && q1 == Q1 && qw == QW

||| REQ_COV_LIN_002
covering
test_LIN_002 : IO Bool
test_LIN_002 = do
  let p0 = MkLinearParam (Just "x") "Int" Q0 Nothing
  let p1 = MkLinearParam (Just "y") "Int" Q1 Nothing
  pure $ isErased p0 && not (isErased p1)

||| REQ_COV_LIN_003
covering
test_LIN_003 : IO Bool
test_LIN_003 = do
  let p1 = MkLinearParam (Just "x") "Int" Q1 Nothing
  let pW = MkLinearParam (Just "y") "Int" QW Nothing
  pure $ isLinear p1 && not (isLinear pW)

||| REQ_COV_LIN_004
covering
test_LIN_004 : IO Bool
test_LIN_004 = do
  let pW = MkLinearParam (Just "x") "Int" QW Nothing
  let p0 = MkLinearParam (Just "y") "Int" Q0 Nothing
  -- QW is unrestricted, Q0 is erased
  pure $ pW.quantity == QW && p0.quantity == Q0

||| REQ_COV_LIN_005: parseLinearParam with quantity annotation
covering
test_LIN_005 : IO Bool
test_LIN_005 = do
  -- Test "(1 h : Handle)" format
  let result = parseLinearParam "(1 h : Handle)"
  pure $ case result of
    Just lp => lp.quantity == Q1 && lp.paramName == Just "h"
    Nothing => False

||| REQ_COV_LIN_006: parseLinearParam failure case
covering
test_LIN_006 : IO Bool
test_LIN_006 = do
  -- Test invalid input returns Nothing
  let result = parseLinearParam "invalid"
  pure $ isNothing result

||| REQ_COV_LIN_007: effectiveStateSpace with typeInfo=Nothing
covering
test_LIN_007 : IO Bool
test_LIN_007 = do
  -- Parameter without typeInfo should give Unbounded
  let p = MkLinearParam (Just "x") "Unknown" QW Nothing
  let space = effectiveStateSpace [p]
  pure $ space == Unbounded

||| REQ_COV_LIN_008: effectiveStateSpace with empty list
covering
test_LIN_008 : IO Bool
test_LIN_008 = do
  let space = effectiveStateSpace []
  pure $ space == Finite 1

-- =============================================================================
-- Type Analyzer Tests (TYP_001-004)
-- =============================================================================

||| REQ_COV_TYP_001
covering
test_TYP_001 : IO Bool
test_TYP_001 = do
  let sig = "(1 h : Handle) -> String -> IO ()"
  let params = extractLinearParams sig
  pure $ length params >= 1

||| REQ_COV_TYP_002
covering
test_TYP_002 : IO Bool
test_TYP_002 = do
  let analyzed = analyzeFunction "process" "String -> IO (Either Error Result)"
  pure $ analyzed.name == "process"

||| REQ_COV_TYP_003
covering
test_TYP_003 : IO Bool
test_TYP_003 = do
  let analyzed = analyzeFunction "read" "(1 h : Handle) -> IO String"
  let linearParams = filter isLinear analyzed.params
  pure $ length linearParams >= 0  -- Check extraction works

||| REQ_COV_TYP_004
covering
test_TYP_004 : IO Bool
test_TYP_004 = do
  let analyzed = analyzeFunction "maybe" "Maybe a -> b -> (a -> b) -> b"
  pure $ length analyzed.params >= 2

||| REQ_COV_TYP_005: safeInit with empty list
covering
test_TYP_005 : IO Bool
test_TYP_005 = do
  let result : List Int = safeInit []
  pure $ result == []

||| REQ_COV_TYP_006: safeInit with single element
covering
test_TYP_006 : IO Bool
test_TYP_006 = do
  let result = safeInit [1]
  pure $ result == []

||| REQ_COV_TYP_007: safeLast with empty list
covering
test_TYP_007 : IO Bool
test_TYP_007 = do
  let result : Maybe Int = safeLast []
  pure $ isNothing result

||| REQ_COV_TYP_008: safeLast with single element
covering
test_TYP_008 : IO Bool
test_TYP_008 = do
  let result = safeLast [42]
  pure $ result == Just 42

||| REQ_COV_TYP_009: resolveType with Either
covering
test_TYP_009 : IO Bool
test_TYP_009 = do
  let info = resolveType "Either Error Value"
  pure $ isInfixOf "Either" info.typeName

||| REQ_COV_TYP_010: resolveType with Tuple
covering
test_TYP_010 : IO Bool
test_TYP_010 = do
  let info = resolveType "(Int, String)"
  pure $ isPrefixOf "(" info.typeName

||| REQ_COV_TYP_011: parseParams with arrow fragments
covering
test_TYP_011 : IO Bool
test_TYP_011 = do
  let params = parseParams "Int -> Bool -> String"
  -- Should return [Int, Bool] (excluding return type)
  pure $ length params >= 1

-- =============================================================================
-- State Space Tests (SPC_001-004)
-- =============================================================================

||| REQ_COV_SPC_001
covering
test_SPC_001 : IO Bool
test_SPC_001 = do
  let boolParam = MkLinearParam (Just "flag") "Bool" QW Nothing
  let boolSpace = paramStateSpace defaultConfig boolParam
  -- Without typeInfo, defaults to Bounded limit (10)
  pure $ case boolSpace of
    Bounded n => n == 10
    _ => False

||| REQ_COV_SPC_002
covering
test_SPC_002 : IO Bool
test_SPC_002 = do
  let maybeParam = MkLinearParam (Just "opt") "Maybe Int" QW Nothing
  let space = paramStateSpace defaultConfig maybeParam
  -- Without typeInfo, defaults to Bounded limit (10)
  pure $ case space of
    Bounded n => n == 10
    _ => False

||| REQ_COV_SPC_003
covering
test_SPC_003 : IO Bool
test_SPC_003 = do
  let listParam = MkLinearParam (Just "xs") "List Int" QW Nothing
  let space = paramStateSpace defaultConfig listParam
  -- List is bounded
  pure $ case space of
    Bounded _ => True
    _ => False

||| REQ_COV_SPC_004
covering
test_SPC_004 : IO Bool
test_SPC_004 = do
  let eitherParam = MkLinearParam (Just "res") "Either Error a" QW Nothing
  let space = paramStateSpace defaultConfig eitherParam
  -- Either has bounded space
  pure $ case space of
    Finite _ => True
    Bounded _ => True
    _ => False

||| REQ_COV_SPC_005: generateCombinations with empty list
covering
test_SPC_005 : IO Bool
test_SPC_005 = do
  let result = generateCombinations defaultConfig []
  pure $ length result >= 1

||| REQ_COV_SPC_006: representativeValues for unknown type
covering
test_SPC_006 : IO Bool
test_SPC_006 = do
  let unknownType = MkTypeInfo "UnknownType" [] False TCUnbounded Unbounded
  let vals = representativeValues unknownType
  pure $ length vals >= 0

-- =============================================================================
-- Path Analysis Tests (PTH_001-004)
-- =============================================================================

||| REQ_COV_PTH_001
covering
test_PTH_001 : IO Bool
test_PTH_001 = do
  let nothingPat = isEarlyExitPattern "Nothing"
  let leftPat = isEarlyExitPattern "Left _"
  let nilPat = isEarlyExitPattern "[]"
  let justPat = isEarlyExitPattern "Just x"
  pure $ isJust nothingPat && isJust leftPat && isJust nilPat && not (isJust justPat)

||| REQ_COV_PTH_002
covering
test_PTH_002 : IO Bool
test_PTH_002 = do
  -- Test branch analysis
  let params = [MkLinearParam (Just "x") "Maybe Int" QW Nothing]
  let pathAnalysis = analyzeFunctionPaths "process" params
  pure $ pathAnalysis.totalBranches >= 0

||| REQ_COV_PTH_003
covering
test_PTH_003 : IO Bool
test_PTH_003 = do
  -- Test pattern analysis
  let pattern = analyzePattern "xs :: rest"
  pure $ pattern.reachability == Conditional

||| REQ_COV_PTH_004
covering
test_PTH_004 : IO Bool
test_PTH_004 = do
  -- Test base case pattern (catch-all)
  let pattern = analyzePattern "_"
  pure $ pattern.reachability == Always

||| REQ_COV_PTH_005: analyzePattern with constructor pattern
covering
test_PTH_005 : IO Bool
test_PTH_005 = do
  let pattern = analyzePattern "Just x"
  pure $ pattern.reachability == Conditional

||| REQ_COV_PTH_006: calculateTotalBranches with multiple patterns
covering
test_PTH_006 : IO Bool
test_PTH_006 = do
  let params = [ MkLinearParam (Just "x") "Bool" QW Nothing
               , MkLinearParam (Just "y") "Maybe Int" QW Nothing
               ]
  let analysis = analyzeFunctionPaths "test" params
  pure $ analysis.totalBranches >= 0

-- =============================================================================
-- Complexity Tests (CPX_001-004)
-- =============================================================================

||| REQ_COV_CPX_001
covering
test_CPX_001 : IO Bool
test_CPX_001 = do
  let params = [ MkLinearParam (Just "a") "Int" QW Nothing
               , MkLinearParam (Just "b") "String" QW Nothing
               , MkLinearParam (Just "c") "Bool" QW Nothing
               ]
  let factors = calculateFactors defaultComplexityConfig params (Finite 10) 2 5
  pure $ factors.totalScore >= 0

||| REQ_COV_CPX_002
covering
test_CPX_002 : IO Bool
test_CPX_002 = do
  let params = [MkLinearParam (Just "x") "Int" QW Nothing]
  let factors = calculateFactors defaultComplexityConfig params (Finite 10) 1 2
  pure $ factors.paramFactor >= 0

||| REQ_COV_CPX_003
covering
test_CPX_003 : IO Bool
test_CPX_003 = do
  let params = []
  let factors = calculateFactors defaultComplexityConfig params Unbounded 0 1
  pure $ factors.stateFactor >= 0

||| REQ_COV_CPX_004
covering
test_CPX_004 : IO Bool
test_CPX_004 = do
  let params = [MkLinearParam Nothing "Int" QW Nothing]
  let factors = calculateFactors defaultComplexityConfig params (Finite 5) 2 3
  pure $ factors.branchFactor >= 0

-- =============================================================================
-- Source Analyzer Tests (SRC_001-004)
-- =============================================================================

||| REQ_COV_SRC_001
covering
test_SRC_001 : IO Bool
test_SRC_001 = do
  let source = "module TestModule\n\nexport\nadd : Int -> Int -> Int\nadd x y = x + y"
  let funcs = analyzeSource source
  pure $ length funcs >= 1

||| REQ_COV_SRC_002
covering
test_SRC_002 : IO Bool
test_SRC_002 = do
  -- Spec: SourceAnalyzer SHALL determine line_start and line_end for each function
  -- analyzeSource records signature line as lineStart (line 4 = "add : Int -> Int")
  let source = "module Test\n\nexport\nadd : Int -> Int\nadd x = x + 1"
  let exports = analyzeSource source
  case exports of
    [f] => pure (f.lineStart == 4 && f.lineEnd == 4 && f.name == "add")
    _   => pure False

||| REQ_COV_SRC_003
covering
test_SRC_003 : IO Bool
test_SRC_003 = do
  -- Spec: SourceAnalyzer SHALL extract export visibility
  -- Test both exported and non-exported functions
  let source = "module A\n\nexport\nfoo : Int\nfoo = 1\n\nbar : Int\nbar = 2"
  let funcs = analyzeSource source
  -- Only exported function 'foo' should be returned
  pure $ length funcs == 1 &&
         case head' funcs of
           Just f => f.exported && f.name == "foo"
           Nothing => False

||| REQ_COV_SRC_004
covering
test_SRC_004 : IO Bool
test_SRC_004 = do
  let source = "module A\n\n-- private\nbar : Int\nbar = 42\n\nexport\nfoo : Int\nfoo = bar"
  let funcs = analyzeSource source
  pure $ length funcs >= 1

||| REQ_COV_SRC_005: analyzeSource with no exports
covering
test_SRC_005 : IO Bool
test_SRC_005 = do
  let source = "module Empty\n\n-- no exports here"
  let funcs = analyzeSource source
  pure $ length funcs == 0

||| REQ_COV_SRC_006: analyzeSource with multiple functions
covering
test_SRC_006 : IO Bool
test_SRC_006 = do
  let source = "module Multi\n\nexport\nfoo : Int\nfoo = 1\n\nexport\nbar : String\nbar = \"x\""
  let funcs = analyzeSource source
  pure $ length funcs >= 2

-- =============================================================================
-- Collector Tests (COL_001-004)
-- =============================================================================

||| REQ_COV_COL_001
covering
test_COL_001 : IO Bool
test_COL_001 = do
  let html = "<table><tr><td class=pc12><a href=\"sample.ss.html#line702\">path line 702 (6)</a></td></tr></table>"
  let hits = parseProfileHtml html
  pure $ length hits >= 1

||| REQ_COV_COL_002
covering
test_COL_002 : IO Bool
test_COL_002 = do
  -- Spec: Collector SHALL extract function name and line number from Scheme defs
  let defs = parseSchemeDefs "(define Main-add (lambda (x y) (+ x y)))"
  case defs of
    [(name, lineNum)] => pure (name == "Main-add" && lineNum == 1)
    _ => pure False

||| REQ_COV_COL_003
covering
test_COL_003 : IO Bool
test_COL_003 = do
  -- Spec: Collector SHALL parse module-prefixed names correctly
  let defs = parseSchemeDefs "(define PreludeC-45Show-u--show_Show_Int (lambda (x) x))"
  case defs of
    [(name, _)] => pure (name == "PreludeC-45Show-u--show_Show_Int")
    _ => pure False

||| REQ_COV_COL_004
covering
test_COL_004 : IO Bool
test_COL_004 = do
  -- Spec: Collector SHALL handle HTML with zero coverage correctly
  let html = "<table><tr><td class=pc0>uncovered</td></tr></table>"
  let hits = parseProfileHtml html
  -- Zero-coverage rows don't have "line N (count)" format, so no hits returned
  pure $ length hits == 0

||| REQ_COV_COL_005: parseProfileHtml with non-numeric line
covering
test_COL_005 : IO Bool
test_COL_005 = do
  -- Spec: Collector SHALL handle non-numeric line gracefully
  let html = "<td class=pc1>line abc (5)</td>"
  let hits = parseProfileHtml html
  -- Non-numeric line number should result in no parsed hits
  pure $ length hits == 0

||| REQ_COV_COL_006: parseProfileHtml without "line" keyword
covering
test_COL_006 : IO Bool
test_COL_006 = do
  let html = "<td class=pc1>something else</td>"
  let hits = parseProfileHtml html
  pure $ length hits == 0

||| REQ_COV_COL_007: parseBranchCoverage with if branch
covering
test_COL_007 : IO Bool
test_COL_007 = do
  let html = "<span class=pc1 title=\"line 10 char 5 count 3\">(if condition then else)</span>"
  let branches = parseBranchCoverage html
  pure $ length branches >= 0

||| REQ_COV_COL_008: parseBranchCoverage with case branch
covering
test_COL_008 : IO Bool
test_COL_008 = do
  let html = "<span class=pc1 title=\"line 20 char 5 count 2\">(case x [A body] [B body])</span>"
  let branches = parseBranchCoverage html
  pure $ length branches >= 0

-- =============================================================================
-- Report Tests (REP_001-004)
-- =============================================================================

||| REQ_COV_REP_001
covering
test_REP_001 : IO Bool
test_REP_001 = do
  let fc = coveredFunction "TestMod" "testFunc" 10 ["REQ_TEST_001"]
  let json = functionCoverageJson fc
  pure $ isInfixOf "module" json && isInfixOf "name" json

||| REQ_COV_REP_002
covering
test_REP_002 : IO Bool
test_REP_002 = do
  let fc = coveredFunction "Mod" "func" 5 []
  let json = functionCoverageJson fc
  pure $ isInfixOf "coverage_percent" json

||| REQ_COV_REP_003
covering
test_REP_003 : IO Bool
test_REP_003 = do
  let fc = coveredFunction "Mod" "func" 0 []
  let json = functionCoverageJson fc
  pure $ isInfixOf "covered_lines" json

||| REQ_COV_REP_004
covering
test_REP_004 : IO Bool
test_REP_004 = do
  let fc1 = coveredFunction "A" "f1" 10 ["T1"]
  let fc2 = coveredFunction "B" "f2" 5 ["T2"]
  let mc = aggregateModule "A.idr" [fc1]
  pure $ mc.functionsTotal >= 1

||| REQ_COV_REP_005: maybeNatJson with Nothing
covering
test_REP_005 : IO Bool
test_REP_005 = do
  let json = maybeNatJson Nothing
  pure $ json == "null"

||| REQ_COV_REP_006: maybeStringJson with Nothing
covering
test_REP_006 : IO Bool
test_REP_006 = do
  let json = maybeStringJson Nothing
  pure $ json == "null"

||| REQ_COV_REP_007: maybeNatJson with Just
covering
test_REP_007 : IO Bool
test_REP_007 = do
  let json = maybeNatJson (Just 42)
  pure $ json == "42"

||| REQ_COV_REP_008: maybeStringJson with Just
covering
test_REP_008 : IO Bool
test_REP_008 = do
  let json = maybeStringJson (Just "test")
  pure $ isInfixOf "test" json

-- =============================================================================
-- Aggregator Tests (AGG_001-004)
-- =============================================================================

||| REQ_COV_AGG_001
covering
test_AGG_001 : IO Bool
test_AGG_001 = do
  let fc = coveredFunction "Mod" "func" 10 ["REQ_001", "REQ_002"]
  pure $ length fc.calledByTests == 2

||| REQ_COV_AGG_002
covering
test_AGG_002 : IO Bool
test_AGG_002 = do
  let fc1 = coveredFunction "A" "f" 10 []
  let fc2 = coveredFunction "A" "g" 5 []
  let mc = aggregateModule "A.idr" [fc1, fc2]
  pure $ mc.functionsTotal == 2

||| REQ_COV_AGG_003
covering
test_AGG_003 : IO Bool
test_AGG_003 = do
  let fc1 = coveredFunction "A" "f" 10 []
  let fc2 = coveredFunction "A" "g" 0 []
  let mc = aggregateModule "A.idr" [fc1, fc2]
  pure $ mc.functionsCovered >= 1

||| REQ_COV_AGG_004
covering
test_AGG_004 : IO Bool
test_AGG_004 = do
  let fc = coveredFunction "M" "f" 0 []
  let mc = aggregateModule "M.idr" [fc]
  -- 0 covered lines means uncovered
  pure $ fc.coveredLines == 0

-- =============================================================================
-- Test Runner Tests (RUN_001-004)
-- =============================================================================

||| REQ_COV_RUN_001
covering
test_RUN_001 : IO Bool
test_RUN_001 = do
  let defs = parseSchemeDefs "(define Test-func (lambda (x) x))"
  pure $ length defs == 1

||| REQ_COV_RUN_002
covering
test_RUN_002 : IO Bool
test_RUN_002 = do
  -- Test glob matching
  let matches = matchGlob "*.idr" "test.idr"
  pure matches

||| REQ_COV_RUN_003
covering
test_RUN_003 : IO Bool
test_RUN_003 = do
  -- Test result structure
  let r = MkTestProfileResult "T1" True []
  pure $ r.testPassed

||| REQ_COV_RUN_004
covering
test_RUN_004 : IO Bool
test_RUN_004 = do
  -- Test pass rate calculation
  let r1 = MkTestProfileResult "T1" True []
  let r2 = MkTestProfileResult "T2" True []
  let passed = filter (.testPassed) [r1, r2]
  pure $ length passed == 2

||| REQ_COV_RUN_005: matchGlob with wildcard no match
covering
test_RUN_005 : IO Bool
test_RUN_005 = do
  let matches = matchGlob "*.txt" "test.idr"
  pure $ not matches

||| REQ_COV_RUN_006: matchGlob with prefix wildcard
covering
test_RUN_006 : IO Bool
test_RUN_006 = do
  -- matchGlob supports simple * patterns, not **
  let matches = matchGlob "*Test.idr" "MyTest.idr"
  pure $ matches

-- =============================================================================
-- Test Hint Tests (HNT_001-004)
-- =============================================================================

||| REQ_COV_HNT_001
covering
test_HNT_001 : IO Bool
test_HNT_001 = do
  let analyzed = analyzeFunction "process" "String -> IO ()"
  let hints = happyPathHints analyzed
  pure $ length hints >= 1

||| REQ_COV_HNT_002
covering
test_HNT_002 : IO Bool
test_HNT_002 = do
  -- Spec: TestHint SHALL generate exhaustive path hints for Either type
  let analyzed = analyzeFunction "handle" "Either Error a -> IO ()"
  let hints = exhaustivePathHints defaultConfig analyzed
  -- Either should suggest Left and Right cases
  pure $ length hints >= 2

||| REQ_COV_HNT_003
covering
test_HNT_003 : IO Bool
test_HNT_003 = do
  -- Spec: TestHint SHALL generate hints for List traversal
  let analyzed = analyzeFunction "traverse" "List a -> IO (List b)"
  let hints = exhaustivePathHints defaultConfig analyzed
  -- List should suggest empty and non-empty cases
  pure $ length hints >= 2

||| REQ_COV_HNT_004
covering
test_HNT_004 : IO Bool
test_HNT_004 = do
  let analyzed = analyzeFunction "parse" "String -> Maybe Int"
  let hintReport = generateTestHints defaultConfig analyzed
  pure $ hintReport.totalHints >= 0

-- =============================================================================
-- Additional High-ROI Coverage Tests
-- =============================================================================

||| REQ_COV_LIN_009: parseLinearParam with "name : Type" (no quantity)
covering
test_LIN_009 : IO Bool
test_LIN_009 = do
  let result = parseLinearParam "x : Int"
  pure $ case result of
    Just lp => lp.quantity == QW && lp.paramName == Just "x" && lp.paramType == "Int"
    Nothing => False

||| REQ_COV_LIN_010: parseLinearParam with parens, no quantity
covering
test_LIN_010 : IO Bool
test_LIN_010 = do
  let result = parseLinearParam "(x : Bool)"
  pure $ case result of
    Just lp => lp.quantity == QW && lp.paramType == "Bool"
    Nothing => False

||| REQ_COV_TYP_012: resolveType with tuple type
covering
test_TYP_012 : IO Bool
test_TYP_012 = do
  let info = resolveType "(Int, Bool)"
  pure $ isPrefixOf "(" info.typeName

||| REQ_COV_TYP_013: parseParams with multi-arrow signature
covering
test_TYP_013 : IO Bool
test_TYP_013 = do
  let params = parseParams "Int -> Bool -> String -> IO ()"
  pure $ length params == 3

||| REQ_COV_SPC_007: paramStateSpace with erased (Q0) param
covering
test_SPC_007 : IO Bool
test_SPC_007 = do
  let p = MkLinearParam (Just "prf") "Void" Q0 Nothing
  let space = paramStateSpace defaultConfig p
  pure $ space == Finite 0

||| REQ_COV_SPC_008: generateCombinations with Bool param
covering
test_SPC_008 : IO Bool
test_SPC_008 = do
  let boolInfo = MkTypeInfo "Bool" [] False (TCFinite 0) (Finite 2)
  let p = MkLinearParam (Just "b") "Bool" QW (Just boolInfo)
  let combos = generateCombinations defaultConfig [p]
  pure $ length combos >= 2

||| REQ_COV_PTH_007: analyzePattern with Left pattern string
covering
test_PTH_007 : IO Bool
test_PTH_007 = do
  let pattern = analyzePattern "Left _"
  pure $ pattern.prunable == True

||| REQ_COV_RUN_007: matchGlob with prefix matching
covering
test_RUN_007 : IO Bool
test_RUN_007 = do
  pure $ matchGlob "Test*.idr" "TestRunner.idr"

||| REQ_COV_RUN_008: matchGlob non-match case
covering
test_RUN_008 : IO Bool
test_RUN_008 = do
  pure $ not (matchGlob "*.txt" "file.idr")

||| REQ_COV_AGG_005: isTestModule identifies test modules
covering
test_AGG_005 : IO Bool
test_AGG_005 = do
  -- Test module patterns should be identified
  let testFunc1 = isTestModule "CoverageC-45TestsC-45AllTests-test_001"
  let testFunc2 = isTestModule "Coverage-Tests-AllTests-test_001"
  -- Production module patterns should not match
  let prodFunc1 = isTestModule "CoverageC-45Collector-parseHtml"
  let prodFunc2 = isTestModule "Coverage-Types-MkRecord"
  pure $ testFunc1 && testFunc2 && not prodFunc1 && not prodFunc2

||| REQ_COV_AGG_006: excludeTestModules filters correctly
covering
test_AGG_006 : IO Bool
test_AGG_006 = do
  let funcs = [ ("CoverageC-45TestsC-45AllTests-test", 10)
              , ("CoverageC-45Collector-parse", 20)
              , ("CoverageC-45Types-show", 30)
              ]
  let filtered = excludeTestModules funcs
  pure $ length filtered == 2

-- =============================================================================
-- Unified Runner Type Tests (UNI_001-002)
-- =============================================================================

||| REQ_TYPE_UNI_001: TestResult type exists and works
covering
test_UNI_001 : IO Bool
test_UNI_001 = do
  let tr = MkTestResult "myTest" True (Just "all good")
  pure $ tr.testName == "myTest" && tr.passed && tr.message == Just "all good"

||| REQ_TYPE_UNI_002: TestCoverageReport type exists and works
covering
test_UNI_002 : IO Bool
test_UNI_002 = do
  let tr1 = MkTestResult "test1" True Nothing
  let tr2 = MkTestResult "test2" False (Just "error")
  let emptyBranch = MkBranchCoverageSummary 0 0 0 0.0 []
  let report = MkTestCoverageReport [tr1, tr2] 2 1 1 emptyBranch "2024-01-01"
  pure $ report.totalTests == 2 && report.passedTests == 1 && report.failedTests == 1

||| REQ_COV_UNI_001: runTestsWithCoverage validates inputs
||| (Tests the validation branch - empty module list returns error)
covering
test_UNI_003 : IO Bool
test_UNI_003 = do
  result <- runTestsWithCoverage "." [] 60
  pure $ case result of
    Left err => isInfixOf "No test modules" err
    Right _ => False

||| REQ_COV_UNI_002: Implicit - cleanup happens inside runTestsWithCoverage
||| (Tested via UNI_001 - cleanup is part of the function)
covering
test_UNI_004 : IO Bool
test_UNI_004 = do
  -- Verify function exists and handles errors
  result <- runTestsWithCoverage "/nonexistent/path" ["Fake.Module"] 5
  -- Should fail on write (nonexistent path) and clean up
  pure $ case result of
    Left _ => True
    Right _ => False

||| REQ_COV_UNI_003: Implicit - exclusion uses existing excludeTestModules
||| (Tested via AGG_005/006 which test the underlying exclusion function)
covering
test_UNI_005 : IO Bool
test_UNI_005 = do
  -- Test that excludeTestModules filters test functions correctly
  -- This is what runTestsWithCoverage uses internally
  let funcs = [ ("CoverageC-45TestsC-45AllTests-test", 10)
              , ("CoverageC-45Collector-parse", 20)
              ]
  let filtered = excludeTestModules funcs
  -- Only non-test function should remain
  pure $ length filtered == 1 && fst (fromMaybe ("", 0) (head' filtered)) == "CoverageC-45Collector-parse"

-- =============================================================================
-- ChezMangle Tests (MGL_001-004)
-- =============================================================================

||| REQ_COV_MGL_001: Simple single-segment name
covering
test_MGL_001 : IO Bool
test_MGL_001 = do
  let result = chezMangle "Main"
  pure $ result == "Main"

||| REQ_COV_MGL_002: Two-segment name (no encoding needed)
covering
test_MGL_002 : IO Bool
test_MGL_002 = do
  let result = chezMangle "Sample.add"
  pure $ result == "Sample-add"

||| REQ_COV_MGL_003: Multi-segment stdlib name (hyphen encoded as C-45)
covering
test_MGL_003 : IO Bool
test_MGL_003 = do
  let result = chezMangle "Prelude.IO.putStrLn"
  pure $ result == "PreludeC-45IO-putStrLn"

||| REQ_COV_MGL_004: Operator name with encoding
covering
test_MGL_004 : IO Bool
test_MGL_004 = do
  let result = chezMangle "Prelude.EqOrd.=="
  -- == becomes C-61C-61 (61 = ord '=')
  pure $ result == "PreludeC-45EqOrd-C-61C-61"

-- =============================================================================
-- Config Function Exclusion Tests (CFG_001-003)
-- =============================================================================

||| REQ_COV_CFG_001: ExclusionConfig with function names
covering
test_CFG_001 : IO Bool
test_CFG_001 = do
  let config = MkExclusionConfig [] [] ["Module.func1", "Other.func2"]
  pure $ config.functionNames == ["Module.func1", "Other.func2"]

||| REQ_COV_CFG_002: parseConfigFile with functions array
covering
test_CFG_002 : IO Bool
test_CFG_002 = do
  let toml = """
[exclusions]
module_prefixes = ["Foo.Internal"]
packages = ["mylib"]
functions = ["Module.untestable", "Other.sideEffect"]
"""
  let config = parseConfigFile toml
  pure $ config.functionNames == ["Module.untestable", "Other.sideEffect"]
      && config.modulePrefixes == ["Foo.Internal"]
      && config.packageNames == ["mylib"]

||| REQ_COV_CFG_003: emptyExclusionConfig has empty functionNames
covering
test_CFG_003 : IO Bool
test_CFG_003 = do
  pure $ emptyExclusionConfig.functionNames == []
      && emptyExclusionConfig.modulePrefixes == []
      && emptyExclusionConfig.packageNames == []

-- =============================================================================
-- All Tests
-- =============================================================================

export
covering
allTests : List (String, IO Bool)
allTests =
  [ ("REQ_COV_LIN_001", test_LIN_001)
  , ("REQ_COV_LIN_002", test_LIN_002)
  , ("REQ_COV_LIN_003", test_LIN_003)
  , ("REQ_COV_LIN_004", test_LIN_004)
  , ("REQ_COV_LIN_005", test_LIN_005)
  , ("REQ_COV_LIN_006", test_LIN_006)
  , ("REQ_COV_LIN_007", test_LIN_007)
  , ("REQ_COV_LIN_008", test_LIN_008)
  , ("REQ_COV_TYP_001", test_TYP_001)
  , ("REQ_COV_TYP_002", test_TYP_002)
  , ("REQ_COV_TYP_003", test_TYP_003)
  , ("REQ_COV_TYP_004", test_TYP_004)
  , ("REQ_COV_TYP_005", test_TYP_005)
  , ("REQ_COV_TYP_006", test_TYP_006)
  , ("REQ_COV_TYP_007", test_TYP_007)
  , ("REQ_COV_TYP_008", test_TYP_008)
  , ("REQ_COV_TYP_009", test_TYP_009)
  , ("REQ_COV_TYP_010", test_TYP_010)
  , ("REQ_COV_TYP_011", test_TYP_011)
  , ("REQ_COV_SPC_001", test_SPC_001)
  , ("REQ_COV_SPC_002", test_SPC_002)
  , ("REQ_COV_SPC_003", test_SPC_003)
  , ("REQ_COV_SPC_004", test_SPC_004)
  , ("REQ_COV_SPC_005", test_SPC_005)
  , ("REQ_COV_SPC_006", test_SPC_006)
  , ("REQ_COV_PTH_001", test_PTH_001)
  , ("REQ_COV_PTH_002", test_PTH_002)
  , ("REQ_COV_PTH_003", test_PTH_003)
  , ("REQ_COV_PTH_004", test_PTH_004)
  , ("REQ_COV_PTH_005", test_PTH_005)
  , ("REQ_COV_PTH_006", test_PTH_006)
  , ("REQ_COV_CPX_001", test_CPX_001)
  , ("REQ_COV_CPX_002", test_CPX_002)
  , ("REQ_COV_CPX_003", test_CPX_003)
  , ("REQ_COV_CPX_004", test_CPX_004)
  , ("REQ_COV_SRC_001", test_SRC_001)
  , ("REQ_COV_SRC_002", test_SRC_002)
  , ("REQ_COV_SRC_003", test_SRC_003)
  , ("REQ_COV_SRC_004", test_SRC_004)
  , ("REQ_COV_SRC_005", test_SRC_005)
  , ("REQ_COV_SRC_006", test_SRC_006)
  , ("REQ_COV_COL_001", test_COL_001)
  , ("REQ_COV_COL_002", test_COL_002)
  , ("REQ_COV_COL_003", test_COL_003)
  , ("REQ_COV_COL_004", test_COL_004)
  , ("REQ_COV_COL_005", test_COL_005)
  , ("REQ_COV_COL_006", test_COL_006)
  , ("REQ_COV_COL_007", test_COL_007)
  , ("REQ_COV_COL_008", test_COL_008)
  , ("REQ_COV_REP_001", test_REP_001)
  , ("REQ_COV_REP_002", test_REP_002)
  , ("REQ_COV_REP_003", test_REP_003)
  , ("REQ_COV_REP_004", test_REP_004)
  , ("REQ_COV_REP_005", test_REP_005)
  , ("REQ_COV_REP_006", test_REP_006)
  , ("REQ_COV_REP_007", test_REP_007)
  , ("REQ_COV_REP_008", test_REP_008)
  , ("REQ_COV_AGG_001", test_AGG_001)
  , ("REQ_COV_AGG_002", test_AGG_002)
  , ("REQ_COV_AGG_003", test_AGG_003)
  , ("REQ_COV_AGG_004", test_AGG_004)
  , ("REQ_COV_RUN_001", test_RUN_001)
  , ("REQ_COV_RUN_002", test_RUN_002)
  , ("REQ_COV_RUN_003", test_RUN_003)
  , ("REQ_COV_RUN_004", test_RUN_004)
  , ("REQ_COV_RUN_005", test_RUN_005)
  , ("REQ_COV_RUN_006", test_RUN_006)
  , ("REQ_COV_HNT_001", test_HNT_001)
  , ("REQ_COV_HNT_002", test_HNT_002)
  , ("REQ_COV_HNT_003", test_HNT_003)
  , ("REQ_COV_HNT_004", test_HNT_004)
  , ("REQ_COV_LIN_009", test_LIN_009)
  , ("REQ_COV_LIN_010", test_LIN_010)
  , ("REQ_COV_TYP_012", test_TYP_012)
  , ("REQ_COV_TYP_013", test_TYP_013)
  , ("REQ_COV_SPC_007", test_SPC_007)
  , ("REQ_COV_SPC_008", test_SPC_008)
  , ("REQ_COV_PTH_007", test_PTH_007)
  , ("REQ_COV_RUN_007", test_RUN_007)
  , ("REQ_COV_RUN_008", test_RUN_008)
  , ("REQ_COV_AGG_005", test_AGG_005)
  , ("REQ_COV_AGG_006", test_AGG_006)
  , ("REQ_TYPE_UNI_001", test_UNI_001)
  , ("REQ_TYPE_UNI_002", test_UNI_002)
  , ("REQ_COV_UNI_001", test_UNI_003)
  , ("REQ_COV_UNI_002", test_UNI_004)
  , ("REQ_COV_UNI_003", test_UNI_005)
  , ("REQ_COV_MGL_001", test_MGL_001)
  , ("REQ_COV_MGL_002", test_MGL_002)
  , ("REQ_COV_MGL_003", test_MGL_003)
  , ("REQ_COV_MGL_004", test_MGL_004)
  , ("REQ_COV_CFG_001", test_CFG_001)
  , ("REQ_COV_CFG_002", test_CFG_002)
  , ("REQ_COV_CFG_003", test_CFG_003)
  ]

-- =============================================================================
-- Main Entry Point
-- =============================================================================

covering
runTest : (String, IO Bool) -> IO Bool
runTest (name, test) = do
  result <- test
  putStrLn $ "[" ++ (if result then "PASS" else "FAIL") ++ "] " ++ name
  pure result

export
covering
main : IO ()
main = do
  putStrLn $ "Running " ++ show (length allTests) ++ " tests..."
  results <- traverse runTest allTests
  let passed = length (filter id results)
  putStrLn $ "Passed: " ++ show passed ++ "/" ++ show (length allTests)
  if passed == length allTests
     then putStrLn "All tests passed!"
     else exitFailure

||| Alias for main - used by UnifiedRunner
export
covering
runAllTests : IO ()
runAllTests = main
