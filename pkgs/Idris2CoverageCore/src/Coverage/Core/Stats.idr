||| Coverage Statistics Types
|||
||| Shared statistics types for coverage analysis across backends.
||| Provides consistent reporting structure for:
|||   - Function-level coverage
|||   - Project-level aggregation
|||   - Exclusion breakdown
module Coverage.Core.Stats

import Data.List
import Data.String

import Coverage.Core.Types
import Coverage.Core.Exclusions
import Coverage.Classification.BranchClass

%default total

-- =============================================================================
-- Dumpcases Statistics
-- =============================================================================

||| Summary statistics for dumpcases analysis
public export
record DumpcasesStats where
  constructor MkDumpcasesStats
  totalFunctions : Nat
  totalCaseExprs : Nat
  totalBranches : Nat
  excludedFuncs : Nat
  includedFuncs : Nat

public export
Show DumpcasesStats where
  show ds = "Functions: " ++ show ds.totalFunctions ++
            " (included: " ++ show ds.includedFuncs ++
            ", excluded: " ++ show ds.excludedFuncs ++ ")" ++
            ", Cases: " ++ show ds.totalCaseExprs ++
            ", Branches: " ++ show ds.totalBranches

public export
Eq DumpcasesStats where
  a == b = a.totalFunctions == b.totalFunctions &&
           a.totalBranches == b.totalBranches

-- =============================================================================
-- Annotated Function Cases
-- =============================================================================

||| Function cases with exclusion annotation
public export
record AnnotatedFuncCases where
  constructor MkAnnotatedFuncCases
  funcCases : FuncCases
  exclusionReason : ExclusionReason

public export
Show AnnotatedFuncCases where
  show afc =
    let base = show afc.funcCases
    in case afc.exclusionReason of
         NotExcluded => base
         reason => base ++ " [" ++ show reason ++ "]"

||| Check if annotated function is excluded
public export
isAnnotatedExcluded : AnnotatedFuncCases -> Bool
isAnnotatedExcluded afc = isExcludedReason afc.exclusionReason

-- =============================================================================
-- Exclusion Breakdown
-- =============================================================================

||| Detailed breakdown of excluded branches by category
public export
record ExclusionBreakdown where
  constructor MkExclusionBreakdown
  compilerGenerated : Nat  -- {csegen:N}, _builtin.*, prim__*
  standardLibrary : Nat    -- Prelude.*, System.*, Data.*, etc.
  typeConstructors : Nat   -- Names ending with "."
  dependencies : Nat       -- User-specified packages/modules
  testCode : Nat           -- *.Tests.*, test_* (test code, not coverage target)

public export
Show ExclusionBreakdown where
  show b = "compiler=" ++ show b.compilerGenerated ++
           ", stdlib=" ++ show b.standardLibrary ++
           ", typeCtor=" ++ show b.typeConstructors ++
           ", deps=" ++ show b.dependencies ++
           ", test=" ++ show b.testCode

public export
totalExcludedFromDenom : ExclusionBreakdown -> Nat
totalExcludedFromDenom b =
  b.compilerGenerated + b.standardLibrary + b.typeConstructors +
  b.dependencies + b.testCode

-- =============================================================================
-- Test Analysis
-- =============================================================================

||| Summary of test coverage analysis
|||
||| Breakdown based on dunham's classification:
|||   - totalCanonical: Reachable cases (denominator)
|||   - totalExcluded: NoClauses (void etc) - excluded
|||   - totalBugs: UnhandledInput (partial) - coverage gaps
|||   - totalOptimizerArtifacts: Nat case not covered - warnings
|||   - totalUnknown: Unknown CRASHes - investigate
public export
record TestAnalysis where
  constructor MkTestAnalysis
  totalFunctions : Nat
  totalCanonical : Nat              -- Reachable cases (denominator)
  totalExcluded : Nat               -- NoClauses (void etc)
  totalBugs : Nat                   -- UnhandledInput (partial)
  totalOptimizerArtifacts : Nat     -- Nat case not covered
  totalUnknown : Nat                -- Unknown CRASHes
  exclusionBreakdown : ExclusionBreakdown
  functionsWithCrash : Nat

public export
Show TestAnalysis where
  show a = unlines
    [ "Test Coverage Analysis:"
    , "  Functions analyzed: " ++ show a.totalFunctions
    , "  Canonical cases (denominator): " ++ show a.totalCanonical
    , "  Excluded (NoClauses): " ++ show a.totalExcluded
    , "  Bugs (UnhandledInput): " ++ show a.totalBugs
    , "  Optimizer artifacts: " ++ show a.totalOptimizerArtifacts
    , "  Unknown CRASHes: " ++ show a.totalUnknown
    , "  Functions with CRASH: " ++ show a.functionsWithCrash
    ]

-- =============================================================================
-- Test Coverage
-- =============================================================================

||| Test coverage record for a function or project
public export
record TestCoverage where
  constructor MkTestCoverage
  name : String
  totalCanonical : Nat    -- Denominator
  totalExcluded : Nat     -- Reference value (not in denominator)
  executedCanonical : Nat -- Numerator (branches hit at runtime)

public export
Show TestCoverage where
  show tc = tc.name ++ ": " ++ show tc.executedCanonical ++
            "/" ++ show tc.totalCanonical ++ " test coverage"

||| Calculate test coverage percentage
public export
testCoveragePercent : TestCoverage -> Double
testCoveragePercent tc =
  if tc.totalCanonical == 0
  then 100.0  -- All excluded → 100%
  else cast tc.executedCanonical / cast tc.totalCanonical * 100.0

-- =============================================================================
-- Coverage Report
-- =============================================================================

||| Complete coverage report combining static analysis and runtime data
public export
record CoverageReport where
  constructor MkCoverageReport
  staticAnalysis : StaticBranchAnalysis
  testCoverage : TestCoverage
  analysis : TestAnalysis

public export
Show CoverageReport where
  show cr = "Coverage: " ++ show (testCoveragePercent cr.testCoverage) ++ "% " ++
            "(" ++ show cr.testCoverage.executedCanonical ++ "/" ++
            show cr.testCoverage.totalCanonical ++ " canonical branches)"

-- =============================================================================
-- Utility Functions
-- =============================================================================

||| Create empty DumpcasesStats
public export
emptyDumpcasesStats : DumpcasesStats
emptyDumpcasesStats = MkDumpcasesStats 0 0 0 0 0

||| Create empty ExclusionBreakdown
public export
emptyExclusionBreakdown : ExclusionBreakdown
emptyExclusionBreakdown = MkExclusionBreakdown 0 0 0 0 0

||| Create empty TestAnalysis
public export
emptyTestAnalysis : TestAnalysis
emptyTestAnalysis = MkTestAnalysis 0 0 0 0 0 0 emptyExclusionBreakdown 0

||| Calculate coverage percentage safely
public export
calcCoveragePercent : Nat -> Nat -> Double
calcCoveragePercent coveredCount totalCount =
  if totalCount == 0
  then 100.0
  else cast coveredCount / cast totalCount * 100.0
