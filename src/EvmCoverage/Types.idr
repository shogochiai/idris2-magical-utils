||| Coverage type definitions for idris2-evm-coverage
||| Uses Chez Scheme profiler for EVM semantic coverage
module EvmCoverage.Types

import Data.List
import Data.List1
import Data.Maybe
import Data.String

-- Import shared types from coverage-core
import public Coverage.Core.HighImpact
import public Coverage.Core.RuntimeHit

%default total

-- =============================================================================
-- Branch Identification
-- =============================================================================

||| Unique identifier for a branch in dumpcases output
||| Format: "ModuleName.funcName:branchIdx"
public export
record BranchId where
  constructor MkBranchId
  moduleName : String
  funcName   : String
  branchIdx  : Nat

public export
Show BranchId where
  show b = "\{b.moduleName}.\{b.funcName}:\{show b.branchIdx}"

public export
Eq BranchId where
  b1 == b2 = b1.moduleName == b2.moduleName
          && b1.funcName == b2.funcName
          && b1.branchIdx == b2.branchIdx

public export
Ord BranchId where
  compare b1 b2 =
    case compare b1.moduleName b2.moduleName of
      EQ => case compare b1.funcName b2.funcName of
              EQ => compare b1.branchIdx b2.branchIdx
              x => x
      x => x

-- =============================================================================
-- Branch Classification (from dunham's taxonomy)
-- =============================================================================

||| Branch classification for semantic coverage
||| Based on Idris2 --dumpcases CRASH reason analysis
public export
data BranchClass : Type where
  ||| Reachable case - included in denominator
  BCCanonical : BranchClass
  ||| "No clauses in ..." - void/uninhabited, excluded from denominator
  BCExcludedNoClauses : BranchClass
  ||| "Unhandled input for ..." - partial code bug, flagged
  BCBugUnhandledInput : BranchClass
  ||| "Nat case not covered" - optimizer artifact, non-semantic
  BCOptimizerNat : BranchClass
  ||| Unknown CRASH - never exclude, show separately
  BCUnknownCrash : String -> BranchClass
  ||| Compiler-generated function, excluded
  BCCompilerGenerated : BranchClass

public export
Show BranchClass where
  show BCCanonical = "Canonical"
  show BCExcludedNoClauses = "ExcludedNoClauses"
  show BCBugUnhandledInput = "BugUnhandledInput"
  show BCOptimizerNat = "OptimizerNat"
  show (BCUnknownCrash msg) = "UnknownCrash(\{msg})"
  show BCCompilerGenerated = "CompilerGenerated"

public export
Eq BranchClass where
  BCCanonical == BCCanonical = True
  BCExcludedNoClauses == BCExcludedNoClauses = True
  BCBugUnhandledInput == BCBugUnhandledInput = True
  BCOptimizerNat == BCOptimizerNat = True
  BCUnknownCrash m1 == BCUnknownCrash m2 = m1 == m2
  BCCompilerGenerated == BCCompilerGenerated = True
  _ == _ = False

||| Is this branch class included in the coverage denominator?
public export
isCanonical : BranchClass -> Bool
isCanonical BCCanonical = True
isCanonical _ = False

||| Is this branch class a bug that should be fixed?
public export
isBug : BranchClass -> Bool
isBug BCBugUnhandledInput = True
isBug _ = False

-- =============================================================================
-- Classified Branch
-- =============================================================================

||| A branch with its classification
public export
record ClassifiedBranch where
  constructor MkClassifiedBranch
  branchId    : BranchId
  branchClass : BranchClass
  pattern     : String        -- Pattern description for debugging

public export
Show ClassifiedBranch where
  show cb = "Branch(\{show cb.branchId}, \{show cb.branchClass})"

public export
Eq ClassifiedBranch where
  b1 == b2 = b1.branchId == b2.branchId && b1.branchClass == b2.branchClass

-- =============================================================================
-- Profile Hit (from Chez Scheme profiler)
-- =============================================================================

||| Profile hit from Chez Scheme profiler HTML output
public export
record ProfileHit where
  constructor MkProfileHit
  schemeFunc : String    -- e.g., "EVMC-45Interpreter-executeOp"
  hitCount   : Nat
  filePath   : String
  line       : Nat

public export
Show ProfileHit where
  show h = "ProfileHit(\{h.schemeFunc}, \{show h.hitCount})"

public export
Eq ProfileHit where
  h1 == h2 = h1.schemeFunc == h2.schemeFunc
          && h1.hitCount == h2.hitCount
          && h1.line == h2.line

-- =============================================================================
-- Static Branch Analysis
-- =============================================================================

||| Result of static analysis (--dumpcases)
public export
record StaticBranchAnalysis where
  constructor MkStaticBranchAnalysis
  allBranches      : List ClassifiedBranch
  canonicalCount   : Nat    -- Denominator
  excludedCount    : Nat    -- NoClauses (void)
  bugsCount        : Nat    -- UnhandledInput
  optimizerCount   : Nat    -- Nat artifacts
  unknownCount     : Nat    -- Unknown crashes
  compilerGenCount : Nat    -- Compiler-generated

public export
Show StaticBranchAnalysis where
  show s = "StaticAnalysis(canonical=\{show s.canonicalCount}, bugs=\{show s.bugsCount})"

-- =============================================================================
-- Test Run Hits
-- =============================================================================

||| Hits from a single test run
public export
record TestRunHits where
  constructor MkTestRunHits
  testName   : String
  branchHits : List (BranchId, Nat)  -- (branchId, hitCount)

public export
Show TestRunHits where
  show t = "TestRunHits(\{t.testName}, \{show $ length t.branchHits} branches)"

-- =============================================================================
-- Aggregated Coverage
-- =============================================================================

||| Final aggregated coverage result
public export
record AggregatedCoverage where
  constructor MkAggregatedCoverage
  canonicalTotal   : Nat    -- Denominator
  canonicalHit     : Nat    -- Numerator (branches with hitCount > 0)
  bugsTotal        : Nat    -- Unhandled inputs (potential bugs)
  unknownTotal     : Nat    -- Unknown crashes to investigate
  coveragePercent  : Double
  uncoveredBranches : List ClassifiedBranch

public export
Show AggregatedCoverage where
  show a = "Coverage: \{show a.canonicalHit}/\{show a.canonicalTotal} (\{show a.coveragePercent}%)"

||| Calculate coverage percentage
public export
calcCoveragePercent : Nat -> Nat -> Double
calcCoveragePercent tot hit =
  if tot == 0
    then 100.0  -- No canonical branches means 100%
    else cast hit / cast tot * 100.0

-- =============================================================================
-- Coverage Gap (for reporting)
-- =============================================================================

||| A coverage gap to report
public export
record CoverageGap where
  constructor MkCoverageGap
  branchId    : BranchId
  reason      : String
  priority    : Nat         -- Higher = more important

public export
Show CoverageGap where
  show g = "Gap(\{show g.branchId}, \{g.reason}, priority=\{show g.priority})"

||| Convert ClassifiedBranch to CoverageGap if it's a bug
public export
branchToGap : ClassifiedBranch -> Maybe CoverageGap
branchToGap cb =
  case cb.branchClass of
    BCBugUnhandledInput => Just $ MkCoverageGap cb.branchId "Unhandled input" 10
    BCUnknownCrash msg  => Just $ MkCoverageGap cb.branchId msg 5
    _                   => Nothing

-- =============================================================================
-- High Impact Target (imported from Coverage.Core.HighImpact)
-- =============================================================================
-- HighImpactTarget, HighImpactKind, showSeverity, compareSeverity
-- are now imported from Coverage.Core.HighImpact

-- =============================================================================
-- JSON Output Types
-- =============================================================================

||| JSON-serializable coverage result
public export
record CoverageJson where
  constructor MkCoverageJson
  timestamp       : String
  target          : String
  canonicalTotal  : Nat
  canonicalHit    : Nat
  coveragePercent : Double
  bugs            : Nat
  unknown         : Nat
  gaps            : List String   -- Gap descriptions

public export
Show CoverageJson where
  show j = """
{
  "timestamp": "\{j.timestamp}",
  "target": "\{j.target}",
  "canonical_total": \{show j.canonicalTotal},
  "canonical_hit": \{show j.canonicalHit},
  "coverage_percent": \{show j.coveragePercent},
  "bugs": \{show j.bugs},
  "unknown": \{show j.unknown},
  "gaps": \{show j.gaps}
}
"""
