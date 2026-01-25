||| Coverage Result Types
|||
||| Shared coverage result types for all Idris2 coverage backends:
|||   - Chez Scheme (idris2-coverage)
|||   - WASM/ICP (idris2-dfx-coverage)
|||   - WASM/EVM (idris2-evm-coverage)
|||
||| Provides a unified API for coverage analysis results.
module Coverage.Core.Result

import Data.List
import Data.String

%default total

-- =============================================================================
-- Coverage Result
-- =============================================================================

||| Result of coverage analysis
||| Unified type for all backends (EVM, DFX/ICP, Chez)
public export
record CoverageResult where
  constructor MkCoverageResult
  ||| Target functions (denominator: dumpcases - exclusions)
  targetFunctions : List String
  ||| Functions that were executed during tests
  coveredFunctions : List String
  ||| Functions NOT covered (High Impact Targets)
  uncoveredFunctions : List String
  ||| Coverage percentage (coveredCount / totalCount * 100)
  coveragePercent : Double
  ||| Number of covered functions
  coveredCount : Nat
  ||| Total number of target functions
  totalCount : Nat

||| Join strings with newlines
joinLines : List String -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x :: xs) = x ++ "\n" ++ joinLines xs

public export
Show CoverageResult where
  show r = joinLines
    [ "=== Coverage Result ==="
    , "Coverage: " ++ show r.coveredCount ++ "/" ++ show r.totalCount ++
      " (" ++ show r.coveragePercent ++ "%)"
    , ""
    , "Covered functions:"
    , joinLines (map ("  + " ++) r.coveredFunctions)
    , ""
    , "Uncovered functions (High Impact Targets):"
    , joinLines (map ("  - " ++) r.uncoveredFunctions)
    ]

public export
Eq CoverageResult where
  a == b = a.targetFunctions == b.targetFunctions
        && a.coveredFunctions == b.coveredFunctions
        && a.uncoveredFunctions == b.uncoveredFunctions
        && a.coveredCount == b.coveredCount
        && a.totalCount == b.totalCount

-- =============================================================================
-- Builder Functions
-- =============================================================================

||| Build coverage result from target and executed function lists
|||
||| @targets    Target functions (denominator)
||| @executed   Functions that were executed
public export
buildCoverageResult : (targets : List String) -> (executed : List String) -> CoverageResult
buildCoverageResult targets executed =
  let covered = filter (\t => elem t executed) targets
      uncovered = filter (\t => not (elem t executed)) targets
      totalCnt = length targets
      covCnt = length covered
      pct = if totalCnt == 0 then 100.0 else (cast covCnt / cast totalCnt) * 100.0
  in MkCoverageResult targets covered uncovered pct covCnt totalCnt

||| Empty coverage result (no targets)
public export
emptyCoverageResult : CoverageResult
emptyCoverageResult = MkCoverageResult [] [] [] 100.0 0 0

-- =============================================================================
-- Utility Functions
-- =============================================================================

||| Get high impact targets (uncovered functions)
|||
||| @result  Coverage result
||| @limit   Maximum number to return
public export
getHighImpactTargets : CoverageResult -> Nat -> List String
getHighImpactTargets result limit = take limit result.uncoveredFunctions

||| Check if coverage meets threshold
|||
||| @result     Coverage result
||| @threshold  Minimum coverage percentage required
public export
meetsCoverageThreshold : CoverageResult -> Double -> Bool
meetsCoverageThreshold result threshold = result.coveragePercent >= threshold

||| Merge multiple coverage results (OR aggregation)
|||
||| When the same function is covered in any run, it counts as covered.
||| Target functions are unioned.
public export
mergeCoverageResults : List CoverageResult -> CoverageResult
mergeCoverageResults [] = emptyCoverageResult
mergeCoverageResults results =
  let allTargets = nub $ concatMap (.targetFunctions) results
      allCovered = nub $ concatMap (.coveredFunctions) results
  in buildCoverageResult allTargets allCovered

-- =============================================================================
-- Coverage Severity
-- =============================================================================

||| Severity level for coverage gaps
public export
data CoverageSeverity = Critical | High | Medium | Low

public export
Show CoverageSeverity where
  show Critical = "critical"
  show High = "high"
  show Medium = "medium"
  show Low = "low"

public export
Eq CoverageSeverity where
  Critical == Critical = True
  High == High = True
  Medium == Medium = True
  Low == Low = True
  _ == _ = False

public export
Ord CoverageSeverity where
  compare Critical Critical = EQ
  compare Critical _ = GT
  compare _ Critical = LT
  compare High High = EQ
  compare High _ = GT
  compare _ High = LT
  compare Medium Medium = EQ
  compare Medium _ = GT
  compare _ Medium = LT
  compare Low Low = EQ

||| Determine severity based on coverage percentage
public export
coverageSeverity : Double -> CoverageSeverity
coverageSeverity pct =
  if pct < 50.0 then Critical
  else if pct < 70.0 then High
  else if pct < 90.0 then Medium
  else Low
