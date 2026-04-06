||| Per-Function Runtime Coverage Data
|||
||| Shared type between CLI and Library API for accurate severity calculation.
||| This enables both Chez Scheme and WASM/ICP backends to use the same
||| runtime hit tracking interface.
|||
||| Design rationale:
|||   - CLI and Library API need the same per-function executed counts
|||   - Previously: CLI used proportional approximation, Library API used 0
|||   - Now: Both use actual profiler data via this shared type
module Coverage.Core.RuntimeHit

import Data.List

%default total

-- =============================================================================
-- FunctionRuntimeHit - Per-function runtime coverage
-- =============================================================================

||| Per-function runtime coverage data
||| This is the shared type between CLI and Library API for accurate severity calculation
|||
||| Fields:
|||   - funcName: Idris function name (e.g., "Main.dispatch")
|||   - schemeFunc: Backend-specific function name (Scheme for Chez, C for WASM)
|||   - canonicalCount: Static canonical branches from --dumpcases
|||   - executedCount: Runtime hit count from profiler
|||   - totalExprs: Expression count in function (from profiler)
|||   - coveredExprs: Covered expression count (from profiler)
public export
record FunctionRuntimeHit where
  constructor MkFunctionRuntimeHit
  funcName       : String
  schemeFunc     : String
  canonicalCount : Nat
  executedCount  : Nat
  totalExprs     : Nat
  coveredExprs   : Nat

public export
Show FunctionRuntimeHit where
  show h = h.funcName ++ ": " ++ show h.executedCount ++ "/" ++ show h.canonicalCount
        ++ " branches, " ++ show h.coveredExprs ++ "/" ++ show h.totalExprs ++ " exprs"

public export
Eq FunctionRuntimeHit where
  h1 == h2 = h1.funcName == h2.funcName && h1.schemeFunc == h2.schemeFunc

||| Calculate coverage percentage for a function
public export
functionRuntimeCoveragePercent : FunctionRuntimeHit -> Double
functionRuntimeCoveragePercent h =
  if h.canonicalCount == 0
  then 100.0
  else cast h.executedCount / cast h.canonicalCount * 100.0

||| Check if function has full coverage
public export
isFullyCovered : FunctionRuntimeHit -> Bool
isFullyCovered h = h.executedCount >= h.canonicalCount

||| Get uncovered branch count
public export
uncoveredCount : FunctionRuntimeHit -> Nat
uncoveredCount h =
  if h.canonicalCount > h.executedCount
  then h.canonicalCount `minus` h.executedCount
  else 0

-- =============================================================================
-- PathRuntimeHit - Per-path runtime coverage
-- =============================================================================

||| Runtime hit for a stable compiler-exported path identifier.
public export
record PathRuntimeHit where
  constructor MkPathRuntimeHit
  pathId   : String
  hitCount : Nat

public export
Show PathRuntimeHit where
  show h = h.pathId ++ ": " ++ show h.hitCount

public export
Eq PathRuntimeHit where
  h1 == h2 = h1.pathId == h2.pathId

||| Check whether a path was covered at least once.
public export
isPathCovered : PathRuntimeHit -> Bool
isPathCovered h = h.hitCount > 0

||| Extract the covered path ids from runtime hits.
public export
coveredPathIds : List PathRuntimeHit -> List String
coveredPathIds hits =
  nub $
    map (.pathId) $
      filter isPathCovered hits
