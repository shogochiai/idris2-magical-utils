||| Idris2 Function Coverage Analysis
|||
||| High-level API for calculating Idris2 function coverage from ic-wasm profiling.
||| This is the main entry point for LazyDfx and other consumers.
|||
||| Usage:
|||   result <- analyzeIdris2Coverage "canister_instrumented.wasm" profilingOutput targetFuncs
|||   case result of
|||     Right coverage => printLn coverage
|||     Left err => printLn $ "Error: " ++ err
module DfxCoverage.Idris2Coverage

import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import System.File

import DfxCoverage.IcWasm.IcpPublicNameParser
import DfxCoverage.IcWasm.ProfilingParser
import DfxCoverage.DumpcasesParser
import DfxCoverage.Exclusions

-- Re-export shared coverage result type
import public Coverage.Core.Result

%default covering

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Apply exclusion patterns to filter function names
||| Returns functions that are NOT excluded
applyExclusions : List ExclPattern -> List String -> List String
applyExclusions patterns funcs =
  Prelude.Types.List.filter (\f => isNothing (isMethodExcluded patterns f)) funcs

-- =============================================================================
-- Core Analysis Functions
-- =============================================================================

||| Analyze Idris2 function coverage
|||
||| @wasmPath       Path to ic-wasm instrumented WASM file
||| @profilingData  Raw output from `dfx canister call ... __get_profiling`
||| @targetFuncs    List of target function names (from dumpcases or source map)
||| @exclusions     Patterns to exclude from target functions
export
analyzeIdris2Coverage : String -> String -> List String -> List ExclPattern ->
                        IO (Either String CoverageResult)
analyzeIdris2Coverage wasmPath profilingData targetFuncs exclusions = do
  -- Step 1: Extract function name mapping from instrumented WASM
  Right funcNames <- extractIcpFuncNames wasmPath
    | Left err => pure $ Left $ "Failed to extract function names: " ++ err

  -- Step 2: Parse profiling data
  let profiling = parseProfilingOutput profilingData
  let executedIds = getExecutedFuncIds profiling

  -- Step 3: Resolve executed IDs to names
  let lookupName = \id => lookupFuncName id funcNames
  let executedNames = mapMaybe lookupName executedIds

  -- Step 4: Filter Idris functions only
  let idrisPrefixes = ["Main_", "PrimIO_", "Prelude_", "Data_", "Control_", "System_"]
  let idrisExecuted = filter (\n => any (\p => isPrefixOf p n) idrisPrefixes) executedNames

  -- Step 5: Apply exclusions to target functions
  let filteredTargets = applyExclusions exclusions targetFuncs

  -- Step 6: Calculate coverage using shared buildCoverageResult
  let normalizedExecuted = map normalizeFuncName idrisExecuted
  let normalizedTargets = map normalizeFuncName filteredTargets

  pure $ Right $ buildCoverageResult normalizedTargets normalizedExecuted

||| Analyze coverage from canister call (high-level convenience function)
|||
||| @wasmPath      Path to ic-wasm instrumented WASM file
||| @canisterId    Canister ID to query profiling from
||| @network       Network (local, ic, etc.)
||| @targetFuncs   List of target function names
||| @exclusions    Patterns to exclude
export
analyzeCanisterCoverage : String -> String -> String -> List String -> List ExclPattern ->
                          IO (Either String CoverageResult)
analyzeCanisterCoverage wasmPath canisterId network targetFuncs exclusions = do
  -- Get profiling data from canister
  Right profiling <- getProfilingData canisterId network
    | Left err => pure $ Left $ "Failed to get profiling data: " ++ err

  -- Convert to string format for analyzeIdris2Coverage
  let profilingStr = show profiling.entries

  -- Use the main analysis function
  analyzeIdris2Coverage wasmPath profilingStr targetFuncs exclusions

-- =============================================================================
-- Dumpcases Integration
-- =============================================================================

||| Get target functions from dumpcases output
|||
||| @dumpcasesPath  Path to dumpcases output file
||| @exclusions     Patterns to exclude (e.g., Prelude, PrimIO runtime functions)
export
getTargetFunctionsFromDumpcases : String -> List ExclPattern -> IO (Either String (List String))
getTargetFunctionsFromDumpcases dumpcasesPath exclusions = do
  Right content <- readFile dumpcasesPath
    | Left err => pure $ Left $ "Failed to read dumpcases: " ++ show err
  let parsed = parseDumpcases content
  let allFuncs = map (.funcName) parsed
  let filtered = applyExclusions exclusions allFuncs
  pure $ Right filtered

||| Run dumpcases and get target functions
|||
||| @projectDir    Project directory containing .ipkg file
||| @ipkgName      Name of the .ipkg file (e.g., "mycanister.ipkg")
||| @exclusions    Patterns to exclude
export
runDumpcasesAndGetTargets : String -> String -> List ExclPattern ->
                            IO (Either String (List String))
runDumpcasesAndGetTargets projectDir ipkgName exclusions = do
  Right result <- runAndParseDumpcases projectDir ipkgName
    | Left err => pure $ Left err
  let allFuncs = map (.funcName) result
  let filtered = applyExclusions exclusions allFuncs
  pure $ Right filtered

-- =============================================================================
-- Default Exclusions
-- =============================================================================

||| Default exclusion patterns for Idris2 coverage analysis
||| Excludes runtime/library functions that aren't typically test targets
export
defaultIdris2Exclusions : List ExclPattern
defaultIdris2Exclusions =
  [ prefixPattern "prim__" "Primitive operations"
  , prefixPattern "Builtin." "Built-in functions"
  , prefixPattern "Prelude.Basics." "Basic prelude (id, const, etc.)"
  , prefixPattern "Prelude.Types." "Type machinery"
  , containsPattern "ifThenElse" "Control flow primitives"
  , containsPattern "believe_me" "Unsafe operations"
  ]

||| Exclusions for Main module only analysis
export
mainModuleOnlyExclusions : List ExclPattern
mainModuleOnlyExclusions =
  defaultIdris2Exclusions ++
  [ prefixPattern "PrimIO." "PrimIO runtime"
  , prefixPattern "Prelude." "Prelude functions"
  , prefixPattern "Data." "Data module"
  , prefixPattern "Control." "Control module"
  , prefixPattern "System." "System module"
  ]

-- =============================================================================
-- Note: Utility functions getHighImpactTargets, meetsCoverageThreshold,
-- mergeCoverageResults, coverageSeverity are provided by Coverage.Core.Result
-- which is re-exported by this module.
-- =============================================================================
