||| High-level Test Coverage API
|||
||| This module provides a simple interface for analyzing Idris2 projects:
|||
|||   analyzeProject : String -> IO (Either String TestAnalysis)
|||   analyzeProjectWithHits : String -> List String -> IO (Either String TestCoverage)
|||
||| The API handles --dumpcases invocation internally, so users don't need to
||| know the non-obvious syntax: idris2 --dumpcases <output> --build <package.ipkg>
|||
||| Issue #5 & #6: This is the recommended entry point for idris2-coverage users.
module Coverage.TestCoverage

import Coverage.Types
import Coverage.DumpcasesParser
import Coverage.Collector
import Coverage.UnifiedRunner
import Coverage.Exclusions
import Coverage.Classification.BranchClass
import Coverage.Standardization.Types

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Clock

%default covering

-- =============================================================================
-- Helper: Split Path into Directory and Filename
-- =============================================================================

||| Split "path/to/project.ipkg" into ("path/to", "project.ipkg")
||| Handles edge cases like just "project.ipkg" -> (".", "project.ipkg")
splitIpkgPath : String -> (String, String)
splitIpkgPath path =
  let parts = forget $ split (== '/') path
  in case parts of
       [] => (".", path)
       [x] => (".", x)
       _ => case (initLast parts) of
              Nothing => (".", path)
              Just (dirParts, lastPart) => (fastConcat $ intersperse "/" dirParts, lastPart)
  where
    -- Safe init + last that returns Maybe
    initLast : List a -> Maybe (List a, a)
    initLast [] = Nothing
    initLast [x] = Just ([], x)
    initLast (x :: xs) = case initLast xs of
      Nothing => Just ([], x)
      Just (ys, z) => Just (x :: ys, z)

-- =============================================================================
-- High-Level API (Issue #5)
-- =============================================================================

||| Analyze project for semantic coverage - static analysis only
|||
||| This is the recommended entry point. It:
||| 1. Runs idris2 --dumpcases internally with correct syntax
||| 2. Parses the output to classify canonical vs impossible cases
||| 3. Returns TestAnalysis with the breakdown
|||
||| @ipkgPath - Path to the .ipkg file (e.g., "myproject.ipkg" or "path/to/project/project.ipkg")
||| @returns  - Either error message or TestAnalysis
public export
analyzeProject : (ipkgPath : String) -> IO (Either String TestAnalysis)
analyzeProject ipkgPath = do
  result <- runProjectDumpcasesWithTempIpkg ipkgPath
  case result of
    Left err => pure $ Left err
    Right content => do
      let funcs = parseDumpcasesFile content
      pure $ Right $ aggregateAnalysis funcs

||| Get detailed function-level analysis
|||
||| @ipkgPath - Path to the .ipkg file
||| @returns  - Either error or list of CompiledFunction with case classifications
public export
analyzeProjectFunctions : (ipkgPath : String) -> IO (Either String (List CompiledFunction))
analyzeProjectFunctions ipkgPath = do
  result <- runProjectDumpcasesWithTempIpkg ipkgPath
  case result of
    Left err => pure $ Left err
    Right content => pure $ Right $ parseDumpcasesFile content

-- =============================================================================
-- Combined Analysis with Runtime Hits (Issue #6)
-- =============================================================================

||| Analyze project with runtime profiler data
|||
||| This combines:
||| 1. Static analysis from --dumpcases (canonical vs impossible classification)
||| 2. Runtime hits from .ss.html profiler output
|||
||| @ipkgPath    - Path to the .ipkg file
||| @testModules - List of test module names to run (e.g., ["Tests.AllTests"])
||| @returns     - Either error or TestCoverage with executed counts
public export
analyzeProjectWithHits : (ipkgPath : String)
                       -> (testModules : List String)
                       -> IO (Either String TestCoverage)
analyzeProjectWithHits ipkgPath testModules = do
  let (projectDir, _) = splitIpkgPath ipkgPath

  -- Step 1: Get static analysis from --dumpcases
  dumpResult <- runProjectDumpcasesWithTempIpkg ipkgPath
  case dumpResult of
    Left err => pure $ Left $ "Dumpcases failed: " ++ err
    Right dumpContent => do
      let funcs = parseDumpcasesFile dumpContent
      let analysis = aggregateAnalysis funcs

      -- Step 2: Run tests with profiler
      testResult <- runTestsWithCoverage projectDir testModules 120
      case testResult of
        Left testErr => do
          -- Return static analysis only with 0 executed
          pure $ Right $ MkTestCoverage
            "project"
            analysis.totalCanonical
            analysis.totalExcluded
            0
        Right report => do
          -- Step 3: Extract executed count from profiler
          -- Use coveredBranches as approximation for executed canonical cases
          let executed = report.branchCoverage.coveredBranches
          pure $ Right $ MkTestCoverage
            "project"
            analysis.totalCanonical
            analysis.totalExcluded
            executed

-- =============================================================================
-- Per-Function Analysis with Runtime Hits (Pragmatic v1.0)
-- =============================================================================

||| Detailed per-function pragmatic coverage with runtime hits
||| Follows the "Absurd を分母から除外" principle:
|||   - Coverage % = executed/canonical (denominator excludes impossible)
|||   - excluded* fields: safe to exclude from denominator (100% achievable)
|||   - bug/unknown fields: CI signals (not in denominator, but flagged)
public export
record FunctionTestCoverage where
  constructor MkFunctionTestCoverage
  funcName           : String
  moduleName         : String

  -- Coverage の分母/分子（ここだけが「カバレッジ」本体）
  totalCanonical     : Nat
  executedCanonical  : Nat
  coveragePercent    : Double

  -- 分母から除外する（= 100% を阻害しない）要素
  excludedNoClauses  : Nat    -- void/uninhabited
  excludedOptimizer  : Nat    -- Nat case not covered

  -- CI の別軸シグナル（分母に混ぜない）
  bugUnhandledInput  : Nat    -- partial code (should fix)
  unknownCrash       : Nat    -- conservative bucket (investigate)

public export
Show FunctionTestCoverage where
  show fsc = fsc.funcName ++ ": " ++ show fsc.executedCanonical
          ++ "/" ++ show fsc.totalCanonical
          ++ " (" ++ show (cast {to=Int} fsc.coveragePercent) ++ "%)"

-- =============================================================================
-- Bucket Classification Functions (Pragmatic v1.0)
-- =============================================================================

-- Practical: safe to exclude from denominator
--   - NoClauses: void/uninhabited bodies
--   - OptimizerNat: Nat->Int optimizer artifact
isExcludedCaseSC : CompiledCase -> Bool
isExcludedCaseSC c = case c.kind of
  NonCanonical CrashNoClauses    => True
  NonCanonical CrashOptimizerNat => True
  _ => False

countCanonicalCasesSC : CompiledFunction -> Nat
countCanonicalCasesSC func = length $ filter (\c => c.kind == Canonical) func.cases

-- Detailed count functions for breakdown
countExcludedNoClausesSC : CompiledFunction -> Nat
countExcludedNoClausesSC func =
  length $ filter (\c => case c.kind of
    NonCanonical CrashNoClauses => True
    _ => False
  ) func.cases

countExcludedOptimizerSC : CompiledFunction -> Nat
countExcludedOptimizerSC func =
  length $ filter (\c => case c.kind of
    NonCanonical CrashOptimizerNat => True
    _ => False
  ) func.cases

countBugUnhandledInputSC : CompiledFunction -> Nat
countBugUnhandledInputSC func =
  length $ filter (\c => case c.kind of
    NonCanonical CrashUnhandledInput => True
    _ => False
  ) func.cases

countUnknownCrashSC : CompiledFunction -> Nat
countUnknownCrashSC func =
  length $ filter (\c => case c.kind of
    NonCanonical (CrashUnknown _) => True
    _ => False
  ) func.cases

-- 互換のために残す（旧名）: NoClauses + Optimizer
countExcludedCasesSC : CompiledFunction -> Nat
countExcludedCasesSC func =
  countExcludedNoClausesSC func + countExcludedOptimizerSC func

||| Convert CompiledFunction to FunctionTestCoverage with hits (Pragmatic v1.0)
||| Legacy API: takes executed count as separate parameter
public export
functionToTestCoverage : CompiledFunction -> Nat -> FunctionTestCoverage
functionToTestCoverage f executed =
  let canonical = countCanonicalCasesSC f in

  let exclNoClauses = countExcludedNoClausesSC f in
  let exclOptimizer = countExcludedOptimizerSC f in

  let bugUnhandled  = countBugUnhandledInputSC f in
  let unknownCrash  = countUnknownCrashSC f in

  let pct = if canonical == 0
            then 100.0
            else cast executed / cast canonical * 100.0

  in MkFunctionTestCoverage
       f.fullName
       f.moduleName
       canonical
       executed
       pct
       exclNoClauses
       exclOptimizer
       bugUnhandled
       unknownCrash

||| Convert FunctionRuntimeHit to FunctionTestCoverage (Pragmatic v1.0)
||| NEW API: uses per-function runtime data for accurate severity calculation
|||
||| @hit - Per-function runtime coverage from runTestsWithFunctionHits
||| @func - Static analysis from --dumpcases (for bug/unknown classification)
public export
runtimeHitToTestCoverage : FunctionRuntimeHit -> CompiledFunction -> FunctionTestCoverage
runtimeHitToTestCoverage hit func =
  let exclNoClauses = countExcludedNoClausesSC func in
  let exclOptimizer = countExcludedOptimizerSC func in

  let bugUnhandled  = countBugUnhandledInputSC func in
  let unknownCrash  = countUnknownCrashSC func in

  let pct = functionRuntimeCoveragePercent hit

  in MkFunctionTestCoverage
       hit.funcName
       func.moduleName
       hit.canonicalCount
       hit.executedCount
       pct
       exclNoClauses
       exclOptimizer
       bugUnhandled
       unknownCrash

||| Convert FunctionRuntimeHit to FunctionTestCoverage (simplified)
||| For cases where CompiledFunction is not available
public export
runtimeHitToTestCoverageSimple : FunctionRuntimeHit -> FunctionTestCoverage
runtimeHitToTestCoverageSimple hit =
  let pct = functionRuntimeCoveragePercent hit
  in MkFunctionTestCoverage
       hit.funcName
       ""  -- module name unavailable
       hit.canonicalCount
       hit.executedCount
       pct
       0   -- excluded info unavailable
       0
       0
       0

-- =============================================================================
-- Function-Level Obligation Mapping
-- =============================================================================

||| Convert compiled functions to function-level semantic obligations.
public export
compiledFunctionsToFunctionObligations : List CompiledFunction -> List CoverageObligation
compiledFunctionsToFunctionObligations funcs =
  map (staticFunctionToCoverageObligation . toStaticFunctionAnalysis) funcs

||| Build the runtime aliases that may identify a Chez-observed function.
|||
||| We include both the logical Idris name and the generated Scheme name because
||| the profiler/runtime side may surface either representation.
public export
chezRuntimeAliasesForObligation : CoverageObligation -> List RuntimeUnitRef
chezRuntimeAliasesForObligation ob =
  [ runtimeFunctionName ob.obligationId
  , generatedFunctionName (idrisFuncToSchemePattern ob.obligationId)
  ]

||| Resolve covered function-level obligation IDs from per-function runtime hits.
public export
coveredFunctionObligationIdsFromRuntimeHits : List CompiledFunction
                                          -> List FunctionRuntimeHit
                                          -> List String
coveredFunctionObligationIdsFromRuntimeHits funcs hits =
  let obligations = compiledFunctionsToFunctionObligations funcs
      obligationMap = buildFunctionObligationMap obligations chezRuntimeAliasesForObligation
      runtimeUnits = concatMap hitUnits hits
  in resolveCoveredDenominatorIds obligations obligationMap runtimeUnits
  where
    hitUnits : FunctionRuntimeHit -> List RuntimeUnitRef
    hitUnits hit =
      if hit.executedCount > 0
         then [runtimeFunctionName hit.funcName, generatedFunctionName hit.schemeFunc]
         else []

||| Produce a semantic coverage measurement from compiled functions and runtime hits.
||| This keeps the denominator and numerator aligned at the function-level
||| obligation layer.
public export
functionCoverageMeasurementFromRuntimeHits : List CompiledFunction
                                         -> List FunctionRuntimeHit
                                         -> CoverageMeasurement
functionCoverageMeasurementFromRuntimeHits funcs hits =
  let obligations = compiledFunctionsToFunctionObligations funcs
      denominatorIds =
        map (.obligationId) $ filter (\ob => countsAsDenominator ob.classification) obligations
      excludedIds =
        map (.obligationId) $ filter (\ob => mustBeExcluded ob.classification) obligations
      unknownIds =
        map (.obligationId) $ filter (\ob => ob.classification == UnknownClassification) obligations
      coveredIds = coveredFunctionObligationIdsFromRuntimeHits funcs hits
  in MkCoverageMeasurement denominatorIds coveredIds excludedIds unknownIds

-- =============================================================================
-- High Impact Targets (imported from Coverage.Core.HighImpact via Coverage.Types)
-- =============================================================================

-- HighImpactKind, HighImpactTarget, MkHighImpactTarget, showSeverity, severityRatio,
-- severityInfinity are now imported from Coverage.Core.HighImpact

||| Extract high-impact targets from a FunctionTestCoverage
||| Returns 0-3 targets depending on which coverage issues exist
||| Now includes executedCount from runtime profiler
||| Severity = branchCount / executedCount (Inf if executedCount = 0)
public export
targetsFromFunction : FunctionTestCoverage -> List HighImpactTarget
targetsFromFunction fsc =
  let -- Calculate untested branches (gap between total and executed)
      untestedBranches = if fsc.totalCanonical > fsc.executedCanonical
                         then fsc.totalCanonical `minus` fsc.executedCanonical
                         else 0
      -- Only flag as untested if there are actual untested branches
      untested = if untestedBranches > 0
                 then [MkHighImpactTarget
                         HIT_UntestedCanonical
                         fsc.funcName
                         fsc.moduleName
                         fsc.totalCanonical
                         fsc.executedCanonical
                         (severityRatio fsc.totalCanonical fsc.executedCanonical)
                         ("Function has " ++ show untestedBranches ++ " untested branches")]
                 else []
      -- Bugs: severity = Inf (always highest priority)
      bugs = if fsc.bugUnhandledInput > 0
             then [MkHighImpactTarget
                     HIT_BugUnhandledInput
                     fsc.funcName
                     fsc.moduleName
                     fsc.bugUnhandledInput
                     0  -- bugs don't have "executed" concept
                     1.0e309  -- Infinity - bugs are always highest priority
                     "Partial code: CRASH 'Unhandled input' detected"]
             else []
      -- Unknown: severity = Inf (needs investigation)
      unknown = if fsc.unknownCrash > 0
                then [MkHighImpactTarget
                        HIT_UnknownCrash
                        fsc.funcName
                        fsc.moduleName
                        fsc.unknownCrash
                        0  -- unknown crashes don't have "executed" concept
                        1.0e309  -- Infinity - unknown needs investigation
                        "Unknown CRASH message - investigate"]
                else []
  in untested ++ bugs ++ unknown

-- sortTargets is now imported from Coverage.Core.HighImpact via Coverage.Types

-- Helper: check if string contains only digits
isDigitString : String -> Bool
isDigitString s = length s > 0 && all isDigit (unpack s)

-- Check for patterns like "Module.Func.1234:567:localName"
-- These are where-clause helpers with source line numbers
isWhereClauseHelper : String -> Bool
isWhereClauseHelper n =
  let parts = forget $ split (== ':') n
  in length parts >= 2 && any isDigitString (take 2 parts)

||| Check if function name is compiler-generated
||| Patterns: {csegen:N}, {eta:N}, _builtin.*, prim__*, line-numbered where clauses
||| See docs/compiler-generated-functions.md for full reference
isCompilerGenerated : String -> Bool
isCompilerGenerated name =
     isPrefixOf "{" name          -- MN names: {csegen:N}, {eta:N}, etc.
  || isPrefixOf "_builtin." name  -- Builtin constructors
  || isPrefixOf "prim__" name     -- Primitive operations
  || isWhereClauseHelper name     -- Line-numbered local functions

||| Check if function is from standard library (not user code)
||| See docs/compiler-generated-functions.md for full reference
isStandardLibrary : String -> Bool
isStandardLibrary name =
     isPrefixOf "Prelude." name
  || isPrefixOf "Builtin." name
  || isPrefixOf "PrimIO." name
  || isPrefixOf "Data." name
  || isPrefixOf "System." name
  || isPrefixOf "Control." name
  || isPrefixOf "Decidable." name
  || isPrefixOf "Language." name
  || isPrefixOf "Debug." name

||| Check if name is a type constructor (ends with '.')
||| These are auto-generated ADT constructor case trees
isTypeConstructor : String -> Bool
isTypeConstructor name =
  isSuffixOf "." name && not (isPrefixOf "{" name)

||| Check if function is test code (should not be coverage target)
||| Patterns: *.Tests.*, *.AllTests.*, test_*, *.test*
||| Rationale: Test code tests other code, it shouldn't be a coverage target itself
export
isTestCode : String -> Bool
isTestCode name =
     isInfixOf ".Tests." name        -- Module.Tests.SubModule.func
  || isInfixOf ".AllTests." name     -- Module.AllTests.func
  || isSuffixOf ".AllTests" name     -- Module.AllTests (module itself)
  || isPrefixOf "test_" name         -- test_funcName (common naming convention)
  || hasTestFuncName name            -- Module.testFoo, Module.Module.testBar
  where
    -- Check if the final function name (after last '.') starts with "test"
    hasTestFuncName : String -> Bool
    hasTestFuncName n =
      let parts = forget $ split (== '.') n
      in case last' parts of
           Nothing => False
           Just funcPart => isPrefixOf "test" funcPart

||| Check if function should be excluded from coverage targets (without config)
shouldExcludeFromTargets : String -> Bool
shouldExcludeFromTargets name =
     isCompilerGenerated name
  || isStandardLibrary name
  || isTypeConstructor name
  || isTestCode name

||| Check if function should be excluded with user config
shouldExcludeFromTargetsWithConfig : ExclusionConfig -> String -> Bool
shouldExcludeFromTargetsWithConfig config name =
  let matchesDep = any (\p => isPrefixOf p name) config.modulePrefixes
                || any (\pkg => isPrefixOf (capitalizeFirst pkg ++ ".") name) config.packageNames
                || any (\fn => fn == name) config.functionNames
  in shouldExcludeFromTargets name || matchesDep
  where
    capitalizeFirst : String -> String
    capitalizeFirst s = case strM s of
      StrNil => ""
      StrCons c rest => singleton (toUpper c) ++ rest

||| Check if function should be excluded with loaded exclusions + user config
||| This is the recommended API for library users (e.g., LazyCore)
|||
||| @excl    - Loaded patterns from exclusions/ directory (or custom patterns)
||| @config  - User config from .idris2-cov.toml or passed programmatically
||| @name    - Function name to check
public export
shouldExcludeWithExclusions : LoadedExclusions -> ExclusionConfig -> String -> Bool
shouldExcludeWithExclusions excl config name =
     shouldExclude excl name  -- Check exclusions/ patterns first
  || shouldExcludeFromTargetsWithConfig config name  -- Then check config + hardcoded

||| Get top K high-impact targets with loaded exclusions + config
||| This is the recommended API for library users (e.g., LazyCore)
|||
||| @excl   - Loaded patterns from exclusions/ directory (via loadExclusions)
||| @config - User config (can be emptyExclusionConfig if not using .idris2-cov.toml)
||| @k      - Maximum number of targets to return
||| @funcs  - List of function coverage data
public export
topKTargetsWithExclusions : LoadedExclusions -> ExclusionConfig -> Nat -> List FunctionTestCoverage -> List HighImpactTarget
topKTargetsWithExclusions excl config k funcs =
  let allTargets = concatMap targetsFromFunction funcs
      -- Filter out excluded functions using both exclusions/ patterns and config
      userTargets = filter (not . shouldExcludeWithExclusions excl config . funcName) allTargets
      sorted = sortTargets userTargets
  in Data.List.take k sorted

||| Get top K high-impact targets from list of functions with config
||| Filters out compiler-generated, standard library, type constructors, and user-specified
||| Note: This uses hardcoded patterns only. For full exclusion support, use topKTargetsWithExclusions
public export
topKTargetsWithConfig : ExclusionConfig -> Nat -> List FunctionTestCoverage -> List HighImpactTarget
topKTargetsWithConfig config k funcs =
  let allTargets = concatMap targetsFromFunction funcs
      -- Filter out non-user functions
      userTargets = filter (not . shouldExcludeFromTargetsWithConfig config . funcName) allTargets
      sorted = sortTargets userTargets
  in Data.List.take k sorted

||| Get top K high-impact targets (default empty config)
public export
topKTargets : Nat -> List FunctionTestCoverage -> List HighImpactTarget
topKTargets = topKTargetsWithConfig emptyExclusionConfig

-- =============================================================================
-- FunctionRuntimeHit-based API (Recommended for accurate severity)
-- =============================================================================

||| Get top K high-impact targets from FunctionRuntimeHits
||| This is the recommended API for CLI/Library shared usage
|||
||| @excl   - Loaded patterns from exclusions/ directory
||| @config - User config from .idris2-cov.toml
||| @k      - Maximum number of targets to return
||| @hits   - Per-function runtime coverage from runTestsWithFunctionHits
public export
topKTargetsFromRuntimeHits : LoadedExclusions -> ExclusionConfig -> Nat -> List FunctionRuntimeHit -> List HighImpactTarget
topKTargetsFromRuntimeHits excl config k hits =
  let funcsCov = map runtimeHitToTestCoverageSimple hits
  in topKTargetsWithExclusions excl config k funcsCov

||| Get top K high-impact targets from FunctionRuntimeHits with CompiledFunctions
||| This provides full bug/unknown classification from static analysis
|||
||| @excl   - Loaded patterns from exclusions/ directory
||| @config - User config from .idris2-cov.toml
||| @k      - Maximum number of targets to return
||| @hits   - Per-function runtime coverage
||| @funcs  - Static analysis results (for bug/unknown counts)
public export
topKTargetsFromRuntimeHitsWithStatic : LoadedExclusions -> ExclusionConfig -> Nat
                                     -> List FunctionRuntimeHit -> List CompiledFunction
                                     -> List HighImpactTarget
topKTargetsFromRuntimeHitsWithStatic excl config k hits funcs =
  let -- Create a map from funcName to CompiledFunction
      funcMap : List (String, CompiledFunction)
      funcMap = map (\f => (f.fullName, f)) funcs

      -- Convert hits to FunctionTestCoverage, looking up static data when available
      funcsCov = map (convertHit funcMap) hits
  in topKTargetsWithExclusions excl config k funcsCov
  where
    lookupFunc : String -> List (String, CompiledFunction) -> Maybe CompiledFunction
    lookupFunc _ [] = Nothing
    lookupFunc name ((n, f) :: rest) = if name == n then Just f else lookupFunc name rest

    convertHit : List (String, CompiledFunction) -> FunctionRuntimeHit -> FunctionTestCoverage
    convertHit fmap hit =
      case lookupFunc hit.funcName fmap of
        Nothing => runtimeHitToTestCoverageSimple hit
        Just func => runtimeHitToTestCoverage hit func

-- =============================================================================
-- Report Generation
-- =============================================================================

||| Generate test coverage summary as text
||| Generate test coverage summary as text
||| Based on dunham's classification from Idris2 community:
|||   - Excluded (NoClauses): void etc, safe to exclude from denominator
|||   - Bugs (UnhandledInput): partial code, coverage issue
|||   - OptimizerArtifacts (Nat case): non-semantic, warn separately
|||   - Unknown: conservative, never exclude
public export
formatTestAnalysis : TestAnalysis -> String
formatTestAnalysis a = unlines
  [ "=== Test Coverage Report ==="
  , ""
  , "Project Summary:"
  , "  Functions analyzed: " ++ show a.totalFunctions
  , "  Canonical cases: " ++ show a.totalCanonical
  , "  Excluded (NoClauses): " ++ show a.totalExcluded
  , "  Bugs (UnhandledInput): " ++ show a.totalBugs
  , "  Optimizer artifacts (Nat): " ++ show a.totalOptimizerArtifacts
  , "  Unknown CRASHes: " ++ show a.totalUnknown
  , "  Functions with CRASH: " ++ show a.functionsWithCrash
  , "  Coverage model: " ++ a.coverageModel
  , "  Unknown policy: " ++ a.unknownPolicy
  , "  Claim admissible: " ++ show a.claimAdmissible
  ]

||| Generate test coverage with hits as text
public export
formatTestCoverage : TestCoverage -> String
formatTestCoverage sc =
  let pct = testCoveragePercent sc
  in unlines
    [ "=== Test Coverage Report ==="
    , ""
    , "Coverage: " ++ show sc.executedCanonical
               ++ "/" ++ show sc.totalCanonical
               ++ " (" ++ show (cast {to=Int} pct) ++ "%)"
    , "Impossible cases (excluded): " ++ show sc.totalImpossible
    ]

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Generate test analysis as JSON
public export
testAnalysisToJson : TestAnalysis -> String
testAnalysisToJson a = unlines
  [ "{"
  , "  \"total_functions\": " ++ show a.totalFunctions ++ ","
  , "  \"total_canonical\": " ++ show a.totalCanonical ++ ","
  , "  \"total_excluded\": " ++ show a.totalExcluded ++ ","
  , "  \"total_bugs\": " ++ show a.totalBugs ++ ","
  , "  \"total_optimizer_artifacts\": " ++ show a.totalOptimizerArtifacts ++ ","
  , "  \"total_unknown\": " ++ show a.totalUnknown ++ ","
  , "  \"coverage_model\": \"" ++ a.coverageModel ++ "\","
  , "  \"unknown_policy\": \"" ++ a.unknownPolicy ++ "\","
  , "  \"claim_admissible\": " ++ show a.claimAdmissible ++ ","
  , "  \"exclusion_breakdown\": {"
  , "    \"compiler_generated\": " ++ show a.exclusionBreakdown.compilerGenerated ++ ","
  , "    \"standard_library\": " ++ show a.exclusionBreakdown.standardLibrary ++ ","
  , "    \"type_constructors\": " ++ show a.exclusionBreakdown.typeConstructors ++ ","
  , "    \"dependencies\": " ++ show a.exclusionBreakdown.dependencies ++ ","
  , "    \"test_code\": " ++ show a.exclusionBreakdown.testCode
  , "  },"
  , "  \"functions_with_crash\": " ++ show a.functionsWithCrash
  , "}"
  ]

||| Generate test coverage with hits as JSON
public export
testCoverageToJson : TestCoverage -> String
testCoverageToJson sc =
  let pct = testCoveragePercent sc
  in unlines
    [ "{"
    , "  \"function\": \"" ++ sc.funcName ++ "\","
    , "  \"total_canonical\": " ++ show sc.totalCanonical ++ ","
    , "  \"total_impossible\": " ++ show sc.totalImpossible ++ ","
    , "  \"executed_canonical\": " ++ show sc.executedCanonical ++ ","
    , "  \"coverage_percent\": " ++ show pct
    , "}"
    ]

jsonStringArray : List String -> String
jsonStringArray xs =
  "[" ++ fastConcat (intersperse ", " (map (\x => "\"" ++ escapeJson x ++ "\"") xs)) ++ "]"
  where
    escapeJson : String -> String
    escapeJson s = fastConcat $ map escapeChar (unpack s)
      where
        escapeChar : Char -> String
        escapeChar '\n' = "\\n"
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar c    = singleton c

||| Render a coverage measurement as JSON.
public export
coverageMeasurementToJson : CoverageMeasurement -> String
coverageMeasurementToJson m = unlines
  [ "{"
  , "  \"denominator_ids\": " ++ jsonStringArray m.denominatorIds ++ ","
  , "  \"covered_ids\": " ++ jsonStringArray m.coveredIds ++ ","
  , "  \"excluded_ids\": " ++ jsonStringArray m.excludedIds ++ ","
  , "  \"unknown_ids\": " ++ jsonStringArray m.unknownIds
  , "}"
  ]

||| Human-readable summary of a function-level semantic coverage measurement.
public export
formatCoverageMeasurementSummary : CoverageMeasurement -> String
formatCoverageMeasurementSummary m = unlines
  [ "Function-level semantic measurement:"
  , "  denominator_ids: " ++ show (length m.denominatorIds)
  , "  covered_ids:     " ++ show (length m.coveredIds)
  , "  excluded_ids:    " ++ show (length m.excludedIds)
  , "  unknown_ids:     " ++ show (length m.unknownIds)
  ]

-- =============================================================================
-- High Impact Targets JSON Output
-- =============================================================================

-- coverageReadingGuide, targetToJson, targetsToJsonArray are now imported from Coverage.Core.HighImpact

||| Full coverage report with high impact targets as JSON
||| This is the canonical output format for both CLI and lazy core ask
public export
coverageReportToJson : TestAnalysis -> List HighImpactTarget -> String
coverageReportToJson analysis targets = unlines
  [ "{"
  , "  \"reading_guide\": \"" ++ escapeJson Coverage.Core.HighImpact.coverageReadingGuide ++ "\","
  , "  \"summary\": {"
  , "    \"total_functions\": " ++ show analysis.totalFunctions ++ ","
  , "    \"total_canonical\": " ++ show analysis.totalCanonical ++ ","
  , "    \"total_excluded\": " ++ show analysis.totalExcluded ++ ","
  , "    \"total_bugs\": " ++ show analysis.totalBugs ++ ","
  , "    \"total_optimizer_artifacts\": " ++ show analysis.totalOptimizerArtifacts ++ ","
  , "    \"total_unknown\": " ++ show analysis.totalUnknown ++ ","
  , "    \"coverage_model\": \"" ++ analysis.coverageModel ++ "\","
  , "    \"unknown_policy\": \"" ++ analysis.unknownPolicy ++ "\","
  , "    \"claim_admissible\": " ++ show analysis.claimAdmissible ++ ","
  , "    \"exclusion_breakdown\": {"
  , "      \"compiler_generated\": " ++ show analysis.exclusionBreakdown.compilerGenerated ++ ","
  , "      \"standard_library\": " ++ show analysis.exclusionBreakdown.standardLibrary ++ ","
  , "      \"type_constructors\": " ++ show analysis.exclusionBreakdown.typeConstructors ++ ","
  , "      \"dependencies\": " ++ show analysis.exclusionBreakdown.dependencies ++ ","
  , "      \"test_code\": " ++ show analysis.exclusionBreakdown.testCode
  , "    }"
  , "  },"
  , "  \"high_impact_targets\": " ++ Coverage.Core.HighImpact.targetsToJsonArray targets
  , "}"
  ]
  where
    escapeJson : String -> String
    escapeJson s = fastConcat $ map escapeChar (unpack s)
      where
        escapeChar : Char -> String
        escapeChar '\n' = "\\n"
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar c    = singleton c

||| Full coverage report with an explicit function-level semantic measurement.
public export
coverageReportToJsonWithMeasurement : TestAnalysis
                                  -> CoverageMeasurement
                                  -> List HighImpactTarget
                                  -> String
coverageReportToJsonWithMeasurement analysis measurement targets = unlines
  [ "{"
  , "  \"reading_guide\": \"" ++ escapeJson Coverage.Core.HighImpact.coverageReadingGuide ++ "\","
  , "  \"summary\": {"
  , "    \"total_functions\": " ++ show analysis.totalFunctions ++ ","
  , "    \"total_canonical\": " ++ show analysis.totalCanonical ++ ","
  , "    \"total_excluded\": " ++ show analysis.totalExcluded ++ ","
  , "    \"total_bugs\": " ++ show analysis.totalBugs ++ ","
  , "    \"total_optimizer_artifacts\": " ++ show analysis.totalOptimizerArtifacts ++ ","
  , "    \"total_unknown\": " ++ show analysis.totalUnknown ++ ","
  , "    \"coverage_model\": \"" ++ analysis.coverageModel ++ "\","
  , "    \"unknown_policy\": \"" ++ analysis.unknownPolicy ++ "\","
  , "    \"claim_admissible\": " ++ show analysis.claimAdmissible ++ ","
  , "    \"exclusion_breakdown\": {"
  , "      \"compiler_generated\": " ++ show analysis.exclusionBreakdown.compilerGenerated ++ ","
  , "      \"standard_library\": " ++ show analysis.exclusionBreakdown.standardLibrary ++ ","
  , "      \"type_constructors\": " ++ show analysis.exclusionBreakdown.typeConstructors ++ ","
  , "      \"dependencies\": " ++ show analysis.exclusionBreakdown.dependencies ++ ","
  , "      \"test_code\": " ++ show analysis.exclusionBreakdown.testCode
  , "    }"
  , "  },"
  , "  \"measurement\": " ++ replaceLeadingSpaces (coverageMeasurementToJson measurement) ++ ","
  , "  \"high_impact_targets\": " ++ Coverage.Core.HighImpact.targetsToJsonArray targets
  , "}"
  ]
  where
    escapeJson : String -> String
    escapeJson s = fastConcat $ map escapeChar (unpack s)
      where
        escapeChar : Char -> String
        escapeChar '\n' = "\\n"
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar c    = singleton c

    replaceLeadingSpaces : String -> String
    replaceLeadingSpaces s =
      let ls = lines s
      in fastConcat $ intersperse "\n" $ map ("  " ++) ls

-- =============================================================================
-- OR Aggregation API (BranchId-based)
-- =============================================================================

||| Convert profiler covered count to TestRunHits
||| Note: This is a simplified mapping - full implementation would
||| correlate .ss.html line hits to specific BranchIds
convertToTestRunHits : String -> Nat -> List CompiledFunction -> TestRunHits
convertToTestRunHits testName coveredCount funcs =
  -- Generate BranchHits for the first N canonical branches
  -- This is an approximation until we have full line-to-branch mapping
  let allBranches = concatMap (.cases) funcs
      canonicalBranches = filter (\c => c.kind == Canonical) allBranches
      -- Mark first coveredCount branches as hit (approximation)
      hitBranches = take coveredCount canonicalBranches
      hits = map (\c => MkBranchHit c.branchId 1) hitBranches
  in MkTestRunHits testName "" hits

||| Analyze project with OR-aggregated coverage from multiple test modules
|||
||| This function:
||| 1. Gets static analysis from --dumpcases (with BranchIds)
||| 2. Runs all test modules with profiling
||| 3. OR-aggregates coverage across all test runs
|||
||| @ipkgPath    - Path to the .ipkg file
||| @testModules - List of test module names (each run separately for aggregation)
||| @returns     - Either error or AggregatedCoverage with OR-union of hits
public export
analyzeProjectWithAggregatedHits : (ipkgPath : String)
                                  -> (testModules : List String)
                                  -> IO (Either String AggregatedCoverage)
analyzeProjectWithAggregatedHits ipkgPath testModules = do
  let (projectDir, _) = splitIpkgPath ipkgPath

  -- Step 1: Get static analysis with BranchIds from --dumpcases
  dumpResult <- runProjectDumpcasesWithTempIpkg ipkgPath
  case dumpResult of
    Left err => pure $ Left $ "Dumpcases failed: " ++ err
    Right dumpContent => do
      let funcs = parseDumpcasesFile dumpContent
      let staticAnalysis = toStaticBranchAnalysis funcs

      -- Step 2: Run tests with profiler
      testResult <- runTestsWithCoverage projectDir testModules 120
      case testResult of
        Left testErr => do
          -- Return static analysis only with 0 covered
          pure $ Right $ aggregateCoverage staticAnalysis []
        Right report => do
          -- Step 3: Convert profiler results to TestRunHits
          let coveredCount = report.branchCoverage.coveredBranches
          let runHits = convertToTestRunHits "all_tests" coveredCount funcs

          -- Step 4: Aggregate (single run for now, but architecture supports multiple)
          pure $ Right $ aggregateCoverage staticAnalysis [runHits]

||| Format AggregatedCoverage as text report
public export
formatAggregatedCoverage : AggregatedCoverage -> String
formatAggregatedCoverage ac =
  let pct = aggregatedCoveragePercent ac
  in unlines
    [ "=== Aggregated Coverage Report ==="
    , ""
    , "Test Runs: " ++ show (length ac.testRuns)
    , "Canonical Coverage: " ++ show ac.canonicalCovered
                            ++ "/" ++ show ac.canonicalTotal
                            ++ " (" ++ show (cast {to=Int} pct) ++ "%)"
    , "Bugs (UnhandledInput): " ++ show ac.bugsTotal
    , "Unknown CRASHes: " ++ show ac.unknownTotal
    , ""
    , "Coverage uses OR-semantics: branch is covered if hit by ANY test run"
    ]

||| AggregatedCoverage to JSON
public export
aggregatedCoverageToJson : AggregatedCoverage -> String
aggregatedCoverageToJson ac =
  let pct = aggregatedCoveragePercent ac
  in unlines
    [ "{"
    , "  \"test_runs\": " ++ show (length ac.testRuns) ++ ","
    , "  \"canonical_total\": " ++ show ac.canonicalTotal ++ ","
    , "  \"canonical_covered\": " ++ show ac.canonicalCovered ++ ","
    , "  \"coverage_percent\": " ++ show pct ++ ","
    , "  \"bugs_total\": " ++ show ac.bugsTotal ++ ","
    , "  \"unknown_total\": " ++ show ac.unknownTotal ++ ","
    , "  \"aggregation\": \"OR\""
    , "}"
    ]
