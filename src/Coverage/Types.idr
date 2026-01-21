||| Coverage type definitions for idris2-coverage
||| REQ_TYPE_COV_001 - REQ_TYPE_COV_005
module Coverage.Types

import Data.List
import Data.List1
import Data.Maybe
import Data.String

-- Re-export core types for branch classification
import public Coverage.Classification.BranchClass

-- Re-export shared runtime hit types from core
import public Coverage.Core.RuntimeHit
import public Coverage.Core.HighImpact

%default total

-- =============================================================================
-- Profile Hit (from Chez Scheme profiler)
-- =============================================================================

||| REQ_TYPE_COV_005: Profile hit from Chez Scheme profiler
public export
record ProfileHit where
  constructor MkProfileHit
  schemeFunc : String    -- e.g., "Sample-add"
  hitCount   : Nat
  filePath   : String
  line       : Nat

public export
Show ProfileHit where
  show h = "ProfileHit(\{h.schemeFunc}, \{show h.hitCount}, \{h.filePath}:\{show h.line})"

public export
Eq ProfileHit where
  h1 == h2 = h1.schemeFunc == h2.schemeFunc
          && h1.hitCount == h2.hitCount
          && h1.filePath == h2.filePath
          && h1.line == h2.line

-- =============================================================================
-- Function Coverage
-- =============================================================================

||| REQ_TYPE_COV_001: Function-level coverage data
public export
record FunctionCoverage where
  constructor MkFunctionCoverage
  moduleName      : String          -- "Audit.Orchestrator"
  name            : String          -- "runAudit"
  signature       : Maybe String    -- "AuditOptions -> IO ()"
  lineStart       : Maybe Nat       -- 28
  lineEnd         : Maybe Nat       -- 100
  coveredLines    : Nat             -- 65
  totalLines      : Nat             -- 72
  coveragePercent : Double          -- 90.3
  calledByTests   : List String     -- ["REQ_AUD_ORCH_001", ...]

public export
Show FunctionCoverage where
  show f = "\{f.moduleName}.\{f.name}: \{show f.coveragePercent}%"

public export
Eq FunctionCoverage where
  f1 == f2 = f1.moduleName == f2.moduleName
          && f1.name == f2.name
          && f1.signature == f2.signature
          && f1.lineStart == f2.lineStart
          && f1.lineEnd == f2.lineEnd

||| Create a covered function
public export
coveredFunction : String -> String -> Nat -> List String -> FunctionCoverage
coveredFunction modName funcName hits tests =
  MkFunctionCoverage modName funcName Nothing Nothing Nothing hits hits 100.0 tests

||| Create an uncovered function
public export
uncoveredFunction : String -> String -> FunctionCoverage
uncoveredFunction modName funcName =
  MkFunctionCoverage modName funcName Nothing Nothing Nothing 0 0 0.0 []

-- =============================================================================
-- Module Coverage
-- =============================================================================

||| REQ_TYPE_COV_002: Module-level coverage summary
public export
record ModuleCoverage where
  constructor MkModuleCoverage
  path                : String    -- "src/Audit/Orchestrator.idr"
  functionsTotal      : Nat
  functionsCovered    : Nat
  lineCoveragePercent : Double

public export
Show ModuleCoverage where
  show m = "\{m.path}: \{show m.functionsCovered}/\{show m.functionsTotal} (\{show m.lineCoveragePercent}%)"

public export
Eq ModuleCoverage where
  m1 == m2 = m1.path == m2.path
          && m1.functionsTotal == m2.functionsTotal
          && m1.functionsCovered == m2.functionsCovered

-- =============================================================================
-- Project Coverage
-- =============================================================================

||| REQ_TYPE_COV_003: Project-level coverage summary
public export
record ProjectCoverage where
  constructor MkProjectCoverage
  totalFunctions        : Nat
  coveredFunctions      : Nat
  lineCoveragePercent   : Double
  branchCoveragePercent : Maybe Double  -- Optional

public export
Show ProjectCoverage where
  show p = "Project: \{show p.coveredFunctions}/\{show p.totalFunctions} functions (\{show p.lineCoveragePercent}%)"

public export
Eq ProjectCoverage where
  p1 == p2 = p1.totalFunctions == p2.totalFunctions
          && p1.coveredFunctions == p2.coveredFunctions

-- =============================================================================
-- Coverage Report
-- =============================================================================

||| REQ_TYPE_COV_004: Complete coverage report
public export
record CoverageReport where
  constructor MkCoverageReport
  functions : List FunctionCoverage
  modules   : List ModuleCoverage
  project   : ProjectCoverage

public export
Show CoverageReport where
  show r = "CoverageReport(\{show $ length r.functions} functions, \{show $ length r.modules} modules)"

-- =============================================================================
-- Scheme Function Name Parsing
-- =============================================================================

||| Parse Scheme function name "Module-func" into (module, func)
||| Returns Nothing if format doesn't match
public export
parseSchemeFunc : String -> Maybe (String, String)
parseSchemeFunc s =
  let parts = forget $ split (== '-') s
  in case parts of
       []            => Nothing
       [_]           => Nothing
       (m :: rest)   => Just (m, fastConcat $ intersperse "-" rest)

||| Convert Idris module name to Scheme prefix
||| "Audit.Orchestrator" -> "AuditC-45Orchestrator"
public export
idrisToSchemeModule : String -> String
idrisToSchemeModule modName =
  fastConcat $ intersperse "C-45" $ forget $ split (== '.') modName

||| Check if a Scheme function belongs to a module
public export
belongsToModule : String -> String -> Bool
belongsToModule schemeFunc idrisModule =
  let modPrefix = idrisToSchemeModule idrisModule
  in isPrefixOf modPrefix schemeFunc

-- =============================================================================
-- Quantity (Linearity)
-- =============================================================================

||| Quantity annotation from Idris2's QTT (Quantitative Type Theory)
public export
data Quantity : Type where
  Q0 : Quantity    -- Erased: compile-time only, no runtime representation
  Q1 : Quantity    -- Linear: must be used exactly once
  QW : Quantity    -- Unrestricted (ω): can be used any number of times

public export
Show Quantity where
  show Q0 = "0"
  show Q1 = "1"
  show QW = "ω"

public export
Eq Quantity where
  Q0 == Q0 = True
  Q1 == Q1 = True
  QW == QW = True
  _  == _  = False

-- =============================================================================
-- State Count (State Space Analysis)
-- =============================================================================

||| State count: finite, bounded, or unbounded
public export
data StateCount : Type where
  Finite    : Nat -> StateCount    -- Exact state count (e.g., Bool = 2)
  Bounded   : Nat -> StateCount    -- Upper bound after equivalence classes
  Unbounded : StateCount           -- Infinite (String, List without bounds)

public export
Show StateCount where
  show (Finite n)  = show n
  show (Bounded n) = "≤" ++ show n
  show Unbounded   = "∞"

public export
Eq StateCount where
  Finite n  == Finite m  = n == m
  Bounded n == Bounded m = n == m
  Unbounded == Unbounded = True
  _         == _         = False

||| Multiply state counts (for product types / multiple params)
public export
multStateCount : StateCount -> StateCount -> StateCount
multStateCount (Finite 0)  _            = Finite 0
multStateCount _           (Finite 0)   = Finite 0
multStateCount (Finite n)  (Finite m)   = Finite (n * m)
multStateCount (Finite n)  (Bounded m)  = Bounded (n * m)
multStateCount (Bounded n) (Finite m)   = Bounded (n * m)
multStateCount (Bounded n) (Bounded m)  = Bounded (n * m)
multStateCount _           _            = Unbounded

||| Add state counts (for sum types)
public export
addStateCount : StateCount -> StateCount -> StateCount
addStateCount (Finite n)  (Finite m)  = Finite (n + m)
addStateCount (Finite n)  (Bounded m) = Bounded (n + m)
addStateCount (Bounded n) (Finite m)  = Bounded (n + m)
addStateCount (Bounded n) (Bounded m) = Bounded (n + m)
addStateCount _           _           = Unbounded

||| Apply equivalence class limit
public export
boundStateCount : Nat -> StateCount -> StateCount
boundStateCount limit (Finite n)  = if n > limit then Bounded limit else Finite n
boundStateCount limit (Bounded n) = Bounded (min limit n)
boundStateCount limit Unbounded   = Bounded limit

-- =============================================================================
-- Type Complexity
-- =============================================================================

||| Type complexity classification
public export
data TypeComplexity : Type where
  TCFinite    : Nat -> TypeComplexity    -- Bool, small enums
  TCBounded   : Nat -> TypeComplexity    -- After equivalence class partitioning
  TCRecursive : Nat -> TypeComplexity    -- Recursive types (depth-bounded)
  TCUnbounded : TypeComplexity           -- Cannot analyze

public export
Show TypeComplexity where
  show (TCFinite n)    = "Finite(" ++ show n ++ ")"
  show (TCBounded n)   = "Bounded(" ++ show n ++ ")"
  show (TCRecursive n) = "Recursive(depth=" ++ show n ++ ")"
  show TCUnbounded     = "Unbounded"

-- =============================================================================
-- Type Information
-- =============================================================================

||| Constructor info: name and argument types
public export
record ConstructorInfo where
  constructor MkConstructorInfo
  ctorName : String
  ctorArgs : List (String, Quantity)   -- (typeName, linearity)

public export
Show ConstructorInfo where
  show c = c.ctorName ++ "(" ++ show (length c.ctorArgs) ++ " args)"

public export
Eq ConstructorInfo where
  c1 == c2 = c1.ctorName == c2.ctorName

||| Extracted type information
public export
record TypeInfo where
  constructor MkTypeInfo
  typeName     : String
  constructors : List ConstructorInfo
  isRecursive  : Bool
  complexity   : TypeComplexity
  stateCount   : StateCount

public export
Show TypeInfo where
  show t = t.typeName ++ " [" ++ show t.stateCount ++ "]"

public export
Eq TypeInfo where
  t1 == t2 = t1.typeName == t2.typeName

-- =============================================================================
-- Linear Parameter
-- =============================================================================

||| Function parameter with linearity annotation
public export
record LinearParam where
  constructor MkLinearParam
  paramName : Maybe String
  paramType : String
  quantity  : Quantity
  typeInfo  : Maybe TypeInfo

public export
Show LinearParam where
  show p = "(" ++ show p.quantity ++ " " ++ fromMaybe "_" p.paramName ++ " : " ++ p.paramType ++ ")"

public export
Eq LinearParam where
  p1 == p2 = p1.paramName == p2.paramName
          && p1.paramType == p2.paramType
          && p1.quantity == p2.quantity

-- =============================================================================
-- Reachability (Path Analysis)
-- =============================================================================

||| Branch reachability classification
public export
data Reachability : Type where
  Always      : Reachability    -- Always reachable
  Conditional : Reachability    -- Reachable under conditions
  EarlyExit   : Reachability    -- Early return (Nothing, Left, etc.)
  Unreachable : Reachability    -- Unreachable (eliminated by types)

public export
Show Reachability where
  show Always      = "Always"
  show Conditional = "Conditional"
  show EarlyExit   = "EarlyExit"
  show Unreachable = "Unreachable"

public export
Eq Reachability where
  Always      == Always      = True
  Conditional == Conditional = True
  EarlyExit   == EarlyExit   = True
  Unreachable == Unreachable = True
  _           == _           = False

||| Pattern match branch with reachability info
public export
record PatternBranch where
  constructor MkPatternBranch
  pattern      : String          -- "Just x", "Nothing", etc.
  reachability : Reachability
  prunable     : Bool            -- Can be represented by other cases
  stateCount   : StateCount      -- States this branch covers

public export
Show PatternBranch where
  show b = b.pattern ++ " [" ++ show b.reachability ++ "]"

-- =============================================================================
-- Linear Resource Tracking
-- =============================================================================

||| A resource that must be consumed (for linear types)
public export
record LinearResource where
  constructor MkLinearResource
  resourceType : String          -- "FileHandle", "Connection"
  acquiredBy   : List String     -- Functions that create this resource
  consumedBy   : List String     -- Functions that consume this resource

public export
Show LinearResource where
  show r = r.resourceType ++ " (acquire: " ++ show r.acquiredBy ++ ", consume: " ++ show r.consumedBy ++ ")"

-- =============================================================================
-- Function State Space
-- =============================================================================

||| Extended function coverage with state space analysis
public export
record FunctionStateSpace where
  constructor MkFunctionStateSpace
  funcCoverage       : FunctionCoverage
  params             : List LinearParam
  estimatedCases     : StateCount
  actualCases        : Nat
  stateSpaceCoverage : Double
  prunedPaths        : List PatternBranch
  linearResources    : List LinearResource

public export
Show FunctionStateSpace where
  show f = show f.funcCoverage ++ " [state space: " ++ show f.stateSpaceCoverage ++ "%]"

-- =============================================================================
-- Complexity Metrics
-- =============================================================================

||| Function complexity metrics for split recommendations
public export
record ComplexityMetrics where
  constructor MkComplexityMetrics
  paramCount      : Nat
  stateSpaceSize  : StateCount
  patternDepth    : Nat           -- Max nesting depth of pattern matches
  branchCount     : Nat           -- Number of case/if branches
  linearParams    : Nat           -- Count of linear parameters
  shouldSplit     : Bool
  splitReason     : Maybe String

public export
Show ComplexityMetrics where
  show c = "Complexity(params=" ++ show c.paramCount
        ++ ", branches=" ++ show c.branchCount
        ++ ", split=" ++ show c.shouldSplit ++ ")"

-- =============================================================================
-- Branch Coverage Types
-- =============================================================================

||| Type of branch construct in Scheme code
public export
data BranchType : Type where
  IfBranch   : BranchType    -- (if cond then else)
  CaseBranch : BranchType    -- (case expr [pattern body]...)
  CondBranch : BranchType    -- (cond [test body]...)

public export
Show BranchType where
  show IfBranch   = "if"
  show CaseBranch = "case"
  show CondBranch = "cond"

public export
Eq BranchType where
  IfBranch   == IfBranch   = True
  CaseBranch == CaseBranch = True
  CondBranch == CondBranch = True
  _          == _          = False

||| A single branch decision point with coverage info
public export
record BranchPoint where
  constructor MkBranchPoint
  line            : Nat
  char            : Nat
  branchType      : BranchType
  totalBranches   : Nat       -- 2 for if, N for case/cond
  coveredBranches : Nat       -- How many branches had count > 0
  branchDetails   : List (String, Nat)  -- (branchLabel, hitCount)

public export
Show BranchPoint where
  show bp = show bp.branchType ++ "@" ++ show bp.line ++ ":" ++ show bp.char
         ++ " [" ++ show bp.coveredBranches ++ "/" ++ show bp.totalBranches ++ "]"

public export
Eq BranchPoint where
  bp1 == bp2 = bp1.line == bp2.line && bp1.char == bp2.char

||| Branch coverage summary for a function
public export
record FunctionBranchCoverage where
  constructor MkFunctionBranchCoverage
  schemeFunc      : String
  branchPoints    : List BranchPoint
  totalBranches   : Nat
  coveredBranches : Nat
  branchPercent   : Double

public export
Show FunctionBranchCoverage where
  show fbc = fbc.schemeFunc ++ ": " ++ show fbc.branchPercent ++ "% branch coverage"

||| Project-level branch coverage summary
public export
record BranchCoverageSummary where
  constructor MkBranchCoverageSummary
  totalBranchPoints : Nat
  totalBranches     : Nat
  coveredBranches   : Nat
  branchPercent     : Double
  uncoveredBranches : List (String, BranchPoint)  -- (funcName, branchPoint)

public export
Show BranchCoverageSummary where
  show bcs = "Branch coverage: " ++ show bcs.coveredBranches ++ "/" ++ show bcs.totalBranches
          ++ " (" ++ show bcs.branchPercent ++ "%)"

-- =============================================================================
-- Extended Coverage Report
-- =============================================================================

||| Coverage report with state space analysis
public export
record StateSpaceReport where
  constructor MkStateSpaceReport
  runtimeCoverage    : CoverageReport
  functionStateSpace : List FunctionStateSpace
  totalEstimated     : StateCount
  totalActual        : Nat
  stateSpaceCoverage : Double
  complexityWarnings : List (String, ComplexityMetrics)   -- Functions that should be split

-- =============================================================================
-- Test Result Types (Unified Runner)
-- =============================================================================

||| Individual test execution result
public export
record TestResult where
  constructor MkTestResult
  testName : String
  passed   : Bool
  message  : Maybe String

public export
Show TestResult where
  show r = "[" ++ (if r.passed then "PASS" else "FAIL") ++ "] " ++ r.testName

public export
Eq TestResult where
  r1 == r2 = r1.testName == r2.testName && r1.passed == r2.passed

||| Combined test and coverage report
public export
record TestCoverageReport where
  constructor MkTestCoverageReport
  testResults     : List TestResult
  totalTests      : Nat
  passedTests     : Nat
  failedTests     : Nat
  branchCoverage  : BranchCoverageSummary
  timestamp       : String

public export
Show TestCoverageReport where
  show r = "Tests: " ++ show r.passedTests ++ "/" ++ show r.totalTests
        ++ ", Branch: " ++ show r.branchCoverage.branchPercent ++ "%"

-- =============================================================================
-- BranchId, BranchClass, ClassifiedBranch, StaticFunctionAnalysis,
-- StaticBranchAnalysis are imported from Coverage.Classification.BranchClass
-- =============================================================================

-- =============================================================================
-- TestRunHits - Runtime hit data from a single test run
-- =============================================================================

||| Branch hits from a single test execution
public export
record BranchHit where
  constructor MkBranchHit
  branchId : BranchId
  hitCount : Nat

public export
Show BranchHit where
  show bh = show bh.branchId ++ " ×" ++ show bh.hitCount

public export
Eq BranchHit where
  bh1 == bh2 = bh1.branchId == bh2.branchId

||| Hits from a single test run
public export
record TestRunHits where
  constructor MkTestRunHits
  testName  : String
  timestamp : String
  hits      : List BranchHit

public export
Show TestRunHits where
  show trh = trh.testName ++ ": " ++ show (length trh.hits) ++ " branches hit"

-- =============================================================================
-- AggregatedCoverage - OR-union across test runs
-- =============================================================================

||| Aggregated coverage across multiple test runs
||| Uses OR semantics: branch is covered if hit by ANY test run
public export
record AggregatedCoverage where
  constructor MkAggregatedCoverage
  staticAnalysis   : StaticBranchAnalysis
  testRuns         : List TestRunHits
  coveredBranches  : List BranchId      -- Branches hit by at least one test
  -- Per-category coverage (per dunham's classification)
  canonicalTotal   : Nat
  canonicalCovered : Nat
  bugsTotal        : Nat                -- UnhandledInput count
  unknownTotal     : Nat                -- Unknown CRASHes count

public export
Show AggregatedCoverage where
  show ac = "AggregatedCoverage: " ++ show ac.canonicalCovered
         ++ "/" ++ show ac.canonicalTotal ++ " canonical ("
         ++ show (coveragePercent ac) ++ "%)"
  where
    coveragePercent : AggregatedCoverage -> Double
    coveragePercent a =
      if a.canonicalTotal == 0
      then 100.0
      else cast a.canonicalCovered / cast a.canonicalTotal * 100.0

||| Calculate coverage percentage for aggregated coverage
public export
aggregatedCoveragePercent : AggregatedCoverage -> Double
aggregatedCoveragePercent ac =
  if ac.canonicalTotal == 0
  then 100.0
  else cast ac.canonicalCovered / cast ac.canonicalTotal * 100.0

-- =============================================================================
-- Coverage Aggregation Functions
-- =============================================================================

||| Merge BranchHits from multiple test runs (OR semantics)
||| A branch is covered if hit by ANY test run
public export
mergeBranchHits : List TestRunHits -> List BranchId
mergeBranchHits runs =
  let allHits = concatMap (.hits) runs
      -- Get unique branch IDs that were hit at least once
      hitBranches = map (.branchId) $ filter (\h => h.hitCount > 0) allHits
  in nub hitBranches

||| Count how many branches of a given class are covered
public export
countCoveredByClass : BranchClass -> List ClassifiedBranch -> List BranchId -> Nat
countCoveredByClass cls allBranches coveredIds =
  let classedBranches = filter (\cb => cb.branchClass == cls) allBranches
      classedIds = map (.branchId) classedBranches
  in length $ filter (\bid => elem bid coveredIds) classedIds

||| Create aggregated coverage from static analysis and test runs
public export
aggregateCoverage : StaticBranchAnalysis -> List TestRunHits -> AggregatedCoverage
aggregateCoverage static runs =
  let covered = mergeBranchHits runs
      canonTotal = length $ filter (\cb => cb.branchClass == BCCanonical) static.allBranches
      canonCovered = countCoveredByClass BCCanonical static.allBranches covered
      bugsTotal = length $ filter (\cb => cb.branchClass == BCBugUnhandledInput) static.allBranches
      unknownTotal = length $ filter (\cb => case cb.branchClass of
                                               BCUnknownCrash _ => True
                                               _ => False) static.allBranches
  in MkAggregatedCoverage static runs covered canonTotal canonCovered bugsTotal unknownTotal

-- =============================================================================
-- FunctionRuntimeHit - Re-exported from Coverage.Core.RuntimeHit
-- =============================================================================

-- FunctionRuntimeHit, MkFunctionRuntimeHit, functionRuntimeCoveragePercent
-- are now imported from Coverage.Core.RuntimeHit