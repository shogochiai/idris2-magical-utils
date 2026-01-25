||| State space calculation for type-driven coverage
||| Computes estimated test cases from type signatures
module Coverage.StateSpace

import Data.List
import Data.Maybe
import Data.String

import Coverage.Types
import Coverage.Linearity
import Coverage.TypeAnalyzer

%default total

-- =============================================================================
-- Configuration
-- =============================================================================

||| State space calculation configuration
public export
record StateSpaceConfig where
  constructor MkStateSpaceConfig
  equivalenceClassLimit : Nat      -- Max states per parameter (default: 10)
  maxTotalStates        : Nat      -- Max total state space (default: 1000)
  recursionDepth        : Nat      -- Max depth for recursive types (default: 3)
  pruneEarlyExits       : Bool     -- Prune Nothing/Left paths (default: True)

||| Default configuration
export
defaultConfig : StateSpaceConfig
defaultConfig = MkStateSpaceConfig 10 1000 3 True

-- =============================================================================
-- State Space Calculation
-- =============================================================================

||| Apply configuration limits to a state count
export
applyLimits : StateSpaceConfig -> StateCount -> StateCount
applyLimits cfg sc = boundStateCount cfg.equivalenceClassLimit sc

||| Calculate state space for a single parameter
export
paramStateSpace : StateSpaceConfig -> LinearParam -> StateCount
paramStateSpace cfg param =
  case param.quantity of
    Q0 => Finite 0  -- Erased, no runtime states
    _  => case param.typeInfo of
            Just ti => applyLimits cfg ti.stateCount
            Nothing => Bounded cfg.equivalenceClassLimit

capTotal : Nat -> StateCount -> StateCount
capTotal maxTotal (Finite n)  = if n > maxTotal then Bounded maxTotal else Finite n
capTotal maxTotal (Bounded n) = Bounded (min maxTotal n)
capTotal maxTotal Unbounded   = Bounded maxTotal

||| Calculate total state space for function parameters
export
functionStateSpace : StateSpaceConfig -> List LinearParam -> StateCount
functionStateSpace cfg [] = Finite 1
functionStateSpace cfg params =
  let paramCounts = map (paramStateSpace cfg) params
  in capTotal cfg.maxTotalStates (foldl multStateCount (Finite 1) paramCounts)

-- =============================================================================
-- Test Case Estimation
-- =============================================================================

||| Estimated test case with parameter values
public export
record EstimatedTestCase where
  constructor MkEstimatedTestCase
  paramValues  : List (String, String)  -- (paramName, representativeValue)
  category     : String                 -- "happy", "boundary", "error"
  priority     : Nat                    -- Higher = more important

export
Show EstimatedTestCase where
  show tc = tc.category ++ ": " ++ show tc.paramValues

||| Generate representative values for a type
export
representativeValues : TypeInfo -> List (String, String)
representativeValues ti =
  case ti.typeName of
    "Bool"    => [("True", "happy"), ("False", "happy")]
    "Nat"     => [("0", "boundary"), ("1", "typical"), ("100", "typical")]
    "Int"     => [("-1", "boundary"), ("0", "boundary"), ("1", "typical")]
    "String"  => [("\"\"", "boundary"), ("\"a\"", "typical"), ("\"abc\"", "typical")]
    _         =>
      if isPrefixOf "Maybe" ti.typeName
        then [("Nothing", "error"), ("Just _", "happy")]
      else if isPrefixOf "Either" ti.typeName
        then [("Left _", "error"), ("Right _", "happy")]
      else if isPrefixOf "List" ti.typeName
        then [("[]", "boundary"), ("[x]", "typical"), ("[x,y,z]", "typical")]
      else
        -- Use constructors if available
        case ti.constructors of
          [] => [("_", "unknown")]
          ctors => map (\c => (c.ctorName, "typical")) ctors

||| Combine a parameter value with existing test cases
combineWithTestCase : String -> String -> String -> EstimatedTestCase -> EstimatedTestCase
combineWithTestCase pName v cat tc =
  MkEstimatedTestCase
    ((pName, v) :: tc.paramValues)
    (if cat == "error" then "error" else tc.category)
    tc.priority

||| Generate combinations of test cases
export
generateCombinations : StateSpaceConfig -> List LinearParam -> List EstimatedTestCase
generateCombinations _ [] = [MkEstimatedTestCase [] "empty" 0]
generateCombinations cfg (p :: ps) =
  let pName = fromMaybe "_" p.paramName
      pValues = case p.typeInfo of
        Just ti => representativeValues ti
        Nothing => [("_", "unknown")]
      restCases = generateCombinations cfg ps
      combined = concatMap (\vc => map (combineWithTestCase pName (fst vc) (snd vc)) restCases) pValues
  in take (cast cfg.maxTotalStates) combined

||| Generate test case suggestions for a function
export
suggestTestCases : StateSpaceConfig -> AnalyzedFunction -> List EstimatedTestCase
suggestTestCases cfg func =
  if null func.params
    then [MkEstimatedTestCase [] "unit" 1]
    else generateCombinations cfg func.params

-- =============================================================================
-- Coverage Gap Analysis
-- =============================================================================

||| Gap in test coverage
public export
record CoverageGap where
  constructor MkCoverageGap
  funcName     : String
  missingCase  : EstimatedTestCase
  reason       : String
  severity     : Nat              -- 1-5, higher = more critical

export
Show CoverageGap where
  show g = g.funcName ++ ": missing " ++ g.reason ++ " (severity " ++ show g.severity ++ ")"

categorySeverity : String -> Nat
categorySeverity "error"    = 5  -- Error paths are critical
categorySeverity "boundary" = 4  -- Boundary cases often hide bugs
categorySeverity "happy"    = 3
categorySeverity "typical"  = 2
categorySeverity _          = 1

||| Identify coverage gaps
export
findCoverageGaps : StateSpaceConfig -> AnalyzedFunction -> List String -> List CoverageGap
findCoverageGaps cfg func coveredCases =
  let suggested = suggestTestCases cfg func
      coveredCategories = nub coveredCases
      missingCategories = filter (\tc => not (tc.category `elem` coveredCategories)) suggested
  in map (toGap func.name) missingCategories
  where
    toGap : String -> EstimatedTestCase -> CoverageGap
    toGap funcName tc = MkCoverageGap funcName tc tc.category (categorySeverity tc.category)

-- =============================================================================
-- State Space Report
-- =============================================================================

||| State space analysis for a single function
public export
record FunctionStateSpaceAnalysis where
  constructor MkFunctionStateSpaceAnalysis
  function         : AnalyzedFunction
  estimatedCases   : StateCount
  suggestedTests   : List EstimatedTestCase
  gaps             : List CoverageGap
  linearityImpact  : String          -- Description of linearity effects

export
Show FunctionStateSpaceAnalysis where
  show a = show a.function ++ " | estimated: " ++ show a.estimatedCases
        ++ " | gaps: " ++ show (length a.gaps)

||| Analyze a function's state space
export
analyzeStateSpace : StateSpaceConfig -> AnalyzedFunction -> List String -> FunctionStateSpaceAnalysis
analyzeStateSpace cfg func coveredCases =
  let estimated = functionStateSpace cfg func.params
      suggested = suggestTestCases cfg func
      gaps = findCoverageGaps cfg func coveredCases
      linearityDesc = describeLinearity func.params
  in MkFunctionStateSpaceAnalysis func estimated suggested gaps linearityDesc
  where
    describeLinearity : List LinearParam -> String
    describeLinearity ps =
      let linear = filter isLinear ps
          erased = filter isErased ps
      in case (length linear, length erased) of
        (0, 0) => "No linearity annotations"
        (l, 0) => show l ++ " linear param(s) - usage guaranteed"
        (0, e) => show e ++ " erased param(s) - compile-time only"
        (l, e) => show l ++ " linear, " ++ show e ++ " erased"

-- =============================================================================
-- Batch Analysis
-- =============================================================================

||| Analyze all functions in a module
export
analyzeModuleStateSpace : StateSpaceConfig -> ModuleAnalysis -> List FunctionStateSpaceAnalysis
analyzeModuleStateSpace cfg modAnalysis =
  map (\f => analyzeStateSpace cfg f []) modAnalysis.functions

||| Summary statistics for state space analysis
public export
record StateSpaceSummary where
  constructor MkStateSpaceSummary
  totalFunctions    : Nat
  totalEstimated    : StateCount
  avgStateSpace     : Double
  functionsWithGaps : Nat
  totalGaps         : Nat
  highSeverityGaps  : Nat

export
Show StateSpaceSummary where
  show s = "Functions: " ++ show s.totalFunctions
        ++ " | Total estimated: " ++ show s.totalEstimated
        ++ " | Gaps: " ++ show s.totalGaps
        ++ " (high: " ++ show s.highSeverityGaps ++ ")"

||| Compute summary from analyses
export
summarizeStateSpace : List FunctionStateSpaceAnalysis -> StateSpaceSummary
summarizeStateSpace analyses =
  let totalCount = length analyses
      estimated = foldl addStateCount (Finite 0) (map (.estimatedCases) analyses)
      avgSS = case estimated of
        Finite n  => cast n / cast (max 1 totalCount)
        Bounded n => cast n / cast (max 1 totalCount)
        Unbounded => 0.0
      allGaps = concatMap (.gaps) analyses
      withGaps = length $ filter (\a => not (null a.gaps)) analyses
      highSev = length $ filter (\g => g.severity >= 4) allGaps
  in MkStateSpaceSummary totalCount estimated avgSS withGaps (length allGaps) highSev
