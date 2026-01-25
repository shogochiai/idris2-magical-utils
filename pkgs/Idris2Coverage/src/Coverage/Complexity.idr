||| Complexity metrics and split recommendations
||| Helps manage combinatorial explosion through function decomposition
module Coverage.Complexity

import Data.List
import Data.Maybe
import Data.String

import Coverage.Types
import Coverage.Linearity
import Coverage.TypeAnalyzer
import Coverage.StateSpace
import Coverage.PathAnalysis

%default total

-- =============================================================================
-- Complexity Thresholds
-- =============================================================================

||| Configuration for complexity analysis
public export
record ComplexityConfig where
  constructor MkComplexityConfig
  maxParams          : Nat      -- Max recommended parameters (default: 4)
  maxStateSpace      : Nat      -- Max recommended state space (default: 50)
  maxPatternDepth    : Nat      -- Max nesting of pattern matches (default: 3)
  maxBranches        : Nat      -- Max branches in function (default: 10)
  warnLinearOverload : Bool     -- Warn if too many linear params (default: True)

||| Default complexity configuration
export
defaultComplexityConfig : ComplexityConfig
defaultComplexityConfig = MkComplexityConfig 4 50 3 10 True

-- =============================================================================
-- Complexity Scoring
-- =============================================================================

||| Individual complexity factors
public export
record ComplexityFactors where
  constructor MkComplexityFactors
  paramFactor       : Nat       -- Based on parameter count
  stateFactor       : Nat       -- Based on state space size
  depthFactor       : Nat       -- Based on pattern match depth
  branchFactor      : Nat       -- Based on branch count
  linearityFactor   : Nat       -- Based on linear parameter handling
  totalScore        : Nat       -- Combined score

export
Show ComplexityFactors where
  show f = "Complexity(" ++ show f.totalScore ++ "): "
        ++ "params=" ++ show f.paramFactor
        ++ ", states=" ++ show f.stateFactor
        ++ ", depth=" ++ show f.depthFactor
        ++ ", branches=" ++ show f.branchFactor

paramComplexity : ComplexityConfig -> Nat -> Nat
paramComplexity cfg n =
  if n <= cfg.maxParams then 0
  else (n `minus` cfg.maxParams) * 10

covering
natDiv : Nat -> Nat -> Nat
natDiv _ Z = 0
natDiv n (S d) = go n (S d) 0
  where
    covering
    go : Nat -> Nat -> Nat -> Nat
    go Z _ acc = acc
    go n divisor acc = if n < divisor then acc else go (n `minus` divisor) divisor (S acc)

covering
stateComplexity : ComplexityConfig -> StateCount -> Nat
stateComplexity cfg (Finite n)  = if n <= cfg.maxStateSpace then 0 else natDiv n 10
stateComplexity cfg (Bounded n) = if n <= cfg.maxStateSpace then 5 else natDiv n 5
stateComplexity cfg Unbounded   = 50

depthComplexity : ComplexityConfig -> Nat -> Nat
depthComplexity cfg d =
  if d <= cfg.maxPatternDepth then 0
  else (d `minus` cfg.maxPatternDepth) * 15

branchComplexity : ComplexityConfig -> Nat -> Nat
branchComplexity cfg b =
  if b <= cfg.maxBranches then 0
  else (b `minus` cfg.maxBranches) * 5

linearComplexity : ComplexityConfig -> List LinearParam -> Nat
linearComplexity cfg ps =
  let linearCount = length $ filter isLinear ps
  in if cfg.warnLinearOverload && linearCount > 2
     then linearCount * 10
     else 0

||| Calculate complexity factors for a function
export
covering
calculateFactors : ComplexityConfig -> List LinearParam -> StateCount -> Nat -> Nat -> ComplexityFactors
calculateFactors cfg params stateSpace patternDepth branchCount =
  let paramF = paramComplexity cfg (length params)
      stateF = stateComplexity cfg stateSpace
      depthF = depthComplexity cfg patternDepth
      branchF = branchComplexity cfg branchCount
      linearF = linearComplexity cfg params
      totalScore = paramF + stateF + depthF + branchF + linearF
  in MkComplexityFactors paramF stateF depthF branchF linearF totalScore

-- =============================================================================
-- Split Recommendations
-- =============================================================================

||| Reason for recommending a function split
public export
data SplitReason : Type where
  TooManyParams     : Nat -> SplitReason
  StateSpaceTooBig  : StateCount -> SplitReason
  TooManyBranches   : Nat -> SplitReason
  DeepNesting       : Nat -> SplitReason
  LinearOverload    : Nat -> SplitReason
  CombinedComplexity : Nat -> SplitReason

public export
Show SplitReason where
  show (TooManyParams n)      = "Too many parameters: " ++ show n
  show (StateSpaceTooBig s)   = "State space too large: " ++ show s
  show (TooManyBranches n)    = "Too many branches: " ++ show n
  show (DeepNesting n)        = "Deep nesting: " ++ show n ++ " levels"
  show (LinearOverload n)     = "Too many linear params: " ++ show n
  show (CombinedComplexity n) = "Combined complexity score: " ++ show n

||| Split recommendation
public export
record SplitRecommendation where
  constructor MkSplitRecommendation
  funcName       : String
  reasons        : List SplitReason
  suggestedSplit : String          -- Description of how to split
  priority       : Nat             -- Higher = more urgent

export
Show SplitRecommendation where
  show r = r.funcName ++ " (priority " ++ show r.priority ++ "): "
        ++ fastConcat (intersperse ", " $ map show r.reasons)

||| Check if function should be split
export
shouldSplit : ComplexityConfig -> ComplexityFactors -> Bool
shouldSplit cfg factors = factors.totalScore >= 30

||| Generate split reasons
export
generateSplitReasons : ComplexityConfig -> List LinearParam -> StateCount -> Nat -> Nat -> List SplitReason
generateSplitReasons cfg params stateSpace depth branches =
  let reasons = []
      reasons' = if length params > cfg.maxParams
                   then TooManyParams (length params) :: reasons
                   else reasons
      reasons'' = case stateSpace of
                    Finite n  => if n > cfg.maxStateSpace
                                   then StateSpaceTooBig stateSpace :: reasons'
                                   else reasons'
                    Bounded n => if n > cfg.maxStateSpace
                                   then StateSpaceTooBig stateSpace :: reasons'
                                   else reasons'
                    Unbounded => StateSpaceTooBig stateSpace :: reasons'
      reasons''' = if branches > cfg.maxBranches
                     then TooManyBranches branches :: reasons''
                     else reasons''
      reasons'''' = if depth > cfg.maxPatternDepth
                      then DeepNesting depth :: reasons'''
                      else reasons'''
      linearCount = length $ filter isLinear params
      reasonsFinal = if linearCount > 2
                       then LinearOverload linearCount :: reasons''''
                       else reasons''''
  in reasonsFinal

||| Suggest how to split a function
export
suggestSplit : List SplitReason -> String
suggestSplit reasons =
  let suggestions = map reasonToSuggestion reasons
  in fastConcat $ intersperse "; " suggestions
  where
    reasonToSuggestion : SplitReason -> String
    reasonToSuggestion (TooManyParams _)     = "Extract parameter groups into records"
    reasonToSuggestion (StateSpaceTooBig _)  = "Split into smaller focused functions"
    reasonToSuggestion (TooManyBranches _)   = "Extract branches into helper functions"
    reasonToSuggestion (DeepNesting _)       = "Flatten nested matches with helper functions"
    reasonToSuggestion (LinearOverload _)    = "Group linear resources into a resource record"
    reasonToSuggestion (CombinedComplexity _)= "Consider overall decomposition"

-- =============================================================================
-- Full Complexity Analysis
-- =============================================================================

||| Complete complexity analysis for a function
export
covering
analyzeComplexity : ComplexityConfig -> AnalyzedFunction -> FunctionPathAnalysis -> ComplexityMetrics
analyzeComplexity cfg func pathAnalysis =
  let params = func.params
      stateSpace = func.stateSpace
      patternDepth = 1  -- Simplified; would need AST analysis for accurate depth
      branchCount = pathAnalysis.totalBranches
      factors = calculateFactors cfg params stateSpace patternDepth branchCount
      split = shouldSplit cfg factors
      reasons : List SplitReason = if split
                  then generateSplitReasons cfg params stateSpace patternDepth branchCount
                  else []
      reasonStr = if null reasons then Nothing else Just (suggestSplit reasons)
      linearCount = length $ filter isLinear params
  in MkComplexityMetrics
       (length params)
       stateSpace
       patternDepth
       branchCount
       linearCount
       split
       reasonStr

-- =============================================================================
-- Module-Level Analysis
-- =============================================================================

||| Complexity analysis for all functions in a module
public export
record ModuleComplexityReport where
  constructor MkModuleComplexityReport
  moduleName         : String
  functionMetrics    : List (String, ComplexityMetrics)
  splitRecommendations : List SplitRecommendation
  avgComplexity      : Double
  maxComplexity      : Nat
  functionsNeedingSplit : Nat

export
Show ModuleComplexityReport where
  show r = r.moduleName ++ ": avg complexity " ++ show r.avgComplexity
        ++ ", " ++ show r.functionsNeedingSplit ++ " functions need splitting"

||| Analyze complexity for a module
export
covering
analyzeModuleComplexity : ComplexityConfig -> List (AnalyzedFunction, FunctionPathAnalysis) -> ModuleComplexityReport
analyzeModuleComplexity cfg funcAnalyses =
  let metrics = map analyzeFunc funcAnalyses
      splitRecs = mapMaybe toSplitRec metrics
      scores = map (totalScore . snd) metrics
      avgScore = if null scores then 0.0
                 else cast (sum scores) / cast (length scores)
      maxScore = foldr max 0 scores
      needsSplit = length splitRecs
  in MkModuleComplexityReport "" metrics splitRecs avgScore maxScore needsSplit
  where
    analyzeFunc : (AnalyzedFunction, FunctionPathAnalysis) -> (String, ComplexityMetrics)
    analyzeFunc (func, pathA) =
      let m = analyzeComplexity cfg func pathA
      in (func.name, m)

    totalScore : ComplexityMetrics -> Nat
    totalScore m =
      let stateScore = case m.stateSpaceSize of
            Finite n  => n
            Bounded n => n
            Unbounded => 100
      in m.paramCount * 5 + stateScore + m.branchCount * 2

    toSplitRec : (String, ComplexityMetrics) -> Maybe SplitRecommendation
    toSplitRec (name, m) =
      if m.shouldSplit
        then Just $ MkSplitRecommendation name [] (fromMaybe "Consider splitting" m.splitReason) 1
        else Nothing

-- =============================================================================
-- Complexity Warnings
-- =============================================================================

||| Warning about function complexity
public export
record ComplexityWarning where
  constructor MkComplexityWarning
  funcName    : String
  warningType : String
  message     : String
  severity    : Nat           -- 1-5

export
Show ComplexityWarning where
  show w = "[" ++ show w.severity ++ "] " ++ w.funcName ++ ": " ++ w.message

||| Generate warnings from complexity analysis
export
generateWarnings : ComplexityConfig -> String -> ComplexityMetrics -> List ComplexityWarning
generateWarnings cfg funcName metrics =
  let warnings = []
      warnings' = case metrics.stateSpaceSize of
        Unbounded => MkComplexityWarning funcName "unbounded"
                       "Unbounded state space - consider adding constraints" 5 :: warnings
        Bounded n => if n > cfg.maxStateSpace * 2
                       then MkComplexityWarning funcName "large-state"
                              ("State space " ++ show n ++ " exceeds recommended limit") 4 :: warnings
                       else warnings
        Finite n  => if n > cfg.maxStateSpace * 2
                       then MkComplexityWarning funcName "large-state"
                              ("State space " ++ show n ++ " exceeds recommended limit") 3 :: warnings
                       else warnings
      warnings'' = if metrics.paramCount > cfg.maxParams
                     then MkComplexityWarning funcName "many-params"
                            ("Function has " ++ show metrics.paramCount ++ " parameters") 3 :: warnings'
                     else warnings'
      warnings''' = if metrics.linearParams > 2
                      then MkComplexityWarning funcName "linear-heavy"
                             ("Function has " ++ show metrics.linearParams ++ " linear parameters") 2 :: warnings''
                      else warnings''
  in warnings'''
