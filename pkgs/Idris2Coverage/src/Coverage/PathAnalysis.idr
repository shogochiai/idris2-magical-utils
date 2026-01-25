||| Path analysis for reachability and pruning
||| Analyzes pattern matches and early exits
module Coverage.PathAnalysis

import Data.List
import Data.Maybe
import Data.String

import Coverage.Types
import Coverage.Linearity

%default total

-- =============================================================================
-- Early Exit Patterns
-- =============================================================================

||| Known early exit patterns
||| These patterns typically short-circuit execution
public export
data EarlyExitPattern : Type where
  NothingPattern : EarlyExitPattern               -- Nothing in Maybe
  LeftPattern    : EarlyExitPattern               -- Left in Either
  NilPattern     : EarlyExitPattern               -- [] in List
  FailPattern    : String -> EarlyExitPattern     -- Custom failure pattern
  GuardPattern   : String -> EarlyExitPattern     -- Guard condition

public export
Show EarlyExitPattern where
  show NothingPattern   = "Nothing"
  show LeftPattern      = "Left _"
  show NilPattern       = "[]"
  show (FailPattern s)  = "fail: " ++ s
  show (GuardPattern s) = "guard: " ++ s

||| Check if a pattern string represents an early exit
export
isEarlyExitPattern : String -> Maybe EarlyExitPattern
isEarlyExitPattern "Nothing"     = Just NothingPattern
isEarlyExitPattern "None"        = Just NothingPattern
isEarlyExitPattern "Left _"      = Just LeftPattern
isEarlyExitPattern "Left"        = Just LeftPattern
isEarlyExitPattern "[]"          = Just NilPattern
isEarlyExitPattern "Nil"         = Just NilPattern
isEarlyExitPattern s =
  if isPrefixOf "Left " s then Just LeftPattern
  else if isPrefixOf "fail" s then Just (FailPattern s)
  else Nothing

-- =============================================================================
-- Pattern Branch Analysis
-- =============================================================================

||| Analyze a single pattern for reachability
export
analyzePattern : String -> PatternBranch
analyzePattern pat =
  case isEarlyExitPattern pat of
    Just _ => MkPatternBranch pat EarlyExit True (Finite 1)
    Nothing =>
      if pat == "_" || pat == "otherwise"
        then MkPatternBranch pat Always False Unbounded  -- Catch-all
        else MkPatternBranch pat Conditional False (Finite 1)

||| Analyze patterns from a case expression
export
analyzePatterns : List String -> List PatternBranch
analyzePatterns = map analyzePattern

isEarlyExitBranch : PatternBranch -> Bool
isEarlyExitBranch b = b.reachability == EarlyExit

||| Count reachable branches (excluding early exits if configured)
export
reachableBranchCount : Bool -> List PatternBranch -> Nat
reachableBranchCount pruneEarly branches =
  let filtered = if pruneEarly
                   then filter (not . isEarlyExitBranch) branches
                   else branches
  in length (the (List PatternBranch) filtered)

-- =============================================================================
-- Maybe/Either Path Analysis
-- =============================================================================

||| Analysis result for Maybe-typed parameter
public export
record MaybePathAnalysis where
  constructor MkMaybePathAnalysis
  paramName      : String
  nothingBranch  : PatternBranch
  justBranch     : PatternBranch
  canPruneNothing : Bool           -- True if Nothing leads to trivial early return

export
Show MaybePathAnalysis where
  show m = m.paramName ++ ": Maybe (prune Nothing: " ++ show m.canPruneNothing ++ ")"

||| Analyze Maybe parameter paths
export
analyzeMaybePaths : String -> MaybePathAnalysis
analyzeMaybePaths paramName =
  let nothing = MkPatternBranch "Nothing" EarlyExit True (Finite 1)
      justP   = MkPatternBranch ("Just " ++ paramName) Always False (Finite 1)
  in MkMaybePathAnalysis paramName nothing justP True

||| Analysis result for Either-typed parameter
public export
record EitherPathAnalysis where
  constructor MkEitherPathAnalysis
  paramName     : String
  leftBranch    : PatternBranch
  rightBranch   : PatternBranch
  canPruneLeft  : Bool            -- True if Left leads to error propagation

export
Show EitherPathAnalysis where
  show e = e.paramName ++ ": Either (prune Left: " ++ show e.canPruneLeft ++ ")"

||| Analyze Either parameter paths
export
analyzeEitherPaths : String -> EitherPathAnalysis
analyzeEitherPaths paramName =
  let leftP  = MkPatternBranch ("Left " ++ paramName) EarlyExit True (Finite 1)
      rightP = MkPatternBranch ("Right " ++ paramName) Always False (Finite 1)
  in MkEitherPathAnalysis paramName leftP rightP True

-- =============================================================================
-- Function Path Analysis
-- =============================================================================

||| Path analysis for a function
public export
record FunctionPathAnalysis where
  constructor MkFunctionPathAnalysis
  funcName        : String
  totalBranches   : Nat
  reachable       : Nat
  earlyExits      : Nat
  prunedPaths     : List PatternBranch
  maybeAnalyses   : List MaybePathAnalysis
  eitherAnalyses  : List EitherPathAnalysis
  effectiveStates : StateCount        -- After pruning

export
Show FunctionPathAnalysis where
  show f = f.funcName ++ ": " ++ show f.reachable ++ "/" ++ show f.totalBranches
        ++ " branches (pruned: " ++ show (length f.prunedPaths) ++ ")"

||| Analyze a function's parameters for path pruning opportunities
export
analyzeFunctionPaths : String -> List LinearParam -> FunctionPathAnalysis
analyzeFunctionPaths funcName params =
  let maybeParams = filter isMaybeType params
      eitherParams = filter isEitherType params
      maybeAnalyses = map (\p => analyzeMaybePaths (fromMaybe "_" p.paramName)) maybeParams
      eitherAnalyses = map (\p => analyzeEitherPaths (fromMaybe "_" p.paramName)) eitherParams
      -- Prunable paths
      prunableMaybe = filter (.canPruneNothing) maybeAnalyses
      prunableEither = filter (.canPruneLeft) eitherAnalyses
      prunedPaths = map (.nothingBranch) prunableMaybe ++ map (.leftBranch) prunableEither
      -- Calculate effective state count
      totalBranches = calculateTotalBranches params
      prunedCount = length prunedPaths
      effectiveCount = totalBranches `minus` prunedCount
  in MkFunctionPathAnalysis
       funcName
       totalBranches
       effectiveCount
       (length prunableMaybe + length prunableEither)
       prunedPaths
       maybeAnalyses
       eitherAnalyses
       (Finite effectiveCount)
  where
    isMaybeType : LinearParam -> Bool
    isMaybeType p = isPrefixOf "Maybe" p.paramType

    isEitherType : LinearParam -> Bool
    isEitherType p = isPrefixOf "Either" p.paramType

    calculateTotalBranches : List LinearParam -> Nat
    calculateTotalBranches [] = 1
    calculateTotalBranches (p :: ps) =
      let thisBranches = if isMaybeType p then 2
                         else if isEitherType p then 2
                         else 1
      in thisBranches * calculateTotalBranches ps

-- =============================================================================
-- Path Coverage Calculation
-- =============================================================================

||| Calculate effective test count after path pruning
export
effectiveTestCount : FunctionPathAnalysis -> StateCount -> StateCount
effectiveTestCount pathAnalysis originalCount =
  let pruneFactor = length pathAnalysis.prunedPaths
  in case originalCount of
    Finite n  => Finite (n `minus` pruneFactor)
    Bounded n => Bounded (n `minus` pruneFactor)
    Unbounded => Unbounded

||| Determine if a test case covers an early exit path
export
coversEarlyExit : String -> List PatternBranch -> Bool
coversEarlyExit testName paths =
  any (\p => p.reachability == EarlyExit && isInfixOf p.pattern testName) paths

-- =============================================================================
-- Dependency Analysis
-- =============================================================================

||| Parameter dependency (one param's value affects another's relevance)
public export
record ParamDependency where
  constructor MkParamDependency
  sourceParam : String
  sourceValue : String            -- e.g., "Nothing"
  targetParam : String
  effect      : String            -- "irrelevant", "constrained", etc.

export
Show ParamDependency where
  show d = d.sourceParam ++ "=" ++ d.sourceValue ++ " => " ++ d.targetParam ++ " " ++ d.effect

||| Find dependencies between parameters
||| e.g., if first Maybe is Nothing, second param is irrelevant
export
findDependencies : List LinearParam -> List ParamDependency
findDependencies params =
  concatMap (findDepsFor params) (zip [0..length params] params)
  where
    findDepsFor : List LinearParam -> (Nat, LinearParam) -> List ParamDependency
    findDepsFor allParams (idx, p) =
      let pName = fromMaybe ("p" ++ show idx) p.paramName
      in if isPrefixOf "Maybe" p.paramType
         then -- If this is Nothing, later params may be irrelevant
              let laterParams = drop (cast $ idx + 1) allParams
                  deps = map (\lp => MkParamDependency pName "Nothing"
                                       (fromMaybe "_" lp.paramName) "irrelevant") laterParams
              in deps
         else []

-- =============================================================================
-- Path Pruning Report
-- =============================================================================

||| Summary of path pruning for a module
public export
record PathPruningReport where
  constructor MkPathPruningReport
  moduleName       : String
  totalFunctions   : Nat
  totalPaths       : Nat
  prunedPaths      : Nat
  effectivePaths   : Nat
  savingsPercent   : Double

export
Show PathPruningReport where
  show r = r.moduleName ++ ": " ++ show r.effectivePaths ++ "/" ++ show r.totalPaths
        ++ " paths (" ++ show r.savingsPercent ++ "% reduction)"

||| Generate pruning report for multiple function analyses
export
generatePruningReport : String -> List FunctionPathAnalysis -> PathPruningReport
generatePruningReport modName analyses =
  let totalPaths = sum $ map (.totalBranches) analyses
      pruned = sum $ map (length . (.prunedPaths)) analyses
      effective = totalPaths `minus` pruned
      savings = if totalPaths == 0 then 0.0
                else (cast pruned / cast totalPaths) * 100.0
  in MkPathPruningReport modName (length analyses) totalPaths pruned effective savings
