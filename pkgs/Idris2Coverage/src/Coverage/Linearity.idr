||| Linearity analysis for coverage
||| Handles QTT (Quantitative Type Theory) annotations
module Coverage.Linearity

import Data.List
import Data.Maybe
import Data.String

import Coverage.Types

%default total

-- =============================================================================
-- Linearity Impact on State Space
-- =============================================================================

||| Calculate how linearity affects state space
||| Q0 (erased): No runtime test needed
||| Q1 (linear): Must be used exactly once, eliminates "unused" paths
||| QW (unrestricted): Normal state space
public export
linearityFactor : Quantity -> StateCount -> StateCount
linearityFactor Q0 _         = Finite 0     -- Erased, no tests needed
linearityFactor Q1 s         = s            -- Linear: same count, but usage guaranteed
linearityFactor QW s         = s            -- Unrestricted: normal

||| Check if a parameter is erased (compile-time only)
public export
isErased : LinearParam -> Bool
isErased p = p.quantity == Q0

||| Check if a parameter is linear
public export
isLinear : LinearParam -> Bool
isLinear p = p.quantity == Q1

||| Filter out erased parameters (they don't need runtime tests)
public export
runtimeParams : List LinearParam -> List LinearParam
runtimeParams = filter (not . isErased)

-- =============================================================================
-- Linear Resource Analysis
-- =============================================================================

||| Known linear resource patterns
||| These are types that typically require acquire/release semantics
public export
knownLinearResources : List (String, List String, List String)
knownLinearResources =
  [ ("File",       ["openFile", "withFile"],     ["closeFile"])
  , ("Handle",     ["openHandle"],               ["close"])
  , ("Connection", ["connect", "openConnection"], ["disconnect", "close"])
  , ("Socket",     ["socket", "accept"],          ["close"])
  , ("Lock",       ["acquire", "lock"],           ["release", "unlock"])
  , ("Channel",    ["makeChannel"],               ["closeChannel"])
  ]

||| Check if a type name suggests a linear resource
public export
isLinearResourceType : String -> Bool
isLinearResourceType typeName =
  any (\(name, _, _) => isInfixOf name typeName) knownLinearResources

||| Get resource info for a type
public export
getResourceInfo : String -> Maybe LinearResource
getResourceInfo typeName =
  case find (\(name, _, _) => isInfixOf name typeName) knownLinearResources of
    Just (name, acquirers, consumers) =>
      Just $ MkLinearResource name acquirers consumers
    Nothing => Nothing

-- =============================================================================
-- Linear Path Pruning
-- =============================================================================

||| Determine if a pattern branch can be pruned for linear params
||| Linear params must be used, so "ignore" branches don't exist
public export
canPruneForLinearity : List LinearParam -> PatternBranch -> Bool
canPruneForLinearity params branch =
  -- If any linear param would be unused in this branch, it's invalid
  -- (Idris2 type checker would reject it)
  branch.reachability == Unreachable

||| Prune branches that are impossible due to linearity constraints
public export
pruneLinearBranches : List LinearParam -> List PatternBranch -> List PatternBranch
pruneLinearBranches params = filter (not . canPruneForLinearity params)

-- =============================================================================
-- Resource Coverage Verification
-- =============================================================================

||| Check if a linear resource is properly covered
||| All acquire paths should have corresponding consume coverage
public export
isResourceCovered : LinearResource -> List FunctionCoverage -> Bool
isResourceCovered res coverages =
  let coveredFuncs = map (.name) coverages
      hasAcquire = any (`elem` coveredFuncs) res.acquiredBy
      hasConsume = any (`elem` coveredFuncs) res.consumedBy
  in hasAcquire && hasConsume

||| Find uncovered resource operations
public export
findUncoveredResources : List LinearResource -> List FunctionCoverage -> List (LinearResource, String)
findUncoveredResources resources coverages =
  let coveredFuncs = map (.name) coverages
  in concatMap (checkResource coveredFuncs) resources
  where
    checkResource : List String -> LinearResource -> List (LinearResource, String)
    checkResource covered res =
      let uncoveredAcquire = filter (not . (`elem` covered)) res.acquiredBy
          uncoveredConsume = filter (not . (`elem` covered)) res.consumedBy
      in map (\f => (res, "acquire: " ++ f)) uncoveredAcquire
      ++ map (\f => (res, "consume: " ++ f)) uncoveredConsume

-- =============================================================================
-- Linear Parameter State Space Calculation
-- =============================================================================

||| Calculate effective state space considering linearity
public export
effectiveStateSpace : List LinearParam -> StateCount
effectiveStateSpace [] = Finite 1
effectiveStateSpace params =
  let runtime = runtimeParams params
      counts = map getParamStateCount runtime
  in foldl multStateCount (Finite 1) counts
  where
    getParamStateCount : LinearParam -> StateCount
    getParamStateCount p = case p.typeInfo of
      Just ti => linearityFactor p.quantity ti.stateCount
      Nothing => Unbounded  -- Unknown type

-- =============================================================================
-- Linearity Warnings
-- =============================================================================

||| Warning about linear resource usage
public export
record LinearityWarning where
  constructor MkLinearityWarning
  funcName    : String
  paramName   : String
  warningType : String
  message     : String

public export
Show LinearityWarning where
  show w = w.funcName ++ "." ++ w.paramName ++ ": " ++ w.message

||| Check for potential linearity issues
public export
checkLinearityIssues : String -> List LinearParam -> List LinearityWarning
checkLinearityIssues funcName params =
  concatMap (checkParam funcName) params
  where
    checkParam : String -> LinearParam -> List LinearityWarning
    checkParam fn p =
      let name = fromMaybe "_" p.paramName
      in case p.quantity of
        Q1 => if isLinearResourceType p.paramType
              then [MkLinearityWarning fn name "resource"
                    "Linear resource - ensure acquire/consume coverage"]
              else []
        Q0 => [MkLinearityWarning fn name "erased"
               "Erased parameter - no runtime tests needed"]
        QW => []

-- =============================================================================
-- Quantity Parsing
-- =============================================================================

||| Parse quantity from string (from Idris2 output)
public export
parseQuantity : String -> Quantity
parseQuantity "0" = Q0
parseQuantity "1" = Q1
parseQuantity "ω" = QW
parseQuantity _   = QW  -- Default to unrestricted

||| Parse linear parameter from signature fragment
||| e.g., "(1 h : FileHandle)" -> LinearParam
public export
parseLinearParam : String -> Maybe LinearParam
parseLinearParam s =
  let trimmed = trim s
      -- Remove parentheses if present
      content = if isPrefixOf "(" trimmed && isSuffixOf ")" trimmed
                then substr 1 (length trimmed `minus` 2) trimmed
                else trimmed
      parts = words content
  in case parts of
    [q, name, ":", ty] =>
      Just $ MkLinearParam (Just name) ty (parseQuantity q) Nothing
    [name, ":", ty] =>
      Just $ MkLinearParam (Just name) ty QW Nothing
    _ => Nothing
