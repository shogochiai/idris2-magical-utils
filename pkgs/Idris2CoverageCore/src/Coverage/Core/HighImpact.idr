||| High Impact Target Types for Coverage Analysis
module Coverage.Core.HighImpact

import Data.List
import Data.String

import Coverage.Core.RuntimeHit

%default covering

-- =============================================================================
-- High Impact Kind Classification
-- =============================================================================

||| Classification of high-impact coverage gaps
public export
data HighImpactKind
  = HIT_UntestedCanonical
  | HIT_BugUnhandledInput
  | HIT_UnknownCrash

public export
Show HighImpactKind where
  show HIT_UntestedCanonical = "untested_canonical"
  show HIT_BugUnhandledInput = "bug_unhandled_input"
  show HIT_UnknownCrash      = "unknown_crash"

public export
Eq HighImpactKind where
  HIT_UntestedCanonical == HIT_UntestedCanonical = True
  HIT_BugUnhandledInput == HIT_BugUnhandledInput = True
  HIT_UnknownCrash      == HIT_UnknownCrash      = True
  _ == _ = False

||| Priority order for HighImpactKind (lower = higher priority)
public export
kindPriority : HighImpactKind -> Nat
kindPriority HIT_BugUnhandledInput = 0
kindPriority HIT_UnknownCrash      = 1
kindPriority HIT_UntestedCanonical = 2

-- =============================================================================
-- High Impact Target
-- =============================================================================

||| A high-impact target for coverage improvement
public export
record HighImpactTarget where
  constructor MkHighImpactTarget
  kind          : HighImpactKind
  funcName      : String
  moduleName    : String
  branchCount   : Nat
  executedCount : Nat
  severity      : Double
  note          : String

||| Format severity as string (handles Inf case)
public export
showSeverity : Double -> String
showSeverity s =
  if s > 1.0e308
     then "Inf"
     else let rounded = cast {to=Int} (s * 100.0)
          in show (cast {to=Double} rounded / 100.0)

public export
Show HighImpactTarget where
  show t = "[" ++ show (kind t) ++ "] " ++ funcName t
        ++ " (executed=" ++ show (executedCount t) ++ "/" ++ show (branchCount t)
        ++ ", severity=" ++ showSeverity (severity t) ++ ")"

public export
Eq HighImpactTarget where
  t1 == t2 = kind t1 == kind t2
          && funcName t1 == funcName t2
          && branchCount t1 == branchCount t2

-- =============================================================================
-- Severity Calculation
-- =============================================================================

||| Infinity constant for severity comparisons
public export
severityInfinity : Double
severityInfinity = 1.0e309

||| Calculate severity score as branchCount/executedCount ratio
public export
severityRatio : Nat -> Nat -> Double
severityRatio bc ec =
  if ec == 0
     then severityInfinity
     else cast bc / cast ec

-- =============================================================================
-- Sorting and Filtering
-- =============================================================================

compareSeverity : HighImpactTarget -> HighImpactTarget -> Ordering
compareSeverity a b =
  case compare (severity b) (severity a) of
    EQ => compare (branchCount b) (branchCount a)
    x  => x

||| Sort targets by severity (descending)
public export
sortTargets : List HighImpactTarget -> List HighImpactTarget
sortTargets = sortBy compareSeverity

||| Take top K targets from sorted list
public export
topKTargets : Nat -> List HighImpactTarget -> List HighImpactTarget
topKTargets k targets = take k (sortTargets targets)

calcUncov : Nat -> Nat -> Nat
calcUncov t e = if t > e then minus t e else 0

||| Create an untested canonical target
public export
mkUntestedTarget : String -> String -> Nat -> Nat -> HighImpactTarget
mkUntestedTarget fn mn totl exec =
  MkHighImpactTarget HIT_UntestedCanonical fn mn totl exec
    (severityRatio totl exec)
    ("Function has " ++ show (calcUncov totl exec) ++ " untested branches")

||| Create a bug (unhandled input) target
public export
mkBugTarget : String -> String -> Nat -> HighImpactTarget
mkBugTarget fn mn count =
  MkHighImpactTarget HIT_BugUnhandledInput fn mn count 0 severityInfinity
    "Partial code: CRASH 'Unhandled input' detected"

||| Create an unknown crash target
public export
mkUnknownCrashTarget : String -> String -> Nat -> HighImpactTarget
mkUnknownCrashTarget fn mn count =
  MkHighImpactTarget HIT_UnknownCrash fn mn count 0 severityInfinity
    "Unknown CRASH message - investigate"

||| Extract high-impact target from FunctionRuntimeHit if it has coverage gaps
public export
targetFromRuntimeHit : FunctionRuntimeHit -> Maybe HighImpactTarget
targetFromRuntimeHit hit =
  let uc = uncoveredCount hit
  in if uc > 0
     then Just (mkUntestedTarget (funcName hit) "" (canonicalCount hit) (executedCount hit))
     else Nothing

||| Extract all high-impact targets from a list of runtime hits
public export
targetsFromRuntimeHits : List FunctionRuntimeHit -> List HighImpactTarget
targetsFromRuntimeHits = mapMaybe targetFromRuntimeHit

||| Convert single HighImpactTarget to JSON object string
public export
targetToJson : HighImpactTarget -> String
targetToJson t = unlines
  [ "    {"
  , "      \"kind\": \"" ++ show (kind t) ++ "\","
  , "      \"funcName\": \"" ++ funcName t ++ "\","
  , "      \"moduleName\": \"" ++ moduleName t ++ "\","
  , "      \"branchCount\": " ++ show (branchCount t) ++ ","
  , "      \"executedCount\": " ++ show (executedCount t) ++ ","
  , "      \"severity\": \"" ++ showSeverity (severity t) ++ "\","
  , "      \"note\": \"" ++ note t ++ "\""
  , "    }"
  ]

||| Convert list of targets to JSON array
public export
targetsToJsonArray : List HighImpactTarget -> String
targetsToJsonArray [] = "[]"
targetsToJsonArray targets =
  let items = map targetToJson targets
      joined = fastConcat (intersperse ",\n" items)
  in "[\n" ++ joined ++ "\n  ]"

||| Reading guide for LLM/tool consumption
public export
coverageReadingGuide : String
coverageReadingGuide =
  "high_impact_targets: Functions with coverage issues, sorted by severity. " ++
  "kind: untested_canonical = has untested branches; bug_unhandled_input = partial code; unknown_crash = investigate. " ++
  "severity: branchCount/executedCount ratio. Inf = no branches executed or bug/unknown."

-- =============================================================================
-- Filtered Targets (with exclusion tracking)
-- =============================================================================

||| Check if a function name matches a simple string exclusion pattern
public export
matchesExclusion : String -> String -> Bool
matchesExclusion fn pat = isPrefixOf pat fn || isInfixOf pat fn

||| Container for filtered targets with exclusion tracking
public export
record FilteredTargets where
  constructor MkFilteredTargets
  targets       : List HighImpactTarget
  excludedCount : Nat
  totalCount    : Nat

||| Create FilteredTargets from a list (no exclusions)
public export
mkFilteredTargets : List HighImpactTarget -> FilteredTargets
mkFilteredTargets ts = MkFilteredTargets ts 0 (length ts)

||| Default exclusion patterns (stdlib, test helpers, primitives)
public export
defaultExclPatterns : List String
defaultExclPatterns =
  [ "Prelude."
  , "Data."
  , "Control."
  , "System."
  , "Decidable."
  , "Language."
  , "test_"
  , "prim__"
  , "believe_me"
  , "assert_"
  ]

||| Create FilteredTargets with exclusion patterns applied
public export
mkFilteredTargetsWithPatterns : List String -> List HighImpactTarget -> FilteredTargets
mkFilteredTargetsWithPatterns pats ts =
  let allPats = defaultExclPatterns ++ pats
      isExcluded = \t => any (matchesExclusion (funcName t)) allPats
      (excluded, kept) = partition isExcluded ts
  in MkFilteredTargets kept (length excluded) (length ts)

||| Get top K targets from filtered set
public export
getTopK : Nat -> FilteredTargets -> List HighImpactTarget
getTopK k ft = take k (sortTargets (targets ft))

||| Get count of excluded targets
public export
getExcludedCount : FilteredTargets -> Nat
getExcludedCount = excludedCount

||| Get all targets (after exclusion)
public export
getTargets : FilteredTargets -> List HighImpactTarget
getTargets = targets

||| Get total count before exclusion
public export
getTotalCount : FilteredTargets -> Nat
getTotalCount = totalCount
