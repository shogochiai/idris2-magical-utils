||| Coverage Analyzer
|||
||| Analyzes canister method coverage based on call results.
||| Compares tested methods against Candid interface definition.
module DfxCoverage.CoverageAnalyzer

import Data.List
import Data.Maybe
import Data.String
import Data.SortedSet

import DfxCoverage.CandidParser
import DfxCoverage.CanisterCall
import DfxCoverage.Exclusions

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Coverage severity level
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

||| Coverage status for a single method
public export
data MethodCoverage
  = Covered Nat           -- Hit count
  | NotCovered
  | Excluded String       -- Reason for exclusion
  | Error String          -- Call failed with error

public export
Show MethodCoverage where
  show (Covered n) = "covered (" ++ show n ++ " calls)"
  show NotCovered = "NOT covered"
  show (Excluded reason) = "excluded: " ++ reason
  show (Error e) = "error: " ++ e

||| Information about coverage for a single method
public export
record MethodCoverageInfo where
  constructor MkMethodCoverageInfo
  methodName : String
  mode : MethodMode
  coverage : MethodCoverage
  severity : CoverageSeverity

public export
Show MethodCoverageInfo where
  show m = m.methodName ++ " (" ++ show m.mode ++ "): " ++
           show m.coverage ++ " [" ++ show m.severity ++ "]"

||| Overall coverage analysis result
public export
record CoverageResult where
  constructor MkCoverageResult
  totalMethods : Nat
  coveredMethods : Nat
  excludedMethods : Nat
  coveragePercent : Double
  methodDetails : List MethodCoverageInfo
  coveredNames : List String
  uncoveredNames : List String

public export
Show CoverageResult where
  show r = "Coverage: " ++ show r.coveredMethods ++ "/" ++
           show r.totalMethods ++ " (" ++ show r.coveragePercent ++ "%)" ++
           if r.excludedMethods > 0
             then " [" ++ show r.excludedMethods ++ " excluded]"
             else ""

-- =============================================================================
-- Severity Classification
-- =============================================================================

||| Determine severity based on method characteristics
||| Update methods are generally more critical than query methods
classifySeverity : CandidMethod -> CoverageSeverity
classifySeverity m =
  case m.mode of
    Update => if isInfixOf "transfer" (toLower m.name) ||
                 isInfixOf "owner" (toLower m.name) ||
                 isInfixOf "admin" (toLower m.name)
              then Critical
              else High
    Query => if isInfixOf "balance" (toLower m.name) ||
                isInfixOf "owner" (toLower m.name)
             then Medium
             else Low
    Oneway => High

-- =============================================================================
-- Coverage Analysis
-- =============================================================================

||| Match call records to methods
matchCallToMethod : List CallRecord -> String -> Maybe CallRecord
matchCallToMethod records methodName =
  find (\r => r.methodName == methodName) records

||| Determine coverage for a single method
analyzeSingleMethod : List CallRecord -> List ExclPattern -> CandidMethod -> MethodCoverageInfo
analyzeSingleMethod calls exclusions method =
  let severity = classifySeverity method
      excluded = isMethodExcluded exclusions method.name
      coverage = case excluded of
        Just reason => Excluded reason
        Nothing => case matchCallToMethod calls method.name of
          Nothing => NotCovered
          Just r => case r.result of
            CallSuccess _ => Covered 1
            CallError e => Error e
            CallNotFound => NotCovered
            CallTimeout => Error "timeout"
  in MkMethodCoverageInfo method.name method.mode coverage severity

||| Check if a method is covered (not NotCovered, not Error)
isCovered : MethodCoverageInfo -> Bool
isCovered m = case m.coverage of
  Covered _ => True
  Excluded _ => True   -- Excluded counts as "handled"
  _ => False

isExcluded : MethodCoverageInfo -> Bool
isExcluded m = case m.coverage of
  Excluded _ => True
  _ => False

||| Analyze coverage for all methods in an interface
|||
||| @ci Candid interface (from .did file)
||| @calls Call records from testing
||| @exclusions Exclusion patterns
public export
analyzeCoverage : CandidInterface -> List CallRecord -> List ExclPattern -> CoverageResult
analyzeCoverage ci calls exclusions =
  let methodDetails = map (analyzeSingleMethod calls exclusions) ci.methods
      covered = filter isCovered methodDetails
      excluded = filter isExcluded methodDetails
      notCovered = filter (not . isCovered) methodDetails
      methodCount = length ci.methods
      covCount = length (filter (\m => case m.coverage of
                                         Covered _ => True
                                         _ => False) methodDetails)
      exclCount = length excluded
      percent = if methodCount == 0
                  then 100.0
                  else (cast {to=Double} covCount / cast {to=Double} methodCount) * 100.0
  in MkCoverageResult
       methodCount
       covCount
       exclCount
       percent
       methodDetails
       (map (.methodName) (filter (\m => case m.coverage of
                                           Covered _ => True
                                           _ => False) methodDetails))
       (map (.methodName) (filter (\m => case m.coverage of
                                           NotCovered => True
                                           Error _ => True
                                           _ => False) methodDetails))

-- =============================================================================
-- Full Analysis Pipeline
-- =============================================================================

||| Run full coverage analysis
|||
||| 1. Parse Candid interface
||| 2. Execute canister calls
||| 3. Analyze coverage
||| 4. Return result
|||
||| @didPath Path to .did file
||| @callOpts Options for canister calls
||| @exclusions Exclusion patterns
public export
runCoverageAnalysis : String -> CallOptions -> List ExclPattern -> IO (Either String CoverageResult)
runCoverageAnalysis didPath callOpts exclusions = do
  -- Parse Candid interface
  Right ci <- readCandidFile didPath
    | Left err => pure $ Left $ "Failed to parse Candid: " ++ err

  -- Execute calls for all methods
  calls <- testInterface callOpts ci

  -- Analyze coverage
  let result = analyzeCoverage ci calls exclusions

  pure $ Right result

-- =============================================================================
-- Gap Extraction
-- =============================================================================

||| Gap for uncovered method (for integration with lazy system)
public export
record MethodGap where
  constructor MkMethodGap
  methodName : String
  severity : CoverageSeverity
  message : String

public export
Show MethodGap where
  show g = "[" ++ show g.severity ++ "] " ++ g.methodName ++ ": " ++ g.message

||| Extract gaps from coverage result
public export
extractGaps : CoverageResult -> List MethodGap
extractGaps result =
  mapMaybe toGap result.methodDetails
  where
    toGap : MethodCoverageInfo -> Maybe MethodGap
    toGap m = case m.coverage of
      NotCovered => Just $ MkMethodGap m.methodName m.severity "Method not covered by tests"
      Error e => Just $ MkMethodGap m.methodName m.severity $ "Test error: " ++ e
      _ => Nothing
