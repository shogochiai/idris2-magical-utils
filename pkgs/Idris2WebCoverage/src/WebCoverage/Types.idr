||| Web Coverage Types
|||
||| Types for V8 Coverage + Source Map based coverage analysis
||| Compatible with idris2-coverage's FunctionRuntimeHit for LazyCore integration
module WebCoverage.Types

import Data.List
import Data.String

-- Re-export core types for compatibility
import public Coverage.Core.Types
import public Coverage.Core.Stats
import public Coverage.Standardization.Types
import Execution.Standardization.Model
import Execution.Standardization.Web

%default total

-- =============================================================================
-- Source Map Types
-- =============================================================================

||| A single mapping entry from generated JS to original Idris2 source
public export
record SourceMapping where
  constructor MkSourceMapping
  genLine : Nat       -- Generated JS line (1-indexed)
  genCol : Nat        -- Generated JS column (0-indexed)
  srcFile : String    -- Original source file path
  srcLine : Nat       -- Original source line (1-indexed)
  srcCol : Nat        -- Original source column (0-indexed)

public export
Show SourceMapping where
  show m = "JS:" ++ show m.genLine ++ ":" ++ show m.genCol ++
           " -> " ++ m.srcFile ++ ":" ++ show m.srcLine

-- =============================================================================
-- V8 Coverage Types
-- =============================================================================

||| V8 coverage range for a function
public export
record V8Range where
  constructor MkV8Range
  startOffset : JsByteOffset   -- Byte offset in JS source
  endOffset : JsByteOffset     -- Byte offset in JS source
  count : Nat         -- Execution count

public export
Show V8Range where
  show r = "[" ++ show r.startOffset ++ "-" ++ show r.endOffset ++
           "] count=" ++ show r.count

||| V8 function coverage data
public export
record V8FunctionCoverage where
  constructor MkV8FunctionCoverage
  functionName : String
  ranges : List V8Range
  isBlockCoverage : Bool

public export
Show V8FunctionCoverage where
  show fc = fc.functionName ++ ": " ++ show (length fc.ranges) ++ " ranges"

||| V8 script coverage data
public export
record V8ScriptCoverage where
  constructor MkV8ScriptCoverage
  url : String
  functions : List V8FunctionCoverage

-- =============================================================================
-- Web Function Runtime Hit (compatible with idris2-coverage)
-- =============================================================================

||| Runtime coverage data for a single function
||| Compatible with Coverage.Types.FunctionRuntimeHit from idris2-coverage
public export
record WebFunctionHit where
  constructor MkWebFunctionHit
  funcName : String           -- Idris2 function name (e.g., "Main.dispatch")
  jsName : String             -- JS function name (mangled)
  canonicalCount : Nat        -- Static branches from --dumpcases
  executedCount : Nat         -- Runtime hits from V8 coverage
  totalExprs : Nat            -- Total case expressions
  coveredExprs : Nat          -- Covered case expressions

public export
Show WebFunctionHit where
  show h = h.funcName ++ ": " ++ show h.executedCount ++ "/" ++
           show h.canonicalCount ++ " branches"

||| Convert WebFunctionHit to percentage
public export
hitPercentage : WebFunctionHit -> Double
hitPercentage h =
  if h.canonicalCount == 0 then 100.0
  else cast h.executedCount / cast h.canonicalCount * 100.0

-- =============================================================================
-- File Coverage Summary
-- =============================================================================

||| Coverage summary for a single source file
public export
record FileCoverageSummary where
  constructor MkFileCoverageSummary
  sourcePath : String
  coveredLines : List Nat     -- Lines that were executed
  uncoveredLines : List Nat   -- Lines that were not executed
  totalLines : Nat
  percentage : Double

public export
Show FileCoverageSummary where
  show f = f.sourcePath ++ ": " ++ show (length f.coveredLines) ++ "/" ++
           show f.totalLines ++ " (" ++ show (cast {to=Int} f.percentage) ++ "%)"

-- =============================================================================
-- Web Coverage Report
-- =============================================================================

||| Complete web coverage report
public export
record WebCoverageReport where
  constructor MkWebCoverageReport
  functionHits : List WebFunctionHit
  functionObligations : List CoverageObligation
  measurement : CoverageMeasurement
  coveredObligationIds : List String
  fileSummaries : List FileCoverageSummary
  totalFunctionObligations : Nat
  coveredFunctionObligations : Nat
  totalCanonicalBranchesEstimate : Nat
  executedCanonicalBranchesEstimate : Nat
  overallPercentage : Double
  coverageModel : String
  executionProfile : String
  unknownPolicy : String
  claimAdmissible : Bool

public export
Show WebCoverageReport where
  show r = "WebCoverage: " ++ show r.coveredFunctionObligations ++ "/" ++
           show r.totalFunctionObligations ++ " obligations (" ++
           show (cast {to=Int} r.overallPercentage) ++ "%) " ++
           show (length r.functionHits) ++ " functions, " ++
           show (length r.fileSummaries) ++ " files, " ++
           "profile=" ++ r.executionProfile ++ ", claimAdmissible=" ++ show r.claimAdmissible

public export
reportFunctionHits : WebCoverageReport -> List WebFunctionHit
reportFunctionHits = functionHits

public export
reportFunctionObligations : WebCoverageReport -> List CoverageObligation
reportFunctionObligations = functionObligations

public export
reportMeasurement : WebCoverageReport -> CoverageMeasurement
reportMeasurement = measurement

public export
reportCoveredObligationIds : WebCoverageReport -> List String
reportCoveredObligationIds = coveredObligationIds

public export
reportTotalFunctionObligations : WebCoverageReport -> Nat
reportTotalFunctionObligations = totalFunctionObligations

public export
reportCoveredFunctionObligations : WebCoverageReport -> Nat
reportCoveredFunctionObligations = coveredFunctionObligations

public export
reportTotalCanonicalBranchesEstimate : WebCoverageReport -> Nat
reportTotalCanonicalBranchesEstimate = totalCanonicalBranchesEstimate

public export
reportExecutedCanonicalBranchesEstimate : WebCoverageReport -> Nat
reportExecutedCanonicalBranchesEstimate = executedCanonicalBranchesEstimate

public export
reportOverallPercentage : WebCoverageReport -> Double
reportOverallPercentage = overallPercentage

public export
reportCoverageModel : WebCoverageReport -> String
reportCoverageModel = coverageModel

public export
reportExecutionProfile : WebCoverageReport -> String
reportExecutionProfile = executionProfile

public export
reportUnknownPolicy : WebCoverageReport -> String
reportUnknownPolicy = unknownPolicy

public export
reportClaimAdmissible : WebCoverageReport -> Bool
reportClaimAdmissible = claimAdmissible

public export
defaultWebExecutionProfileName : String
defaultWebExecutionProfileName = webExecutionProfileName
