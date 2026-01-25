||| Code Coverage Result Types
|||
||| Types for representing code-level coverage results.
||| Distinguishes from method-level coverage (CoverageAnalyzer) by tracking
||| which code paths within functions are executed.
module DfxCoverage.CodeCoverage.CodeCoverageResult

import Data.List
import Data.Maybe

import DfxCoverage.WasmMapper.WasmFunc
import DfxCoverage.WasmTrace.TraceEntry

%default covering

-- =============================================================================
-- Coverage Levels
-- =============================================================================

||| Granularity of coverage
public export
data CoverageGranularity
  = FunctionLevel     -- Did the function get called?
  | BranchLevel       -- Were all branches taken?
  | InstructionLevel  -- Which instructions executed?

public export
Show CoverageGranularity where
  show FunctionLevel = "function"
  show BranchLevel = "branch"
  show InstructionLevel = "instruction"

public export
Eq CoverageGranularity where
  FunctionLevel == FunctionLevel = True
  BranchLevel == BranchLevel = True
  InstructionLevel == InstructionLevel = True
  _ == _ = False

-- =============================================================================
-- Function Coverage
-- =============================================================================

||| Coverage status for a single function
public export
data FuncCoverageStatus
  = FuncCovered Nat         -- Hit count
  | FuncNotCovered
  | FuncImport              -- Imported function (not coverable)
  | FuncExcluded String     -- Excluded with reason

public export
Show FuncCoverageStatus where
  show (FuncCovered n) = "covered (" ++ show n ++ " hits)"
  show FuncNotCovered = "NOT covered"
  show FuncImport = "import (N/A)"
  show (FuncExcluded reason) = "excluded: " ++ reason

||| Coverage info for a function
public export
record FuncCoverageInfo where
  constructor MkFuncCoverageInfo
  funcIdx : Nat
  wasmName : String
  idris2Name : Maybe Idris2QName
  status : FuncCoverageStatus
  uniquePCs : Nat           -- Unique instruction addresses hit
  branchCoverage : Maybe Double  -- Branch coverage % (if available)

public export
Show FuncCoverageInfo where
  show fc = "func[" ++ show fc.funcIdx ++ "] " ++ fc.wasmName ++
            ": " ++ show fc.status ++
            " (" ++ show fc.uniquePCs ++ " PCs)" ++
            maybe "" (\b => " [" ++ show b ++ "% branches]") fc.branchCoverage

||| Is function covered?
export
isFuncCovered : FuncCoverageInfo -> Bool
isFuncCovered fc = case fc.status of
  FuncCovered _ => True
  FuncExcluded _ => True
  _ => False

-- =============================================================================
-- Branch Coverage
-- =============================================================================

||| Branch coverage for a single branch point
public export
record BranchCoverageInfo where
  constructor MkBranchCoverageInfo
  funcIdx : Nat
  pc : Nat
  branchType : String       -- "if", "br_if", "br_table"
  takenCount : Nat
  notTakenCount : Nat

public export
Show BranchCoverageInfo where
  show bc = "func[" ++ show bc.funcIdx ++ "] PC=" ++ show bc.pc ++
            " " ++ bc.branchType ++
            " (taken=" ++ show bc.takenCount ++
            ", not_taken=" ++ show bc.notTakenCount ++ ")"

||| Is branch fully covered (both paths taken)?
export
isBranchFullyCovered : BranchCoverageInfo -> Bool
isBranchFullyCovered bc = bc.takenCount > 0 && bc.notTakenCount > 0

||| Is branch partially covered (at least one path)?
export
isBranchPartiallyCovered : BranchCoverageInfo -> Bool
isBranchPartiallyCovered bc = bc.takenCount > 0 || bc.notTakenCount > 0

-- =============================================================================
-- Module Coverage
-- =============================================================================

||| Coverage for an Idris2 module
public export
record ModuleCoverageInfo where
  constructor MkModuleCoverageInfo
  modulePath : String
  funcCount : Nat
  coveredCount : Nat
  coveragePercent : Double
  functions : List FuncCoverageInfo

public export
Show ModuleCoverageInfo where
  show mc = mc.modulePath ++ ": " ++
            show mc.coveredCount ++ "/" ++ show mc.funcCount ++
            " (" ++ show mc.coveragePercent ++ "%)"

-- =============================================================================
-- Overall Coverage Result
-- =============================================================================

||| Complete coverage result for a canister
public export
record CodeCoverageResult where
  constructor MkCodeCoverageResult
  -- Summary statistics
  totalFunctions : Nat
  coveredFunctions : Nat
  excludedFunctions : Nat
  importedFunctions : Nat
  functionCoveragePercent : Double

  -- Branch statistics
  totalBranches : Nat
  fullyCoveredBranches : Nat
  partiallyCoveredBranches : Nat
  branchCoveragePercent : Double

  -- Detailed info
  functionDetails : List FuncCoverageInfo
  branchDetails : List BranchCoverageInfo
  moduleBreakdown : List ModuleCoverageInfo

  -- Gaps
  uncoveredFunctions : List FuncCoverageInfo
  uncoveredBranches : List BranchCoverageInfo

public export
Show CodeCoverageResult where
  show cr = "CodeCoverage: " ++
            show cr.coveredFunctions ++ "/" ++ show cr.totalFunctions ++
            " functions (" ++ show cr.functionCoveragePercent ++ "%), " ++
            show cr.fullyCoveredBranches ++ "/" ++ show cr.totalBranches ++
            " branches (" ++ show cr.branchCoveragePercent ++ "%)"

-- =============================================================================
-- Gap Types (for integration with lazy system)
-- =============================================================================

||| Severity of coverage gap
public export
data GapSeverity = GapCritical | GapHigh | GapMedium | GapLow

public export
Show GapSeverity where
  show GapCritical = "critical"
  show GapHigh = "high"
  show GapMedium = "medium"
  show GapLow = "low"

public export
Eq GapSeverity where
  GapCritical == GapCritical = True
  GapHigh == GapHigh = True
  GapMedium == GapMedium = True
  GapLow == GapLow = True
  _ == _ = False

||| Type of coverage gap
public export
data GapType
  = FunctionNotCovered      -- Function never called
  | BranchNotFullyCovered   -- Branch not both-ways covered
  | LowHitCount             -- Function called but rarely

public export
Show GapType where
  show FunctionNotCovered = "function_not_covered"
  show BranchNotFullyCovered = "branch_not_fully_covered"
  show LowHitCount = "low_hit_count"

||| A coverage gap
public export
record CoverageGap where
  constructor MkCoverageGap
  gapType : GapType
  severity : GapSeverity
  funcIdx : Nat
  funcName : String
  idris2Name : Maybe String
  message : String

public export
Show CoverageGap where
  show g = "[" ++ show g.severity ++ "] " ++
           show g.gapType ++ ": " ++
           g.funcName ++
           maybe "" (\n => " (" ++ n ++ ")") g.idris2Name ++
           " - " ++ g.message

-- =============================================================================
-- Gap Extraction
-- =============================================================================

||| Extract gaps from coverage result
export
extractGaps : CodeCoverageResult -> List CoverageGap
extractGaps cr =
  let funcGaps = mapMaybe funcToGap cr.uncoveredFunctions
      branchGaps = mapMaybe branchToGap cr.uncoveredBranches
  in funcGaps ++ branchGaps
  where
    funcToGap : FuncCoverageInfo -> Maybe CoverageGap
    funcToGap fc = case fc.status of
      FuncNotCovered =>
        Just $ MkCoverageGap
          FunctionNotCovered
          GapHigh
          fc.funcIdx
          fc.wasmName
          (map show fc.idris2Name)
          "Function not covered by any test"
      _ => Nothing

    branchToGap : BranchCoverageInfo -> Maybe CoverageGap
    branchToGap bc =
      if not (isBranchFullyCovered bc) && isBranchPartiallyCovered bc
        then Just $ MkCoverageGap
               BranchNotFullyCovered
               GapMedium
               bc.funcIdx
               ("PC=" ++ show bc.pc)
               Nothing
               ("Branch " ++ bc.branchType ++ " not fully covered")
        else Nothing

-- =============================================================================
-- Result Summaries
-- =============================================================================

||| Get coverage summary as single line
export
oneLiner : CodeCoverageResult -> String
oneLiner cr = show cr.functionCoveragePercent ++ "% functions, " ++
              show cr.branchCoveragePercent ++ "% branches"

||| Get list of uncovered function names
export
uncoveredFunctionNames : CodeCoverageResult -> List String
uncoveredFunctionNames cr = map (.wasmName) cr.uncoveredFunctions

||| Get list of modules with low coverage
export
lowCoverageModules : Double -> CodeCoverageResult -> List ModuleCoverageInfo
lowCoverageModules threshold cr =
  filter (\m => m.coveragePercent < threshold) cr.moduleBreakdown
