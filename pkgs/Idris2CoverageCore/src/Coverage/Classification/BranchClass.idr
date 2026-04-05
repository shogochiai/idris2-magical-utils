||| Branch Classification for Coverage Analysis
|||
||| Provides semantic classification of branches based on:
|||   - dunham's coverage taxonomy
|||   - Idris2 compiler behavior
|||   - Coverage analysis requirements
|||
||| Classifications:
|||   - BCCanonical: Reachable branch, counts in denominator
|||   - BCExcludedNoClauses: void/uninhabited, excluded from denominator
|||   - BCBugUnhandledInput: Partial code bug, coverage gap to fix
|||   - BCOptimizerNat: Nat optimization artifact, non-semantic
|||   - BCUnknownCrash: Unknown CRASH, needs investigation
|||   - BCCompilerGenerated: {csegen:N}, _builtin.*, prim__*, excluded
module Coverage.Classification.BranchClass

import Data.String
import Coverage.Standardization.Types
import Coverage.Standardization.Model

-- Re-export Core.Types for BranchId
import public Coverage.Core.Types
import Coverage.Classification.CrashReason

%default total

-- =============================================================================
-- Branch Classification
-- =============================================================================

||| Branch classification per dunham's semantic coverage taxonomy
public export
data BranchClass : Type where
  ||| Canonical - reachable branch, counts towards coverage denominator
  BCCanonical : BranchClass
  ||| Excluded - NoClauses (void/uninhabited), excluded from denominator
  BCExcludedNoClauses : BranchClass
  ||| Bug - UnhandledInput (partial code), genuine coverage gap to fix
  BCBugUnhandledInput : BranchClass
  ||| Optimizer - Nat case artifact, non-semantic (ignore in coverage)
  BCOptimizerNat : BranchClass
  ||| Unknown - other CRASHes, conservative bucket (investigate)
  BCUnknownCrash : String -> BranchClass
  ||| Compiler-generated - {csegen:N}, _builtin.*, prim__*, excluded from denominator
  BCCompilerGenerated : BranchClass

public export
Show BranchClass where
  show BCCanonical = "canonical"
  show BCExcludedNoClauses = "excluded_void"
  show BCBugUnhandledInput = "bugs"
  show BCOptimizerNat = "optimizer_artifact"
  show (BCUnknownCrash msg) = "unknown(" ++ msg ++ ")"
  show BCCompilerGenerated = "compiler_generated"

public export
Eq BranchClass where
  BCCanonical == BCCanonical = True
  BCExcludedNoClauses == BCExcludedNoClauses = True
  BCBugUnhandledInput == BCBugUnhandledInput = True
  BCOptimizerNat == BCOptimizerNat = True
  BCUnknownCrash m1 == BCUnknownCrash m2 = m1 == m2
  BCCompilerGenerated == BCCompilerGenerated = True
  _ == _ = False

-- =============================================================================
-- Coverage Semantics
-- =============================================================================

public export
branchClassToObligationClass : BranchClass -> ObligationClass
branchClassToObligationClass BCCanonical = ReachableObligation
branchClassToObligationClass BCExcludedNoClauses = LogicallyUnreachable
branchClassToObligationClass BCBugUnhandledInput = UserAdmittedPartialGap
branchClassToObligationClass BCOptimizerNat = CompilerInsertedArtifact
branchClassToObligationClass (BCUnknownCrash _) = UnknownClassification
branchClassToObligationClass BCCompilerGenerated = CompilerInsertedArtifact

||| Check if branch should be counted in coverage denominator
|||
||| Only BCCanonical branches count - they represent reachable code.
public export
isCountedInDenominator : BranchClass -> Bool
isCountedInDenominator = countsAsDenominator . branchClassToObligationClass

||| Check if branch represents a coverage bug
|||
||| BCBugUnhandledInput indicates partial code that needs fixing.
public export
isCoverageBugBranch : BranchClass -> Bool
isCoverageBugBranch cls =
  branchClassToObligationClass cls == UserAdmittedPartialGap

||| Check if branch needs investigation
public export
needsInvestigationBranch : BranchClass -> Bool
needsInvestigationBranch (BCUnknownCrash _) = True
needsInvestigationBranch _ = False

public export
standardCoverageModelName : String
standardCoverageModelName = standardName semanticBranchObligationStandard

public export
standardFunctionCoverageModelName : String
standardFunctionCoverageModelName = standardName semanticFunctionObligationStandard

public export
standardUnknownPolicyName : String
standardUnknownPolicyName =
  case unknownPolicy (soundnessEnvelope semanticBranchObligationStandard) of
    BlockCoverageClaim => "block_claim"
    CountAsGap => "count_as_gap"
    ReportSeparately => "report_separately"

-- =============================================================================
-- Conversion from CrashReason
-- =============================================================================

||| Convert CaseKind to BranchClass
public export
caseKindToBranchClass : CaseKind -> BranchClass
caseKindToBranchClass Canonical = BCCanonical
caseKindToBranchClass (NonCanonical CrashNoClauses) = BCExcludedNoClauses
caseKindToBranchClass (NonCanonical CrashUnhandledInput) = BCBugUnhandledInput
caseKindToBranchClass (NonCanonical CrashOptimizerNat) = BCOptimizerNat
caseKindToBranchClass (NonCanonical (CrashUnknown msg)) = BCUnknownCrash msg

-- =============================================================================
-- Classified Branch
-- =============================================================================

||| A branch with unique ID and classification
public export
record ClassifiedBranch where
  constructor MkClassifiedBranch
  branchId : BranchId
  branchClass : BranchClass
  pattern : String      -- Pattern description (for debugging)

public export
Show ClassifiedBranch where
  show cb = show cb.branchId ++ " [" ++ show cb.branchClass ++ "] " ++ cb.pattern

public export
Eq ClassifiedBranch where
  cb1 == cb2 = cb1.branchId == cb2.branchId

public export
classifiedBranchToCoverageObligation : ClassifiedBranch -> CoverageObligation
classifiedBranchToCoverageObligation cb =
  MkCoverageObligation
    (show cb.branchId)
    ElaboratedCaseTree
    BranchLevel
    cb.pattern
    Nothing
    (branchClassToObligationClass cb.branchClass)

public export
isStandardCoverageClaimAdmissible : List ClassifiedBranch -> Bool
isStandardCoverageClaimAdmissible branches =
  isCoverageClaimAdmissible
    semanticBranchObligationStandard
    (map classifiedBranchToCoverageObligation branches)

-- =============================================================================
-- Function Name Classification
-- =============================================================================

||| Check if function name indicates compiler-generated code
public export
isCompilerGeneratedName : String -> Bool
isCompilerGeneratedName name =
     isPrefixOf "{" name            -- Compiler-generated MN names
  || isPrefixOf "_builtin." name    -- Builtin constructors
  || isPrefixOf "prim__" name       -- Primitive operations

||| Check if function is from standard library
public export
isStandardLibraryName : String -> Bool
isStandardLibraryName name =
     isPrefixOf "Prelude." name
  || isPrefixOf "Builtin." name
  || isPrefixOf "PrimIO." name
  || isPrefixOf "Data." name
  || isPrefixOf "System." name
  || isPrefixOf "Control." name
  || isPrefixOf "Decidable." name
  || isPrefixOf "Language." name
  || isPrefixOf "Debug." name

||| Check if function is a type constructor (ends with '.')
|||
||| These are auto-generated ADT constructor case trees.
public export
isTypeConstructorName : String -> Bool
isTypeConstructorName name =
  isSuffixOf "." name && not (isPrefixOf "{" name)

||| Check if function should be excluded from coverage
public export
shouldExcludeFunctionName : String -> Bool
shouldExcludeFunctionName name =
     isCompilerGeneratedName name
  || isStandardLibraryName name
  || isTypeConstructorName name

-- =============================================================================
-- Static Function Analysis
-- =============================================================================

||| Static analysis result for a single function
public export
record StaticFunctionAnalysis where
  constructor MkStaticFunctionAnalysis
  fullName : String
  branches : List ClassifiedBranch

public export
Show StaticFunctionAnalysis where
  show sfa = sfa.fullName ++ ": " ++ show (length sfa.branches) ++ " branches"

public export
functionObligationClass : List ClassifiedBranch -> ObligationClass
functionObligationClass branches =
  let classes = map (\cb => branchClassToObligationClass cb.branchClass) branches in
    if any (== UnknownClassification) classes then UnknownClassification
    else if any (== UserAdmittedPartialGap) classes then UserAdmittedPartialGap
    else if any (== ReachableObligation) classes then ReachableObligation
    else if any (== LogicallyUnreachable) classes then LogicallyUnreachable
    else CompilerInsertedArtifact

public export
staticFunctionToCoverageObligation : StaticFunctionAnalysis -> CoverageObligation
staticFunctionToCoverageObligation sfa =
  MkCoverageObligation
    sfa.fullName
    ElaboratedCaseTree
    FunctionLevel
    sfa.fullName
    Nothing
    (functionObligationClass sfa.branches)

public export
isStandardFunctionCoverageClaimAdmissible : List StaticFunctionAnalysis -> Bool
isStandardFunctionCoverageClaimAdmissible functions =
  isCoverageClaimAdmissible
    semanticFunctionObligationStandard
    (map staticFunctionToCoverageObligation functions)

||| Static analysis result for entire project
public export
record StaticBranchAnalysis where
  constructor MkStaticBranchAnalysis
  functions : List StaticFunctionAnalysis
  allBranches : List ClassifiedBranch        -- Flat list of all branches
  canonicalCount : Nat                        -- Count of BCCanonical branches

public export
Show StaticBranchAnalysis where
  show sba = "StaticAnalysis: " ++ show (length sba.functions) ++ " functions, "
          ++ show sba.canonicalCount ++ " canonical branches"

||| Count branches by classification
public export
countByClass : BranchClass -> List ClassifiedBranch -> Nat
countByClass cls branches =
  length $ filter (\cb => cb.branchClass == cls) branches
