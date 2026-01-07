||| Parser for Idris2 --dumpcases output
||| Extracts semantic coverage information (canonical vs impossible cases)
|||
||| Issue #5: Provides runDumpcases helper with correct syntax:
|||   idris2 --dumpcases <output-file> --build <package.ipkg>
module Coverage.DumpcasesParser

import Coverage.Types
import Data.List
import Data.List1
import Data.String
import Data.Maybe
import System
import System.File

%default covering

-- =============================================================================
-- Test Coverage Types
-- =============================================================================

||| Reason for CRASH in --dumpcases output
||| Based on Idris2 community feedback (dunham, Thomas):
|||   - "No clauses in ..."     → Excluded (void etc, safe to exclude from denominator)
|||   - "Unhandled input for ..." → Bug (partial code, genuine coverage issue)
|||   - "Nat case not covered"  → OptimizerArtifact (Nat→Integer translation, non-semantic)
|||   - Other                   → Unknown (never exclude, show separately)
public export
data CrashReason : Type where
  ||| "No clauses in ..." - empty function (void etc), EXCLUDED from denominator
  CrashNoClauses      : CrashReason
  ||| "Unhandled input for ..." - partial code bug, INCLUDED as coverage gap
  CrashUnhandledInput : CrashReason
  ||| "Nat case not covered" - optimizer artifact, NON-SEMANTIC (separate warning)
  CrashOptimizerNat   : CrashReason
  ||| Unknown CRASH - NEVER exclude, show in Unknown bucket
  CrashUnknown        : String -> CrashReason

public export
Show CrashReason where
  show CrashNoClauses       = "NoClauses"
  show CrashUnhandledInput  = "UnhandledInput"
  show CrashOptimizerNat    = "OptimizerNat"
  show (CrashUnknown msg)   = "Unknown(" ++ msg ++ ")"

public export
Eq CrashReason where
  CrashNoClauses      == CrashNoClauses      = True
  CrashUnhandledInput == CrashUnhandledInput = True
  CrashOptimizerNat   == CrashOptimizerNat   = True
  CrashUnknown m1     == CrashUnknown m2     = m1 == m2
  _                   == _                   = False

||| Case classification for semantic coverage
public export
data CaseKind : Type where
  ||| Reachable case - included in denominator
  Canonical    : CaseKind
  ||| Unreachable/undefined - excluded from denominator or handled separately
  NonCanonical : CrashReason -> CaseKind

public export
Show CaseKind where
  show Canonical         = "Canonical"
  show (NonCanonical r)  = "NonCanonical(" ++ show r ++ ")"

public export
Eq CaseKind where
  Canonical       == Canonical       = True
  NonCanonical r1 == NonCanonical r2 = r1 == r2
  _               == _               = False

-- =============================================================================
-- Compiled Case with CaseKind and BranchId
-- =============================================================================

||| A single case branch in compiled output with semantic classification
public export
record CompiledCase where
  constructor MkCompiledCase
  branchId : BranchId        -- Unique branch identifier for aggregation
  kind     : CaseKind        -- Canonical or NonCanonical with reason
  pattern  : String          -- Pattern description (for debugging)

public export
Show CompiledCase where
  show c = "Case(" ++ show c.branchId ++ ", " ++ show c.kind ++ ", " ++ c.pattern ++ ")"

||| Convert CompiledCase to ClassifiedBranch
||| For user functions only - compiler-generated functions handled separately
public export
toClassifiedBranch : CompiledCase -> ClassifiedBranch
toClassifiedBranch c = MkClassifiedBranch c.branchId (caseKindToBranchClass c.kind) c.pattern
  where
    caseKindToBranchClass : CaseKind -> BranchClass
    caseKindToBranchClass Canonical = BCCanonical
    caseKindToBranchClass (NonCanonical CrashNoClauses) = BCExcludedNoClauses
    caseKindToBranchClass (NonCanonical CrashUnhandledInput) = BCBugUnhandledInput
    caseKindToBranchClass (NonCanonical CrashOptimizerNat) = BCOptimizerNat
    caseKindToBranchClass (NonCanonical (CrashUnknown msg)) = BCUnknownCrash msg

||| Convert CompiledCase to ClassifiedBranch, marking excluded functions
||| Excludes: compiler-generated, standard library, type constructors
public export
toClassifiedBranchWithFuncName : String -> CompiledCase -> ClassifiedBranch
toClassifiedBranchWithFuncName funcName c =
  if shouldExcludeFunc funcName
     then MkClassifiedBranch c.branchId BCCompilerGenerated c.pattern
     else toClassifiedBranch c
  where
    shouldExcludeFunc : String -> Bool
    shouldExcludeFunc name =
         isPrefixOf "{" name          -- Compiler-generated MN names
      || isPrefixOf "_builtin." name  -- Builtin constructors
      || isPrefixOf "prim__" name     -- Primitive operations
      || isPrefixOf "Prelude." name   -- Standard library
      || isPrefixOf "Data." name
      || isPrefixOf "System." name
      || isPrefixOf "Control." name
      || isPrefixOf "Decidable." name
      || isPrefixOf "Language." name
      || isPrefixOf "Debug." name
      || (isSuffixOf "." name && not (isPrefixOf "{" name))  -- Type constructors

||| Compiled function information extracted from --dumpcases
public export
record CompiledFunction where
  constructor MkCompiledFunction
  fullName       : String              -- e.g., "AbsurdTest.safeHead"
  moduleName     : String              -- e.g., "AbsurdTest"
  funcName       : String              -- e.g., "safeHead"
  arity          : Nat                 -- Number of arguments
  cases          : List CompiledCase   -- All cases with classification
  hasDefaultCase : Bool                -- Whether a catch-all exists

public export
Show CompiledFunction where
  show f = f.fullName ++ ": " ++ show (countCanonical f.cases) ++ " canonical, "
        ++ show (countNonCanonical f.cases) ++ " non-canonical"
  where
    countCanonical : List CompiledCase -> Nat
    countCanonical = length . filter (\c => c.kind == Canonical)

    countNonCanonical : List CompiledCase -> Nat
    countNonCanonical = length . filter (\c => c.kind /= Canonical)

-- =============================================================================
-- Test Coverage Record
-- =============================================================================

||| Test coverage record for a function
public export
record TestCoverage where
  constructor MkTestCoverage
  funcName          : String
  totalCanonical    : Nat    -- |{CanonicalCaseId}| - denominator
  totalImpossible   : Nat    -- |{Impossible derived}| - reference value
  executedCanonical : Nat    -- Canonical cases hit at runtime - numerator

public export
Show TestCoverage where
  show sc = sc.funcName ++ ": " ++ show sc.executedCanonical
         ++ "/" ++ show sc.totalCanonical ++ " test coverage"

public export
Eq TestCoverage where
  sc1 == sc2 = sc1.funcName == sc2.funcName
            && sc1.totalCanonical == sc2.totalCanonical

||| Calculate test coverage percentage
public export
testCoveragePercent : TestCoverage -> Double
testCoveragePercent sc =
  if sc.totalCanonical == 0
  then 100.0  -- All impossible cases → 100%
  else cast sc.executedCanonical / cast sc.totalCanonical * 100.0

-- =============================================================================
-- Summary Statistics
-- =============================================================================

||| Configuration for additional exclusion patterns
||| Can be loaded from .idris2-cov.toml
public export
record ExclusionConfig where
  constructor MkExclusionConfig
  ||| Additional module prefixes to exclude (e.g., ["MyLib.Internal"])
  modulePrefixes : List String
  ||| Package names to exclude (mapped to Module. prefix)
  packageNames   : List String
  ||| Specific function names to exclude (exact match, e.g., ["Module.func"])
  functionNames  : List String

public export
emptyExclusionConfig : ExclusionConfig
emptyExclusionConfig = MkExclusionConfig [] [] []

public export
Show ExclusionConfig where
  show c = "ExclusionConfig(prefixes=" ++ show c.modulePrefixes
        ++ ", packages=" ++ show c.packageNames
        ++ ", functions=" ++ show c.functionNames ++ ")"

||| Summary of test coverage analysis for a module/project
||| Breakdown based on dunham's classification:
|||   - excluded: NoClauses (void etc) - safe to exclude from denominator
|||   - bugs: UnhandledInput (partial) - genuine coverage issues
|||   - optimizerArtifacts: OptimizerNat - non-semantic, warn separately
|||   - unknown: CrashUnknown - never exclude, show in Unknown bucket
|||   - excluded breakdown: compiler-generated, stdlib, type constructors, deps
public export
record ExclusionBreakdown where
  constructor MkExclusionBreakdown
  compilerGenerated : Nat  -- {csegen:N}, _builtin.*, prim__*
  standardLibrary   : Nat  -- Prelude.*, System.*, Data.*, etc.
  typeConstructors  : Nat  -- Names ending with "."
  dependencies      : Nat  -- User-specified packages/modules
  testCode          : Nat  -- *.Tests.*, test_* (test code, not coverage target)

public export
Show ExclusionBreakdown where
  show b = "compiler=" ++ show b.compilerGenerated
        ++ ", stdlib=" ++ show b.standardLibrary
        ++ ", typeCtor=" ++ show b.typeConstructors
        ++ ", deps=" ++ show b.dependencies
        ++ ", test=" ++ show b.testCode

public export
totalExcludedFromDenom : ExclusionBreakdown -> Nat
totalExcludedFromDenom b = b.compilerGenerated + b.standardLibrary + b.typeConstructors + b.dependencies + b.testCode

public export
record TestAnalysis where
  constructor MkTestAnalysis
  totalFunctions       : Nat
  totalCanonical       : Nat   -- Reachable cases (denominator for coverage)
  totalExcluded        : Nat   -- NoClauses (void etc) - excluded from denominator
  totalBugs            : Nat   -- UnhandledInput (partial) - coverage gaps
  totalOptimizerArtifacts : Nat -- Nat case not covered - non-semantic warnings
  totalUnknown         : Nat   -- Unknown CRASHes - never exclude, show separately
  exclusionBreakdown   : ExclusionBreakdown  -- Detailed breakdown of excluded functions
  functionsWithCrash   : Nat

public export
Show TestAnalysis where
  show a = unlines
    [ "Test Coverage Analysis:"
    , "  Functions analyzed: " ++ show a.totalFunctions
    , "  Canonical cases (denominator): " ++ show a.totalCanonical
    , "  Excluded (NoClauses): " ++ show a.totalExcluded
    , "  Bugs (UnhandledInput): " ++ show a.totalBugs
    , "  Optimizer artifacts: " ++ show a.totalOptimizerArtifacts
    , "  Unknown CRASHes: " ++ show a.totalUnknown
    , "  Excluded from denominator:"
    , "    Compiler-generated: " ++ show a.exclusionBreakdown.compilerGenerated
    , "    Standard library: " ++ show a.exclusionBreakdown.standardLibrary
    , "    Type constructors: " ++ show a.exclusionBreakdown.typeConstructors
    , "  Functions with CRASH: " ++ show a.functionsWithCrash
    ]

||| Legacy accessor for backward compatibility
public export
totalCompilerGenerated : TestAnalysis -> Nat
totalCompilerGenerated a = totalExcludedFromDenom a.exclusionBreakdown

-- =============================================================================
-- CrashReason Detection from CRASH Messages
-- =============================================================================

||| Determine CrashReason from CRASH message text
||| Based on dunham's Idris2 community feedback:
|||   "No clauses in ..."         → CrashNoClauses (void etc, excluded)
|||   "Unhandled input for ..."   → CrashUnhandledInput (partial bug, coverage gap)
|||   "Nat case not covered"      → CrashOptimizerNat (optimizer artifact, non-semantic)
|||   Other                       → CrashUnknown (never exclude)
|||
||| Order matters: more specific patterns first
public export
classifyCrashMessage : String -> CrashReason
classifyCrashMessage msg =
  -- Most specific first
  if isInfixOf "No clauses in" msg then CrashNoClauses
  else if isInfixOf "Unhandled input for" msg then CrashUnhandledInput
  else if isInfixOf "Nat case not covered" msg then CrashOptimizerNat
  -- Fallback: everything else is Unknown (conservative - never exclude)
  else CrashUnknown msg

-- =============================================================================
-- Parser Utilities
-- =============================================================================

||| Split on first occurrence of a character
splitFirst : Char -> String -> Maybe (String, String)
splitFirst c s =
  case break (== c) (unpack s) of
    (before, []) => Nothing
    (before, _ :: after) => Just (pack before, pack after)

||| Extract module and function name from "Module.Name.funcName"
parseFullName : String -> (String, String)
parseFullName fullName =
  let parts = toList $ split (== '.') fullName
  in case parts of
       [] => ("", fullName)
       [x] => ("", x)
       (x :: y :: []) => (x, y)
       (x :: rest) => (x, fromMaybe "" (last' rest))
  where
    toList : List1 a -> List a
    toList (x ::: xs) = x :: xs

-- =============================================================================
-- Pattern Detection
-- =============================================================================

||| Check if text contains CRASH marker
hasCrash : String -> Bool
hasCrash s = isInfixOf "(CRASH " s

||| Simple substring search - returns index of first occurrence
findSubstring : String -> String -> Maybe Nat
findSubstring needle haystack = go 0 (unpack haystack) (unpack needle)
  where
    go : Nat -> List Char -> List Char -> Maybe Nat
    go _ [] _ = Nothing
    go idx hs ns =
      if isPrefixOf ns hs
      then Just idx
      else go (S idx) (drop 1 hs) ns

||| Extract CRASH message if present
extractCrashMsg : String -> Maybe String
extractCrashMsg s =
  if isInfixOf "(CRASH \"" s
  then case findSubstring "(CRASH \"" s of
         Nothing => Nothing
         Just idx =>
           let afterCrash = substr (idx + 8) (length s) s
           in case break (== '"') (unpack afterCrash) of
                (msg, _) => Just (pack msg)
  else Nothing

||| Count %concase occurrences (reachable constructor branches)
countConCases : String -> Nat
countConCases s = go 0 (unpack s)
  where
    go : Nat -> List Char -> Nat
    go n [] = n
    go n cs =
      if isPrefixOf (unpack "(%concase") cs
      then go (S n) (drop 9 cs)
      else go n (drop 1 cs)

||| Count %constcase occurrences (reachable constant branches)
countConstCases : String -> Nat
countConstCases s = go 0 (unpack s)
  where
    go : Nat -> List Char -> Nat
    go n [] = n
    go n cs =
      if isPrefixOf (unpack "(%constcase") cs
      then go (S n) (drop 11 cs)
      else go n (drop 1 cs)

-- =============================================================================
-- Main Analysis
-- =============================================================================

||| Generate a list [0, 1, ..., n-1]
range : Nat -> List Nat
range Z = []
range (S n) = range n ++ [n]

||| Generate indexed BranchIds for canonical cases
||| caseIdx=0 for the main %case block, branchIdx increments per %concase/%constcase
generateIndexedCases : String -> String -> Nat -> Nat -> Nat -> List CompiledCase
generateIndexedCases modName funcName caseIdx conCount constCount =
  let conCases = map (\idx => MkCompiledCase
                                (MkBranchId modName funcName caseIdx idx)
                                Canonical
                                "concase")
                     (range conCount)
      constCases = map (\idx => MkCompiledCase
                                  (MkBranchId modName funcName caseIdx (conCount + idx))
                                  Canonical
                                  "constcase")
                       (range constCount)
  in conCases ++ constCases

||| Analyze a single function definition line from --dumpcases output
public export
analyzeFunction : String -> Maybe CompiledFunction
analyzeFunction line =
  case splitFirst '=' line of
    Nothing => Nothing
    Just (namePart, bodyPart) =>
      let fullName = trim namePart
          (modName, funcName) = parseFullName fullName
          conCases = countConCases bodyPart
          constCases = countConstCases bodyPart
          crashMsg = extractCrashMsg bodyPart
          hasDefault = isInfixOf "Just" bodyPart &&
                       not (isInfixOf "Just 0" bodyPart || isInfixOf "Just 1" bodyPart)

          -- Build canonical cases with unique BranchIds
          -- caseIdx=0 for the first (and typically only) %case block
          canonicalCases = generateIndexedCases modName funcName 0 conCases constCases

          -- Build non-canonical cases from CRASH
          -- BranchId uses branchIdx after all canonical cases
          crashIdx = conCases + constCases
          crashCases = case crashMsg of
            Nothing => []
            Just msg => [MkCompiledCase
                          (MkBranchId modName funcName 0 crashIdx)
                          (NonCanonical (classifyCrashMessage msg))
                          msg]

      in Just $ MkCompiledFunction
           fullName
           modName
           funcName
           0  -- TODO: parse arity from [{arg:0}, ...]
           (canonicalCases ++ crashCases)
           hasDefault

-- =============================================================================
-- Helper Functions for Analysis
-- =============================================================================

||| Check if case should be EXCLUDED from denominator
||| Only CrashNoClauses (void etc) is safe to exclude
isExcludedCase : CompiledCase -> Bool
isExcludedCase c = case c.kind of
  NonCanonical CrashNoClauses => True
  _ => False

||| Check if case is a coverage BUG (partial code, should be fixed)
isBugCase : CompiledCase -> Bool
isBugCase c = case c.kind of
  NonCanonical CrashUnhandledInput => True
  _ => False

||| Check if case is an optimizer artifact (non-semantic, warn but don't count)
isOptimizerArtifact : CompiledCase -> Bool
isOptimizerArtifact c = case c.kind of
  NonCanonical CrashOptimizerNat => True
  _ => False

||| Check if case is unknown CRASH (never exclude, show in Unknown bucket)
isUnknownCase : CompiledCase -> Bool
isUnknownCase c = case c.kind of
  NonCanonical (CrashUnknown _) => True
  _ => False

||| Legacy: Check if case is not-covered (bug, should be implemented)
||| Now includes UnhandledInput and Unknown (conservative)
isNotCoveredCase : CompiledCase -> Bool
isNotCoveredCase c = isBugCase c || isUnknownCase c

||| Count canonical cases in a function
public export
countCanonicalCases : CompiledFunction -> Nat
countCanonicalCases f = length $ filter (\c => c.kind == Canonical) f.cases

||| Count excluded cases (NoClauses only)
public export
countExcludedCases : CompiledFunction -> Nat
countExcludedCases f = length $ filter isExcludedCase f.cases

||| Count bug cases (UnhandledInput - partial code)
public export
countBugCases : CompiledFunction -> Nat
countBugCases f = length $ filter isBugCase f.cases

||| Count optimizer artifact cases (Nat case not covered)
public export
countOptimizerArtifacts : CompiledFunction -> Nat
countOptimizerArtifacts f = length $ filter isOptimizerArtifact f.cases

||| Count unknown CRASH cases
public export
countUnknownCases : CompiledFunction -> Nat
countUnknownCases f = length $ filter isUnknownCase f.cases

||| Check if function is compiler-generated
public export
isCompilerGeneratedFunction : CompiledFunction -> Bool
isCompilerGeneratedFunction f =
     isPrefixOf "{" f.fullName
  || isPrefixOf "_builtin." f.fullName
  || isPrefixOf "prim__" f.fullName

||| Check if function is from standard library (external dependency)
||| See docs/compiler-generated-functions.md for reference
public export
isStandardLibraryFunction : CompiledFunction -> Bool
isStandardLibraryFunction f =
     isPrefixOf "Prelude." f.fullName
  || isPrefixOf "Data." f.fullName
  || isPrefixOf "System." f.fullName
  || isPrefixOf "Control." f.fullName
  || isPrefixOf "Decidable." f.fullName
  || isPrefixOf "Language." f.fullName
  || isPrefixOf "Debug." f.fullName

||| Check if function is a type constructor (ends with '.')
||| These are auto-generated ADT constructor case trees
public export
isTypeConstructorFunction : CompiledFunction -> Bool
isTypeConstructorFunction f =
  isSuffixOf "." f.fullName && not (isPrefixOf "{" f.fullName)

||| Check if function should be excluded from coverage denominator
||| Includes: compiler-generated, standard library, type constructors
public export
shouldExcludeFunction : CompiledFunction -> Bool
shouldExcludeFunction f =
     isCompilerGeneratedFunction f
  || isStandardLibraryFunction f
  || isTypeConstructorFunction f

||| Check if function matches user-configured exclusion patterns
||| Checks module prefixes and package names (capitalized as Module.)
public export
isDependencyFunction : ExclusionConfig -> CompiledFunction -> Bool
isDependencyFunction config f =
  let name = f.fullName
      -- Check direct module prefixes
      matchesPrefix = any (\p => isPrefixOf p name) config.modulePrefixes
      -- Check package names (capitalized: "mypackage" -> "Mypackage.")
      capitalizeFirst : String -> String
      capitalizeFirst s = case strM s of
        StrNil => ""
        StrCons c rest => singleton (toUpper c) ++ rest
      matchesPackage = any (\pkg => isPrefixOf (capitalizeFirst pkg ++ ".") name) config.packageNames
      -- Check explicit function names (exact match)
      matchesFunction = any (\fn => fn == name) config.functionNames
  in matchesPrefix || matchesPackage || matchesFunction

||| Check if function should be excluded with user config
public export
shouldExcludeFunctionWithConfig : ExclusionConfig -> CompiledFunction -> Bool
shouldExcludeFunctionWithConfig config f =
     shouldExcludeFunction f
  || isDependencyFunction config f

||| Count compiler-generated cases (entire function's cases count)
public export
countCompilerGeneratedCases : CompiledFunction -> Nat
countCompilerGeneratedCases f =
  if isCompilerGeneratedFunction f
     then length f.cases
     else 0

||| Count standard library cases
public export
countStandardLibraryCases : CompiledFunction -> Nat
countStandardLibraryCases f =
  if isStandardLibraryFunction f
     then length f.cases
     else 0

||| Count type constructor cases
public export
countTypeConstructorCases : CompiledFunction -> Nat
countTypeConstructorCases f =
  if isTypeConstructorFunction f
     then length f.cases
     else 0

||| Count dependency/user-excluded cases
public export
countDependencyCases : ExclusionConfig -> CompiledFunction -> Nat
countDependencyCases config f =
  if isDependencyFunction config f && not (shouldExcludeFunction f)
     then length f.cases
     else 0

||| Check if function is test code (*.Tests.*, test_*)
public export
isTestCodeFunction : CompiledFunction -> Bool
isTestCodeFunction f =
     isInfixOf ".Tests." f.fullName
  || isInfixOf ".AllTests." f.fullName
  || isSuffixOf ".AllTests" f.fullName
  || isPrefixOf "test_" f.fullName

||| Count test code cases
public export
countTestCodeCases : CompiledFunction -> Nat
countTestCodeCases f =
  if isTestCodeFunction f && not (shouldExcludeFunction f)
     then length f.cases
     else 0

||| Legacy: Count not-covered cases (Bug + Unknown)
countNotCoveredCases : CompiledFunction -> Nat
countNotCoveredCases f = length $ filter isNotCoveredCase f.cases

||| Check if function has any CRASH
hasAnyCrash : CompiledFunction -> Bool
hasAnyCrash f = any (\c => c.kind /= Canonical) f.cases

-- =============================================================================
-- File Parsing
-- =============================================================================

||| Parse entire --dumpcases output file
public export
parseDumpcasesFile : String -> List CompiledFunction
parseDumpcasesFile content =
  let ls = lines content
  in mapMaybe analyzeFunction ls

||| Aggregate analysis over all functions with exclusion config
public export
aggregateAnalysisWithConfig : ExclusionConfig -> List CompiledFunction -> TestAnalysis
aggregateAnalysisWithConfig config funcs =
  let -- Filter out all excluded functions (including test code)
      userFuncs = filter (\f => not (shouldExcludeFunctionWithConfig config f) && not (isTestCodeFunction f)) funcs
      -- Count excluded cases by category
      compGenCases = sum (map countCompilerGeneratedCases funcs)
      stdlibCases  = sum (map countStandardLibraryCases funcs)
      typeCtorCases = sum (map countTypeConstructorCases funcs)
      depCases = sum (map (countDependencyCases config) funcs)
      testCases = sum (map countTestCodeCases funcs)
      breakdown = MkExclusionBreakdown compGenCases stdlibCases typeCtorCases depCases testCases
  in MkTestAnalysis
    (length funcs)
    (sum $ map countCanonicalCases userFuncs)  -- Only user code in denominator
    (sum $ map countExcludedCases funcs)
    (sum $ map countBugCases funcs)
    (sum $ map countOptimizerArtifacts funcs)
    (sum $ map countUnknownCases funcs)
    breakdown
    (length $ filter hasAnyCrash funcs)

||| Aggregate analysis over all functions (default empty config)
public export
aggregateAnalysis : List CompiledFunction -> TestAnalysis
aggregateAnalysis = aggregateAnalysisWithConfig emptyExclusionConfig

-- =============================================================================
-- Conversion to StaticBranchAnalysis (for aggregation)
-- =============================================================================

||| Convert CompiledFunction to StaticFunctionAnalysis
||| Uses function name to detect compiler-generated functions
public export
toStaticFunctionAnalysis : CompiledFunction -> StaticFunctionAnalysis
toStaticFunctionAnalysis f =
  MkStaticFunctionAnalysis f.fullName (map (toClassifiedBranchWithFuncName f.fullName) f.cases)

||| Convert list of CompiledFunctions to StaticBranchAnalysis
||| This is the entry point for coverage aggregation
public export
toStaticBranchAnalysis : List CompiledFunction -> StaticBranchAnalysis
toStaticBranchAnalysis funcs =
  let funcAnalyses = map toStaticFunctionAnalysis funcs
      allBranches = concatMap (.branches) funcAnalyses
      canonCount = length $ filter (\cb => cb.branchClass == BCCanonical) allBranches
  in MkStaticBranchAnalysis funcAnalyses allBranches canonCount

||| Convert CompiledFunction to TestCoverage
public export
toTestCoverage : CompiledFunction -> TestCoverage
toTestCoverage f =
  MkTestCoverage
    f.fullName
    (length $ filter (\c => c.kind == Canonical) f.cases)
    (length $ filter isExcludedCase f.cases)
    0  -- executedCanonical comes from runtime profiler

-- =============================================================================
-- Issue #5: runDumpcases Helper
-- =============================================================================

||| Run pack build with --dumpcases flag
|||
||| Uses pack to handle local package dependencies from pack.toml.
||| The --dumpcases flag is passed via IDRIS2_OPTS environment variable.
|||
||| @projectDir - Directory containing the .ipkg file
||| @ipkgName   - Name of the .ipkg file (e.g., "myproject.ipkg")
||| @outputFile - Where to write dumpcases output (default: /tmp/dumpcases.txt)
||| @returns    - Either error message or the dumpcases content
public export
runDumpcases : (projectDir : String)
             -> (ipkgName : String)
             -> (outputFile : String)
             -> IO (Either String String)
runDumpcases projectDir ipkgName outputFile = do
  -- Build using pack with dumpcases flag via IDRIS2_OPTS
  -- pack respects local pack.toml for custom package paths
  let cmd = "cd " ++ projectDir ++ " && IDRIS2_OPTS='--dumpcases " ++ outputFile
         ++ "' pack build " ++ ipkgName ++ " 2>/dev/null"

  -- Execute the command
  exitCode <- system cmd

  -- Read the generated output file
  result <- readFile outputFile
  case result of
    Left err => pure $ Left $ "Failed to read dumpcases output: " ++ show err
    Right content =>
      if null (trim content)
        then pure $ Left "No dumpcases output generated (build may have failed)"
        else pure $ Right content

||| Convenience wrapper with default output file
public export
runDumpcasesDefault : (projectDir : String) -> (ipkgName : String) -> IO (Either String String)
runDumpcasesDefault projectDir ipkgName =
  runDumpcases projectDir ipkgName "/tmp/idris2_dumpcases_output.txt"

-- =============================================================================
-- Runtime Hit Mapping
-- =============================================================================

||| Map profiler hits to canonical case IDs per function
public export
record FunctionHitMapping where
  constructor MkFunctionHitMapping
  funcName          : String
  totalCanonical    : Nat
  executedCanonical : Nat

public export
Show FunctionHitMapping where
  show m = m.funcName ++ ": " ++ show m.executedCanonical
        ++ "/" ++ show m.totalCanonical ++ " executed"

||| Convert Idris module.func to Scheme Module-func format
idrisToSchemePrefix : String -> String
idrisToSchemePrefix s = pack $ map (\c => if c == '.' then '-' else c) (unpack s)

||| Find executed count for a function from line-grouped hits
findExecutedForFunc : String -> List (Nat, Nat, Nat) -> Nat
findExecutedForFunc schemePrefix lineHits =
  sum $ map (\(_, executed, _) => executed) lineHits

||| Match a single compiled function with profiler expression data
matchOneFunction : List (Nat, Nat, Nat) -> CompiledFunction -> FunctionHitMapping
matchOneFunction lineHits f =
  let canonical = countCanonicalCases f
      executed = findExecutedForFunc (idrisToSchemePrefix f.fullName) lineHits
  in MkFunctionHitMapping f.fullName canonical (min executed canonical)

||| Match compiled functions with profiler expression data
||| lineHits: from Coverage.Collector.groupByLine
public export
matchFunctionHits : List CompiledFunction -> List (Nat, Nat, Nat) -> List FunctionHitMapping
matchFunctionHits funcs lineHits = map (matchOneFunction lineHits) funcs

||| Calculate total executed canonical from hit mappings
public export
totalExecutedFromMappings : List FunctionHitMapping -> Nat
totalExecutedFromMappings = sum . map (.executedCanonical)

||| Calculate semantic coverage with runtime hits
||| Combines static analysis (--dumpcases) with runtime profiler data
public export
testCoverageWithHits : List CompiledFunction -> List (Nat, Nat, Nat) -> TestCoverage
testCoverageWithHits funcs lineHits =
  let mappings = matchFunctionHits funcs lineHits
      analysis = aggregateAnalysis funcs
      executed = totalExecutedFromMappings mappings
  in MkTestCoverage
       "project"
       analysis.totalCanonical
       analysis.totalExcluded
       executed
