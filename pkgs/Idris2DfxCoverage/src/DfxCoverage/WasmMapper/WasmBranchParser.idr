||| WASM Branch Parser
|||
||| Parses WAT (WASM text format) source to extract branch points.
||| Maps branch counts to functions for High Impact Target calculation.
|||
||| WASM branch instructions:
|||   br_if   - Conditional branch
|||   br_table - Multi-way branch (switch)
|||   if      - Conditional with else block
|||
||| Based on idris2-evm-coverage's YulMapper pattern.
module DfxCoverage.WasmMapper.WasmBranchParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File

import DfxCoverage.Exclusions

%default covering

-- =============================================================================
-- WAT Function Types
-- =============================================================================

||| A WAT function definition with source location
public export
record WatFunc where
  constructor MkWatFunc
  funcIdx : Nat         -- Function index (from (;N;) annotation)
  funcName : Maybe String  -- Optional $name
  startLine : Nat
  endLine : Nat

public export
Show WatFunc where
  show f = "func[" ++ show f.funcIdx ++ "]" ++
           maybe "" (\n => " $" ++ n) f.funcName ++
           " [L" ++ show f.startLine ++ ".." ++ show f.endLine ++ "]"

public export
Eq WatFunc where
  a == b = a.funcIdx == b.funcIdx && a.funcName == b.funcName

-- =============================================================================
-- WASM Branch Types
-- =============================================================================

||| Branch type in WASM
public export
data WasmBranchType
  = WasmBrIf          -- br_if - conditional branch
  | WasmBrTable Nat   -- br_table - N-way branch (switch)
  | WasmIf            -- if block - 2-way branch
  | WasmIfElse        -- if/else block

public export
Show WasmBranchType where
  show WasmBrIf = "br_if"
  show (WasmBrTable n) = "br_table[" ++ show n ++ "]"
  show WasmIf = "if"
  show WasmIfElse = "if/else"

public export
Eq WasmBranchType where
  WasmBrIf == WasmBrIf = True
  (WasmBrTable a) == (WasmBrTable b) = a == b
  WasmIf == WasmIf = True
  WasmIfElse == WasmIfElse = True
  _ == _ = False

||| Number of branch targets (outgoing edges) for a branch type
public export
branchTargetCount : WasmBranchType -> Nat
branchTargetCount WasmBrIf = 2        -- taken or not taken
branchTargetCount (WasmBrTable n) = n -- N-way
branchTargetCount WasmIf = 2          -- then or fall-through
branchTargetCount WasmIfElse = 2      -- then or else

||| A branch point with location info
public export
record WasmBranch where
  constructor MkWasmBranch
  branchType : WasmBranchType
  lineNumber : Nat
  containingFuncIdx : Maybe Nat

public export
Show WasmBranch where
  show b = show b.branchType ++ " @L" ++ show b.lineNumber ++
           maybe "" (\f => " in func[" ++ show f ++ "]") b.containingFuncIdx

-- =============================================================================
-- Helpers
-- =============================================================================

||| Check if trimmed line starts with given prefix
checkTrimPrefix : String -> String -> Bool
checkTrimPrefix pfx s =
  isPrefixOf pfx (trim s)

||| Find index of element in list
findElemIdx : Eq a => a -> List a -> Maybe Nat
findElemIdx _ [] = Nothing
findElemIdx x (y :: ys) = if x == y then Just 0 else map S (findElemIdx x ys)

||| Count occurrences of substring in string
countSubstr : String -> String -> Nat
countSubstr needle haystack = go 0 (unpack haystack)
  where
    needleLen : Nat
    needleLen = length needle

    go : Nat -> List Char -> Nat
    go acc [] = acc
    go acc cs@(_ :: rest) =
      if pack (take needleLen cs) == needle
        then go (S acc) rest
        else go acc rest

-- =============================================================================
-- WAT Function Parsing
-- =============================================================================

||| Extract function index from line like "(func (;42;) ...)"
extractFuncIdx : String -> Maybe Nat
extractFuncIdx line =
  case break (== ';') (unpack line) of
    (before, ';' :: rest) =>
      case break (== ';') rest of
        (numChars, ';' :: _) =>
          case parsePositive {a=Nat} (pack numChars) of
            Just n => Just n
            Nothing => Nothing
        _ => Nothing
    _ => Nothing

||| Extract function name from line like "(func $funcName ...)"
extractFuncName : String -> Maybe String
extractFuncName line =
  let trimmed = trim line
      chars = unpack trimmed
  in case findElemIdx '$' chars of
       Nothing => Nothing
       Just idx =>
         let afterDollar = drop (idx + 1) chars
             nameChars = takeWhile (\c => c /= ' ' && c /= '(' && c /= ')') afterDollar
         in if null nameChars then Nothing else Just (pack nameChars)

||| Check if line is a function definition (not a type definition)
||| Function definitions look like: (func (;N;) ...
||| Type definitions look like: (type (;N;) (func ...
isFuncDefLine : String -> Bool
isFuncDefLine line =
  let trimmed = trim line
  in checkTrimPrefix "(func " trimmed && not (checkTrimPrefix "(type " trimmed)

||| Parse function definition line
||| Returns (funcIdx, funcName) if this is a function definition
parseFuncDef : String -> Maybe (Nat, Maybe String)
parseFuncDef line =
  let trimmed = trim line
  in if isFuncDefLine line
       then case extractFuncIdx trimmed of
              Just idx => Just (idx, extractFuncName trimmed)
              Nothing => Nothing  -- No index annotation
       else Nothing

||| Collect all function start positions
collectFuncStarts : List (Nat, String) -> List (Nat, Maybe String, Nat)
collectFuncStarts [] = []
collectFuncStarts ((lineNum, line) :: rest) =
  case parseFuncDef line of
    Just (idx, name) => (idx, name, lineNum) :: collectFuncStarts rest
    Nothing => collectFuncStarts rest

||| Adjust end lines: each function ends at the line before the next function starts
adjustFuncEnds : Nat -> List (Nat, Maybe String, Nat) -> List WatFunc
adjustFuncEnds _ [] = []
adjustFuncEnds totalLines [(idx, name, start)] =
  -- Last function ends at end of file
  [MkWatFunc idx name start totalLines]
adjustFuncEnds totalLines ((idx, name, start) :: rest@((_, _, nextStart) :: _)) =
  MkWatFunc idx name start (minus nextStart 1) :: adjustFuncEnds totalLines rest

||| Parse WAT source and extract function definitions
parseWatFunctions : List String -> List WatFunc
parseWatFunctions lns =
  let indexed = zip [0..length lns] lns
      starts = collectFuncStarts indexed
  in adjustFuncEnds (length lns) starts

-- =============================================================================
-- Branch Parsing
-- =============================================================================

||| Extract br_table target count from line
||| br_table with N labels has N+1 targets (N labels + default)
extractBrTableTargets : String -> Nat
extractBrTableTargets line =
  let trimmed = trim line
      -- Count spaces in br_table line (rough estimate of targets)
      -- Each label is a number separated by space
      parts = words trimmed
      -- Subtract 1 for "br_table" itself, each remaining part is a target
  in if length parts > 1 then length parts else 2

||| Parse a line for branch points
parseBranchLine : (Nat, String) -> List (WasmBranchType, Nat)
parseBranchLine (lineNum, line) =
  let trimmed = trim line
      results = []
      -- Check for br_if
      results' = if isInfixOf "br_if" trimmed
                   then (WasmBrIf, lineNum) :: results
                   else results
      -- Check for br_table (switch-like)
      results'' = if isInfixOf "br_table" trimmed
                    then let targets = extractBrTableTargets trimmed
                         in (WasmBrTable targets, lineNum) :: results'
                    else results'
      -- Check for if/else structure
      results''' = if checkTrimPrefix "if " trimmed || checkTrimPrefix "if(" trimmed ||
                      checkTrimPrefix "(if " trimmed || checkTrimPrefix "(if(" trimmed
                    then (WasmIf, lineNum) :: results''
                    else results''
  in results'''

||| Parse all branches from WAT content
export
parseWatBranches : String -> List WasmBranch
parseWatBranches content =
  let contentLines = lines content
      indexed = zip [0..length contentLines] contentLines
      branchPoints = concatMap parseBranchLine indexed
  in map (\(bt, ln) => MkWasmBranch bt ln Nothing) branchPoints

-- =============================================================================
-- Branch Assignment
-- =============================================================================

||| Find containing function for a line number
findContainingFunc : Nat -> List WatFunc -> Maybe Nat
findContainingFunc lineNum funcs =
  case find (\f => lineNum >= f.startLine && lineNum <= f.endLine) funcs of
    Just f => Just f.funcIdx
    Nothing => Nothing

||| Associate branches with their containing functions
export
assignBranchesToFuncs : List WatFunc -> List WasmBranch -> List WasmBranch
assignBranchesToFuncs funcs branches =
  map assignFunc branches
  where
    assignFunc : WasmBranch -> WasmBranch
    assignFunc b = { containingFuncIdx := findContainingFunc b.lineNumber funcs } b

||| Get branches for a specific function
export
branchesForFunc : Nat -> List WasmBranch -> List WasmBranch
branchesForFunc funcIdx = filter (\b => b.containingFuncIdx == Just funcIdx)

||| Count total branch targets for a function
export
branchCountForFunc : Nat -> List WasmBranch -> Nat
branchCountForFunc funcIdx branches =
  let funcBranches = branchesForFunc funcIdx branches
  in foldl (\acc, b => acc + branchTargetCount b.branchType) 0 funcBranches

-- =============================================================================
-- High Impact Targets
-- =============================================================================

||| Function with branch count
public export
record FuncBranchCount where
  constructor MkFuncBranchCount
  funcIdx : Nat
  branchCount : Nat
  branchInstructions : Nat  -- Number of branch instructions (not targets)

public export
Show FuncBranchCount where
  show f = "func[" ++ show f.funcIdx ++ "]: " ++
           show f.branchCount ++ " branches (" ++
           show f.branchInstructions ++ " instructions)"

public export
Eq FuncBranchCount where
  a == b = a.funcIdx == b.funcIdx

||| Sort by branch count descending
sortByBranchCount : List FuncBranchCount -> List FuncBranchCount
sortByBranchCount [] = []
sortByBranchCount (x :: xs) = insert x (sortByBranchCount xs)
  where
    insert : FuncBranchCount -> List FuncBranchCount -> List FuncBranchCount
    insert e [] = [e]
    insert e (y :: ys) = if e.branchCount >= y.branchCount
                           then e :: y :: ys
                           else y :: insert e ys

||| Calculate branch count for a single function
calcFuncBranches : List WasmBranch -> WatFunc -> FuncBranchCount
calcFuncBranches assigned f =
  let funcBranches = branchesForFunc f.funcIdx assigned
      instrCount = length funcBranches
      targetCount = foldl (\acc, b => acc + branchTargetCount b.branchType) 0 funcBranches
  in MkFuncBranchCount f.funcIdx targetCount instrCount

||| Calculate branch counts for all functions
export
calculateBranchCounts : List WatFunc -> List WasmBranch -> List FuncBranchCount
calculateBranchCounts funcs branches =
  let assigned = assignBranchesToFuncs funcs branches
      funcCounts = map (calcFuncBranches assigned) funcs
  in sortByBranchCount funcCounts

||| Get High Impact Targets (functions with most branches)
||| These are functions that need most test cases for full branch coverage
export
getHighImpactTargets : Nat -> List FuncBranchCount -> List FuncBranchCount
getHighImpactTargets n = take n

||| Get uncovered High Impact Targets
||| Returns High Impact functions that haven't been executed during profiling
export
getUncoveredHighImpact : List Nat -> List FuncBranchCount -> List FuncBranchCount
getUncoveredHighImpact executedFuncIds targets =
  filter (\t => not (t.funcIdx `elem` executedFuncIds)) targets

||| Coverage priority: high-branch functions not yet tested
public export
record CoveragePriority where
  constructor MkCoveragePriority
  funcIdx : Nat
  branchCount : Nat
  isCovered : Bool
  coveragePriority : Nat  -- branchCount if uncovered, 0 if covered

public export
Show CoveragePriority where
  show cp = "func[" ++ show cp.funcIdx ++ "]: " ++
            show cp.branchCount ++ " branches " ++
            (if cp.isCovered then "(covered)" else "(UNCOVERED - priority " ++ show cp.coveragePriority ++ ")")

||| Calculate coverage priorities for all functions
export
calculateCoveragePriorities : List Nat -> List FuncBranchCount -> List CoveragePriority
calculateCoveragePriorities executedFuncIds funcCounts =
  sortByPriority $ map toPriority funcCounts
  where
    toPriority : FuncBranchCount -> CoveragePriority
    toPriority fc =
      let covered = fc.funcIdx `elem` executedFuncIds
          priority = if covered then 0 else fc.branchCount
      in MkCoveragePriority fc.funcIdx fc.branchCount covered priority

    sortByPriority : List CoveragePriority -> List CoveragePriority
    sortByPriority [] = []
    sortByPriority (x :: xs) = insert x (sortByPriority xs)
      where
        insert : CoveragePriority -> List CoveragePriority -> List CoveragePriority
        insert e [] = [e]
        insert e (y :: ys) = if e.coveragePriority >= y.coveragePriority
                               then e :: y :: ys
                               else y :: insert e ys

-- =============================================================================
-- Exclusion-Aware Filtering
-- =============================================================================

||| Filter High Impact Targets by exclusion patterns
||| @nameLookup Function to get name from funcIdx (e.g., from WASM name section)
||| @exclusions Exclusion patterns to apply
||| @targets List of targets to filter
export
filterByExclusions : (Nat -> Maybe String) -> List ExclPattern -> List FuncBranchCount -> List FuncBranchCount
filterByExclusions nameLookup exclusions targets =
  filter (not . isExcluded) targets
  where
    isExcluded : FuncBranchCount -> Bool
    isExcluded fc =
      case nameLookup fc.funcIdx of
        Nothing => False  -- Can't exclude if no name
        Just name => isJust (isMethodExcluded exclusions name)

||| FuncBranchCount with resolved name
public export
record NamedFuncBranchCount where
  constructor MkNamedFuncBranchCount
  funcIdx : Nat
  funcName : String
  branchCount : Nat
  branchInstructions : Nat
  isExcluded : Bool
  excludeReason : Maybe String

public export
Show NamedFuncBranchCount where
  show nf = "func[" ++ show nf.funcIdx ++ "] <" ++ nf.funcName ++ ">: " ++
            show nf.branchCount ++ " branches" ++
            (if nf.isExcluded
               then " [EXCLUDED: " ++ fromMaybe "?" nf.excludeReason ++ "]"
               else "")

||| Resolve function names and apply exclusions
export
resolveAndFilter : (Nat -> Maybe String) -> List ExclPattern -> List FuncBranchCount -> List NamedFuncBranchCount
resolveAndFilter nameLookup exclusions targets =
  map resolve targets
  where
    resolve : FuncBranchCount -> NamedFuncBranchCount
    resolve fc =
      let name = fromMaybe ("func_" ++ show fc.funcIdx) (nameLookup fc.funcIdx)
          excludeResult = isMethodExcluded exclusions name
      in MkNamedFuncBranchCount
           fc.funcIdx
           name
           fc.branchCount
           fc.branchInstructions
           (isJust excludeResult)
           excludeResult

||| Get non-excluded High Impact Targets with names
export
getFilteredHighImpact : Nat -> (Nat -> Maybe String) -> List ExclPattern -> List FuncBranchCount -> List NamedFuncBranchCount
getFilteredHighImpact n nameLookup exclusions targets =
  let resolved = resolveAndFilter nameLookup exclusions targets
      filtered = filter (not . isExcluded) resolved
  in take n filtered

-- =============================================================================
-- Analysis Result
-- =============================================================================

||| Branch analysis result
public export
record BranchAnalysis where
  constructor MkBranchAnalysis
  totalBranches : Nat           -- Total branch instructions
  totalBranchTargets : Nat      -- Total branch targets (edges)
  funcBranchCounts : List FuncBranchCount
  highImpactTargets : List FuncBranchCount

public export
Show BranchAnalysis where
  show ba = "Branch Analysis: " ++ show ba.totalBranches ++ " branches, " ++
            show ba.totalBranchTargets ++ " targets, " ++
            show (length ba.highImpactTargets) ++ " high impact functions"

||| Perform full branch analysis on WAT content
export
analyzeWatBranches : String -> BranchAnalysis
analyzeWatBranches content =
  let lns = lines content
      funcs = parseWatFunctions lns
      branches = parseWatBranches content
      assigned = assignBranchesToFuncs funcs branches
      funcCounts = calculateBranchCounts funcs assigned
      totalTargets = foldl (\acc, b => acc + branchTargetCount b.branchType) 0 assigned
  in MkBranchAnalysis
       (length assigned)
       totalTargets
       funcCounts
       (getHighImpactTargets 10 funcCounts)

-- =============================================================================
-- File I/O
-- =============================================================================

||| Read and parse WAT file for functions
export
readWatFunctions : String -> IO (Either String (List WatFunc))
readWatFunctions path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read WAT file: " ++ show err
  let lns = lines content
  pure $ Right $ parseWatFunctions lns

||| Read WAT file and analyze branches
export
readWatBranches : String -> IO (Either String BranchAnalysis)
readWatBranches path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read WAT file: " ++ show err
  pure $ Right $ analyzeWatBranches content

||| Convert WASM to WAT and analyze (requires wasm2wat)
export
analyzeWasmBranches : String -> IO (Either String BranchAnalysis)
analyzeWasmBranches wasmPath = do
  let watPath = wasmPath ++ ".wat"
  let cmd = "wasm2wat \"" ++ wasmPath ++ "\" -o \"" ++ watPath ++ "\" 2>&1"
  exitCode <- system cmd
  if exitCode /= 0
    then pure $ Left "wasm2wat conversion failed"
    else do
      result <- readWatBranches watPath
      _ <- system $ "rm -f \"" ++ watPath ++ "\""
      pure result
