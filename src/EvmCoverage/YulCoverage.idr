||| Yul Function Coverage Analysis
|||
||| Combines YulMapper, AsmJsonParser, and TraceParser to calculate
||| TextDAO source code coverage from EVM execution traces.
|||
||| Pipeline:
|||   1. Parse Yul source to get function definitions with byte offsets
|||   2. Parse asm.json to get PC -> Yul offset mapping
|||   3. Parse execution trace to get executed PCs
|||   4. Map executed PCs -> Yul offsets -> Yul functions -> Idris functions
|||   5. Calculate coverage: executed Idris functions / all Idris functions
|||
module EvmCoverage.YulCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import Data.SortedSet
import System.File

import EvmCoverage.YulMapper
import EvmCoverage.AsmJsonParser
import EvmCoverage.TraceParser

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Coverage result for a single Idris function
public export
record FuncCoverage where
  constructor MkFuncCoverage
  idrisFunc : IdrisFunc
  yulFunc : YulFunc
  hitCount : Nat
  executed : Bool

public export
Show FuncCoverage where
  show fc = show fc.idrisFunc ++ ": " ++
            (if fc.executed then "COVERED (" ++ show fc.hitCount ++ " hits)"
                           else "NOT COVERED")

||| Overall Yul-based coverage result
public export
record YulCoverageResult where
  constructor MkYulCoverageResult
  yulFuncCount : Nat            -- All Yul functions in module
  idrisFuncCount : Nat           -- Mapped Idris functions
  coveredCount : Nat             -- Executed Idris functions
  coveragePercent : Double
  coveredFuncs : List FuncCoverage
  uncoveredFuncs : List FuncCoverage

public export
Show YulCoverageResult where
  show r = "YulCoverage: " ++ show r.coveredCount ++ "/" ++
           show r.idrisFuncCount ++ " Idris functions (" ++
           show r.coveragePercent ++ "%)" ++
           "\n  Yul functions: " ++ show r.yulFuncCount

-- =============================================================================
-- Coverage Calculation
-- =============================================================================

||| Get executed Yul offsets from trace and ASM mapping
export
getExecutedOffsets : List TraceEntry -> List AsmInstr -> SortedSet Int
getExecutedOffsets trace asmInstrs =
  let executedPCs = SortedSet.fromList $ map (.pc) trace
      pcMap = buildPcMap asmInstrs
      -- For each executed PC, get the Yul begin offset
      offsets = mapMaybe (getOffset executedPCs pcMap) asmInstrs
  in SortedSet.fromList offsets
  where
    getOffset : SortedSet Nat -> SortedMap Nat PcToYul -> AsmInstr -> Maybe Int
    getOffset pcs pcMap instr =
      if contains instr.pc pcs
        then Just instr.beginOff
        else Nothing

||| Check if a Yul function was executed (any of its offsets hit)
wasFuncExecuted : YulFunc -> SortedSet Int -> Bool
wasFuncExecuted yf executedOffsets =
  -- A function is executed if any instruction within its offset range was hit
  let funcBegin = cast {to=Int} yf.startOffset
      funcEnd = cast {to=Int} yf.endOffset
  in any (\off => off >= funcBegin && off < funcEnd) (SortedSet.toList executedOffsets)

||| Count hits for a function
countFuncHits : YulFunc -> List AsmInstr -> SortedSet Nat -> Nat
countFuncHits yf instrs executedPCs =
  let funcBegin = cast {to=Int} yf.startOffset
      funcEnd = cast {to=Int} yf.endOffset
      funcInstrs = filter (\i => i.beginOff >= funcBegin && i.beginOff < funcEnd) instrs
      hitInstrs = filter (\i => contains i.pc executedPCs) funcInstrs
  in length hitInstrs

||| Calculate coverage for a list of Yul functions
export
calculateYulCoverage : List YulFunc -> List AsmInstr -> List TraceEntry -> YulCoverageResult
calculateYulCoverage yulFuncs asmInstrs trace =
  let executedPCs = SortedSet.fromList $ map (.pc) trace
      executedOffsets = getExecutedOffsets trace asmInstrs
      -- Map Yul functions to coverage results
      funcResults = mapMaybe (processFn executedPCs executedOffsets) yulFuncs
      covered = filter (.executed) funcResults
      uncovered = filter (not . (.executed)) funcResults
      idrisCount = length funcResults
      covCount = length covered
      pct = if idrisCount == 0 then 0.0
            else cast covCount / cast idrisCount * 100.0
  in MkYulCoverageResult (length yulFuncs) idrisCount covCount pct covered uncovered
  where
    processFn : SortedSet Nat -> SortedSet Int -> YulFunc -> Maybe FuncCoverage
    processFn executedPCs executedOffsets yf =
      case yulToIdris yf of
        Nothing => Nothing  -- Skip Yul helper functions
        Just idris =>
          let hits = countFuncHits yf asmInstrs executedPCs
              exec = hits > 0 || wasFuncExecuted yf executedOffsets
          in Just $ MkFuncCoverage idris yf hits exec

-- =============================================================================
-- Module Filtering
-- =============================================================================

||| Filter Yul functions by module prefix
export
filterByModule : String -> List YulFunc -> List YulFunc
filterByModule modPrefix = filterByPrefix modPrefix

||| Get unique Idris module names from Yul functions
export
getModules : List YulFunc -> List String
getModules funcs =
  nub $ mapMaybe getModPath funcs
  where
    getModPath : YulFunc -> Maybe String
    getModPath yf = map (.modulePath) (yulToIdris yf)

-- =============================================================================
-- Summary Report Generation
-- =============================================================================

||| Generate coverage summary text
export
coverageSummary : YulCoverageResult -> String
coverageSummary r =
  unlines
    [ "=== Yul-based TextDAO Source Coverage ==="
    , ""
    , "Total Yul functions: " ++ show r.yulFuncCount
    , "Mapped Idris functions: " ++ show r.idrisFuncCount
    , "Covered functions: " ++ show r.coveredCount
    , "Coverage: " ++ show r.coveragePercent ++ "%"
    , ""
    , "--- Covered Functions ---"
    ] ++
  unlines (map (\fc => "  " ++ show fc.idrisFunc) r.coveredFuncs) ++
  unlines
    [ ""
    , "--- Uncovered Functions ---"
    ] ++
  unlines (map (\fc => "  " ++ show fc.idrisFunc) r.uncoveredFuncs)

||| Generate per-module coverage breakdown
export
moduleBreakdown : YulCoverageResult -> String
moduleBreakdown r =
  let allFuncs = r.coveredFuncs ++ r.uncoveredFuncs
      modules = nub $ map (\fc => fc.idrisFunc.modulePath) allFuncs
      modStats = map (calcModStats allFuncs) modules
  in unlines $ "=== Per-Module Coverage ===" :: "" :: modStats
  where
    calcModStats : List FuncCoverage -> String -> String
    calcModStats funcs modName =
      let modFuncs = filter (\fc => fc.idrisFunc.modulePath == modName) funcs
          covCount = length $ filter (.executed) modFuncs
          funcCount = length modFuncs
          pct = if funcCount == 0 then 0.0
                else cast covCount / cast funcCount * 100.0
      in modName ++ ": " ++ show covCount ++ "/" ++ show funcCount ++
         " (" ++ show pct ++ "%)"

-- =============================================================================
-- File I/O Integration
-- =============================================================================

||| Run full coverage analysis from files
export
analyzeFromFiles : (yulPath : String) ->
                   (asmJsonPath : String) ->
                   (tracePath : String) ->
                   (moduleFilter : Maybe String) ->
                   IO (Either String YulCoverageResult)
analyzeFromFiles yulPath asmJsonPath tracePath moduleFilter = do
  -- Read Yul file
  Right yulFuncs <- readYulFile yulPath
    | Left err => pure $ Left $ "Yul parse error: " ++ err

  -- Read asm.json
  Right asmInstrs <- readAsmJson asmJsonPath
    | Left err => pure $ Left $ "ASM JSON parse error: " ++ err

  -- Read trace
  Right traceEntries <- readTraceFile tracePath
    | Left err => pure $ Left $ "Trace parse error: " ++ err

  -- Apply module filter if specified
  let filteredYul = case moduleFilter of
                      Nothing => yulFuncs
                      Just pfx => filterByModule pfx yulFuncs

  -- Calculate coverage
  let result = calculateYulCoverage filteredYul asmInstrs traceEntries
  pure $ Right result

||| Quick analysis without trace (just counts functions)
export
countFunctions : (yulPath : String) ->
                 (moduleFilter : Maybe String) ->
                 IO (Either String (Nat, Nat))
countFunctions yulPath moduleFilter = do
  Right yulFuncs <- readYulFile yulPath
    | Left err => pure $ Left err

  let filtered = case moduleFilter of
                   Nothing => yulFuncs
                   Just pfx => filterByModule pfx yulFuncs

  let idrisFuncs = mapMaybe yulToIdris filtered
  pure $ Right (length filtered, length idrisFuncs)

-- =============================================================================
-- Library API (no file I/O for trace)
-- =============================================================================

||| Run coverage analysis with trace data passed directly (no file I/O)
||| This is the preferred API for Library integration with idris2-evm
export
analyzeWithTrace : (yulPath : String) ->
                   (asmJsonPath : String) ->
                   (traceEntries : List TraceEntry) ->
                   (moduleFilter : Maybe String) ->
                   IO (Either String YulCoverageResult)
analyzeWithTrace yulPath asmJsonPath traceEntries moduleFilter = do
  -- Read Yul file
  Right yulFuncs <- readYulFile yulPath
    | Left err => pure $ Left $ "Yul parse error: " ++ err

  -- Read asm.json
  Right asmInstrs <- readAsmJson asmJsonPath
    | Left err => pure $ Left $ "ASM JSON parse error: " ++ err

  -- Apply module filter if specified
  let filteredYul = case moduleFilter of
                      Nothing => yulFuncs
                      Just pfx => filterByModule pfx yulFuncs

  -- Calculate coverage using provided trace
  let result = calculateYulCoverage filteredYul asmInstrs traceEntries
  pure $ Right result
