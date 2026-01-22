||| Function-level coverage using EVM trace + SourceMap
|||
||| Pipeline:
|||   EVM trace (PC, hitCount)
|||     → SourceMap (PC → IdrisLoc)
|||     → Aggregate by function
|||     → Coverage = hitFunctions / totalFunctions
module EvmCoverage.FunctionCoverage

import EvmCoverage.Types
import EvmCoverage.SourceMap
import EvmCoverage.TraceParser
import EvmCoverage.AsmJsonParser
import EvmCoverage.DumpcasesParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import Data.SortedSet
import System.File

%default covering

-- =============================================================================
-- Function Hit Record
-- =============================================================================

||| A function with its hit count from runtime
public export
record FunctionHit where
  constructor MkFunctionHit
  moduleName : String
  funcName   : String
  hitCount   : Nat

public export
Show FunctionHit where
  show fh = fh.moduleName ++ "." ++ fh.funcName ++ ": " ++ show fh.hitCount ++ " hits"

public export
Eq FunctionHit where
  a == b = a.moduleName == b.moduleName && a.funcName == b.funcName

||| Full function name
export
fullFuncName : FunctionHit -> String
fullFuncName fh = fh.moduleName ++ "." ++ fh.funcName

-- =============================================================================
-- Function Coverage Result
-- =============================================================================

||| Function-level coverage result
public export
record FunctionCoverageResult where
  constructor MkFunctionCoverageResult
  totalFunctions : Nat
  hitFunctions   : Nat
  coveragePercent : Double
  hitList        : List FunctionHit
  missedList     : List String      -- Full function names not hit

public export
Show FunctionCoverageResult where
  show r = "FunctionCoverage: " ++ show r.hitFunctions ++ "/" ++ show r.totalFunctions
        ++ " (" ++ show r.coveragePercent ++ "%)"

-- =============================================================================
-- Trace to Function Hits
-- =============================================================================

||| Convert IdrisLoc to function identifier
locToFuncId : IdrisLoc -> String
locToFuncId loc = loc.moduleName

||| Aggregate trace entries by Idris location
||| Returns map: PC -> hit count
aggregateTraceByPc : List TraceEntry -> SortedMap Nat Nat
aggregateTraceByPc entries = foldl addEntry empty entries
  where
    addEntry : SortedMap Nat Nat -> TraceEntry -> SortedMap Nat Nat
    addEntry m e =
      let current = fromMaybe 0 (lookup e.pc m)
      in insert e.pc (S current) m

||| Convert PC hits to IdrisLoc hits using SourceMap
pcHitsToLocHits : List YulComment -> List AsmInstr -> SortedMap Nat Nat -> List (IdrisLoc, Nat)
pcHitsToLocHits comments instrs pcHits =
  mapMaybe convertHit (SortedMap.toList pcHits)
  where
    convertHit : (Nat, Nat) -> Maybe (IdrisLoc, Nat)
    convertHit (pc, hits) =
      case mapPcToIdris comments instrs pc of
        Nothing => Nothing
        Just loc => Just (loc, hits)

||| Aggregate location hits by module (function level)
aggregateByModule : List (IdrisLoc, Nat) -> List FunctionHit
aggregateByModule locHits =
  let grouped = foldl addHit empty locHits
  in map toFunctionHit (SortedMap.toList grouped)
  where
    addHit : SortedMap String Nat -> (IdrisLoc, Nat) -> SortedMap String Nat
    addHit m (loc, hits) =
      let key = loc.moduleName
          current = fromMaybe 0 (lookup key m)
      in insert key (current + hits) m

    toFunctionHit : (String, Nat) -> FunctionHit
    toFunctionHit (modName, hits) =
      -- For now, module name is the function identifier
      -- Later we can parse to extract actual function name
      MkFunctionHit modName "" hits

-- =============================================================================
-- Extract Functions from Static Analysis
-- =============================================================================

||| Get unique function names from static branch analysis
getStaticFunctions : StaticBranchAnalysis -> List String
getStaticFunctions static =
  let funcs = map (\b => b.branchId.funcName) static.allBranches
  in nub funcs

||| Get unique full function names (module.func) from static analysis
getStaticFullFunctions : StaticBranchAnalysis -> SortedSet String
getStaticFullFunctions static =
  let names = map (\b => b.branchId.moduleName ++ "." ++ b.branchId.funcName) static.allBranches
  in fromList names

-- =============================================================================
-- Coverage Calculation
-- =============================================================================

||| Calculate function coverage from static analysis and runtime hits
export
calculateFunctionCoverage : StaticBranchAnalysis -> List FunctionHit -> FunctionCoverageResult
calculateFunctionCoverage static hits =
  let staticFuncs = getStaticFullFunctions static
      staticFuncList = Prelude.toList staticFuncs
      totalFuncs = length staticFuncList
      hitNames = fromList $ map fullFuncName $ filter (\h => h.hitCount > 0) hits
      hitCount = length $ filter (\f => contains f hitNames) staticFuncList
      missedNames = filter (\f => not (contains f hitNames)) staticFuncList
      percent = if totalFuncs == 0 then 100.0
                else cast hitCount / cast totalFuncs * 100.0
  in MkFunctionCoverageResult totalFuncs hitCount percent hits missedNames

-- =============================================================================
-- Main Entry Point
-- =============================================================================

||| Run function coverage analysis
||| @yulPath - Path to Yul source file (with /* loc */ comments)
||| @asmPath - Path to asm.json (PC to Yul offset mapping)
||| @tracePath - Path to EVM trace CSV
||| @static - Static analysis from dumpcases
export
runFunctionCoverage : String -> String -> String -> StaticBranchAnalysis
                   -> IO (Either String FunctionCoverageResult)
runFunctionCoverage yulPath asmPath tracePath static = do
  -- Read Yul comments
  Right comments <- readYulComments yulPath
    | Left err => pure $ Left $ "Failed to read Yul: " ++ err

  -- Read asm.json
  Right instrs <- readAsmJson asmPath
    | Left err => pure $ Left $ "Failed to read asm.json: " ++ err

  -- Read trace
  Right trace <- readTraceFile tracePath
    | Left err => pure $ Left $ "Failed to read trace: " ++ err

  -- Calculate coverage
  let pcHits = aggregateTraceByPc trace
  let locHits = pcHitsToLocHits comments instrs pcHits
  let funcHits = aggregateByModule locHits
  let result = calculateFunctionCoverage static funcHits

  pure $ Right result

-- =============================================================================
-- Simplified Entry (for when trace is already parsed)
-- =============================================================================

||| Calculate function coverage from pre-parsed data
export
calculateFromParsed : List YulComment -> List AsmInstr -> List TraceEntry
                   -> StaticBranchAnalysis -> FunctionCoverageResult
calculateFromParsed comments instrs trace static =
  let pcHits = aggregateTraceByPc trace
      locHits = pcHitsToLocHits comments instrs pcHits
      funcHits = aggregateByModule locHits
  in calculateFunctionCoverage static funcHits
