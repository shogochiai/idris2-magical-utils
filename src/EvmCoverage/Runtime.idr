||| Runtime Coverage Module
|||
||| Provides EVM runtime coverage analysis:
||| - ProfileFlush event parsing from LOG1 events
||| - Label loading from instrumentation CSV
||| - Coverage calculation and report generation
|||
||| This module uses idris2-evm Interpreter to execute instrumented bytecode
||| and collect coverage data from ProfileFlush events.
module EvmCoverage.Runtime

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Bits
import System.File

-- Import from idris2-evm
import public EVM.Interpreter
import EVM.Word256

-- Import from this package
import EvmCoverage.YulMapper

%default covering

-- =============================================================================
-- ProfileFlush Event Parsing
-- =============================================================================

||| ProfileFlush event topic (keccak256("ProfileFlush(uint256[])"))
public export
profileFlushTopic : Word256
profileFlushTopic = fromInteger 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

||| Check if a log is a ProfileFlush event
public export
isProfileFlush : LogEntry -> Bool
isProfileFlush entry = case entry.logTopics of
  [t] => t == profileFlushTopic
  _ => False

||| Parse 32-byte words from log data as counter values
public export
parseCounters : List Bits8 -> List Integer
parseCounters [] = []
parseCounters bytes =
  let (chunk, rest) = splitAt 32 bytes
      val = bytesToInteger chunk
  in if length chunk < 32 then []
     else val :: parseCounters rest
  where
    bytesToInteger : List Bits8 -> Integer
    bytesToInteger bs = foldl (\acc, b => acc * 256 + cast b) 0 bs

||| Extract counters from ProfileFlush logs
public export
extractProfileCounters : List LogEntry -> List Integer
extractProfileCounters logs =
  case Data.List.find isProfileFlush logs of
    Nothing => []
    Just entry => parseCounters entry.logData

-- =============================================================================
-- Label Entry (from instrumentation CSV)
-- =============================================================================

||| Label entry from CSV: (index, function_name, has_switch)
public export
record LabelEntry where
  constructor MkLabelEntry
  labelIndex : Nat
  mangledName : String    -- Original mangled name (e.g., Main_Functions_Vote_u_castVote)
  demangledName : String  -- Demangled name (e.g., Main.Functions.Vote.castVote)
  hasSwitch : Bool

public export
Show LabelEntry where
  show l = show l.labelIndex ++ ": " ++ l.demangledName

||| Demangle using YulMapper.parseYulFuncName
demangle : String -> String
demangle mangledName =
  case parseYulFuncName mangledName of
    Just f => show f
    Nothing => mangledName

||| Parse a single CSV line (index,function_name,has_switch)
parseLabel : String -> Maybe LabelEntry
parseLabel line =
  let parts = forget (split (== ',') line)
  in case parts of
    (idx :: name :: sw :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i name (demangle name) (trim sw == "True"))
        Nothing => Nothing
    _ => Nothing

||| Load labels from CSV file
public export
loadLabels : String -> IO (Either String (List LabelEntry))
loadLabels path = do
  Right content <- readFile path
    | Left err => pure (Left $ "Failed to read labels file: " ++ show err)
  let ls = lines content
  let dataLines = drop 1 ls  -- Skip header
  let labels = mapMaybe parseLabel dataLines
  pure (Right labels)

||| Look up label entry by index
public export
lookupLabel : Nat -> List LabelEntry -> Maybe LabelEntry
lookupLabel idx labels = find (\l => l.labelIndex == idx) labels

-- =============================================================================
-- Function Runtime Hit
-- =============================================================================

||| Per-function runtime coverage data
public export
record FunctionRuntimeHit where
  constructor MkFunctionRuntimeHit
  funcName       : String   -- Demangled Idris name
  mangledFunc    : String   -- Original mangled name
  canonicalCount : Nat      -- Static branch count (0 if no branches)
  executedCount  : Nat      -- Runtime hit count

public export
Show FunctionRuntimeHit where
  show h = h.funcName ++ ": " ++ show h.executedCount ++
           (if h.canonicalCount > 0 then "/" ++ show h.canonicalCount else "")

public export
Eq FunctionRuntimeHit where
  h1 == h2 = h1.funcName == h2.funcName && h1.executedCount == h2.executedCount

||| Coverage percentage for a function
public export
functionCoveragePercent : FunctionRuntimeHit -> Double
functionCoveragePercent h =
  if h.canonicalCount == 0 then 100.0
  else min 100.0 (cast h.executedCount / cast h.canonicalCount * 100.0)

-- =============================================================================
-- Coverage Result
-- =============================================================================

||| Function coverage result
public export
record CoverageResult where
  constructor MkCoverageResult
  totalFunctions : Nat
  hitFunctions   : Nat
  coveragePercent : Double
  hits : List FunctionRuntimeHit

public export
Show CoverageResult where
  show r = "Coverage: " ++ show r.hitFunctions ++ "/" ++ show r.totalFunctions ++
           " (" ++ show r.coveragePercent ++ "%)"

||| Build FunctionRuntimeHit from label entry and hit count
buildHit : LabelEntry -> Integer -> FunctionRuntimeHit
buildHit lbl hitCount =
  let canonical : Nat = if lbl.hasSwitch then 1 else 0
  in MkFunctionRuntimeHit lbl.demangledName lbl.mangledName canonical (cast {to=Nat} hitCount)

||| Make hit from label and count
mkHit : List LabelEntry -> (Nat, Integer) -> Maybe FunctionRuntimeHit
mkHit labels (idx, cnt) = case lookupLabel idx labels of
  Nothing => Nothing
  Just lbl => Just (buildHit lbl cnt)

||| Build coverage result from labels and counter data
public export
buildCoverageResult : List LabelEntry -> List (Nat, Integer) -> CoverageResult
buildCoverageResult labels counters =
  let hits = mapMaybe (mkHit labels) counters in
  let nonZeroHits = filter (\h => h.executedCount > 0) hits in
  let totalFuncs = length labels in
  let hitFuncs = length nonZeroHits in
  let pct = if totalFuncs == 0 then 100.0 else cast hitFuncs / cast totalFuncs * 100.0 in
  MkCoverageResult totalFuncs hitFuncs pct hits

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Convert coverage result to JSON string
public export
coverageToJson : CoverageResult -> String
coverageToJson r = unlines
  [ "{"
  , "  \"totalFunctions\": " ++ show r.totalFunctions ++ ","
  , "  \"hitFunctions\": " ++ show r.hitFunctions ++ ","
  , "  \"coveragePercent\": " ++ show r.coveragePercent ++ ","
  , "  \"functions\": ["
  , "    " ++ joinBy ",\n    " (map hitToJson r.hits)
  , "  ]"
  , "}"
  ]
  where
    hitToJson : FunctionRuntimeHit -> String
    hitToJson h = "{\"name\": \"" ++ h.funcName ++ "\", " ++
                  "\"mangled\": \"" ++ h.mangledFunc ++ "\", " ++
                  "\"canonical\": " ++ show h.canonicalCount ++ ", " ++
                  "\"executed\": " ++ show h.executedCount ++ "}"

-- =============================================================================
-- High-Level API
-- =============================================================================

||| Index counters with their position
public export
indexCounters : List Integer -> List (Nat, Integer)
indexCounters = go 0
  where
    go : Nat -> List Integer -> List (Nat, Integer)
    go _ [] = []
    go n (x :: xs) = (n, x) :: go (S n) xs

||| Analyze coverage from logs and labels
||| Returns CoverageResult from ProfileFlush event data
public export
analyzeCoverage : List LogEntry -> List LabelEntry -> CoverageResult
analyzeCoverage logs labels =
  let counters = extractProfileCounters logs
      indexed = indexCounters counters
  in buildCoverageResult labels indexed
