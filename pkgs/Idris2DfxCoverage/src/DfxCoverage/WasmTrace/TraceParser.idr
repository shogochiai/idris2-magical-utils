||| WASM Trace Parser
|||
||| Parses execution traces from wasmtime profiler or custom instrumentation.
|||
||| Supported formats:
||| 1. wasmtime perf format: funcIdx|pc|opcode|depth
||| 2. JSON trace format (future)
module DfxCoverage.WasmTrace.TraceParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import DfxCoverage.WasmTrace.TraceEntry

%default covering

-- =============================================================================
-- Trace Format Detection
-- =============================================================================

||| Supported trace formats
public export
data TraceFormat
  = PerfFormat       -- wasmtime perf-style: funcIdx|pc|opcode|depth
  | JsonFormat       -- JSON array of trace entries
  | UnknownFormat

public export
Show TraceFormat where
  show PerfFormat = "perf"
  show JsonFormat = "json"
  show UnknownFormat = "unknown"

||| Detect trace format from content
export
detectFormat : String -> TraceFormat
detectFormat content =
  let trimmed = trim content
  in if isPrefixOf "[" trimmed || isPrefixOf "{" trimmed
       then JsonFormat
       else if isInfixOf "|" trimmed
         then PerfFormat
         else UnknownFormat

-- =============================================================================
-- Perf Format Parsing
-- =============================================================================

||| Parse a single line of perf format
||| Format: funcIdx|pc|opcode|depth
parsePerfLine : String -> Maybe WasmTraceEntry
parsePerfLine line =
  let parts = forget $ split (== '|') line
  in case parts of
       [funcIdxStr, pcStr, op, depthStr] =>
         case (parsePositive {a=Nat} (trim funcIdxStr),
               parsePositive {a=Nat} (trim pcStr),
               parsePositive {a=Nat} (trim depthStr)) of
           (Just funcIdx, Just pc, Just depth) =>
             Just $ MkWasmTraceEntry funcIdx pc (trim op) depth
           _ => Nothing
       -- Also support hex PC format: funcIdx|0xABC|opcode|depth
       (funcIdxStr :: pcStr :: op :: depthStr :: _) =>
         let cleanPc = if isPrefixOf "0x" (trim pcStr)
                         then trim $ substr 2 100 (trim pcStr)
                         else trim pcStr
         in case (parsePositive {a=Nat} (trim funcIdxStr),
                  parseHexNat cleanPc,
                  parsePositive {a=Nat} (trim depthStr)) of
              (Just funcIdx, Just pc, Just depth) =>
                Just $ MkWasmTraceEntry funcIdx pc (trim op) depth
              _ => Nothing
       _ => Nothing
  where
    parseHexNat : String -> Maybe Nat
    parseHexNat s = parsePositive {a=Nat} s  -- Simplified: just use decimal for now

||| Parse all lines in perf format
parsePerfTrace : String -> List WasmTraceEntry
parsePerfTrace content =
  let contentLines = lines content
  in mapMaybe parsePerfLine contentLines

-- =============================================================================
-- JSON Format Parsing (Simplified)
-- =============================================================================

findSubstrJson : String -> String -> Maybe Nat
findSubstrJson needle haystack = go 0
  where
    go : Nat -> Maybe Nat
    go idx =
      if idx + length needle > length haystack
        then Nothing
        else if substr idx (length needle) haystack == needle
          then Just idx
          else go (S idx)

extractJsonVal : String -> String -> Maybe String
extractJsonVal key json =
  case findSubstrJson ("\"" ++ key ++ "\":") json of
    Nothing => Nothing
    Just idx =>
      let afterKey = substr (idx + length key + 3) 50 json
          valChars = takeWhile (\c => c /= ',' && c /= '}' && c /= '"') (unpack afterKey)
      in Just $ pack valChars

||| Parse JSON trace entry (simplified string matching)
||| Format: {"func": N, "pc": N, "op": "...", "depth": N}
parseJsonEntry : String -> Maybe WasmTraceEntry
parseJsonEntry json =
  case (extractJsonVal "func" json, extractJsonVal "pc" json,
        extractJsonVal "op" json, extractJsonVal "depth" json) of
    (Just funcStr, Just pcStr, Just op, Just depthStr) =>
      case (parsePositive {a=Nat} (trim funcStr),
            parsePositive {a=Nat} (trim pcStr),
            parsePositive {a=Nat} (trim depthStr)) of
        (Just funcIdx, Just pc, Just depth) =>
          Just $ MkWasmTraceEntry funcIdx pc (trim op) depth
        _ => Nothing
    _ => Nothing

||| Parse JSON array of trace entries
parseJsonTrace : String -> List WasmTraceEntry
parseJsonTrace content =
  -- Split by },{ and parse each
  let entries = forget $ split (== '}') content
  in mapMaybe parseJsonEntry entries

-- =============================================================================
-- Main Parse Functions
-- =============================================================================

||| Parse trace content with auto-detected format
export
parseTrace : String -> List WasmTraceEntry
parseTrace content =
  case detectFormat content of
    PerfFormat => parsePerfTrace content
    JsonFormat => parseJsonTrace content
    UnknownFormat => []

||| Parse trace content with specified format
export
parseTraceWithFormat : TraceFormat -> String -> List WasmTraceEntry
parseTraceWithFormat PerfFormat = parsePerfTrace
parseTraceWithFormat JsonFormat = parseJsonTrace
parseTraceWithFormat UnknownFormat = const []

||| Read and parse trace from file
export
readTraceFile : String -> IO (Either String (List WasmTraceEntry))
readTraceFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read trace file: " ++ show err
  let traces = parseTrace content
  if null traces
    then pure $ Left "No trace entries parsed (check format)"
    else pure $ Right traces

-- =============================================================================
-- Trace Filtering
-- =============================================================================

||| Filter traces to specific function indices
export
filterByFuncIdx : List Nat -> List WasmTraceEntry -> List WasmTraceEntry
filterByFuncIdx funcIdxs = filter (\te => te.funcIdx `elem` funcIdxs)

||| Filter traces to specific opcode categories
export
filterByCategory : List WasmOpcodeCategory -> List WasmTraceEntry -> List WasmTraceEntry
filterByCategory cats traces = filter (\te => traceCategory te `elem` cats) traces

||| Keep only control flow related traces
export
controlFlowOnly : List WasmTraceEntry -> List WasmTraceEntry
controlFlowOnly = filterByCategory [OpControl]

-- =============================================================================
-- Trace Statistics
-- =============================================================================

||| Statistics about a trace
public export
record TraceStats where
  constructor MkTraceStats
  totalEntries : Nat
  uniqueFuncs : Nat
  maxDepth : Nat
  categoryBreakdown : List (WasmOpcodeCategory, Nat)

public export
Show TraceStats where
  show ts = "Trace: " ++ show ts.totalEntries ++ " entries, " ++
            show ts.uniqueFuncs ++ " functions, max depth " ++ show ts.maxDepth

countCategoriesHelper : List WasmTraceEntry -> List (WasmOpcodeCategory, Nat)
countCategoriesHelper ts =
  let allCats = [OpControl, OpLocal, OpGlobal, OpMemory, OpNumeric, OpConversion, OpOther]
  in map (\c => (c, length $ filter (\t => traceCategory t == c) ts)) allCats

||| Compute statistics for trace
export
traceStats : List WasmTraceEntry -> TraceStats
traceStats traces =
  MkTraceStats
    (length traces)
    (length $ nub $ map (.funcIdx) traces)
    (foldr max 0 $ map (.depth) traces)
    (countCategoriesHelper traces)
