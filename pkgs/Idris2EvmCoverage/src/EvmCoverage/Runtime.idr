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

-- Import Core types for Library API (qualified to avoid conflicts)
-- Note: These must be imported without "public" to avoid shadowing local types
import Coverage.Core.RuntimeHit as CoreRH
import Coverage.Core.HighImpact as CoreHI
import Coverage.Core.Result as CoreRes

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
data LabelKind = LKFunction | LKBranch | LKPath

public export
Show LabelKind where
  show LKFunction = "function"
  show LKBranch = "branch"
  show LKPath = "path"

public export
Eq LabelKind where
  LKFunction == LKFunction = True
  LKBranch == LKBranch = True
  LKPath == LKPath = True
  _ == _ = False

public export
record LabelEntry where
  constructor MkLabelEntry
  labelIndex : Nat
  labelKind : LabelKind
  mangledName : String    -- Original mangled name (e.g., Main_Functions_Vote_u_castVote)
  demangledName : String  -- Demangled name (e.g., Main.Functions.Vote.castVote)
  branchId : Maybe String
  canonicalCountHint : Nat

public export
Show LabelEntry where
  show l = show l.labelIndex ++ ": " ++ l.demangledName ++
           maybe "" (\bid => " -> " ++ bid) l.branchId

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
    (idx :: "path" :: mangled :: demangledName :: pid :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i LKPath mangled demangledName
                                  (if null (trim pid) then Nothing else Just (trim pid))
                                  1)
        Nothing => Nothing
    (idx :: "branch" :: mangled :: demangledName :: bid :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i LKBranch mangled demangledName
                                  (if null (trim bid) then Nothing else Just (trim bid))
                                  1)
        Nothing => Nothing
    (idx :: "function" :: mangled :: demangledName :: hint :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i LKFunction mangled demangledName Nothing
                                  (if trim hint == "True" then 1 else 0))
        Nothing => Nothing
    (idx :: name :: sw :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i LKFunction name (demangle name) Nothing
                                  (if trim sw == "True" then 1 else 0))
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
-- Function Runtime Hit (local EVM type)
-- =============================================================================

||| Per-function runtime coverage data (local EVM-specific type)
||| Named to avoid conflict with CoreRH.FunctionRuntimeHit
public export
record EvmFunctionHit where
  constructor MkEvmFunctionHit
  funcName       : String   -- Demangled Idris name
  mangledFunc    : String   -- Original mangled name
  canonicalCount : Nat      -- Static branch count (0 if no branches)
  executedCount  : Nat      -- Runtime hit count

public export
Show EvmFunctionHit where
  show h = h.funcName ++ ": " ++ show h.executedCount ++
           (if h.canonicalCount > 0 then "/" ++ show h.canonicalCount else "")

public export
Eq EvmFunctionHit where
  h1 == h2 = h1.funcName == h2.funcName && h1.executedCount == h2.executedCount

||| Coverage percentage for a function
public export
functionCoveragePercent : EvmFunctionHit -> Double
functionCoveragePercent h =
  if h.canonicalCount == 0 then 100.0
  else min 100.0 (cast h.executedCount / cast h.canonicalCount * 100.0)

-- =============================================================================
-- Coverage Result (local EVM type)
-- =============================================================================

||| Function coverage result (local EVM-specific type)
||| Named to avoid conflict with CoreRes.CoverageResult
public export
record EvmCoverageResult where
  constructor MkEvmCoverageResult
  totalFunctions : Nat
  hitFunctions   : Nat
  coveragePercent : Double
  hits : List EvmFunctionHit

public export
Show EvmCoverageResult where
  show r = "Coverage: " ++ show r.hitFunctions ++ "/" ++ show r.totalFunctions ++
           " (" ++ show r.coveragePercent ++ "%)"

||| Build EvmFunctionHit from label entry and hit count
buildHit : LabelEntry -> Integer -> EvmFunctionHit
buildHit lbl hitCount =
  let canonical : Nat = lbl.canonicalCountHint
  in MkEvmFunctionHit lbl.demangledName lbl.mangledName canonical (cast {to=Nat} hitCount)

||| Make hit from label and count
mkHit : List LabelEntry -> (Nat, Integer) -> Maybe EvmFunctionHit
mkHit labels (idx, cnt) = case lookupLabel idx labels of
  Nothing => Nothing
  Just lbl => Just (buildHit lbl cnt)

||| Build coverage result from labels and counter data
public export
buildEvmCoverageResult : List LabelEntry -> List (Nat, Integer) -> EvmCoverageResult
buildEvmCoverageResult labels counters =
  let hits = mapMaybe (mkHit labels) counters in
  let nonZeroHits = filter (\h => h.executedCount > 0) hits in
  let totalFuncs = length labels in
  let hitFuncs = length nonZeroHits in
  let pct = if totalFuncs == 0 then 100.0 else cast hitFuncs / cast totalFuncs * 100.0 in
  MkEvmCoverageResult totalFuncs hitFuncs pct hits

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Convert coverage result to JSON string
public export
coverageToJson : EvmCoverageResult -> String
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
    hitToJson : EvmFunctionHit -> String
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
||| Returns EvmCoverageResult from ProfileFlush event data
public export
analyzeCoverage : List LogEntry -> List LabelEntry -> EvmCoverageResult
analyzeCoverage logs labels =
  let counters = extractProfileCounters logs
      indexed = indexCounters counters
  in buildEvmCoverageResult labels indexed

-- =============================================================================
-- Text-based Log Parser (for idris2-evm-run output)
-- =============================================================================

||| ProfileFlush topic as hex string (without 0x prefix)
profileFlushTopicHex : String
profileFlushTopicHex = "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"

||| Strip 0x prefix from hex string
stripHexPrefix : String -> String
stripHexPrefix s = if isPrefixOf "0x" s then strSubstr 2 (cast $ length s) s else s

||| Parse a single hex character to Nat
hexCharToNat : Char -> Maybe Nat
hexCharToNat c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

||| Parse hex string to Integer (big-endian)
parseHexString : String -> Maybe Integer
parseHexString s =
  let hex = stripHexPrefix s
  in foldlM addHexDigit 0 (unpack hex)
  where
    addHexDigit : Integer -> Char -> Maybe Integer
    addHexDigit acc c = case hexCharToNat c of
      Nothing => Nothing
      Just n => Just (acc * 16 + cast n)

||| Parse 64-char hex (32 bytes) chunks from hex string into counter values
parseHexCounters : String -> List Integer
parseHexCounters s =
  let hex = stripHexPrefix s
  in go hex
  where
    go : String -> List Integer
    go str =
      let chunk = strSubstr 0 64 str
          rest = strSubstr 64 (cast $ length str) str
      in if length chunk < 64 then []
         else case parseHexString chunk of
           Nothing => []
           Just val => val :: go rest

||| Decode ABI-wrapped uint256[] payload if present.
||| EVM logs for dynamic arrays may appear as:
|||   [offset, length, elem0, elem1, ...]
||| or sometimes as:
|||   [length, elem0, elem1, ...]
||| If no ABI header is detected, return words unchanged.
decodeCounterPayload : List Integer -> List Integer
decodeCounterPayload [] = []
decodeCounterPayload [w0] = [w0]
decodeCounterPayload (w0 :: w1 :: ws) =
  if w1 == cast {to=Integer} (length ws)
     then ws
  else if w0 == cast {to=Integer} (S (length ws))
     then w1 :: ws
  else w0 :: w1 :: ws

||| Check if a line looks like ProfileFlush hex data (long hex string)
isLongHexData : String -> Bool
isLongHexData s =
  let trimmed = ltrim s
  in isPrefixOf "0x" trimmed && length trimmed > 200

||| Extract hex data line (long 0x... line after ProfileFlush topic)
extractHexDataLine : List String -> Maybe String
extractHexDataLine [] = Nothing
extractHexDataLine (l :: rest) =
  let trimmed = ltrim l
  in if isLongHexData l
       then Just trimmed  -- Found long hex data
       else if isPrefixOf "Log #" trimmed || isPrefixOf "Result:" trimmed
         then Nothing  -- Hit next section without finding data
         else extractHexDataLine rest

||| Find ProfileFlush log and extract its hex data
||| Looks for topic line then scans forward for long hex data
findProfileFlushData : List String -> Maybe String
findProfileFlushData [] = Nothing
findProfileFlushData (l :: rest) =
  if isInfixOf profileFlushTopicHex l
    then extractHexDataLine rest  -- Topic found, look for hex data
    else findProfileFlushData rest

||| Find ProfileFlush log data in trace output text
||| Looks for log entry with topic matching profileFlushTopicHex
public export
extractProfileFlushFromText : String -> Maybe String
extractProfileFlushFromText content =
  let ls = lines content
  in findProfileFlushData ls

||| Parse ProfileFlush counters from trace output text
public export
parseProfileFlushCounters : String -> List Integer
parseProfileFlushCounters content =
  case extractProfileFlushFromText content of
    Nothing => []
    Just hexData => decodeCounterPayload (parseHexCounters hexData)

||| Analyze coverage from trace output text and labels
||| Higher-level function that works with idris2-evm-run output
public export
analyzeCoverageFromText : String -> List LabelEntry -> EvmCoverageResult
analyzeCoverageFromText traceOutput labels =
  let counters = parseProfileFlushCounters traceOutput
      indexed = indexCounters counters
  in buildEvmCoverageResult labels indexed

||| Read trace file and analyze coverage
public export
analyzeCoverageFromFile : String -> String -> IO (Either String EvmCoverageResult)
analyzeCoverageFromFile tracePath labelPath = do
  Right traceContent <- readFile tracePath
    | Left err => pure (Left $ "Failed to read trace file: " ++ show err)
  Right labels <- loadLabels labelPath
    | Left err => pure (Left err)
  let result = analyzeCoverageFromText traceContent labels
  pure (Right result)

public export
record EvmBranchCoverageResult where
  constructor MkEvmBranchCoverageResult
  staticTotalBranches : Nat
  materializedTotalBranches : Nat
  hitBranches : Nat
  coveragePercent : Double
  coveredBranchIds : List String
  uncoveredBranchIds : List String
  coveredFunctions : List String
  uncoveredFunctions : List String
  unobservableBranchIds : List String
  materializedBranchIds : List String

public export
record BranchCounterDiagnostic where
  constructor MkBranchCounterDiagnostic
  labelIndex : Nat
  demangledName : String
  branchId : String
  inStaticDenominator : Bool
  observedCount : Integer
  wasObserved : Bool

dedupStrings : List String -> List String
dedupStrings [] = []
dedupStrings (x :: xs) = if elem x xs then dedupStrings xs else x :: dedupStrings xs

branchLabels : List LabelEntry -> List LabelEntry
branchLabels = filter (\l => l.labelKind == LKBranch && isJust l.branchId)

pathLabels : List LabelEntry -> List LabelEntry
pathLabels = filter (\l => l.labelKind == LKPath && isJust l.branchId)

coveredBranchIdsFromCounters : List LabelEntry -> List (Nat, Integer) -> List String
coveredBranchIdsFromCounters labels counters =
  dedupStrings $
    mapMaybe resolveCovered counters
  where
    resolveCovered : (Nat, Integer) -> Maybe String
    resolveCovered (idx, cnt) =
      if cnt <= 0 then Nothing
      else branchId =<< lookupLabel idx labels

public export
pathHitsFromCounters : List LabelEntry -> List (Nat, Integer) -> List CoreRH.PathRuntimeHit
pathHitsFromCounters labels counters =
  mapMaybe resolveCovered counters
  where
    resolveCovered : (Nat, Integer) -> Maybe CoreRH.PathRuntimeHit
    resolveCovered (idx, cnt) =
      if cnt <= 0 then Nothing
      else case lookupLabel idx labels of
             Just lbl =>
               case (lbl.labelKind, lbl.branchId) of
                 (LKPath, Just pid) => Just (CoreRH.MkPathRuntimeHit pid (cast cnt))
                 _ => Nothing
             Nothing => Nothing

branchFunctionName : LabelEntry -> Maybe String
branchFunctionName lbl = if lbl.labelKind == LKBranch then Just lbl.demangledName else Nothing

counterAtIndex : Nat -> List (Nat, Integer) -> Integer
counterAtIndex idx [] = 0
counterAtIndex idx ((other, cnt) :: rest) =
  if idx == other then cnt else counterAtIndex idx rest

public export
buildBranchCounterDiagnostics : List LabelEntry -> List String -> List (Nat, Integer) -> List BranchCounterDiagnostic
buildBranchCounterDiagnostics labels staticDenominatorIds counters =
  mapMaybe toDiagnostic (branchLabels labels)
  where
    toDiagnostic : LabelEntry -> Maybe BranchCounterDiagnostic
    toDiagnostic lbl =
      case lbl.branchId of
        Nothing => Nothing
        Just bid =>
          let observed = counterAtIndex lbl.labelIndex counters in
          Just $
            MkBranchCounterDiagnostic
              lbl.labelIndex
              lbl.demangledName
              bid
              (elem bid staticDenominatorIds)
              observed
              (observed > 0)

diagnosticToCsvLine : BranchCounterDiagnostic -> String
diagnosticToCsvLine d =
  show d.labelIndex ++ ","
  ++ d.demangledName ++ ","
  ++ d.branchId ++ ","
  ++ show d.inStaticDenominator ++ ","
  ++ show d.observedCount ++ ","
  ++ show d.wasObserved

public export
branchCounterDiagnosticsCsv : List BranchCounterDiagnostic -> String
branchCounterDiagnosticsCsv diags =
  "label_index,demangled_name,branch_id,in_static_denominator,observed_count,was_observed\n"
  ++ unlines (map diagnosticToCsvLine diags)

public export
buildBranchCoverageResult : List LabelEntry -> List String -> List (Nat, Integer) -> EvmBranchCoverageResult
buildBranchCoverageResult labels staticDenominatorIds counters =
  let branchLbls = branchLabels labels in
  let observableIds = dedupStrings (mapMaybe branchId branchLbls) in
  let materializedIds = filter (\bid => elem bid observableIds) staticDenominatorIds in
  let coveredIds = coveredBranchIdsFromCounters labels counters in
  let uncoveredIds = filter (\bid => not (elem bid coveredIds)) materializedIds in
  let unobservableIds = filter (\bid => not (elem bid observableIds)) staticDenominatorIds in
  let totalCount = length materializedIds in
  let hitCount = length (filter (\bid => elem bid materializedIds) coveredIds) in
  let pct = if totalCount == 0 then 100.0 else cast hitCount / cast totalCount * 100.0 in
  let coveredFns = coveredFunctionNames coveredIds branchLbls in
  let uncoveredFns = coveredFunctionNames uncoveredIds branchLbls in
  MkEvmBranchCoverageResult
    (length staticDenominatorIds)
    totalCount
    hitCount
    pct
    coveredIds
    uncoveredIds
    coveredFns
    uncoveredFns
    unobservableIds
    materializedIds
  where
    coveredFunctionNames : List String -> List LabelEntry -> List String
    coveredFunctionNames branchIds lbls =
      dedupStrings $
        mapMaybe (\lbl => case lbl.branchId of
                             Just bid => if elem bid branchIds then Just lbl.demangledName else Nothing
                             Nothing => Nothing) lbls

public export
analyzeBranchCoverageFromText : String -> List LabelEntry -> List String -> EvmBranchCoverageResult
analyzeBranchCoverageFromText traceOutput labels staticDenominatorIds =
  let counters = parseProfileFlushCounters traceOutput
      indexed = indexCounters counters
  in buildBranchCoverageResult labels staticDenominatorIds indexed

public export
analyzeBranchCoverageFromFile : String -> String -> List String -> IO (Either String EvmBranchCoverageResult)
analyzeBranchCoverageFromFile tracePath labelPath staticDenominatorIds = do
  Right traceContent <- readFile tracePath
    | Left err => pure (Left $ "Failed to read trace file: " ++ show err)
  Right labels <- loadLabels labelPath
    | Left err => pure (Left err)
  pure $ Right $ analyzeBranchCoverageFromText traceContent labels staticDenominatorIds

public export
analyzePathHitsFromText : String -> List LabelEntry -> List CoreRH.PathRuntimeHit
analyzePathHitsFromText traceOutput labels =
  let counters = parseProfileFlushCounters traceOutput
      indexed = indexCounters counters
  in pathHitsFromCounters labels indexed

public export
analyzePathHitsFromFile : String -> String -> IO (Either String (List CoreRH.PathRuntimeHit))
analyzePathHitsFromFile tracePath labelPath = do
  Right traceContent <- readFile tracePath
    | Left err => pure (Left $ "Failed to read trace file: " ++ show err)
  Right labels <- loadLabels labelPath
    | Left err => pure (Left err)
  pure $ Right $ analyzePathHitsFromText traceContent labels

public export
writeBranchCounterDiagnostics : String -> String -> List String -> String -> IO (Either String ())
writeBranchCounterDiagnostics tracePath labelPath staticDenominatorIds csvPath = do
  Right traceContent <- readFile tracePath
    | Left err => pure (Left $ "Failed to read trace file: " ++ show err)
  Right labels <- loadLabels labelPath
    | Left err => pure (Left err)
  let counters = indexCounters (parseProfileFlushCounters traceContent)
  let diagnostics = buildBranchCounterDiagnostics labels staticDenominatorIds counters
  Right () <- writeFile csvPath (branchCounterDiagnosticsCsv diagnostics)
    | Left err => pure (Left $ "Failed to write diagnostics CSV: " ++ show err)
  pure (Right ())

-- =============================================================================
-- Library API: Conversion to Core Types
-- =============================================================================

||| Convert local EvmFunctionHit to CoreRH.FunctionRuntimeHit
||| Maps mangledFunc to schemeFunc, uses 0 for totalExprs/coveredExprs
public export
toCoreRuntimeHit : EvmFunctionHit -> CoreRH.FunctionRuntimeHit
toCoreRuntimeHit h =
  CoreRH.MkFunctionRuntimeHit
    h.funcName
    h.mangledFunc          -- schemeFunc
    h.canonicalCount
    h.executedCount
    0                      -- totalExprs (not tracked in EVM)
    0                      -- coveredExprs (not tracked in EVM)

||| Convert local EvmCoverageResult to CoreRes.CoverageResult
||| Extracts function names into target/covered/uncovered lists
public export
toCoreCoverageResult : EvmCoverageResult -> CoreRes.CoverageResult
toCoreCoverageResult r =
  let theHits = hits r
      allFuncs = map funcName theHits
      coveredFuncs = map funcName $ filter (\h => executedCount h > 0) theHits
      uncoveredFuncs = map funcName $ filter (\h => executedCount h == 0) theHits
  in CoreRes.MkCoverageResult
       allFuncs
       coveredFuncs
       uncoveredFuncs
       (coveragePercent r)
       (hitFunctions r)
       (totalFunctions r)

||| Convert local EvmFunctionHit list to CoreRH.FunctionRuntimeHit list
public export
toCoreRuntimeHits : List EvmFunctionHit -> List CoreRH.FunctionRuntimeHit
toCoreRuntimeHits = map toCoreRuntimeHit

-- =============================================================================
-- Library API: High Impact Targets
-- =============================================================================

||| Get High Impact Targets from coverage result
||| Returns list of HighImpactTarget sorted by severity (descending)
public export
getHighImpactTargets : EvmCoverageResult -> List CoreHI.HighImpactTarget
getHighImpactTargets r =
  let coreHits = toCoreRuntimeHits (hits r)
  in CoreHI.targetsFromRuntimeHits coreHits

||| Get Top K High Impact Targets from coverage result
||| Sorted by severity (branchCount/executedCount ratio, descending)
public export
getTopKHighImpactTargets : Nat -> EvmCoverageResult -> List CoreHI.HighImpactTarget
getTopKHighImpactTargets k r = CoreHI.topKTargets k (getHighImpactTargets r)

-- =============================================================================
-- Library API: Unified Entry Points
-- =============================================================================

||| Analyze coverage from trace and labels, return Core types
||| Primary Library API for LazyEvm integration
|||
||| Returns (CoreRes.CoverageResult, List CoreHI.HighImpactTarget)
public export
analyzeEvmCoverage : String -> String -> IO (Either String (CoreRes.CoverageResult, List CoreHI.HighImpactTarget))
analyzeEvmCoverage tracePath labelPath = do
  Right localResult <- analyzeCoverageFromFile tracePath labelPath
    | Left err => pure (Left err)
  let coreCov = toCoreCoverageResult localResult
  let hits = getHighImpactTargets localResult
  pure (Right (coreCov, hits))

||| Analyze coverage from LogEntry list directly (for Interpreter integration)
||| Used when running EVM bytecode with idris2-evm Interpreter
|||
||| Returns (CoreRes.CoverageResult, List CoreHI.HighImpactTarget)
public export
analyzeEvmCoverageFromLogs : List LogEntry -> List LabelEntry -> (CoreRes.CoverageResult, List CoreHI.HighImpactTarget)
analyzeEvmCoverageFromLogs logs labels =
  let localResult = analyzeCoverage logs labels
      coreCov = toCoreCoverageResult localResult
      hits = getHighImpactTargets localResult
  in (coreCov, hits)
