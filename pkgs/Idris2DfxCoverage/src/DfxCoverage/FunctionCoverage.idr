||| Function Coverage Analysis
|||
||| Analyzes function-level coverage using:
||| 1. wasm-objdump to extract function index → name mapping
||| 2. ic-wasm profiling data from __get_profiling
||| 3. idris2-c.map for Idris function names
|||
||| Identifies High Impact Targets (uncovered critical functions)
module DfxCoverage.FunctionCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import Data.SortedSet
import System
import System.File

import DfxCoverage.WasmMapper.WasmFunc

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Profiling trace entry (from ic-wasm __get_profiling)
public export
record TraceEntry where
  constructor MkTraceEntry
  funcIdx : Int           -- Positive = entry, negative = exit
  timestamp : Integer

public export
Show TraceEntry where
  show t = "func[" ++ show t.funcIdx ++ "] @ " ++ show t.timestamp

||| Function with coverage info
public export
record FuncCoverageInfo where
  constructor MkFuncCoverageInfo
  funcIdx : Nat
  wasmName : String
  idrisName : Maybe String   -- Dotted form: "Main.dispatchCommand"
  hitCount : Nat
  isExport : Bool
  isUpdate : Bool            -- Update method (state-changing)

public export
Show FuncCoverageInfo where
  show f = "[" ++ show f.funcIdx ++ "] " ++ f.wasmName ++
           (if f.hitCount > 0 then " ✓ (" ++ show f.hitCount ++ ")" else " ✗") ++
           (if f.isUpdate then " [update]" else "")

||| High Impact Target - uncovered function that matters
public export
record HighImpactTarget where
  constructor MkHighImpactTarget
  funcInfo : FuncCoverageInfo
  reason : String            -- Why it's high impact

public export
Show HighImpactTarget where
  show t = "⚠️ " ++ t.funcInfo.wasmName ++ " [" ++ t.reason ++ "]"

||| Complete function coverage result
public export
record FuncCoverageResult where
  constructor MkFuncCoverageResult
  totalFunctions : Nat
  coveredFunctions : Nat
  coveragePercent : Double
  functions : List FuncCoverageInfo
  highImpactTargets : List HighImpactTarget

public export
Show FuncCoverageResult where
  show r = "Function Coverage: " ++ show r.coveredFunctions ++ "/" ++
           show r.totalFunctions ++ " (" ++ show r.coveragePercent ++ "%)" ++
           (if not (null r.highImpactTargets)
              then "\nHigh Impact Gaps: " ++ show (length r.highImpactTargets)
              else "")

-- =============================================================================
-- WASM Function Parsing (using wasm-objdump)
-- =============================================================================

||| Find index of character in list
findCharIndex : Char -> List Char -> Nat
findCharIndex c cs = go 0 cs
  where
    go : Nat -> List Char -> Nat
    go n [] = 0
    go n (x :: xs) = if x == c then n else go (S n) xs

||| Parse a single function line from wasm-objdump
||| Format: " - func[123] sig=2 <function_name>"
parseWasmFuncLine : String -> Maybe (Nat, String)
parseWasmFuncLine line =
  let trimmed = trim line
      chars = unpack trimmed
  in if isPrefixOf "- func[" trimmed
       then do
         -- Extract index: find text between [ and ]
         let afterFunc = drop 7 chars  -- Drop "- func["
         let idxEnd = findCharIndex ']' afterFunc
         let idxStr = pack (take idxEnd afterFunc)
         idx <- parsePositive idxStr
         -- Extract name from <...>
         let nameStart = findCharIndex '<' afterFunc
         let nameEnd = findCharIndex '>' afterFunc
         let name = pack (take (minus nameEnd (S nameStart)) (drop (S nameStart) afterFunc))
         if null name then Nothing else Just (idx, name)
       else Nothing

||| Check if function is an exported update method
isUpdateMethod : String -> Bool
isUpdateMethod name = isInfixOf "canister_update" name

||| Check if function is an exported query method
isQueryMethod : String -> Bool
isQueryMethod name = isInfixOf "canister_query" name

||| Check if function is exported (update or query)
isExportedMethod : String -> Bool
isExportedMethod name = isUpdateMethod name || isQueryMethod name

||| Filter to interesting Idris functions (exclude runtime, csegen, etc.)
isIdrisFunctionName : String -> Bool
isIdrisFunctionName name =
  not (isPrefixOf "ic0_" name) &&
  not (isPrefixOf "__wasi" name) &&
  not (isPrefixOf "__wasm" name) &&
  not (isPrefixOf "csegen" name) &&
  not (isPrefixOf "_brace" name) &&
  not (isPrefixOf "idris2_" name) &&
  not (isPrefixOf "mpz_" name) &&
  not (isPrefixOf "mpn_" name) &&
  not (isPrefixOf "gmp_" name) &&
  not (isPrefixOf "dl" name) &&
  not (isPrefixOf "canister_" name) &&
  not (isInfixOf "wasi" (toLower name)) &&
  not (isInfixOf "emscripten" (toLower name))

||| Convert WASM mangled name to Idris dotted form
||| Example: "Main_dispatchCommand" -> "Main.dispatchCommand"
wasmNameToIdris : String -> String
wasmNameToIdris name =
  -- Replace underscores with dots, but handle special cases
  let parts = forget $ split (== '_') name
  in joinBy "." (toList parts)

||| Parse WASM functions from wasm-objdump output file
export
parseWasmFunctions : String -> IO (Either String (List FuncCoverageInfo))
parseWasmFunctions wasmPath = do
  let tmpFile = "/tmp/wasm_funcs_" ++ show !time ++ ".txt"
  let cmd = "wasm-objdump -x " ++ wasmPath ++ " 2>&1 | grep -E '^ - func\\[' > " ++ tmpFile
  exitCode <- system cmd
  if exitCode /= 0
    then pure $ Left "wasm-objdump failed"
    else do
      Right content <- readFile tmpFile
        | Left err => pure $ Left $ "Failed to read output: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      let lines = lines content
      let parsed = mapMaybe parseWasmFuncLine lines
      let funcs = map toFuncInfo parsed
      pure $ Right funcs
  where
    toFuncInfo : (Nat, String) -> FuncCoverageInfo
    toFuncInfo (idx, name) = MkFuncCoverageInfo
      { funcIdx = idx
      , wasmName = name
      , idrisName = if isIdrisFunctionName name
                      then Just (wasmNameToIdris name)
                      else Nothing
      , hitCount = 0
      , isExport = isExportedMethod name
      , isUpdate = isUpdateMethod name
      }

-- =============================================================================
-- Profiling Data Parsing
-- =============================================================================

||| Parse a single profiling record
parseProfilingRecord : String -> Maybe TraceEntry
parseProfilingRecord part =
  -- Look for pattern like "203 : int32"
  let trimmed = trim part
      nums = words trimmed
  in case nums of
       (numStr :: ":" :: "int32" :: _) =>
         case parseInteger numStr of
           Just n => Just $ MkTraceEntry (cast n) 0
           Nothing => Nothing
       _ => Nothing

||| Parse profiling output from dfx canister call __get_profiling
||| Format: (vec { record { 203 : int32; 49_811 : int64 }; ... }, null)
export
parseProfilingOutput : String -> List TraceEntry
parseProfilingOutput output =
  let parts = toList $ split (== ';') output
  in mapMaybe parseProfilingRecord parts

||| Get unique executed function indices from traces
export
getExecutedFunctions : List TraceEntry -> SortedSet Nat
getExecutedFunctions traces =
  fromList $ map (cast . abs . funcIdx) traces

-- =============================================================================
-- High Impact Target Identification
-- =============================================================================

||| Check if function is high impact based on name patterns
isHighImpactFunction : FuncCoverageInfo -> Maybe String
isHighImpactFunction f =
  let name = toLower f.wasmName
  in if f.isUpdate then Just "state-changing update method"
     else if isInfixOf "transfer" name then Just "transfer operation"
     else if isInfixOf "owner" name then Just "ownership related"
     else if isInfixOf "admin" name then Just "admin function"
     else if isInfixOf "donate" name then Just "financial operation"
     else if isInfixOf "reward" name then Just "reward distribution"
     else if isInfixOf "execute" name then Just "execution handler"
     else if isInfixOf "submit" name then Just "submission handler"
     else if isInfixOf "register" name then Just "registration handler"
     else Nothing

||| Find high impact targets (uncovered + important)
findHighImpactTargets : List FuncCoverageInfo -> List HighImpactTarget
findHighImpactTargets funcs =
  let uncovered = filter (\f => f.hitCount == 0) funcs
      highImpact = mapMaybe toTarget uncovered
  in highImpact
  where
    toTarget : FuncCoverageInfo -> Maybe HighImpactTarget
    toTarget f = case isHighImpactFunction f of
      Just reason => Just $ MkHighImpactTarget f reason
      Nothing => Nothing

-- =============================================================================
-- Coverage Calculation
-- =============================================================================

||| Update function info with hit count from profiling
updateWithHits : SortedSet Nat -> FuncCoverageInfo -> FuncCoverageInfo
updateWithHits executed f =
  if contains f.funcIdx executed
    then { hitCount := 1 } f
    else f

||| Calculate function coverage
export
calculateCoverage : List FuncCoverageInfo -> SortedSet Nat -> FuncCoverageResult
calculateCoverage funcs executed =
  let idrisFuncs = filter (isJust . idrisName) funcs
      updated = map (updateWithHits executed) idrisFuncs
      totalCount = length updated
      coveredCount = length (filter (\f => f.hitCount > 0) updated)
      pct : Double
      pct = if totalCount == 0 then 100.0
            else (cast coveredCount / cast totalCount) * 100.0
      highImpact = findHighImpactTargets updated
  in MkFuncCoverageResult totalCount coveredCount pct updated highImpact

-- =============================================================================
-- Full Pipeline
-- =============================================================================

||| Run complete function coverage analysis
|||
||| @wasmPath Path to WASM file
||| @profilingOutput Output from __get_profiling call
export
runFunctionCoverage : String -> String -> IO (Either String FuncCoverageResult)
runFunctionCoverage wasmPath profilingOutput = do
  -- Parse WASM functions
  Right funcs <- parseWasmFunctions wasmPath
    | Left err => pure $ Left $ "Failed to parse WASM: " ++ err

  -- Parse profiling data
  let traces = parseProfilingOutput profilingOutput
  let executed = getExecutedFunctions traces

  -- Calculate coverage
  let result = calculateCoverage funcs executed

  pure $ Right result

||| Format coverage result for display
export
formatCoverageResult : FuncCoverageResult -> String
formatCoverageResult r =
  unlines
    [ "═══════════════════════════════════════════════════════════════"
    , "Function Coverage Report"
    , "═══════════════════════════════════════════════════════════════"
    , ""
    , "Coverage: " ++ show r.coveredFunctions ++ "/" ++ show r.totalFunctions ++
      " functions (" ++ show r.coveragePercent ++ "%)"
    , ""
    , if null r.highImpactTargets
        then "✓ No high-impact gaps found"
        else "⚠️ High Impact Uncovered Functions:"
    ] ++ (if null r.highImpactTargets then ""
          else unlines (map formatTarget r.highImpactTargets))
  where
    formatTarget : HighImpactTarget -> String
    formatTarget t = "  ⚠️ " ++ (fromMaybe t.funcInfo.wasmName t.funcInfo.idrisName) ++
                     " [" ++ t.reason ++ "]"
