||| Function Coverage Analysis compatibility layer.
module DfxCoverage.FunctionCoverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedSet
import System
import System.File

import DfxCoverage.WasmMapper.WasmFunc

%default covering

public export
record TraceEntry where
  constructor MkTraceEntry
  funcIdx : Int
  timestamp : Integer

public export
Show TraceEntry where
  show t = "func[" ++ show t.funcIdx ++ "] @ " ++ show t.timestamp

public export
record FuncCoverageInfo where
  constructor MkFuncCoverageInfo
  funcIdx : Nat
  wasmName : String
  idrisName : Maybe String
  hitCount : Nat
  isExport : Bool
  isUpdate : Bool

public export
Show FuncCoverageInfo where
  show f = "[" ++ show f.funcIdx ++ "] " ++ f.wasmName ++
           (if f.hitCount > 0 then " ✓ (" ++ show f.hitCount ++ ")" else " ✗") ++
           (if f.isUpdate then " [update]" else "")

public export
record HighImpactTarget where
  constructor MkHighImpactTarget
  funcInfo : FuncCoverageInfo
  reason : String

public export
Show HighImpactTarget where
  show t = "⚠️ " ++ t.funcInfo.wasmName ++ " [" ++ t.reason ++ "]"

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

findCharIndex : Char -> List Char -> Nat
findCharIndex c cs = go 0 cs
  where
    go : Nat -> List Char -> Nat
    go _ [] = 0
    go n (x :: xs) = if x == c then n else go (S n) xs

parseWasmFuncLine : String -> Maybe (Nat, String)
parseWasmFuncLine line =
  let trimmed = trim line
      chars = unpack trimmed
  in if isPrefixOf "- func[" trimmed
       then do
         let afterFunc = drop 7 chars
         let idxEnd = findCharIndex ']' afterFunc
         let idxStr = pack (take idxEnd afterFunc)
         idx <- parsePositive idxStr
         let nameStart = findCharIndex '<' afterFunc
         let nameEnd = findCharIndex '>' afterFunc
         let name = pack (take (minus nameEnd (S nameStart)) (drop (S nameStart) afterFunc))
         if null name then Nothing else Just (idx, name)
       else Nothing

isUpdateMethod : String -> Bool
isUpdateMethod name = isInfixOf "canister_update" name

isQueryMethod : String -> Bool
isQueryMethod name = isInfixOf "canister_query" name

isExportedMethod : String -> Bool
isExportedMethod name = isUpdateMethod name || isQueryMethod name

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

wasmNameToIdris : String -> String
wasmNameToIdris name =
  let parts = forget $ split (== '_') name
  in joinBy "." (toList parts)

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
      let parsed = mapMaybe parseWasmFuncLine (lines content)
      pure $ Right $ map toFuncInfo parsed
  where
    toFuncInfo : (Nat, String) -> FuncCoverageInfo
    toFuncInfo (idx, name) = MkFuncCoverageInfo
      { funcIdx = idx
      , wasmName = name
      , idrisName = if isIdrisFunctionName name then Just (wasmNameToIdris name) else Nothing
      , hitCount = 0
      , isExport = isExportedMethod name
      , isUpdate = isUpdateMethod name
      }

parseProfilingRecord : String -> Maybe TraceEntry
parseProfilingRecord part =
  let trimmed = trim part
      nums = words trimmed
  in case nums of
       (numStr :: ":" :: "int32" :: _) =>
         case parseInteger numStr of
           Just n => Just $ MkTraceEntry (cast n) 0
           Nothing => Nothing
       _ => Nothing

export
parseProfilingOutput : String -> List TraceEntry
parseProfilingOutput output =
  let parts = toList $ split (== ';') output
  in mapMaybe parseProfilingRecord parts

export
getExecutedFunctions : List TraceEntry -> SortedSet Nat
getExecutedFunctions traces =
  fromList $ map (cast . abs . funcIdx) traces

isHighImpactFunction : FuncCoverageInfo -> Maybe String
isHighImpactFunction f =
  let name = toLower f.wasmName in
  if f.isUpdate then Just "state-changing update method"
  else if isInfixOf "transfer" name then Just "transfer operation"
  else if isInfixOf "owner" name then Just "ownership related"
  else if isInfixOf "admin" name then Just "admin function"
  else if isInfixOf "donate" name then Just "financial operation"
  else if isInfixOf "reward" name then Just "reward distribution"
  else if isInfixOf "execute" name then Just "execution handler"
  else if isInfixOf "submit" name then Just "submission handler"
  else if isInfixOf "register" name then Just "registration handler"
  else Nothing

findHighImpactTargets : List FuncCoverageInfo -> List HighImpactTarget
findHighImpactTargets funcs =
  let uncovered = filter (\f => f.hitCount == 0) funcs
  in mapMaybe toTarget uncovered
  where
    toTarget : FuncCoverageInfo -> Maybe HighImpactTarget
    toTarget f = case isHighImpactFunction f of
      Just reason => Just $ MkHighImpactTarget f reason
      Nothing => Nothing

updateWithHits : SortedSet Nat -> FuncCoverageInfo -> FuncCoverageInfo
updateWithHits executed f =
  if contains f.funcIdx executed then { hitCount := 1 } f else f

export
calculateCoverage : List FuncCoverageInfo -> SortedSet Nat -> FuncCoverageResult
calculateCoverage funcs executed =
  let idrisFuncs = filter (isJust . idrisName) funcs
      updated = map (updateWithHits executed) idrisFuncs
      totalCount = length updated
      coveredCount = length (filter (\f => f.hitCount > 0) updated)
      pct : Double
      pct = if totalCount == 0 then 100.0 else (cast coveredCount / cast totalCount) * 100.0
      highImpact = findHighImpactTargets updated
  in MkFuncCoverageResult totalCount coveredCount pct updated highImpact

export
runFunctionCoverage : String -> String -> IO (Either String FuncCoverageResult)
runFunctionCoverage wasmPath profilingOutput = do
  Right funcs <- parseWasmFunctions wasmPath
    | Left err => pure $ Left $ "Failed to parse WASM: " ++ err
  let traces = parseProfilingOutput profilingOutput
  let executed = getExecutedFunctions traces
  pure $ Right $ calculateCoverage funcs executed

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
