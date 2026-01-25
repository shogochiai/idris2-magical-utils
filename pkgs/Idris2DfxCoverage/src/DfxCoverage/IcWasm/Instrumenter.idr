||| ic-wasm Instrumenter
|||
||| Instruments WASM files using ic-wasm for execution tracing.
||| The instrumented WASM will emit traces to stable memory or
||| provide __get_profiling query method.
module DfxCoverage.IcWasm.Instrumenter

import Data.List
import Data.String
import System
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Options for ic-wasm instrument command
public export
record InstrumentOptions where
  constructor MkInstrumentOptions
  inputWasm : String           -- Input WASM file path
  outputWasm : String          -- Output WASM file path
  traceOnly : List String      -- Only trace these functions (empty = all)
  startPage : Maybe Nat        -- Stable memory start page
  pageLimit : Maybe Nat        -- Max pages for trace storage

||| Default options
export
defaultInstrumentOptions : String -> String -> InstrumentOptions
defaultInstrumentOptions input output = MkInstrumentOptions
  { inputWasm = input
  , outputWasm = output
  , traceOnly = []
  , startPage = Nothing
  , pageLimit = Nothing
  }

-- =============================================================================
-- Command Building
-- =============================================================================

||| Build the ic-wasm instrument command
buildCommand : InstrumentOptions -> String
buildCommand opts =
  let base = "ic-wasm " ++ opts.inputWasm ++ " -o " ++ opts.outputWasm ++ " instrument"
      traceFlags = concatMap (\f => " --trace-only " ++ f) opts.traceOnly
      startFlag = maybe "" (\p => " --start-page " ++ show p) opts.startPage
      limitFlag = maybe "" (\p => " --page-limit " ++ show p) opts.pageLimit
  in base ++ traceFlags ++ startFlag ++ limitFlag

-- =============================================================================
-- Instrument Execution
-- =============================================================================

||| Instrument a WASM file
export
instrumentWasm : InstrumentOptions -> IO (Either String ())
instrumentWasm opts = do
  let cmd = buildCommand opts
  exitCode <- system $ cmd ++ " 2>&1"
  if exitCode == 0
    then pure $ Right ()
    else pure $ Left $ "ic-wasm instrument failed with exit code " ++ show exitCode

||| Quick instrument with defaults for idris2-wasm canisters
||| Uses start page 10 to avoid conflict with canister stable memory (pages 0-9)
||| idris2-wasm generated canisters pre-allocate 26 pages for profiling support
export
quickInstrument : String -> String -> IO (Either String ())
quickInstrument input output =
  let opts = MkInstrumentOptions
        { inputWasm = input
        , outputWasm = output
        , traceOnly = []
        , startPage = Just 10     -- Pages 0-9 reserved for canister data
        , pageLimit = Just 16     -- Use pages 10-25 for profiling
        }
  in instrumentWasm opts

-- =============================================================================
-- WASM Info
-- =============================================================================

||| Get function count from WASM using ic-wasm info
export
getWasmFunctionCount : String -> IO (Either String Nat)
getWasmFunctionCount wasmPath = do
  let tmpFile = "/tmp/ic_wasm_info.txt"
  let cmd = "ic-wasm " ++ wasmPath ++ " info > " ++ tmpFile ++ " 2>&1"
  exitCode <- system cmd
  if exitCode == 0
    then do
      Right content <- readFile tmpFile
        | Left err => pure $ Left $ "Failed to read info: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      pure $ Right $ parseFunctionCount content
    else pure $ Left "ic-wasm info failed"
  where
    parseFunctionCount : String -> Nat
    parseFunctionCount content =
      let ls = lines content
          funcLine = find (isInfixOf "Number of functions:") ls
      in case funcLine of
           Nothing => 0
           Just line =>
             let parts = words line
             in case last' parts of
                  Nothing => 0
                  Just numStr => maybe 0 cast (parsePositive numStr)

-- Helper for parsing exported methods
extractMethodName : String -> Maybe String
extractMethodName line =
  -- Format: "    canister_query methodName (func_N)"
  let parts = words line
  in case parts of
       (_ :: name :: _) => Just name
       _ => Nothing

parseExportedMethods : String -> List String
parseExportedMethods content =
  let ls = lines content
      -- Find lines with "canister_query" or "canister_update"
      methodLines = filter (\l => isInfixOf "canister_query" l ||
                                   isInfixOf "canister_update" l) ls
  in mapMaybe extractMethodName methodLines

||| Get exported methods from WASM
export
getExportedMethods : String -> IO (Either String (List String))
getExportedMethods wasmPath = do
  let tmpFile = "/tmp/ic_wasm_info.txt"
  let cmd = "ic-wasm " ++ wasmPath ++ " info > " ++ tmpFile ++ " 2>&1"
  exitCode <- system cmd
  if exitCode == 0
    then do
      Right content <- readFile tmpFile
        | Left err => pure $ Left $ "Failed to read info: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      pure $ Right $ parseExportedMethods content
    else pure $ Left "ic-wasm info failed"

-- =============================================================================
-- Full Pipeline
-- =============================================================================

||| Full instrumentation pipeline result
public export
record InstrumentResult where
  constructor MkInstrumentResult
  instrumentedPath : String
  functionCount : Nat
  exportedMethods : List String

||| Run full instrumentation pipeline
export
runInstrumentPipeline : String -> String -> IO (Either String InstrumentResult)
runInstrumentPipeline inputWasm outputWasm = do
  -- Get info before instrumenting
  Right funcCount <- getWasmFunctionCount inputWasm
    | Left err => pure $ Left $ "Failed to get function count: " ++ err

  Right methods <- getExportedMethods inputWasm
    | Left err => pure $ Left $ "Failed to get methods: " ++ err

  -- Instrument
  Right () <- quickInstrument inputWasm outputWasm
    | Left err => pure $ Left err

  pure $ Right $ MkInstrumentResult outputWasm funcCount methods
