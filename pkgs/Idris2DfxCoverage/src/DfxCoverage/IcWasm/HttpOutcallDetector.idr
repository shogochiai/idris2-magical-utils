||| HTTP Outcall Detector
|||
||| Detects HTTP Outcall dependencies by analyzing WASM imports and exports.
||| HTTP Outcalls in ICP use ic0 call_* functions to call the management canister.
|||
||| Detection criteria:
||| - Imports: call_new, call_data_append, call_cycles_add128, call_perform
||| - Exports: http_reply_callback, http_reject_callback (optional but indicative)
module DfxCoverage.IcWasm.HttpOutcallDetector

import Data.List
import Data.String
import System
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| HTTP Outcall detection result
public export
record HttpOutcallInfo where
  constructor MkHttpOutcallInfo
  hasCallNew         : Bool  -- call_new import
  hasCallDataAppend  : Bool  -- call_data_append import
  hasCyclesAdd       : Bool  -- call_cycles_add128 import
  hasCallPerform     : Bool  -- call_perform import
  hasReplyCallback   : Bool  -- http_reply_callback export
  hasRejectCallback  : Bool  -- http_reject_callback export
  allIc0Imports      : List String  -- All IC0 imports

||| Check if canister can make HTTP Outcalls (has all required imports)
public export
canMakeHttpOutcalls : HttpOutcallInfo -> Bool
canMakeHttpOutcalls info =
  info.hasCallNew && info.hasCallDataAppend &&
  info.hasCyclesAdd && info.hasCallPerform

||| Check if canister has HTTP callback handlers
public export
hasHttpCallbacks : HttpOutcallInfo -> Bool
hasHttpCallbacks info =
  info.hasReplyCallback || info.hasRejectCallback

public export
Show HttpOutcallInfo where
  show info =
    let capable = if canMakeHttpOutcalls info then "Yes" else "No"
        callbacks = if hasHttpCallbacks info then "Yes" else "No"
    in "HttpOutcall capable: " ++ capable ++ ", Callbacks: " ++ callbacks

-- =============================================================================
-- Parsing ic-wasm info output
-- =============================================================================

||| IC0 call-related imports for HTTP Outcall
httpOutcallImports : List String
httpOutcallImports = ["call_new", "call_data_append", "call_cycles_add128", "call_perform"]

||| HTTP callback export names
httpCallbackExports : List String
httpCallbackExports = ["http_reply_callback", "http_reject_callback"]

||| Extract quoted strings from a line
extractQuotedStrings : String -> List String
extractQuotedStrings line =
  let chars = unpack line
  in extractQuoted chars False []
  where
    extractQuoted : List Char -> Bool -> List Char -> List String
    extractQuoted [] _ acc = if null acc then [] else [pack (reverse acc)]
    extractQuoted ('"' :: rest) False acc = extractQuoted rest True []
    extractQuoted ('"' :: rest) True acc = pack (reverse acc) :: extractQuoted rest False []
    extractQuoted (c :: rest) True acc = extractQuoted rest True (c :: acc)
    extractQuoted (_ :: rest) False acc = extractQuoted rest False acc

||| Parse IC0 imports from ic-wasm info output
||| Expected format: Imported IC0 System API: [ "import1", "import2", ... ]
parseIc0Imports : String -> List String
parseIc0Imports content =
  let ls = lines content
      importLine = find (isInfixOf "Imported IC0 System API:") ls
  in case importLine of
       Nothing => []
       Just line => extractQuotedStrings line

||| Check if any export line contains a callback name
hasExportCallback : String -> String -> Bool
hasExportCallback content callbackName =
  any (isInfixOf callbackName) (lines content)

||| Parse full ic-wasm info output to extract HTTP Outcall info
export
parseHttpOutcallInfo : String -> HttpOutcallInfo
parseHttpOutcallInfo content =
  let imports = parseIc0Imports content
  in MkHttpOutcallInfo
       (elem "call_new" imports)
       (elem "call_data_append" imports)
       (elem "call_cycles_add128" imports)
       (elem "call_perform" imports)
       (hasExportCallback content "http_reply_callback")
       (hasExportCallback content "http_reject_callback")
       imports

-- =============================================================================
-- Detection via ic-wasm
-- =============================================================================

||| Get HTTP Outcall info from WASM file using ic-wasm
export
detectHttpOutcalls : String -> IO (Either String HttpOutcallInfo)
detectHttpOutcalls wasmPath = do
  let tmpFile = "/tmp/ic_wasm_http_info.txt"
  let cmd = "ic-wasm " ++ wasmPath ++ " info > " ++ tmpFile ++ " 2>&1"
  exitCode <- system cmd
  if exitCode == 0
    then do
      Right content <- readFile tmpFile
        | Left err => pure $ Left $ "Failed to read info: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      pure $ Right $ parseHttpOutcallInfo content
    else pure $ Left "ic-wasm info failed"

-- =============================================================================
-- Report Generation
-- =============================================================================

||| Dependency status
public export
data DependencyStatus = Required | Optional | NotUsed

public export
Show DependencyStatus where
  show Required = "[REQUIRED]"
  show Optional = "[OPTIONAL]"
  show NotUsed  = "[NOT USED]"

||| HTTP Outcall dependency report entry
public export
record HttpOutcallDependency where
  constructor MkHttpOutcallDependency
  name        : String
  description : String
  status      : DependencyStatus

||| Generate dependency report from HttpOutcallInfo
export
generateDependencyReport : HttpOutcallInfo -> List HttpOutcallDependency
generateDependencyReport info =
  let canOutcall = canMakeHttpOutcalls info
      hasCallbacks = hasHttpCallbacks info
  in
    [ MkHttpOutcallDependency
        "IC Management Canister"
        "Required for HTTP Outcalls (call to aaaaa-aa)"
        (if canOutcall then Required else NotUsed)
    , MkHttpOutcallDependency
        "Cycles Payment"
        "HTTP Outcalls require cycle payment"
        (if info.hasCyclesAdd then Required else NotUsed)
    , MkHttpOutcallDependency
        "Async Callbacks"
        "HTTP response handling via callbacks"
        (if hasCallbacks then Required else
         if canOutcall then Optional else NotUsed)
    ]

||| Format dependency report as string
export
formatDependencyReport : List HttpOutcallDependency -> String
formatDependencyReport deps =
  unlines $ map formatDep deps
  where
    formatDep : HttpOutcallDependency -> String
    formatDep d = "  " ++ show d.status ++ " " ++ d.name ++ ": " ++ d.description

-- =============================================================================
-- External Network Dependencies
-- =============================================================================

||| External network dependency type
public export
data ExternalDependency
  = HttpsEndpoint String     -- HTTPS endpoint URL pattern
  | JsonRpcEndpoint String   -- JSON-RPC endpoint (EVM chains)
  | ManagementCanister       -- IC Management Canister for HTTP
  | UnknownExternal String   -- Unknown external dependency

public export
Show ExternalDependency where
  show (HttpsEndpoint url)    = "HTTPS: " ++ url
  show (JsonRpcEndpoint url)  = "JSON-RPC: " ++ url
  show ManagementCanister     = "IC Management Canister (aaaaa-aa)"
  show (UnknownExternal desc) = "Unknown: " ++ desc

||| Detect external dependencies from HTTP Outcall capability
export
detectExternalDeps : HttpOutcallInfo -> List ExternalDependency
detectExternalDeps info =
  if canMakeHttpOutcalls info
    then [ManagementCanister]
    else []
