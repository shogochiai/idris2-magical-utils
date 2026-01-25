||| Canister Call Execution
|||
||| Executes dfx canister call commands and parses results.
||| Used to test canister methods and track coverage.
module DfxCoverage.CanisterCall

import Data.List
import Data.Maybe
import Data.String
import System
import System.File
import System.Clock

import DfxCoverage.CandidParser
import DfxCoverage.IcWasm.Instrumenter as Instr
import WasmBuilder.WasmBuilder as WB

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Result of a canister call
public export
data CallResult
  = CallSuccess String      -- Successful call with result
  | CallError String        -- Call failed with error message
  | CallTimeout             -- Call timed out
  | CallNotFound            -- Method not found

public export
Show CallResult where
  show (CallSuccess s) = "Success: " ++ substr 0 50 s ++ if length s > 50 then "..." else ""
  show (CallError e) = "Error: " ++ e
  show CallTimeout = "Timeout"
  show CallNotFound = "NotFound"

||| Record of a single call attempt
public export
record CallRecord where
  constructor MkCallRecord
  methodName : String
  args : String
  result : CallResult
  durationMs : Nat

public export
Show CallRecord where
  show r = r.methodName ++ "(" ++ r.args ++ ") -> " ++ show r.result ++
           " (" ++ show r.durationMs ++ "ms)"

||| Options for canister calls
public export
record CallOptions where
  constructor MkCallOptions
  canisterId : String
  network : String          -- "local", "ic", etc.
  timeoutMs : Nat           -- Timeout in milliseconds
  dfxPath : String          -- Path to dfx binary (usually "dfx")

public export
defaultCallOptions : CallOptions
defaultCallOptions = MkCallOptions
  { canisterId = ""
  , network = "local"
  , timeoutMs = 30000       -- 30 seconds default
  , dfxPath = "dfx"
  }

-- =============================================================================
-- Command Execution
-- =============================================================================

||| Build dfx canister call command
buildCallCommand : CallOptions -> String -> String -> String
buildCallCommand opts method args =
  opts.dfxPath ++ " canister call " ++ opts.canisterId ++
  " " ++ method ++
  (if null args then "" else " '" ++ args ++ "'") ++
  " --network " ++ opts.network

||| Execute a shell command and capture output
||| Returns (exit code, stdout, stderr)
executeCommand : String -> IO (Int, String, String)
executeCommand cmd = do
  -- Use temporary files to capture output
  let stdoutFile = "/tmp/dfx_stdout_" ++ show !time ++ ".txt"
  let stderrFile = "/tmp/dfx_stderr_" ++ show !time ++ ".txt"
  let fullCmd = cmd ++ " > " ++ stdoutFile ++ " 2> " ++ stderrFile

  exitCode <- system fullCmd

  Right stdout <- readFile stdoutFile
    | Left _ => pure (exitCode, "", "")
  Right stderr <- readFile stderrFile
    | Left _ => pure (exitCode, stdout, "")

  -- Cleanup temp files
  _ <- system $ "rm -f " ++ stdoutFile ++ " " ++ stderrFile

  pure (exitCode, trim stdout, trim stderr)

||| Get current time in nanoseconds
getTimeNs : IO Integer
getTimeNs = do
  t <- clockTime Monotonic
  pure $ nanoseconds t

||| Parse call result from dfx output
parseCallResult : Int -> String -> String -> CallResult
parseCallResult exitCode stdout stderr =
  if exitCode == 0
    then CallSuccess stdout
    else if isInfixOf "method not found" (toLower stderr)
      then CallNotFound
      else CallError stderr

-- =============================================================================
-- Canister Call API
-- =============================================================================

||| Call a canister method with arguments
|||
||| @opts Call options (canisterId, network, etc.)
||| @method Method name to call
||| @args Arguments in Candid text format (e.g., "(42, \"hello\")")
public export
callMethod : CallOptions -> String -> String -> IO CallRecord
callMethod opts method args = do
  let cmd = buildCallCommand opts method args
  startTime <- getTimeNs
  (exitCode, stdout, stderr) <- executeCommand cmd
  endTime <- getTimeNs

  let result = parseCallResult exitCode stdout stderr
  let durationNs = endTime - startTime
  let durationMs = cast {to=Nat} (durationNs `div` 1000000)

  pure $ MkCallRecord method args result durationMs

||| Call a canister method without arguments (query)
public export
callQuery : CallOptions -> String -> IO CallRecord
callQuery opts method = callMethod opts method ""

||| Test multiple methods and return call records
public export
testMethods : CallOptions -> List String -> IO (List CallRecord)
testMethods opts methods = traverse (\m => callQuery opts m) methods

||| Test all methods from a Candid interface
public export
testInterface : CallOptions -> CandidInterface -> IO (List CallRecord)
testInterface opts ci = testMethods opts (getMethodNames ci)

-- =============================================================================
-- Canister Deployment
-- =============================================================================

||| Result of a deploy operation
public export
data DeployResult
  = DeploySuccess String   -- Success with canister ID
  | DeployError String     -- Deploy failed with error
  | DeployAlreadyRunning   -- Canister already deployed

public export
Show DeployResult where
  show (DeploySuccess cid) = "Deployed: " ++ cid
  show (DeployError e) = "Deploy error: " ++ e
  show DeployAlreadyRunning = "Already deployed"

||| Options for deployment
public export
record DeployOptions where
  constructor MkDeployOptions
  canisterName : String
  network : String
  dfxPath : String
  projectDir : String
  forTestBuild : Bool      -- Build with src/Main_test.idr for coverage

public export
defaultDeployOptions : DeployOptions
defaultDeployOptions = MkDeployOptions
  { canisterName = ""
  , network = "local"
  , dfxPath = "dfx"
  , projectDir = "."
  , forTestBuild = False
  }

||| Find test module path by searching for **/Tests/AllTests.idr
||| Returns relative path from project root (e.g., "src/Economics/Tests/AllTests.idr")
export
findTestModulePath : String -> IO (Maybe String)
findTestModulePath projectDir = do
  -- Use find command to search for Tests/AllTests.idr
  let cmd = "find " ++ projectDir ++ "/src -name 'AllTests.idr' -path '*/Tests/*' 2>/dev/null | head -1"
  (exitCode, stdout, _) <- executeCommand cmd
  let result = trim stdout
  if result == ""
     then pure Nothing
     else do
       -- Convert absolute path to relative from project dir
       let prefixLen = cast {to=Int} (length projectDir) + 1  -- +1 for trailing /
       let resultLen = cast {to=Int} (length result)
       let relative = if isPrefixOf projectDir result
                         then strSubstr prefixLen (resultLen - prefixLen) result
                         else result
       pure (Just relative)

||| Check if local replica is running
public export
isReplicaRunning : DeployOptions -> IO Bool
isReplicaRunning opts = do
  let cmd = "cd " ++ opts.projectDir ++ " && " ++
            opts.dfxPath ++ " ping " ++ opts.network ++ " 2>/dev/null"
  exitCode <- system cmd
  pure (exitCode == 0)

||| Start local replica in background
public export
startReplica : DeployOptions -> IO (Either String ())
startReplica opts = do
  let cmd = "cd " ++ opts.projectDir ++ " && " ++
            opts.dfxPath ++ " start --clean --background 2>&1"
  (exitCode, stdout, stderr) <- executeCommand cmd
  if exitCode == 0
    then pure (Right ())
    else pure (Left $ if null stderr then stdout else stderr)

||| Stop local replica
public export
stopReplica : DeployOptions -> IO ()
stopReplica opts = do
  let cmd = "cd " ++ opts.projectDir ++ " && " ++ opts.dfxPath ++ " stop 2>/dev/null"
  _ <- system cmd
  pure ()

||| Install WASM to canister directly (skips dfx build which validates .did)
public export
installWasm : DeployOptions -> String -> IO DeployResult
installWasm opts wasmPath = do
  -- Ensure canister exists (create if needed)
  let createCmd = "cd " ++ opts.projectDir ++ " && " ++
                  opts.dfxPath ++ " canister create " ++ opts.canisterName ++
                  " --network " ++ opts.network ++ " 2>/dev/null || true"
  _ <- system createCmd

  -- Install WASM directly
  let cmd = "cd " ++ opts.projectDir ++ " && " ++
            opts.dfxPath ++ " canister install " ++ opts.canisterName ++
            " --wasm " ++ wasmPath ++
            " --mode reinstall -y" ++
            " --network " ++ opts.network ++ " 2>&1"
  putStrLn $ "    Deploying: " ++ opts.canisterName
  (exitCode, stdout, stderr) <- executeCommand cmd
  if exitCode == 0
    then do
      -- Get canister ID
      let idCmd = "cd " ++ opts.projectDir ++ " && " ++
                  opts.dfxPath ++ " canister id " ++ opts.canisterName ++
                  " --network " ++ opts.network ++ " 2>/dev/null"
      (_, canisterId, _) <- executeCommand idCmd
      pure $ DeploySuccess (trim canisterId)
    else pure $ DeployError (if null stderr then stdout else stderr)

||| Deploy a canister (legacy - uses dfx deploy)
public export
deployCanister : DeployOptions -> IO DeployResult
deployCanister opts = do
  let cmd = "cd " ++ opts.projectDir ++ " && " ++
            opts.dfxPath ++ " deploy " ++ opts.canisterName ++
            " --network " ++ opts.network ++ " 2>&1"
  (exitCode, stdout, stderr) <- executeCommand cmd
  if exitCode == 0
    then do
      -- Get canister ID
      let idCmd = "cd " ++ opts.projectDir ++ " && " ++
                  opts.dfxPath ++ " canister id " ++ opts.canisterName ++
                  " --network " ++ opts.network ++ " 2>/dev/null"
      (_, canisterId, _) <- executeCommand idCmd
      pure $ DeploySuccess (trim canisterId)
    else pure $ DeployError (if null stderr then stdout else stderr)

||| Ensure canister is deployed: build WASM, start replica, deploy
||| Returns canister ID on success
||| If forTestBuild is True, dynamically generates test Main from Tests.AllTests
public export
ensureDeployed : DeployOptions -> IO (Either String String)
ensureDeployed opts = do
  -- Step 1: Build WASM (always rebuild to avoid cache bugs)
  -- forTestBuild generates temp Main.idr in /tmp importing Tests.AllTests (atomic)
  -- First, find test module if forTestBuild is enabled
  testModPath <- if opts.forTestBuild
                 then findTestModulePath opts.projectDir
                 else pure Nothing
  when opts.forTestBuild $ do
    case testModPath of
      Just p  => putStrLn $ "    Found test module: " ++ p
      Nothing => putStrLn "    Warning: No Tests/AllTests.idr found, using default path"
  let buildOpts = { projectDir := opts.projectDir
                  , canisterName := opts.canisterName
                  , forTestBuild := opts.forTestBuild
                  , testModulePath := testModPath } WB.defaultBuildOptions
  when opts.forTestBuild $ putStrLn "    Building with test code (dynamically generated from Tests.AllTests)..."
  buildResult <- WB.buildCanisterAuto buildOpts
  case buildResult of
    WB.BuildError err => pure (Left $ "WASM build failed: " ++ err)
    WB.BuildSuccess wasmPath => do
      putStrLn $ "    WASM built: " ++ wasmPath

      -- Step 2: Instrument WASM with ic-wasm for profiling
      let instrumentedPath = wasmPath ++ ".instrumented"
      putStrLn "    Instrumenting WASM with ic-wasm..."
      instrResult <- Instr.quickInstrument wasmPath instrumentedPath
      deployWasm <- case instrResult of
        Right () => do
          putStrLn $ "    Instrumented: " ++ instrumentedPath
          pure instrumentedPath
        Left err => do
          putStrLn $ "    Warning: ic-wasm instrument failed: " ++ err
          putStrLn "    Deploying non-instrumented WASM (function coverage unavailable)"
          pure wasmPath

      -- Step 3: Start replica if needed (for local network)
      when (opts.network == "local") $ do
        running <- isReplicaRunning opts
        unless running $ do
          putStrLn "    Starting local replica..."
          Right () <- startReplica opts
            | Left err => pure ()  -- Will fail at deploy anyway
          pure ()

      -- Step 4: Install WASM directly (skips dfx build which validates .did)
      result <- installWasm opts deployWasm
      case result of
        DeploySuccess cid => pure (Right cid)
        DeployError err => pure (Left $ "Deploying: " ++ opts.canisterName ++ "\n" ++ err)
        DeployAlreadyRunning => do
          -- Get existing canister ID
          let idCmd = "cd " ++ opts.projectDir ++ " && " ++
                      opts.dfxPath ++ " canister id " ++ opts.canisterName ++
                      " --network " ++ opts.network ++ " 2>/dev/null"
          (_, canisterId, _) <- executeCommand idCmd
          pure (Right (trim canisterId))

-- =============================================================================
-- Result Analysis
-- =============================================================================

||| Check if a call was successful
public export
isSuccess : CallRecord -> Bool
isSuccess r = case r.result of
  CallSuccess _ => True
  _ => False

||| Get successful calls from a list
public export
getSuccessfulCalls : List CallRecord -> List CallRecord
getSuccessfulCalls = filter isSuccess

||| Get failed calls from a list
public export
getFailedCalls : List CallRecord -> List CallRecord
getFailedCalls = filter (not . isSuccess)

||| Calculate success rate
public export
successRate : List CallRecord -> Double
successRate [] = 0.0
successRate rs =
  let totalCount = cast {to=Double} (length rs)
      successCount = cast {to=Double} (length (getSuccessfulCalls rs))
  in (successCount / totalCount) * 100.0
