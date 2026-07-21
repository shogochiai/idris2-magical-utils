||| Canister Call Execution
|||
||| Executes dfx canister call commands and parses results.
||| Used to test canister methods and track coverage.
module DfxCoverage.CanisterCall

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Clock
import System.Directory

import DfxCoverage.CandidParser
import DfxCoverage.IcWasm.Instrumenter as Instr

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
  instrumentBranchProbes : Bool
  instrumentPathHits : Bool  -- Compiler-injected canonical path-id hits (--path-hits)
  forTestBuild : Bool      -- Build with src/Main_test.idr for coverage

public export
defaultDeployOptions : DeployOptions
defaultDeployOptions = MkDeployOptions
  { canisterName = ""
  , network = "local"
  , dfxPath = "dfx"
  , projectDir = "."
  , instrumentBranchProbes = False
  , instrumentPathHits = False
  , forTestBuild = False
  }

extractIpkgField : String -> String -> Maybe String
extractIpkgField field content =
  case find (isPrefixOf (field ++ " =")) (map trim (lines content)) of
    Just line =>
      let raw = trim (pack (drop (length (field ++ " =")) (unpack line)))
      in Just $
           if isPrefixOf "\"" raw && isSuffixOf "\"" raw && length raw >= 2
              then strSubstr 1 (cast {to=Int} (length raw) - 2) raw
              else raw
    Nothing => Nothing

moduleToPath : String -> String
moduleToPath modName =
  pack (map (\c => if c == '.' then '/' else c) (unpack modName)) ++ ".idr"

resolveMainModulePath : String -> IO String
resolveMainModulePath projectDir = do
  let stableIpkgFilter = " ! -name 'temp*.ipkg' ! -name 'dumpcases-temp-*.ipkg' ! -name 'dfx-dumppaths-temp-*.ipkg' ! -name '*-temp-*.ipkg'"
  let cmd = "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f" ++ stableIpkgFilter ++ " | sort | head -1"
  (_, ipkgPath, _) <- executeCommand cmd
  if null (trim ipkgPath)
     then pure "src/Main.idr"
     else do
       Right ipkgContent <- readFile (trim ipkgPath)
         | Left _ => pure "src/Main.idr"
       let sourcedir = fromMaybe "src" (extractIpkgField "sourcedir" ipkgContent)
       let mainModule = fromMaybe "Main" (extractIpkgField "main" ipkgContent)
       pure $ sourcedir ++ "/" ++ moduleToPath mainModule

probeIc0SupportDir : String -> IO (Maybe String)
probeIc0SupportDir projectDir = do
  mHome <- getEnv "HOME"
  let homeVal = fromMaybe "" mHome
  go (candidates homeVal)
  where
    candidates : String -> List String
    candidates homeVal =
      [ projectDir ++ "/lib/ic0"
      -- HOME-anchored conventional checkout: resolves from ANY projectDir
      -- (soundness probe dirs, worktrees) where the relative candidates miss.
      , homeVal ++ "/code/idris2-magical-utils/pkgs/Idris2IcWasm/support/ic0"
      , projectDir ++ "/../idris2-icwasm/support/ic0"
      , projectDir ++ "/../Idris2IcWasm/support/ic0"
      , projectDir ++ "/../../Idris2IcWasm/support/ic0"
      , projectDir ++ "/../../../Idris2IcWasm/support/ic0"
      ]

    go : List String -> IO (Maybe String)
    go [] = pure Nothing
    go (dir :: rest) = do
      Right _ <- readFile (dir ++ "/canister_entry.c")
        | Left _ => go rest
      pure (Just dir)

ensureProjectIc0Support : String -> IO ()
ensureProjectIc0Support projectDir = do
  let projectIc0 = projectDir ++ "/lib/ic0"
  Right _ <- readFile (projectIc0 ++ "/canister_entry.c")
    | Left _ => do
        mSupportDir <- probeIc0SupportDir projectDir
        case mSupportDir of
          Nothing => pure ()
          Just supportDir =>
            if supportDir == projectIc0
               then pure ()
               else do
                 _ <- system $ "mkdir -p " ++ projectDir ++ "/lib"
                 _ <- system $ "rm -rf " ++ projectIc0
                 _ <- system $ "ln -s " ++ supportDir ++ " " ++ projectIc0
                 pure ()
  pure ()

||| Ensure the canister's custom (non-stdlib) deps are pack-installed so the
||| numerator WASM build's `idris2 -p <dep>` resolves them. The build uses the
||| pack-wrapper idris2 (IDRIS2_BIN/IDRIS2_PACKAGE_PATH unset → wrapper runs
||| `pack package-path`), and `pack package-path` only lists packages that have
||| been `pack install`ed — local custom packages (e.g. icp-indexer, idris2-cdk)
||| are otherwise absent and the build fails with "Can't find package <dep>".
||| Installing them all in ONE `pack install` call (idempotent, env -u so the
||| wrapper toolchain is used) keeps the package-path stable for the build.
installCanisterDepsViaPack : (instrumentPathHits : Bool) -> String -> IO ()
installCanisterDepsViaPack instrumentPathHits projectDir = do
  -- Collect `depends` lists from the project's stable ipkgs (skip generated temps).
  let stableIpkgFilter = " ! -name 'temp*.ipkg' ! -name 'dumpcases-temp-*.ipkg' ! -name 'dfx-dumppaths-temp-*.ipkg' ! -name '*-temp-*.ipkg' ! -name 'canister-dfxprepared-*.ipkg'"
  (_, ipkgList, _) <- executeCommand $
    "find " ++ projectDir ++ " -maxdepth 1 -name '*.ipkg' -type f" ++ stableIpkgFilter
  let ipkgs = filter (not . null) (map trim (lines ipkgList))
  rawDeps <- traverse readDependsLine ipkgs
  let deps = nub (filter isCustomDep (concat rawDeps))
  unless (null deps) $
    if instrumentPathHits
       then do
         -- Path-hits build uses the FORKED compiler for codegen (--dumppathshits),
         -- so it must resolve FORK-built dep TTCs. Do NOT evict them; instead
         -- (re)install each dep with the fork into ~/.idris2 so the fork codegen
         -- resolves them. (pack-built TTCs are released-compiler and would be
         -- rejected by the fork as "older compiler version".)
         putStrLn $ "    [dep-install] fork-installing deps for path-hits build: " ++ show deps
         let forkBin = "/Users/bob/code/idrislang-idris2/build/exec/idris2"
         let installLog = "/tmp/dfxcov-dep-install.log"
         rc <- system $ "cd \"" ++ projectDir ++ "\""
                     ++ " && IDRIS2_BIN=" ++ forkBin
                     ++ " env -u IDRIS2_PACKAGE_PATH pack install "
                     ++ unwords deps ++ " > " ++ installLog ++ " 2>&1"
         putStrLn $ "    [dep-install] fork pack install exit=" ++ show rc ++ " (log: " ++ installLog ++ ")"
         pure ()
    else do
    -- THE FIX for denominator→numerator pollution (2026-06-22, verified):
    -- The denominator (chunked dumppaths) step's installNeededDepsIntoFork runs
    -- `idris2 --install` with the FORKED compiler, which writes FORK-built TTCs to
    -- the SHARED ~/.idris2/idris2-0.8.0/<dep>-*. idris2's default lib dir (~/.idris2)
    -- then SHADOWS the pack store, so the numerator pack-wrapper build resolves the
    -- fork-built TTCs → "installed with an older compiler version".
    --   FIX: remove the fork-polluted ~/.idris2/<dep>-* dirs before the build. The
    -- pack STORE still holds the correct pack-built copies, and the wrapper's
    -- IDRIS2_PACKAGE_PATH (= `pack package-path`) points there — so with ~/.idris2
    -- clean, the build resolves the pack-built deps. (pack install does NOT
    -- repopulate ~/.idris2 and is a no-op once its store DB says installed, so the
    -- rm is the operative step; the pack install below only matters on a clean tree
    -- where the store itself lacks a dep.) Verified: fork-pollute ~/.idris2 then rm
    -- → WASM build reaches "Build complete".
    putStrLn $ "    [dep-install] evicting fork-built deps from ~/.idris2 + pack install: " ++ show deps
    _ <- system $ "rm -rf "
                ++ unwords (map (\d => "\"$HOME/.idris2/idris2-0.8.0/" ++ d ++ "-\"*") deps)
    let installLog = "/tmp/dfxcov-dep-install.log"
    rc <- system $ "cd \"" ++ projectDir ++ "\""
                ++ " && env -u IDRIS2_BIN -u IDRIS2_PACKAGE_PATH pack install "
                ++ unwords deps ++ " > " ++ installLog ++ " 2>&1"
    putStrLn $ "    [dep-install] pack install exit=" ++ show rc ++ " (log: " ++ installLog ++ ")"
    pure ()
  where
    -- stdlib / always-available packages need no install
    isCustomDep : String -> Bool
    isCustomDep d = not (elem d ["base", "contrib", "prelude", "network", "linear", "test"])
    readDependsLine : String -> IO (List String)
    readDependsLine path = do
      Right content <- readFile path
        | Left _ => pure []
      -- `depends` can span MULTIPLE lines (ipkg continuation style):
      --   depends = base, idris2-test-suite
      --           , idris2-cdk
      --           , icp-indexer
      -- Collect the `depends =` line AND its leading-comma continuation lines.
      let depBlock = collectDependsBlock (lines content) False
      pure $ concatMap parseDeps depBlock
      where
        parseDeps : String -> List String
        parseDeps l =
          let afterEq = case break (== '=') (unpack l) of
                          (_, rest) => pack (drop 1 rest)  -- strip up to and incl '='
              -- continuation lines have no '=', so afterEq drops nothing → keep whole
              src = if isInfixOf "=" l then afterEq else l
              parts = map trim (forget (split (== ',') src))
          in filter (\p => not (null p)) parts
        -- Gather the depends line and subsequent continuation lines (start with ',').
        collectDependsBlock : List String -> Bool -> List String
        collectDependsBlock [] _ = []
        collectDependsBlock (l :: ls) inBlock =
          let t = trim l in
          if not inBlock
            then if isInfixOf "depends" l && isInfixOf "=" l
                   then l :: collectDependsBlock ls True
                   else collectDependsBlock ls False
            else if isPrefixOf "," t
                   then l :: collectDependsBlock ls True
                   else collectDependsBlock ls False  -- block ended; keep scanning for other ipkg sections

probeExecutable : List String -> IO (Maybe String)
probeExecutable [] = pure Nothing
probeExecutable (candidate :: rest) =
  if null candidate
     then probeExecutable rest
     else do
       exitCode <- system ("test -x " ++ candidate ++ " >/dev/null 2>&1")
       if exitCode == 0 then pure (Just candidate) else probeExecutable rest

resolveIcWasmExecutable : String -> IO (Maybe String)
resolveIcWasmExecutable projectDir = do
  mEnv <- getEnv "IDRIS2_ICWASM_BIN"
  cwd <- currentDir
  mHome <- getEnv "HOME"
  let envCandidate = fromMaybe "" mEnv
  let cwdVal = fromMaybe "." cwd
  let homeVal = fromMaybe "" mHome
  let candidates =
        [ envCandidate
        -- HOME-anchored conventional checkout: works from ANY cwd/projectDir
        -- (a soundness probe dir, a worktree, /tmp) — the relative candidates
        -- below only resolve when projectDir sits inside the monorepo layout,
        -- and the ~/.local/bin pack WRAPPER resolves per-cwd and dies outside
        -- a pack.toml context (the fixture calibration hit exactly that).
        , homeVal ++ "/code/idris2-magical-utils/pkgs/Idris2IcWasm/build/exec/idris2-icwasm"
        -- Prefer the freshly-built local idris2-icwasm (with current fixes) over a
        -- stale `~/.local/bin/idris2-icwasm` (pack install-app does not refresh the
        -- ~/.local/bin copy, so it can lag behind source). The canonical monorepo
        -- layout is <root>/idris2-magical-utils/pkgs/Idris2IcWasm and a sibling
        -- consumer like <root>/etherclaw/pkgs/<Canister>, so from projectDir the
        -- icwasm build/exec is ../../../idris2-magical-utils/pkgs/Idris2IcWasm/...
        , projectDir ++ "/../../../idris2-magical-utils/pkgs/Idris2IcWasm/build/exec/idris2-icwasm"
        , projectDir ++ "/../../../../idris2-magical-utils/pkgs/Idris2IcWasm/build/exec/idris2-icwasm"
        , projectDir ++ "/../../idris2-magical-utils/pkgs/Idris2IcWasm/build/exec/idris2-icwasm"
        , projectDir ++ "/../Idris2IcWasm/build/exec/idris2-icwasm"
        , projectDir ++ "/../../Idris2IcWasm/build/exec/idris2-icwasm"
        , projectDir ++ "/../../../Idris2IcWasm/build/exec/idris2-icwasm"
        , cwdVal ++ "/pkgs/Idris2IcWasm/build/exec/idris2-icwasm"
        , cwdVal ++ "/Idris2IcWasm/build/exec/idris2-icwasm"
        ]
  found <- probeExecutable candidates
  case found of
    Just path => pure (Just path)
    Nothing => do
      let lookupCmd = "command -v idris2-icwasm 2>/dev/null"
      (_, stdout, _) <- executeCommand lookupCmd
      let resolved = trim stdout
      pure (if null resolved then Nothing else Just resolved)

shellQuote : String -> String
shellQuote s = "\"" ++ s ++ "\""

hasGmpHeader : String -> IO Bool
hasGmpHeader dir = do
  Right _ <- readFile (dir ++ "/gmp.h")
    | Left _ => pure False
  pure True

firstExistingGmpInclude : List String -> IO (Maybe String)
firstExistingGmpInclude [] = pure Nothing
firstExistingGmpInclude (dir :: rest) =
  if null dir
     then firstExistingGmpInclude rest
     else do
       found <- hasGmpHeader dir
       if found
          then pure (Just dir)
          else firstExistingGmpInclude rest

resolveGmpIncludePath : String -> IO (Maybe String)
resolveGmpIncludePath projectDir = do
  mMiniGmp <- getEnv "MINI_GMP"
  pure !(firstExistingGmpInclude
    (catMaybes
      [ mMiniGmp
      , Just "/tmp/mini-gmp"
      , Just (projectDir ++ "/.ci-cache/mini-gmp")
      , Just (projectDir ++ "/../.ci-cache/mini-gmp")
      , Just (projectDir ++ "/../../.ci-cache/mini-gmp")
      , Just "/opt/homebrew/opt/gmp/include"
      , Just "/usr/local/opt/gmp/include"
      ]))

findPackInstallBase : IO (Maybe String)
findPackInstallBase = do
  let tmpFile = "/tmp/pack-install-base-dfx-canister.txt"
  _ <- system $ "ls -td ~/.local/state/pack/install/*/ 2>/dev/null | head -1 > " ++ tmpFile
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path then pure Nothing else pure (Just path)

discoverPackagePath : IO String
discoverPackagePath = do
  mBase <- findPackInstallBase
  case mBase of
    Nothing => pure ""
    Just basePath => do
      let tmpFile = "/tmp/idris2-dfx-canister-package-paths.txt"
      let cmd = "find " ++ basePath ++ " -type d -name 'idris2-*' 2>/dev/null > " ++ tmpFile
      _ <- system cmd
      Right content <- readFile tmpFile
        | Left _ => pure ""
      pure $ joinBy ":" (filter (not . null) (map trim (lines content)))

buildWasmViaIcWasmCli : DeployOptions -> String -> Maybe String -> IO (Either String String)
buildWasmViaIcWasmCli opts mainModulePath testModPath = do
  mIcWasm <- resolveIcWasmExecutable opts.projectDir
  case mIcWasm of
    Nothing => pure (Left "Unable to locate idris2-icwasm executable")
    Just icwasmBin => do
      -- NOTE: we intentionally no longer reconstruct IDRIS2_PACKAGE_PATH (the old
      -- mPackagePath/discoverPackagePath path). The pack wrapper `idris2` computes
      -- the correct package path via `pack package-path`; pre-setting it broke
      -- `-p idris2-icwasm` resolution. See the env block below.
      mCPath <- getEnv "CPATH"
      mGmpInclude <- resolveGmpIncludePath opts.projectDir
      let cpathValue =
            case mGmpInclude of
              Nothing => mCPath
              Just inc => Just $ case mCPath of
                                   Nothing => inc
                                   Just old => inc ++ ":" ++ old
      -- The numerator WASM build must use the PACK-MANAGED idris2, NOT the forked
      -- compiler that the coverage run sets via IDRIS2_BIN for the dumppaths
      -- denominator (the fork's package path lacks the idris2-icwasm library).
      --
      -- ROOT CAUSE + FIX (2026-06-22): `~/.local/bin/idris2` is a PACK WRAPPER that
      -- does `export IDRIS2_PACKAGE_PATH="$(pack package-path)"` — i.e. it computes
      -- the CORRECT per-dep versioned package path itself. The old code instead
      -- PRE-SET IDRIS2_PACKAGE_PATH to a reconstructed value (discoverPackagePath)
      -- that does NOT match pack's structure, so `-p idris2-icwasm` failed with
      -- "Package idris2-icwasm is not built or installed". The fix is to UNSET both
      -- IDRIS2_BIN (so idris2-icwasm's resolveIdris2Bin falls back to the wrapper
      -- `idris2`) and IDRIS2_PACKAGE_PATH (so the wrapper runs `pack package-path`).
      -- CRITICAL: must UNSET via `env -u`, not set EMPTY (`VAR=`): empty
      -- IDRIS2_PACKAGE_PATH is honoured as "no search path" and still breaks
      -- `-p idris2-icwasm`, whereas `env -u IDRIS2_PACKAGE_PATH` lets the wrapper
      -- recompute it. Verified: `env -u IDRIS2_PACKAGE_PATH IDRIS2_BIN= idris2 --check
      -- -p idris2-icwasm` resolves; `IDRIS2_PACKAGE_PATH= ...` does NOT. CPATH (GMP
      -- include) is still threaded as a normal assignment.
      -- Path-coverage instrumentation (--path-hits) REQUIRES the forked compiler:
      -- the pack-managed released idris2 silently ignores --dumppathshits (it lacks
      -- the CompileExpr pass), so the WASM gets the __get_path_hits query export but
      -- NO idris2_recordPathHit calls → 0 recorded hits. Use the fork for codegen,
      -- and give it the pack-computed IDRIS2_PACKAGE_PATH so `-p idris2-icwasm` still
      -- resolves (the fork's own package path lacks it). For non-instrumented builds
      -- keep the original pack-wrapper path (env -u both).
      forkBin <- getEnv "IDRIS2_PATHCOV_FORK_BIN"
      let forkPath = fromMaybe "/Users/bob/code/idrislang-idris2/build/exec/idris2"
                              (map trim forkBin)
      let unsetPrefix =
            if opts.instrumentPathHits
               then [ "env"
                    , "IDRIS2_BIN=" ++ shellQuote forkPath
                    , "IDRIS2_PACKAGE_PATH=\"$(pack package-path)\"" ]
               else ["env", "-u", "IDRIS2_BIN", "-u", "IDRIS2_PACKAGE_PATH"]
      let cpathAssign = maybe [] (\v => ["CPATH=" ++ shellQuote v]) cpathValue
      -- ISOLATE the numerator build from the denominator's ~/.idris2 pollution.
      -- The denominator (chunked dumppaths) step's installNeededDepsIntoFork builds
      -- the canister deps (idris2-icwasm, ...) into the SHARED ~/.idris2/idris2-0.8.0
      -- against the FORKED compiler; the pack-wrapper numerator build would then
      -- resolve those fork-built TTCs → "installed with an older compiler version".
      -- The verified fix is the eviction of those fork-built ~/.idris2 dep dirs in
      -- installCanisterDepsViaPack (above, run at ensureDeployed start): with
      -- ~/.idris2 cleaned, the default-prefix build falls through to the pack store
      -- (via the wrapper's IDRIS2_PACKAGE_PATH) and resolves the pack-built deps.
      -- NOTE: do NOT override IDRIS2_PREFIX to a clean temp dir here — that points
      -- idris2 at an EMPTY lib dir and breaks resolution (the store is reached via
      -- the lib path rooted at the real prefix); eviction + default prefix is what
      -- the isolation test verified ("Build complete").
      let baseArgs =
            [ "build"
            , "--project=" ++ opts.projectDir
            , "--canister=" ++ opts.canisterName
            , "--main=" ++ mainModulePath
            ]
      let probeArgs =
            if opts.instrumentBranchProbes then ["--branch-probes"] else []
      let pathHitsArgs =
            if opts.instrumentPathHits then ["--path-hits"] else []
      let testArgs =
            if opts.forTestBuild
               then "--for-test-build" ::
                    maybe [] (\path => ["--test-module=" ++ path]) testModPath
               else []
      let cmd = unwords (unsetPrefix ++ cpathAssign ++ (icwasmBin :: baseArgs ++ probeArgs ++ pathHitsArgs ++ testArgs))
      putStrLn $ "    [numerator] WASM build via " ++ icwasmBin
      (exitCode, stdout, stderr) <- executeCommand cmd
      if exitCode /= 0
         then pure (Left (if null stderr then stdout else stderr))
         else do
           let wasmPath = opts.projectDir ++ "/build/" ++ opts.canisterName ++ "_stubbed.wasm"
           Right _ <- readFile wasmPath
             | Left _ => pure (Left ("idris2-icwasm reported success but no wasm found at " ++ wasmPath))
           pure (Right wasmPath)

||| Find test module path by searching for **/Tests/AllTests.idr
||| Returns relative path from project root (e.g., "src/Economics/Tests/AllTests.idr")
export
findTestModulePath : String -> IO (Maybe String)
findTestModulePath projectDir = do
  -- Prefer src/Tests/AllTests.idr (root-level); fall back to find in subdirectories
  let cmd = "if [ -f " ++ projectDir ++ "/src/Tests/AllTests.idr ]; then echo " ++ projectDir ++ "/src/Tests/AllTests.idr; else find " ++ projectDir ++ "/src -name 'AllTests.idr' -path '*/Tests/*' 2>/dev/null | head -1; fi"
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

||| Strip projectDir prefix from a path so it becomes relative to projectDir
stripPrefix : String -> String -> String
stripPrefix dir path =
  let pfx = dir ++ "/"
  in if isPrefixOf pfx path
       then substr (length pfx) (length path) path
       else path

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
  -- wasmPath is relative to invocation cwd (e.g. "pkgs/X/build/foo.wasm")
  -- but we cd into projectDir, so strip that prefix to avoid double-nesting
  let localWasm = stripPrefix opts.projectDir wasmPath
      cmd = "cd " ++ opts.projectDir ++ " && " ++
            opts.dfxPath ++ " canister install " ++ opts.canisterName ++
            " --wasm " ++ localWasm ++
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
  ensureProjectIc0Support opts.projectDir
  installCanisterDepsViaPack opts.instrumentPathHits opts.projectDir
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
  mainModulePath <- resolveMainModulePath opts.projectDir
  when opts.forTestBuild $ putStrLn "    Building with test code (dynamically generated from Tests.AllTests)..."
  buildResult <- buildWasmViaIcWasmCli opts mainModulePath testModPath
  case buildResult of
    Left err => pure (Left $ "WASM build failed: " ++ err)
    Right wasmPath => do
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
