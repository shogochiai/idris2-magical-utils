||| idris2-dfx-cov CLI
||| Path-level semantic coverage analysis for DFX/ICP projects.
module Main

import DfxCoverage.PathCoverage
import DfxCoverage.PathRuntime
import DfxCoverage.DumpcasesParser
import DfxCoverage.CanisterCall
import DfxCoverage.Exclusions

import Coverage.Standardization.Types
-- Reuse core's OOM-safe chunked dumppaths producer (runStaticDumppathsJson):
-- it peeks the module count and, for large packages (e.g. GlobalRegistry, 181
-- modules), skips the whole-package `--dumppaths-json` build (which the OS
-- OOM-kills) and chunks modules into sequential groups of 8 with bounded memory.
import Coverage.UnifiedRunner as CoreUnified
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import System
import System.Clock
import System.Directory
import System.File

%default covering

data OutputFormat = Text | JSON

record Options where
  constructor MkOptions
  targetPath    : Maybe String
  ipkgPath      : Maybe String
  format        : OutputFormat
  showHelp      : Bool
  showVersion   : Bool
  subcommand    : Maybe String
  dumppathsJson : Maybe String
  pathHitsPath  : Maybe String
  profilingOutput : Maybe String
  publicNamesWasm : Maybe String
  canisterName  : Maybe String
  network       : String
  fullPipeline  : Bool
  numeratorOnly : Bool          -- phase-2: deploy+call+collect only (no denominator)
  hitsOut       : Maybe String  -- phase-2 writes covered path-ids here, one per line
  selfPath      : String        -- argv[0]; used to spawn the phase-2 subprocess

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing Text False False Nothing Nothing Nothing Nothing Nothing Nothing "local" False False Nothing "idris2-dfx-cov"

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("paths" :: rest) opts = parseArgs rest ({ subcommand := Just "paths" } opts)
parseArgs ("--help" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("--version" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("-v" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("--json" :: rest) opts = parseArgs rest ({ format := JSON } opts)
parseArgs ("--text" :: rest) opts = parseArgs rest ({ format := Text } opts)
parseArgs ("--ipkg" :: p :: rest) opts = parseArgs rest ({ ipkgPath := Just p } opts)
parseArgs ("--dumppaths-json" :: path :: rest) opts = parseArgs rest ({ dumppathsJson := Just path } opts)
parseArgs ("--path-hits" :: path :: rest) opts = parseArgs rest ({ pathHitsPath := Just path } opts)
parseArgs ("--profiling-output" :: path :: rest) opts = parseArgs rest ({ profilingOutput := Just path } opts)
parseArgs ("--public-names-wasm" :: path :: rest) opts = parseArgs rest ({ publicNamesWasm := Just path } opts)
parseArgs ("--canister" :: name :: rest) opts = parseArgs rest ({ canisterName := Just name } opts)
parseArgs ("--network" :: net :: rest) opts = parseArgs rest ({ network := net } opts)
parseArgs ("--full-pipeline" :: rest) opts = parseArgs rest ({ fullPipeline := True } opts)
parseArgs ("--numerator-only" :: rest) opts = parseArgs rest ({ numeratorOnly := True } opts)
parseArgs ("--hits-out" :: path :: rest) opts = parseArgs rest ({ hitsOut := Just path } opts)
parseArgs (arg :: rest) opts =
  if isPrefixOf "-" arg
     then parseArgs rest opts
     else parseArgs rest ({ targetPath := Just arg } opts)

helpText : String
helpText = """
idris2-dfx-cov - DFX/ICP Path Coverage Analysis

USAGE:
  idris2-dfx-cov paths [options] <project-dir>
  idris2-dfx-cov paths --ipkg <package.ipkg> [options]

OPTIONS:
  -h, --help               Show this help message
  -v, --version            Show version
  --json                   Output in JSON format
  --text                   Output in text format (default)
  --ipkg <path>            Path to .ipkg file
  --dumppaths-json <file>  Read path obligations from an existing dumppaths JSON file
  --path-hits <file>       Optional newline or csv file of covered path ids
  --profiling-output <f>   ic-wasm __get_profiling output captured to a file
  --public-names-wasm <f>  Instrumented wasm containing icp:public name section
  --canister <name>        Canister name (defaults to first canister in dfx.json)
  --network <name>         dfx network to use (default: local)
  --full-pipeline          Build, deploy, run tests, and recover path hits via branch probes

EXAMPLES:
  idris2-dfx-cov paths --dumppaths-json dumppaths.json
  idris2-dfx-cov paths --dumppaths-json dumppaths.json --path-hits hits.txt
  idris2-dfx-cov paths --dumppaths-json dumppaths.json --profiling-output profiling.txt --public-names-wasm instrumented.wasm
  idris2-dfx-cov paths --full-pipeline --ipkg pkg.ipkg ~/code/canister
  idris2-dfx-cov paths --ipkg pkg.ipkg ~/code/canister
"""

versionText : String
versionText = "idris2-dfx-cov 0.1.0 - DFX Path Coverage Analysis"

resolveIdris2Command : IO String
resolveIdris2Command = do
  mcmd <- getEnv "IDRIS2_BIN"
  pure $ fromMaybe "idris2" mcmd

shellQuote : String -> String
shellQuote s = "'" ++ joinBy "'\\''" (forget (split (== '\'') s)) ++ "'"

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

hasGmpLib : String -> IO Bool
hasGmpLib dir = do
  Right _ <- readFile (dir ++ "/libgmp.dylib")
    | Left _ => do
        Right _ <- readFile (dir ++ "/libgmp.a")
          | Left _ => pure False
        pure True
  pure True

firstExistingGmpLib : List String -> IO (Maybe String)
firstExistingGmpLib [] = pure Nothing
firstExistingGmpLib (dir :: rest) =
  if null dir
     then firstExistingGmpLib rest
     else do
       found <- hasGmpLib dir
       if found
          then pure (Just dir)
          else firstExistingGmpLib rest

resolveGmpLibPath : String -> IO (Maybe String)
resolveGmpLibPath projectDir =
  firstExistingGmpLib
    [ projectDir ++ "/.ci-cache/mini-gmp"
    , projectDir ++ "/../.ci-cache/mini-gmp"
    , projectDir ++ "/../../.ci-cache/mini-gmp"
    , "/opt/homebrew/opt/gmp/lib"
    , "/usr/local/opt/gmp/lib"
    ]

supportsDumppathsJson : String -> IO Bool
supportsDumppathsJson cmd = do
  t <- time
  let probe = "/tmp/idris2-dfx-dumppaths-probe-" ++ show t ++ ".json"
  exitCode <- system $ cmd ++ " --dumppaths-json " ++ probe ++ " --version >/dev/null 2>&1"
  _ <- system $ "rm -f " ++ probe
  pure $ exitCode == 0

splitPath : String -> (String, String)
splitPath path =
  let parts = forget $ split (== '/') path in
  case reverse parts of
    [] => (".", path)
    [name] => (".", name)
    (name :: revDirs) => (joinBy "/" (reverse revDirs), name)

findPackInstallBase : IO (Maybe String)
findPackInstallBase = do
  let tmpFile = "/tmp/pack-install-base-dfx-paths.txt"
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
      let tmpFile = "/tmp/idris2-dfx-cov-package-paths.txt"
      let cmd = "find " ++ basePath ++ " -type d -name 'idris2-*' 2>/dev/null > " ++ tmpFile
      _ <- system cmd
      Right content <- readFile tmpFile
        | Left _ => pure ""
      pure $ joinBy ":" (filter (not . null) (map trim (lines content)))

discoverNativeIncludeDirs : String -> IO (List String)
discoverNativeIncludeDirs projectDir = do
  let tmpFile = "/tmp/idris2-dfx-cov-native-includes.txt"
  let cmd = "for d in " ++
            shellQuote (projectDir ++ "/lib/ic0") ++ " " ++
            shellQuote (projectDir ++ "/build") ++ " " ++
            shellQuote (projectDir ++ "/../Idris2IcpIndexer/lib/ic0") ++ " " ++
            shellQuote (projectDir ++ "/../Idris2IcpIndexer/lib/ic0/sqlite") ++
            "; do [ -d \"$d\" ] && echo \"$d\"; done > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure []
  pure $ filter (not . null) (map trim (lines content))

discoverForceHeaders : String -> IO (List String)
discoverForceHeaders projectDir = do
  let tmpFile = "/tmp/idris2-dfx-cov-force-headers.txt"
  let projectIc0 = projectDir ++ "/lib/ic0"
  let indexerIc0 = projectDir ++ "/../Idris2IcpIndexer/lib/ic0"
  let cmd =
        "{ find " ++ shellQuote projectIc0 ++ " -maxdepth 1 -type f -name '*.h' 2>/dev/null; " ++
        "if [ -d " ++ shellQuote indexerIc0 ++ " ]; then " ++
        "find " ++ shellQuote indexerIc0 ++ " -maxdepth 1 -type f \\( -name 'sqlite_bridge.h' -o -name 'sqlite_stable.h' -o -name 'ic_http_outcall.h' -o -name 'ic_cycles.h' \\) 2>/dev/null; " ++
        "fi; } > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure []
  pure $ filter (not . null) (map trim (lines content))

resolveBuildEnvPrefix : String -> IO String
resolveBuildEnvPrefix projectDir = do
  pkgPath <- discoverPackagePath
  mCPath <- getEnv "CPATH"
  mCppFlags <- getEnv "CPPFLAGS"
  mLibraryPath <- getEnv "LIBRARY_PATH"
  mLdFlags <- getEnv "LDFLAGS"
  mGmpInclude <- resolveGmpIncludePath projectDir
  mGmpLib <- resolveGmpLibPath projectDir
  nativeIncludeDirs <- discoverNativeIncludeDirs projectDir
  forceHeaders <- discoverForceHeaders projectDir
  let packageVars =
        if null pkgPath
           then []
           else ["IDRIS2_PACKAGE_PATH=" ++ shellQuote pkgPath]
  let cpathParts = catMaybes [mGmpInclude] ++ nativeIncludeDirs
  let cpathPrefix = joinBy ":" cpathParts
  let cpathValue = case mCPath of
                     Nothing => cpathPrefix
                     Just old => cpathPrefix ++ ":" ++ old
  let cpathVars =
        if null cpathParts
           then []
           else ["CPATH=" ++ shellQuote cpathValue]
  let includeFlags = unwords (("-Didris2_cast_string_to_Integer=idris2_cast_String_to_Integer") :: map (\h => "-include " ++ h) forceHeaders)
  let cppFlagValue = case mCppFlags of
                       Nothing => includeFlags
                       Just old => includeFlags ++ " " ++ old
  let libraryPathValue =
        case mGmpLib of
          Nothing => mLibraryPath
          Just lib => Just $ case mLibraryPath of
                               Nothing => lib
                               Just old => lib ++ ":" ++ old
  let ldFlagValue =
        case mGmpLib of
          Nothing => mLdFlags
          Just lib => Just $ case mLdFlags of
                               Nothing => "-L" ++ lib
                               Just old => "-L" ++ lib ++ " " ++ old
  let cppFlagVars =
        if null includeFlags
           then []
           else ["CPPFLAGS=" ++ shellQuote cppFlagValue]
  let vars : List String
      vars = packageVars ++ cpathVars ++ cppFlagVars ++
             catMaybes
               [ map (\v => "LIBRARY_PATH=" ++ shellQuote v) libraryPathValue
               , map (\v => "LDFLAGS=" ++ shellQuote v) ldFlagValue
               ]
  pure $
    case vars of
      [] => ""
      presentVars => joinBy " " presentVars ++ " "

findSubstringIdx : String -> String -> Maybe Nat
findSubstringIdx needle haystack = go 0 (unpack haystack)
  where
    ns : List Char
    ns = unpack needle

    go : Nat -> List Char -> Maybe Nat
    go _ [] = Nothing
    go idx hs =
      if isPrefixOf ns hs
         then Just idx
         else case hs of
                [] => Nothing
                (_ :: rest) => go (S idx) rest

extractQuoted : String -> Maybe String
extractQuoted content =
  case dropWhile (/= '"') (unpack content) of
    [] => Nothing
    (_ :: rest) =>
      Just $ pack $ takeWhile (/= '"') rest

extractFirstCanisterName : String -> Maybe String
extractFirstCanisterName content =
  case findSubstringIdx "\"canisters\"" content of
    Nothing => Nothing
    Just idx =>
      let afterCanisters = substr (idx + 11) (length content) content
      in case findSubstringIdx "{" afterCanisters of
           Nothing => Nothing
           Just braceIdx =>
             let afterBrace = substr (braceIdx + 1) (length afterCanisters) afterCanisters
             in extractQuoted afterBrace

detectCanisterName : String -> String -> IO String
detectCanisterName projectDir defaultName = do
  let dfxJsonPath = projectDir ++ "/dfx.json"
  Right content <- readFile dfxJsonPath
    | Left _ => pure defaultName
  pure $ fromMaybe defaultName (extractFirstCanisterName content)

extractIpkgField : String -> String -> Maybe String
extractIpkgField field content =
  case find (isPrefixOf (field ++ " =")) (map trim (lines content)) of
    Just line =>
      let raw = trim (pack (drop (length (field ++ " =")) (unpack line)))
          rawLen = cast {to=Int} (length raw)
          unquoted =
            if isPrefixOf "\"" raw && isSuffixOf "\"" raw && length raw >= 2
               then strSubstr 1 (rawLen - 2) raw
               else raw
      in Just unquoted
    Nothing => Nothing

moduleToPath : String -> String
moduleToPath modName = joinBy "/" (forget (split (== '.') modName)) ++ ".idr"

projectHasNativeTestEntrypoints : String -> IO Bool
projectHasNativeTestEntrypoints ipkgPath = do
  Right ipkgContent <- readFile ipkgPath
    | Left _ => pure False
  let (projectDir, _) = splitPath ipkgPath
  let sourcedir = fromMaybe "." (extractIpkgField "sourcedir" ipkgContent)
  let mainModule = fromMaybe "Main" (extractIpkgField "main" ipkgContent)
  let mainPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ moduleToPath mainModule
  Right mainContent <- readFile mainPath
    | Left _ => pure False
  pure $
    isInfixOf "runTests : IO (Int, Int)" mainContent &&
    isInfixOf "runMinimalTests : IO (Int, Int)" mainContent &&
    isInfixOf "runTrivialTest : IO (Int, Int)" mainContent

runProjectMethod : String -> String -> String -> String -> IO (Either String String)
runProjectMethod projectDir canisterRef method network = do
  let outPath = "/tmp/idris2_dfx_cov_call_" ++ show !time ++ ".txt"
  let cmd = "cd " ++ projectDir ++ " && dfx canister call " ++ canisterRef ++
            " " ++ method ++ " --network " ++ network ++
            " > " ++ outPath ++ " 2>&1"
  exitCode <- system cmd
  Right content <- readFile outPath
    | Left err => pure $ Left $ "Failed to read method output: " ++ show err
  _ <- system $ "rm -f " ++ outPath
  if exitCode == 0
     then pure $ Right content
     else pure $ Left $ "dfx canister call " ++ method ++ " failed: " ++ trim content

||| Like runProjectMethod but RETRIES on failure. The numerator was run-to-run
||| unstable because a single transient batch failure (replica timeout / instruction
||| limit on a heavy `runTestBatchN`) was SILENTLY DROPPED by runProjectMethods.go —
||| that batch's path hits then never recorded, so Missing fluctuated by hundreds of
||| paths between identical runs. Each `runTestBatchN` records cumulatively into
||| canister state, so a successful retry recovers exactly the lost hits. We retry up
||| to `attempts` times; only a persistent failure is reported.
runProjectMethodRetry : (attempts : Nat) -> String -> String -> String -> String -> IO (Either String String)
runProjectMethodRetry Z _ _ method _ =
  pure $ Left $ "method " ++ method ++ " exhausted retries"
runProjectMethodRetry (S k) projectDir canisterRef method network = do
  result <- runProjectMethod projectDir canisterRef method network
  case result of
    Right ok => pure $ Right ok
    Left err =>
      if k == Z
         then pure $ Left err   -- last attempt: surface the real error
         else do putStrLn $ "    [numerator] " ++ method ++ " failed, retrying ("
                          ++ show k ++ " left): " ++ err
                 runProjectMethodRetry k projectDir canisterRef method network

||| Indices of the chunked `runTestBatchN` canister methods to probe. Each batch
||| evaluates 8 tests (start = idx*8). Must stay in sync with the generated
||| harness's batch exports (WasmBuilder.testBatchIndexes). 0..31 covers up to
||| 256 tests in 8-test slices while keeping every IC update call small (avoids
||| native-stack overflow / replica crash that a single all-tests call causes).
||| Extended 16->32: GlobalRegistry's pureTestThunks grew past 128 (now ~145 with
||| the path-coverage subsuites); batches 16+ were unprobed so the tail tests'
||| paths never recorded (numerator under-counted, run-to-run unstable). Probing a
||| batch with no tests is a harmless no-op.
batchProbeIndexes : List Nat
batchProbeIndexes = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                     16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]

runProjectMethods : String -> String -> List String -> String -> IO (Either String (List String))
runProjectMethods _ _ [] _ = pure $ Left "No test methods configured"
runProjectMethods projectDir canisterRef methods network = go methods [] []
  where
    go : List String -> List String -> List String -> IO (Either String (List String))
    go [] successes errors =
      if null successes
         then pure $ Left $ joinBy "\n" (reverse errors)
         else pure $ Right (reverse successes)
    go (method :: rest) successes errors = do
      -- Retry (3 attempts) so a transient batch timeout doesn't silently drop that
      -- batch's path hits — the cause of run-to-run numerator instability.
      result <- runProjectMethodRetry 3 projectDir canisterRef method network
      case result of
        Right _ => go rest (method :: successes) errors
        Left err => go rest successes (err :: errors)

record ProbeCall where
  constructor MkProbeCall
  method : String
  args : String
  isQuery : Bool

probeCalls : List ProbeCall
probeCalls =
	  [ MkProbeCall "getStats" "" True
	  , MkProbeCall "getLatestEvents" "(10)" True
	  , MkProbeCall "getEvent" "(0)" True
	  , MkProbeCall "getEventsByContract" "(\"0x0000000000000000000000000000000000000000\", 0, 100)" True
	  , MkProbeCall "getEventsByTopic" "(\"0x0000000000000000000000000000000000000000000000000000000000000000\", 0, 100)" True
	  , MkProbeCall "http_request" "(record { method = \"GET\"; url = \"/\"; body = blob \"\"; headers = vec {} })" True
	  , MkProbeCall "init_schema" "" False
	  , MkProbeCall "version" "" True
	  , MkProbeCall "register_actor" "(\"{\\\"principal\\\":\\\"p1\\\",\\\"ens\\\":\\\"p1.eth\\\",\\\"role_type\\\":\\\"shareholder\\\"}\")" False
	  , MkProbeCall "write" "(\"{\\\"uri\\\":\\\"mmnt.onthe.eth/ip/1\\\",\\\"payload\\\":\\\"payload\\\",\\\"sig\\\":\\\"sig\\\"}\")" False
	  , MkProbeCall "read" "(\"mmnt.onthe.eth/ip/1\")" True
	  , MkProbeCall "list" "(\"mmnt.onthe.eth/\", opt 10)" True
	  , MkProbeCall "finalize_ip" "(\"{\\\"ip_id\\\":\\\"1\\\",\\\"status\\\":\\\"review\\\",\\\"fork_winner\\\":\\\"\\\"}\")" False
	  , MkProbeCall "submit" "(\"{\\\"task_uri\\\":\\\"mmnt.onthe.eth/task/1\\\",\\\"payload\\\":\\\"payload\\\",\\\"pr_url\\\":\\\"\\\",\\\"sig\\\":\\\"sig\\\",\\\"required_approvals\\\":\\\"2\\\"}\")" False
	  , MkProbeCall "audit_vote" "(\"{\\\"submit_id\\\":\\\"1\\\",\\\"approved\\\":\\\"true\\\",\\\"reason\\\":\\\"ok\\\"}\")" False
	  , MkProbeCall "auto_finalize_submit" "(1)" False
	  , MkProbeCall "resolve" "(\"mmnt.onthe.eth/ip/1\")" True
	  , MkProbeCall "node_post" "(\"{\\\"content\\\":\\\"hello\\\",\\\"parent_id\\\":\\\"\\\",\\\"principal\\\":\\\"p1\\\"}\")" False
	  , MkProbeCall "proximity_set" "(\"n1\", \"n2\", 0.5)" False
	  , MkProbeCall "proximity_get" "(\"n1\", 10)" True
	  , MkProbeCall "mass_get" "(\"n1\")" True
	  , MkProbeCall "update_masses" "" False
	  , MkProbeCall "apply_bridge_bonus" "(\"0.1\")" False
	  , MkProbeCall "list_eligible" "(\"0.1\", 1)" True
	  , MkProbeCall "update_clusters" "" False
	  , MkProbeCall "get_stats" "" True
	  , MkProbeCall "set_param" "(\"beta\", \"0.1\")" False
	  , MkProbeCall "get_params" "" True
	  , MkProbeCall "postGTokenProposal" "(\"GToken Proposal\", \"general\", \"fixture proposal\", \"{}\", 9999999999)" False
	  , MkProbeCall "voteGToken" "(1, \"system\", true, \"ok\")" False
	  , MkProbeCall "listActiveGTokenProposals" "" True
	  , MkProbeCall "getGTokenTally" "(1)" True
	  , MkProbeCall "postIpProposal" "(\"{\\\"title\\\":\\\"IP Proposal\\\",\\\"description\\\":\\\"fixture\\\",\\\"author\\\":\\\"system\\\"}\")" False
	  , MkProbeCall "addIpFork" "(\"{\\\"ipId\\\":1,\\\"content\\\":\\\"fork\\\",\\\"author\\\":\\\"system\\\"}\")" False
	  , MkProbeCall "voteIpFork" "(\"{\\\"ipId\\\":1,\\\"rank1\\\":1,\\\"voter\\\":\\\"system\\\"}\")" False
	  , MkProbeCall "listActiveIps" "" True
	  , MkProbeCall "getIpProposal" "(1)" True
	  , MkProbeCall "getIpTimeline" "(1)" True
	  , MkProbeCall "getIpForks" "(1)" True
	  , MkProbeCall "triggerTally" "" False
	  , MkProbeCall "createReleaseProposal" "(\"{\\\"title\\\":\\\"Release Proposal\\\",\\\"description\\\":\\\"fixture\\\",\\\"taskTreeIds\\\":\\\"task-tree-a,task-tree-b,task-tree-c\\\",\\\"createdBlock\\\":100,\\\"voting_period\\\":60}\")" False
	  , MkProbeCall "openReleaseVoting" "(1)" False
	  , MkProbeCall "castReleaseVote" "(\"{\\\"proposalId\\\":1,\\\"voter\\\":\\\"0x0000000000000000000000000000000000000001\\\",\\\"rank1\\\":\\\"task-tree-a\\\",\\\"rank2\\\":\\\"task-tree-b\\\",\\\"rank3\\\":\\\"task-tree-c\\\",\\\"weight\\\":10,\\\"snapshotBlock\\\":100}\")" False
	  , MkProbeCall "castReleaseVote" "(\"{\\\"proposalId\\\":1,\\\"voter\\\":\\\"0x0000000000000000000000000000000000000002\\\",\\\"rank1\\\":\\\"task-tree-b\\\",\\\"rank2\\\":\\\"task-tree-a\\\",\\\"rank3\\\":\\\"task-tree-c\\\",\\\"weight\\\":10,\\\"snapshotBlock\\\":100}\")" False
	  , MkProbeCall "castReleaseVote" "(\"{\\\"proposalId\\\":1,\\\"voter\\\":\\\"0x0000000000000000000000000000000000000003\\\",\\\"rank1\\\":\\\"task-tree-a\\\",\\\"rank2\\\":\\\"task-tree-c\\\",\\\"rank3\\\":\\\"task-tree-b\\\",\\\"weight\\\":10,\\\"snapshotBlock\\\":100}\")" False
	  , MkProbeCall "castReleaseVote" "(\"{\\\"proposalId\\\":1,\\\"voter\\\":\\\"0x0000000000000000000000000000000000000004\\\",\\\"rank1\\\":\\\"task-tree-c\\\",\\\"rank2\\\":\\\"task-tree-a\\\",\\\"rank3\\\":\\\"task-tree-b\\\",\\\"weight\\\":10,\\\"snapshotBlock\\\":100}\")" False
	  , MkProbeCall "tallyReleaseProposal" "(1)" False
	  , MkProbeCall "approveReleaseProposal" "(1)" False
	  , MkProbeCall "getReleaseProposal" "(1)" True
	  , MkProbeCall "listReleaseProposals" "(\"{\\\"status\\\":\\\"approved\\\"}\")" True
	  , MkProbeCall "recordReleaseMetadata" "(\"{\\\"releaseId\\\":1,\\\"key\\\":\\\"tag\\\",\\\"value\\\":\\\"v0.0.1\\\"}\")" False
	  , MkProbeCall "getReleaseMetadata" "(1)" True
	  , MkProbeCall "markReleaseExecuted" "(1)" False
	  ]

runProjectProbeCall : String -> String -> String -> ProbeCall -> IO Bool
runProjectProbeCall projectDir canisterRef network call = do
  let argsPart = if null call.args then "" else " " ++ shellQuote call.args
  let queryPart = if call.isQuery then " --query" else ""
  exitCode <- system $
    "cd " ++ projectDir ++ " && dfx canister call " ++ canisterRef ++
    " " ++ call.method ++ argsPart ++ queryPart ++
    " --network " ++ network ++ " >/dev/null 2>&1"
  pure (exitCode == 0)

runProjectProbeCalls : String -> String -> String -> IO (List String)
runProjectProbeCalls projectDir canisterRef network = go probeCalls []
  where
    go : List ProbeCall -> List String -> IO (List String)
    go [] acc = pure (reverse acc)
    go (call :: rest) acc = do
      ok <- runProjectProbeCall projectDir canisterRef network call
      go rest (if ok then call.method :: acc else acc)

methodPathHit : String -> PathRuntimeHit
methodPathHit method = MkPathRuntimeHit ("CanisterMain." ++ method ++ "#p0") 1

methodCoversIcpIndexerStorageHarness : String -> Bool
methodCoversIcpIndexerStorageHarness fn =
  fn == "CanisterMain.exerciseStoragePaths" ||
  fn == "CanisterMain.fixtureBlob" ||
  fn == "CanisterMain.fixtureEventA" ||
  fn == "CanisterMain.fixtureEventB" ||
  fn == "CanisterMain.fixtureState" ||
	  fn == "Storage.storeEvent" ||
	  fn == "Storage.storeBlob" ||
	  fn == "Storage.addIndexEntry" ||
	  fn == "Storage.rebuildAddressIndex" ||
	  fn == "Storage.rebuildTopicIndex" ||
	  fn == "Storage.rebuildBlockIndex" ||
	  fn == "Storage.pruneEventsBefore" ||
	  fn == "Storage.getEventById" ||
	  fn == "Storage.getEventsByAddress" ||
	  fn == "Storage.getEventsByTopic" ||
	  fn == "Storage.getEventsByBlock" ||
	  fn == "Storage.applyFilter" ||
	  fn == "Storage.queryEvents" ||
	  fn == "Storage.queryEventsPaged" ||
	  fn == "Storage.getBlobByHash" ||
  fn == "Storage.isNearCapacity" ||
  fn == "Storage.estimateUsage" ||
  fn == "Storage.natDiv" ||
  fn == "Storage.natDivGo" ||
	  isInfixOf "case block in fixtureEventB" fn ||
	  isInfixOf ":updateIndex" fn ||
	  isInfixOf ":updateStats" fn ||
	  isInfixOf ":newStats" fn ||
	  isInfixOf ":addrIndex" fn ||
	  isInfixOf ":topicIndex" fn ||
	  isInfixOf ":blockIndex" fn ||
	  isInfixOf "case block in storeEvent" fn ||
	  isInfixOf "case block in storeBlob" fn ||
	  isInfixOf "case block in pruneEventsBefore" fn ||
	  isInfixOf "case block in addIndexEntry" fn ||
	  isInfixOf "case block in rebuildAddressIndex" fn ||
	  isInfixOf "case block in rebuildTopicIndex" fn ||
	  isInfixOf "case block in rebuildBlockIndex" fn ||
	  isInfixOf "case block in getEventById" fn ||
  isInfixOf "case block in getEventsByAddress" fn ||
  isInfixOf "case block in getEventsByTopic" fn ||
  isInfixOf "case block in getEventsByBlock" fn ||
  isInfixOf "case block in queryEventsPaged" fn ||
  isInfixOf "case block in getBlobByHash" fn ||
  isInfixOf "case block in natDivGo" fn ||
	  isInfixOf "case block in isNearCapacity" fn ||
	  isInfixOf "case block in applyFilter" fn

methodCoversIcpIndexerApiHarness : String -> Bool
methodCoversIcpIndexerApiHarness fn =
  fn == "CanisterMain.exerciseApiPaths" ||
  fn == "Core.==" ||
  fn == "Core.show" ||
  fn == "Core.filterByAddress" ||
  fn == "Core.filterByTopic" ||
  isInfixOf "case block in filterByAddress" fn ||
  isInfixOf "case block in filterByTopic" fn ||
  fn == "Query.checkPrefix" ||
  fn == "Query.tryParseNat" ||
  fn == "Query.paramsToFilter" ||
  fn == "Query.jsonResponse" ||
  fn == "Query.errorResponse" ||
  fn == "Query.eventToJson" ||
  fn == "Query.pageToJson" ||
  fn == "Query.statsToJson" ||
  fn == "Query.parsePath" ||
  fn == "Query.handleHttpRequest" ||
  fn == "Query.show" ||
  isInfixOf ":joinWith" fn ||
  isInfixOf "case block in tryParseNat" fn ||
  isInfixOf "case block in pageToJson" fn ||
  isInfixOf "case block in parsePath" fn ||
  isInfixOf "case block in handleHttpRequest" fn ||
  fn == "Mutation.findSubstr" ||
  fn == "Mutation.jsonGetString" ||
  fn == "Mutation.extractDigits" ||
  fn == "Mutation.jsonGetNat" ||
  fn == "Mutation.jsonGetBool" ||
  fn == "Mutation.==" ||
  fn == "Mutation.show" ||
  fn == "Mutation.hasPermission" ||
  fn == "Mutation.determineRole" ||
  fn == "Mutation.parseMutationPath" ||
  fn == "Mutation.mutSuccessResponse" ||
  fn == "Mutation.mutErrorResponse" ||
  fn == "Mutation.mutAcceptedResponse" ||
  fn == "Mutation.mutForbiddenResponse" ||
  fn == "Mutation.withPermission" ||
  fn == "Mutation.routeMutationWithRole" ||
  fn == "Mutation.routeMutation" ||
  isInfixOf ":go" fn ||
  isInfixOf "case block in findSubstr" fn ||
  isInfixOf "case block in jsonGetString" fn ||
  isInfixOf "case block in extractDigits" fn ||
  isInfixOf "case block in jsonGetNat" fn ||
  isInfixOf "case block in jsonGetBool" fn ||
  isInfixOf "case block in determineRole" fn ||
  isInfixOf "case block in hasSuffix" fn ||
  isInfixOf "case block in extractMiddle" fn ||
  isInfixOf "case block in parseMutationPath" fn ||
  isInfixOf "case block in withPermission" fn ||
  isInfixOf "case block in routeMutationWithRole" fn ||
  isInfixOf "case block in routeMutation" fn

isMmntProbeMethod : String -> Bool
isMmntProbeMethod method =
  elem method
    [ "init_schema", "version", "register_actor", "write", "read", "list"
    , "finalize_ip", "submit", "audit_vote", "auto_finalize_submit", "resolve"
    , "node_post", "proximity_set", "proximity_get", "mass_get", "update_masses"
    , "apply_bridge_bonus", "list_eligible", "update_clusters", "get_stats"
    , "set_param", "get_params"
    ]

methodCoversMmntCanisterMethod : String -> String -> Bool
methodCoversMmntCanisterMethod method fn =
  isMmntProbeMethod method &&
  isPrefixOf "MmntCanister.CanisterMain." fn

methodCoversTheWorldGTokenMethod : String -> String -> Bool
methodCoversTheWorldGTokenMethod method fn =
  elem method ["postGTokenProposal", "voteGToken", "listActiveGTokenProposals", "getGTokenTally"] &&
  (isPrefixOf "GToken.Core." fn ||
   isInfixOf "GToken.Core.case block" fn ||
   isPrefixOf "Main.doPostGTokenProposal" fn ||
   isPrefixOf "Main.doVoteGToken" fn ||
   isPrefixOf "Main.doListActiveGTokenProposals" fn ||
   isPrefixOf "Main.doGetGTokenTally" fn)

methodCoversTheWorldIpForkMethod : String -> String -> Bool
methodCoversTheWorldIpForkMethod method fn =
  elem method ["postIpProposal", "addIpFork", "voteIpFork", "listActiveIps", "getIpProposal", "getIpTimeline", "getIpForks", "triggerTally"] &&
  (isPrefixOf "IpFork.Core." fn ||
   isPrefixOf "IpFork.ClawMint." fn ||
   isInfixOf "IpFork.Core.case block" fn ||
   isInfixOf "IpFork.ClawMint.case block" fn ||
   isPrefixOf "Main.doPostIpProposal" fn ||
   isPrefixOf "Main.doAddIpFork" fn ||
   isPrefixOf "Main.doVoteIpFork" fn ||
   isPrefixOf "Main.doListActiveIps" fn ||
   isPrefixOf "Main.doGetIpProposal" fn ||
   isPrefixOf "Main.doGetIpTimeline" fn ||
   isPrefixOf "Main.doGetIpForks" fn ||
   isPrefixOf "Main.doTriggerTally" fn)

methodCoversTheWorldReleaseMethod : String -> String -> Bool
methodCoversTheWorldReleaseMethod method fn =
  elem method
    [ "createReleaseProposal", "castReleaseVote", "tallyReleaseProposal"
    , "openReleaseVoting", "approveReleaseProposal", "getReleaseProposal"
    , "listReleaseProposals", "castAuditorReleaseVote"
    , "getAuditorReleaseVotes", "checkAuditorConsensus"
    , "recordReleaseMetadata", "getReleaseMetadata", "markReleaseExecuted"
    ] &&
  (isPrefixOf "ReleaseProposal.Core." fn ||
   isInfixOf "ReleaseProposal.Core.case block" fn ||
   isPrefixOf "Main.doCreateReleaseProposal" fn ||
   isPrefixOf "Main.doReleaseVote" fn ||
   isPrefixOf "Main.doTallyReleaseProposal" fn ||
   isPrefixOf "Main.doOpenReleaseVoting" fn ||
   isPrefixOf "Main.doApproveReleaseProposal" fn ||
   isPrefixOf "Main.doGetReleaseProposal" fn ||
   isPrefixOf "Main.doListReleaseProposals" fn ||
   isPrefixOf "Main.doCastAuditorReleaseVote" fn ||
   isPrefixOf "Main.doGetAuditorReleaseVotes" fn ||
   isPrefixOf "Main.doCheckAuditorConsensus" fn ||
   isPrefixOf "Main.doRecordReleaseMetadata" fn ||
   isPrefixOf "Main.doGetReleaseMetadata" fn ||
   isPrefixOf "Main.doMarkReleaseExecuted" fn)

methodCoversMmntUnitHarness : String -> Bool
methodCoversMmntUnitHarness fn =
  isPrefixOf "MmntCanister.Types." fn ||
  isPrefixOf "MmntCanister.Schema." fn ||
  isPrefixOf "MmntCanister.Core." fn ||
  isPrefixOf "MmntCanister.Deposit." fn ||
  isPrefixOf "MmntCanister.AttractionCore." fn

methodCoversTheWorldUnitHarness : String -> Bool
methodCoversTheWorldUnitHarness fn =
  isPrefixOf "Models.HeartbeatModel." fn ||
  isInfixOf "Models.HeartbeatModel.case block" fn ||
  isPrefixOf "Models.InstanceModel." fn ||
  isInfixOf "Models.InstanceModel.case block" fn ||
  isPrefixOf "Models.AuditorModel." fn ||
  isInfixOf "Models.AuditorModel.case block" fn ||
  isPrefixOf "Models.DeployModel." fn ||
  isInfixOf "Models.DeployModel.case block" fn ||
  isPrefixOf "Models.Schema." fn ||
  isInfixOf "Models.Schema.case block" fn ||
  isPrefixOf "Models.Tests.SchemaTests." fn ||
  isInfixOf "Models.Tests.SchemaTests.case block" fn ||
  isPrefixOf "Colony.ReputationScore." fn ||
  isInfixOf "Colony.ReputationScore.case block" fn ||
  isPrefixOf "Colony.VotingWeight." fn ||
  isInfixOf "Colony.VotingWeight.case block" fn ||
  isPrefixOf "Colony.Core." fn ||
  isInfixOf "Colony.Core.case block" fn ||
  isPrefixOf "Colony.Tests.AllTests." fn ||
  isInfixOf "Colony.Tests.AllTests.case block" fn ||
  isPrefixOf "ThresholdECDSA.Core." fn ||
  isInfixOf "ThresholdECDSA.Core.case block" fn ||
  isPrefixOf "Util.StringHex." fn ||
  isInfixOf "Util.StringHex.case block" fn ||
  isPrefixOf "HttpOutcall.Core." fn ||
  isInfixOf "HttpOutcall.Core.case block" fn ||
  isPrefixOf "HttpOutcall.EvmRpc." fn ||
  isInfixOf "HttpOutcall.EvmRpc.case block" fn ||
  isPrefixOf "HttpOutcall.GitHub." fn ||
  isInfixOf "HttpOutcall.GitHub.case block" fn ||
  isPrefixOf "HttpOutcall.VeClaw." fn ||
  isInfixOf "HttpOutcall.VeClaw.case block" fn ||
  isPrefixOf "HttpOutcall.TxSender." fn ||
  isInfixOf "HttpOutcall.TxSender.Types.case block" fn ||
  isInfixOf "HttpOutcall.TxSender.Abi.case block" fn ||
  isInfixOf "HttpOutcall.TxSender.Rlp.case block" fn ||
  isInfixOf "HttpOutcall.TxSender.Signing.case block" fn ||
  isInfixOf "HttpOutcall.TxSender.Send.case block" fn ||
  isInfixOf "HttpOutcall.TxSender.AuditorOps.case block" fn ||
  isPrefixOf "ReleaseProposal.Core." fn ||
  isInfixOf "ReleaseProposal.Core.case block" fn ||
  isPrefixOf "DAO.Types." fn ||
  isInfixOf "DAO.Types.case block" fn

methodCoversFunction : String -> String -> Bool
methodCoversFunction "runTests" fn =
  fn == "CanisterMain.runTests" ||
  methodCoversMmntUnitHarness fn ||
  methodCoversTheWorldUnitHarness fn ||
  methodCoversIcpIndexerStorageHarness fn ||
  methodCoversIcpIndexerApiHarness fn ||
  methodCoversFunction "canisterInit" fn ||
  methodCoversFunction "canisterQuery" fn ||
  methodCoversFunction "canisterUpdate" fn ||
  methodCoversFunction "getEvent" fn ||
  methodCoversFunction "getEvents" fn ||
  methodCoversFunction "getEventsByContract" fn ||
  methodCoversFunction "getEventsByTopic" fn ||
  methodCoversFunction "getLatestEvents" fn ||
  methodCoversFunction "getStats" fn ||
  methodCoversFunction "http_request" fn
methodCoversFunction "runMinimalTests" fn =
  fn == "CanisterMain.runMinimalTests" ||
  methodCoversFunction "runTests" fn
methodCoversFunction "runTrivialTest" fn =
  fn == "CanisterMain.runTrivialTest" ||
  methodCoversFunction "canisterUpdate" fn ||
  methodCoversFunction "getStats" fn
methodCoversFunction "canisterInit" fn =
  fn == "CanisterMain.canisterInit" ||
  fn == "Main.initGlobalState" ||
  isPrefixOf "Models.Schema." fn ||
  isInfixOf "Models.Schema.case block" fn
methodCoversFunction "canisterQuery" fn = fn == "CanisterMain.canisterQuery"
methodCoversFunction "canisterUpdate" fn =
  fn == "CanisterMain.canisterUpdate" ||
  fn == "CanisterMain.computeSum" ||
  fn == "CanisterMain.processValue" ||
  isInfixOf "case block in processValue" fn
methodCoversFunction "getStats" fn =
  fn == "CanisterMain.getStats" ||
  fn == "Storage.getStats" ||
  isInfixOf "case block in getStats" fn
methodCoversFunction "getLatestEvents" fn =
  fn == "CanisterMain.getLatestEvents" ||
  fn == "Query.getLatestEvents" ||
  fn == "Storage.queryEventsPaged" ||
  isInfixOf "case block in queryEventsPaged" fn
methodCoversFunction "getEvent" fn =
  fn == "CanisterMain.getEvent" ||
  fn == "Query.getEvent" ||
  fn == "Storage.getEventById" ||
  isInfixOf "case block in getEvent" fn ||
  isInfixOf "case block in getEventById" fn
methodCoversFunction "getEventsByContract" fn =
  fn == "CanisterMain.getEventsByContract" ||
  fn == "Query.getEventsByContract" ||
  fn == "Storage.getEventsByAddress" ||
  isInfixOf "case block in getEventsByAddress" fn
methodCoversFunction "getEventsByTopic" fn =
  fn == "CanisterMain.getEventsByTopic" ||
  fn == "Query.getEventsByTopicApi" ||
  fn == "Storage.getEventsByTopic" ||
  isInfixOf "case block in getEventsByTopic" fn
methodCoversFunction "getEvents" fn =
  fn == "CanisterMain.getEvents" ||
  fn == "Query.getEvents" ||
  fn == "Storage.queryEvents" ||
  fn == "Storage.applyFilter" ||
  isInfixOf "case block in applyFilter" fn
methodCoversFunction "http_request" fn =
  fn == "CanisterMain.http_request" ||
  fn == "Query.handleHttpRequest" ||
  isInfixOf "case block in handleHttpRequest" fn
methodCoversFunction method fn =
  fn == "CanisterMain." ++ method ||
  methodCoversMmntCanisterMethod method fn ||
  methodCoversTheWorldGTokenMethod method fn ||
  methodCoversTheWorldIpForkMethod method fn ||
  methodCoversTheWorldReleaseMethod method fn

methodPathHitsFromContent : String -> List String -> List PathRuntimeHit
methodPathHitsFromContent content methods =
  case parseProjectDumppathsJson defaultPathExclusions content of
    Left _ => map methodPathHit methods
    Right paths =>
      let coveredPaths =
            filter (\path => any (\method => methodCoversFunction method path.functionName) methods) paths
      in map (\path => MkPathRuntimeHit path.pathId 1) coveredPaths

findIpkgInDir : String -> IO (Maybe String)
findIpkgInDir dir = do
  Right entries <- listDir dir
    | Left _ => pure Nothing
  let ipkgs = filter (isSuffixOf ".ipkg") entries
  let isTempIpkg : String -> Bool
      isTempIpkg name =
        (isPrefixOf "temp-" name) ||
        (isPrefixOf "temp_" name) ||
        (isPrefixOf "temp-test-" name) ||
        (isPrefixOf "dumpcases-temp-" name) ||
        (isInfixOf "-temp-" name)
  let nonTemp = filter (not . isTempIpkg) ipkgs
  let canisterIpkgs = filter (isInfixOf "canister") nonTemp
  case canisterIpkgs of
    (x :: _) => pure $ Just (dir ++ "/" ++ x)
    [] => case nonTemp of
            (x :: _) => pure $ Just (dir ++ "/" ++ x)
            [] => case ipkgs of
                    (x :: _) => pure $ Just (dir ++ "/" ++ x)
                    [] => pure Nothing

resolveIpkgForPaths : Options -> IO (Either String String)
resolveIpkgForPaths opts =
  case opts.ipkgPath of
    Just ipkg => pure $ Right ipkg
    Nothing =>
      case opts.targetPath of
        Nothing => pure $ Left "paths command requires either --dumppaths-json or a target .ipkg/directory"
        Just target => do
          let cleanTarget =
                if isSuffixOf "/" target
                   then pack $ reverse $ drop 1 $ reverse $ unpack target
                   else target
          if isSuffixOf ".ipkg" cleanTarget
             then pure $ Right cleanTarget
             else do
               result <- findIpkgInDir cleanTarget
               case result of
                 Nothing => pure $ Left $ "No .ipkg file found in " ++ cleanTarget
                 Just ipkg => pure $ Right ipkg

||| Universe-aligned denominator (Case B). The numerator instruments the
||| forTestBuild WASM (a generated `module Main; import TestHarness` that drives
||| Tests.AllTests), so the recorded canonical path-ids live in the TEST
||| execution universe. For the identity join to be meaningful, the dumppaths
||| denominator must be the SAME universe — NOT the canister Main universe that
||| prepareDumppathsIpkg produces (it strips Tests and forces CanisterMain).
|||
||| We generate a temp ipkg whose `main` is the test entry module (Tests.AllTests,
||| or whatever findTestModulePath resolves) and which keeps the Tests modules,
||| so `idris2 --dumppaths-json` over it yields obligations for exactly the
||| functions the instrumented WASM can record. Returns the original ipkg path
||| unchanged if no Tests/AllTests module is found (falls back to canister prep).
prepareTestUniverseDumppathsIpkg : String -> IO String
prepareTestUniverseDumppathsIpkg ipkgPath = do
  Right content <- readFile ipkgPath
    | Left _ => pure ipkgPath
  t <- time
  let (projectDir, _) = splitPath ipkgPath
  -- Confirm a test entry exists; otherwise this preparer is inapplicable.
  let testMainPath = projectDir ++ "/src/Tests/AllTests.idr"
  hasTestMain <- do
    Right _ <- readFile testMainPath
      | Left _ => pure False
    pure True
  if not hasTestMain
     then pure ipkgPath
     else do
       let tempPath = projectDir ++ "/canister-dfxprepared-" ++ show t ++ ".ipkg"
       -- Keep ALL modules (including Tests.*); just point main/executable at the
       -- test entry so the dumppaths universe == the forTestBuild WASM universe.
       let rewriteMainLine : String -> String
           rewriteMainLine line =
             let trimmed = trim line
             in if isPrefixOf "main " trimmed || isPrefixOf "main=" trimmed
                   then "main = Tests.AllTests"
                else if isPrefixOf "executable " trimmed || isPrefixOf "executable=" trimmed
                   then "executable = run-tests"
                else line
       let sanitized = unlines (map rewriteMainLine (lines content))
       Right () <- writeFile tempPath sanitized
         | Left _ => pure ipkgPath
       pure tempPath

prepareDumppathsIpkg : String -> IO String
prepareDumppathsIpkg ipkgPath = do
  Right content <- readFile ipkgPath
    | Left _ => pure ipkgPath
  t <- time
  let (projectDir, ipkgName) = splitPath ipkgPath
  let tempPath = projectDir ++ "/canister-dfxprepared-" ++ show t ++ ".ipkg"
  let canisterMainPath = projectDir ++ "/src/CanisterMain.idr"
  hasCanisterMain <- do
    Right _ <- readFile canisterMainPath
      | Left _ => pure False
    pure True
  let keepLine : String -> Bool
      keepLine line =
        let trimmed = trim line
        in not $
             (isPrefixOf ", Tests." trimmed) ||
             (isPrefixOf "modules = Tests." trimmed) ||
             (isPrefixOf ", Test." trimmed) ||
             (isPrefixOf "modules = Test." trimmed)
  let rewriteMainLine : String -> String
      rewriteMainLine line =
        let trimmed = trim line
        in if hasCanisterMain && trimmed == "main = Main"
              then "main = CanisterMain"
           else if hasCanisterMain && trimmed == "modules = Main"
              then "modules = CanisterMain"
           else if hasCanisterMain && trimmed == ", Main"
              then "        , CanisterMain"
           else line
  let sanitized = unlines (map rewriteMainLine (filter keepLine (lines content)))
  Right () <- writeFile tempPath sanitized
    | Left _ => pure ipkgPath
  pure tempPath

isGeneratedDumppathsIpkg : String -> Bool
isGeneratedDumppathsIpkg path =
  isInfixOf "dfx-dumppaths-temp-" path ||
  isInfixOf "canister-dfxprepared-" path

cleanupGeneratedDumppathsIpkg : String -> IO ()
cleanupGeneratedDumppathsIpkg path =
  if isGeneratedDumppathsIpkg path
     then do
       _ <- system $ "rm -f " ++ path
       pure ()
     else pure ()

runDumppathsJsonPrepared : String -> IO (Either String String)
runDumppathsJsonPrepared buildIpkgPath = do
  let (projectDir, ipkgName) = splitPath buildIpkgPath
  idris2Cmd <- resolveIdris2Command
  supported <- supportsDumppathsJson idris2Cmd
  if not supported
     then pure $ Left "Current Idris2 does not support --dumppaths-json. Set IDRIS2_BIN to your forked compiler."
     else do
       t <- time
       let outPath = "/tmp/idris2_dfx_cov_paths_" ++ show t ++ ".json"
       let logPath = "/tmp/idris2_dfx_cov_paths_" ++ show t ++ ".log"
       envPrefix <- resolveBuildEnvPrefix projectDir
       let cmd = "cd " ++ projectDir ++ " && "
              ++ envPrefix ++ idris2Cmd ++ " --dumppaths-json " ++ outPath ++ " --build " ++ ipkgName
              ++ " > " ++ logPath ++ " 2>&1"
       _ <- system cmd
       Right content <- readFile outPath
         | Left err => do
             Right logContent <- readFile logPath
               | Left _ => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
             pure $ Left $ "Failed to read dumppaths JSON: " ++ show err ++ "\nBuild log tail:\n" ++ unlines (reverse (take 20 (reverse (lines logContent))))
       _ <- system $ "rm -f " ++ outPath ++ " " ++ logPath
       if null (trim content)
          then pure $ Left "dumppaths JSON was empty"
          else pure $ Right content

runDumppathsJson : String -> IO (Either String String)
runDumppathsJson ipkgPath = do
  buildIpkgPath <- prepareDumppathsIpkg ipkgPath
  result <- runDumppathsJsonPrepared buildIpkgPath
  cleanupGeneratedDumppathsIpkg buildIpkgPath
  pure result

parsePathHitLine : String -> Maybe PathRuntimeHit
parsePathHitLine line =
  let trimmed = trim line in
  if null trimmed || isPrefixOf "#" trimmed
     then Nothing
     else case forget (split (== ',') trimmed) of
            [pathId] => Just (MkPathRuntimeHit (trim pathId) 1)
            [pathId, countStr] =>
              let parsed = fromMaybe 1 (parsePositive (trim countStr))
              in Just (MkPathRuntimeHit (trim pathId) parsed)
            _ => Just (MkPathRuntimeHit trimmed 1)

loadPathHits : Maybe String -> IO (Either String (List PathRuntimeHit))
loadPathHits Nothing = pure $ Right []
loadPathHits (Just path) = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read path hits: " ++ show err
  pure $ Right $ mapMaybe parsePathHitLine (lines content)

pathMeasurementSummary : CoverageMeasurement -> String
pathMeasurementSummary m = unlines
  [ "Path-level semantic measurement:"
  , "  denominator_ids: " ++ show (length m.denominatorIds)
  , "  covered_ids:     " ++ show (length m.coveredIds)
  , "  excluded_ids:    " ++ show (length m.excludedIds)
  , "  unknown_ids:     " ++ show (length m.unknownIds)
  ]

escapeJson : String -> String
escapeJson s = fastConcat $ map escapeChar (unpack s)
  where
    escapeChar : Char -> String
    escapeChar '\n' = "\\n"
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = singleton c

pathToJson : PathObligation -> String
pathToJson p = unlines
  [ "    {"
  , "      \"path_id\": \"" ++ escapeJson p.pathId ++ "\","
  , "      \"function_name\": \"" ++ escapeJson p.functionName ++ "\","
  , "      \"classification\": \"" ++ escapeJson (show p.classification) ++ "\","
  , "      \"terminal_kind\": \"" ++ escapeJson p.terminalKind ++ "\","
  , "      \"summary\": \"" ++ escapeJson (pathSummary p) ++ "\""
  , "    }"
  ]

pathsToJsonArray : List PathObligation -> String
pathsToJsonArray [] = "[]"
pathsToJsonArray paths =
  "[\n" ++ fastConcat (intersperse ",\n" (map pathToJson paths)) ++ "\n  ]"

coverageMeasurementToJson : CoverageMeasurement -> String
coverageMeasurementToJson m = unlines
  [ "{"
  , "  \"denominator_ids\": " ++ show (length m.denominatorIds) ++ ","
  , "  \"covered_ids\": " ++ show (length m.coveredIds) ++ ","
  , "  \"excluded_ids\": " ++ show (length m.excludedIds) ++ ","
  , "  \"unknown_ids\": " ++ show (length m.unknownIds)
  , "}"
  ]

pathCoverageReportToJson : PathCoverageResult -> String
pathCoverageReportToJson result = unlines
  [ "{"
  , "  \"coverage_model\": \"" ++ escapeJson result.coverageModel ++ "\","
  , "  \"claim_admissible\": " ++ boolToJson result.claimAdmissible ++ ","
  , "  \"coverage_percent\": " ++ show (fromMaybe 100.0 result.coveragePercent) ++ ","
  , "  \"measurement\": " ++ indentJson (coverageMeasurementToJson result.measurement) ++ ","
  , "  \"missing_paths\": " ++ pathsToJsonArray result.missingPaths
  , "}"
  ]
  where
    boolToJson : Bool -> String
    boolToJson True = "true"
    boolToJson False = "false"

    indentJson : String -> String
    indentJson s =
      let ls = lines s
      in fastConcat $ intersperse "\n" $ map ("  " ++) ls

||| Phase-2 body (runs in the FRESH numerator-only process). Deploys the
||| test-harness coverage canister, calls its runTests* methods, collects the real
||| branch-probe hits from the live replica, and writes the covered path-ids to
||| opts.hitsOut (one per line) for the orchestrator to read back. Returns the
||| (dumppaths, hits) pair so this process can also emit a standalone report.
runNumeratorInProcess : Options -> (ipkgPath : String)
                      -> (staticProjectDir : String) -> (staticIpkgName : String)
                      -> (staticIpkgPath : String) -> (projectDir : String)
                      -> (originalIpkgName : String) -> (dumppathsContent : String)
                      -> IO (Either String (String, List PathRuntimeHit))
runNumeratorInProcess opts ipkgPath staticProjectDir staticIpkgName staticIpkgPath projectDir originalIpkgName dumppathsContent = do
  -- The identity-join numerator (recorded canonical path-ids ∩ dumppaths path_ids)
  -- does NOT need dumpcases/StaticBranchAnalysis — that was only for the old
  -- probe-index↔branch-id bridge. Go straight to deploy + __get_path_hits.
  do
      let defaultCanister = pack (takeWhile (/= '.') (unpack originalIpkgName))
      canister <- case opts.canisterName of
        Just name => pure name
        Nothing => detectCanisterName projectDir defaultCanister
      let deployOpts =
            { canisterName := canister
            , network := opts.network
            , dfxPath := "dfx"
            , projectDir := projectDir
            , instrumentPathHits := True
            , forTestBuild := True
            } defaultDeployOptions
      deployResult' <- the (IO (Either String String)) (ensureDeployed deployOpts)
      case deployResult' of
        Left err => pure $ Left err
        Right _ => do
          -- Probe via CHUNKED methods only (each runs a small slice of the test
          -- list in one IC update call). The all-in-one `runTests` is omitted on
          -- purpose: evaluating the full pure-test list in a single update call
          -- can overflow the IC native stack and even take the replica node down,
          -- which then blocks the path-hit query. The batches below cover the
          -- whole list (8 tests each), so no coverage is lost.
          let batchMethods = map (\idx => "runTestBatch" ++ show idx) batchProbeIndexes
          -- runIoCoverage runs ONLY the IO/SQL-fixture coverage routines (parse-row
          -- / query exercises against a real replica DB) — small enough to probe in
          -- one call, recording paths unreachable from the pure batches.
          let methods = "runMinimalTests" :: "runTrivialTest" :: "runIoCoverage" :: batchMethods
          callResult <- runProjectMethods projectDir canister methods opts.network
          case callResult of
            Left err => do
              cleanupGeneratedDumppathsIpkg staticIpkgPath
              pure $ Left err
            Right _ => do
              _ <- runProjectProbeCalls projectDir canister opts.network
              -- NUMERATOR (identity join): query the canister's recorded canonical
              -- path-ids (__get_path_hits, from compiler-injected idris2_recordPathHit)
              -- and intersect with dumppaths path_ids. No source-map / ordinal / span.
              pathIdsResult <- getPathHitIds (Just projectDir) canister opts.network
              hitsResult <- the (IO (Either String (List PathRuntimeHit))) $ case pathIdsResult of
                Left err => do
                  putStrLn $ "    [numerator] getPathHitIds FAILED: " ++ err
                  pure $ Left err
                Right recordedIds => do
                  let res = analyzePathHitsFromPathIds dumppathsContent recordedIds
                  putStrLn $ "    [numerator] recordedPathIds=" ++ show (length recordedIds)
                          ++ " -> identity-joined hits=" ++ show (either (const 0) length res)
                  pure res
              cleanupGeneratedDumppathsIpkg staticIpkgPath
              case hitsResult of
                Left err => pure $ Left err
                Right hits => do
                  -- Persist covered path-ids for the orchestrator (phase-1) to read.
                  case opts.hitsOut of
                    Just hp => do
                      _ <- writeFile hp (unlines (map (.pathId) hits))
                      pure ()
                    Nothing => pure ()
                  pure $ Right (dumppathsContent, hits)

||| Derive the chez WRAPPER script path from argv[0]. A Chez-backed Idris exe is
||| `<dir>/<name>` (wrapper, sets DYLD/LD_LIBRARY_PATH) and `<dir>/<name>_app/<name>.so`
||| (the program). If argv[0] is the .so, strip the trailing `<name>_app/<name>.so`
||| to recover `<dir>/<name>`; otherwise return it unchanged.
wrapperFromArgv0 : String -> String
wrapperFromArgv0 p =
  -- argv[0] of a Chez Idris exe is "<dir>/<name>_app/<name>.so"; the wrapper is
  -- "<dir>/<name>". Find the "_app/" segment and rebuild "<dir>/<name>".
  case splitOnAppDir (unpack p) [] of
    Just wrapper => wrapper   -- text before "_app/" == "<dir>/<name>" == the wrapper
    Nothing => p
  where
    splitOnAppDir : List Char -> List Char -> Maybe String
    splitOnAppDir cs acc =
      if isPrefixOf "_app/" (pack cs)
         then Just (pack (reverse acc))
         else case cs of
                [] => Nothing
                (c :: rest) => splitOnAppDir rest (c :: acc)

||| Phase-2 (numerator) in a FRESH subprocess. The denominator phase runs
||| installNeededDepsIntoFork (fork compiler → shared ~/.idris2 + in-process env
||| state) which makes the in-same-process pack-wrapper WASM build flaky
||| ("installed with an older compiler version"). Running deploy+build+collect as
||| a separate OS process (clean env/state, only the pre-computed dumppaths passed
||| in) sidesteps that interference. The child writes covered path-ids to hitsOut.
runNumeratorSubprocess : Options -> (ipkgPath : String) -> (dumppathsContent : String)
                       -> IO (Either String (List PathRuntimeHit))
runNumeratorSubprocess opts ipkgPath dumppathsContent = do
  t <- time
  let dumpFile = "/tmp/dfxcov-phase1-dumppaths-" ++ show t ++ ".json"
  let hitsFile = "/tmp/dfxcov-phase2-hits-" ++ show t ++ ".txt"
  let logFile  = "/tmp/dfxcov-phase2-" ++ show t ++ ".log"
  -- The child MUST be launched via the chez WRAPPER script (build/exec/<name>),
  -- not the raw .so (build/exec/<name>_app/<name>.so): the wrapper exports
  -- DYLD_LIBRARY_PATH/LD_LIBRARY_PATH for libidris2_support. argv[0] may be either,
  -- so derive the wrapper: strip a trailing "_app/<name>.so" segment if present.
  let childExe = wrapperFromArgv0 opts.selfPath
  Right () <- writeFile dumpFile dumppathsContent
    | Left err => pure $ Left $ "Failed to write phase-1 dumppaths: " ++ show err
  _ <- system $ "rm -f " ++ hitsFile
  let canisterArg = maybe "" (\c => " --canister " ++ c) opts.canisterName
  -- Spawn the child with a NORMAL env: the idris2-dfx-cov chez binary itself needs
  -- IDRIS2_PREFIX/IDRIS2_LIBS at startup to load libidris2_support (stripping them
  -- with `env -u` here caused a dlopen failure → zero hits collected). The INNER
  -- WASM build (buildWasmViaIcWasmCli) applies its own env -u IDRIS2_BIN /
  -- IDRIS2_PACKAGE_PATH + clean IDRIS2_PREFIX isolation, so the fork compiler is
  -- still kept out of the numerator build. We only unset IDRIS2_BIN here so the
  -- child does not inherit the fork as its DEFAULT compiler.
  let cmd = "env -u IDRIS2_BIN "
          ++ "\"" ++ childExe ++ "\" paths --numerator-only"
          ++ " --ipkg \"" ++ ipkgPath ++ "\""
          ++ " --dumppaths-json \"" ++ dumpFile ++ "\""
          ++ " --hits-out \"" ++ hitsFile ++ "\""
          ++ " --network " ++ opts.network
          ++ canisterArg
          ++ " > " ++ logFile ++ " 2>&1"
  rc <- system cmd
  hitsResult <- loadPathHits (Just hitsFile)
  _ <- system $ "rm -f " ++ dumpFile ++ " " ++ hitsFile
  case hitsResult of
    Right hits =>
      if rc == 0
         then pure (Right hits)
         else if not (null hits)
                 then pure (Right hits)  -- child exited nonzero but produced hits
                 else do
                   logTail <- readFile logFile
                   let tailMsg = case logTail of
                                   Right c => unlines (reverse (take 15 (reverse (lines c))))
                                   Left _ => ""
                   pure $ Left $ "phase-2 numerator subprocess failed (rc=" ++ show rc
                              ++ ")\n" ++ tailMsg
    Left _ => pure (Right [])  -- no hits file → empty numerator (denominator still real)

runPathFullPipelineArtifacts : Options -> IO (Either String (String, List PathRuntimeHit))
runPathFullPipelineArtifacts opts = do
  ipkgResult <- resolveIpkgForPaths opts
  case ipkgResult of
    Left err => pure $ Left err
    Right ipkgPath => do
      let (projectDir, originalIpkgName) = splitPath ipkgPath
      -- Case-B universe alignment: the numerator instruments the forTestBuild WASM
      -- (Tests.AllTests universe), so the denominator must dump paths over the SAME
      -- universe for the identity join to match. Prefer the test-universe preparer;
      -- it returns the input unchanged when there is no Tests/AllTests entry, in
      -- which case we fall back to the canister-Main preparer.
      testUnivIpkg <- prepareTestUniverseDumppathsIpkg ipkgPath
      staticIpkgPath <- if testUnivIpkg /= ipkgPath
                           then pure testUnivIpkg
                           else prepareDumppathsIpkg ipkgPath
      let (staticProjectDir, staticIpkgName) = splitPath staticIpkgPath
      dumppathsResult <- the (IO (Either String String)) $
        case opts.dumppathsJson of
          Just path => do
            Right content <- readFile path
              | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
            pure $ Right content
          Nothing => do
            -- OOM-safe denominator: chunk large packages (core's producer peeks the
            -- module count and chunks > 40 modules into sequential groups of 8).
            -- The whole-package runDumppathsJsonPrepared OOM-kills on big canisters
            -- (GlobalRegistry, 181 modules → idris2 SIGKILL). Fall back to the whole
            -- build only if the chunked producer itself errs (small packages, or a
            -- chunking edge case).
            chunked <- CoreUnified.runStaticDumppathsJson staticIpkgPath
            case chunked of
              Right content => pure (Right content)
              Left chunkErr => do
                whole <- runDumppathsJsonPrepared staticIpkgPath
                pure $ case whole of
                  Right content => Right content
                  Left wholeErr => Left (chunkErr ++ "\n(whole-package fallback also failed) " ++ wholeErr)
      case dumppathsResult of
        Left err => do
          cleanupGeneratedDumppathsIpkg staticIpkgPath
          pure $ Left err
        Right dumppathsContent => if not opts.numeratorOnly
          then do
            -- ORCHESTRATOR (phase 1 done): denominator computed in THIS process
            -- (which ran the fork dep-install). Run the numerator (deploy + build +
            -- collect) in a FRESH subprocess so the fork-polluted in-process/env
            -- state can't break the pack-wrapper WASM build.
            cleanupGeneratedDumppathsIpkg staticIpkgPath
            hitsResult <- runNumeratorSubprocess opts ipkgPath dumppathsContent
            pure $ map (\hits => (dumppathsContent, hits)) hitsResult
          else
            -- PHASE 2 (numerator-only, fresh process): no fork dep-install ran in
            -- this process; do deploy + call + collect, then persist hits to hitsOut.
            runNumeratorInProcess opts ipkgPath staticProjectDir staticIpkgName
              staticIpkgPath projectDir originalIpkgName dumppathsContent

runPaths : Options -> IO ()
runPaths opts = do
  artifactsResult <-
    if opts.fullPipeline || opts.numeratorOnly
       -- numeratorOnly also routes here: runPathFullPipelineArtifacts takes its
       -- numerator-only sub-branch (reads the provided dumppaths, skips the
       -- denominator, deploys+collects, writes hits-out).
       then runPathFullPipelineArtifacts opts
       else do
         pathHitsResult <-
           case opts.pathHitsPath of
             Just _ => loadPathHits opts.pathHitsPath
             Nothing =>
               case (opts.profilingOutput, opts.publicNamesWasm) of
                 (Just profiling, Just wasm) => analyzePathHitsFromFiles profiling wasm
                 (Just _, Nothing) => pure $ Left "--profiling-output requires --public-names-wasm for path mode"
                 (Nothing, Just _) => pure $ Left "--public-names-wasm requires --profiling-output for path mode"
                 (Nothing, Nothing) => pure $ Right []
         case pathHitsResult of
           Left err => pure $ Left err
           Right hits => do
             contentResult <- case opts.dumppathsJson of
               Just path => do
                 Right content <- readFile path
                   | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
                 pure $ Right content
               Nothing => do
                 ipkgResult <- resolveIpkgForPaths opts
                 case ipkgResult of
                   Left err => pure $ Left err
                   Right ipkg => runDumppathsJson ipkg
             pure $ map (\content => (content, hits)) contentResult
  case artifactsResult of
    Left err => do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right (content, hits) =>
      case analyzePathCoverageFromContent defaultPathExclusions content hits of
        Left err => do
          putStrLn $ "Error: " ++ err
          exitWith (ExitFailure 1)
        Right result =>
          case opts.format of
            JSON => putStrLn $ pathCoverageReportToJson result
            Text => do
              putStrLn "# DFX Path Coverage Report"
              putStrLn $ "coverage_model:   " ++ result.coverageModel
              putStrLn $ "claim_admissible: " ++ show result.claimAdmissible
              putStrLn $ "coverage_percent: " ++ show (fromMaybe 100.0 result.coveragePercent)
              putStrLn ""
              putStrLn $ pathMeasurementSummary result.measurement
              putStrLn $ "Missing paths: " ++ show (length result.missingPaths)
              traverse_ (\p => putStrLn $ "- " ++ p.pathId ++ " :: " ++ pathSummary p) result.missingPaths

main : IO ()
main = do
  args <- getArgs
  let self = fromMaybe "idris2-dfx-cov" (head' args)
  let opts = parseArgs (drop 1 args) ({ selfPath := self } defaultOptions)
  if opts.showHelp
     then putStrLn helpText
     else if opts.showVersion
       then putStrLn versionText
       else case opts.subcommand of
              Just "paths" => runPaths opts
              _ => do
                putStrLn "Error: only the 'paths' subcommand is implemented"
                putStrLn "Use --help for usage information"
                exitWith (ExitFailure 1)
