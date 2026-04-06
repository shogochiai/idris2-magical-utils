||| CLI for idris2-icwasm build tool
module CLI

import System
import System.Directory
import System.File
import Data.String
import Data.List
import Data.Maybe
import WasmBuilder.WasmBuilder
import WasmBuilder.CandidStubs
import WasmBuilder.CanisterEntryGen

%default covering

-- =============================================================================
-- CLI Options
-- =============================================================================

record Options where
  constructor MkOptions
  canisterName : String
  mainModule : String
  projectDir : String
  packages : List String
  showHelp : Bool

defaultOptions : Options
defaultOptions = MkOptions
  { canisterName = "canister"
  , mainModule = "src/Main.idr"
  , projectDir = "."
  , packages = ["contrib"]
  , showHelp = False
  }

-- =============================================================================
-- Argument Parsing
-- =============================================================================

||| Parse --key=value style argument
parseKeyValue : String -> Maybe (String, String)
parseKeyValue arg =
  case break (== '=') arg of
    (key, val) => if val == "" then Nothing
                  else Just (key, assert_total $ strTail val)  -- drop '='

parseArgs : List String -> Options
parseArgs args = go defaultOptions args
  where
    go : Options -> List String -> Options
    go opts [] = opts
    go opts ("--help" :: rest) = go ({ showHelp := True } opts) rest
    go opts ("-h" :: rest) = go ({ showHelp := True } opts) rest
    go opts (arg :: rest) =
      case parseKeyValue arg of
        Just ("--canister", val) => go ({ canisterName := val } opts) rest
        Just ("--main", val) => go ({ mainModule := val } opts) rest
        Just ("--project", val) => go ({ projectDir := val } opts) rest
        Just ("--package", val) => go ({ packages $= (val ::) } opts) rest
        Just ("-p", val) => go ({ packages $= (val ::) } opts) rest
        _ => go opts rest  -- Skip unknown args

-- =============================================================================
-- Main
-- =============================================================================

usage : String
usage = """
idris2-icwasm - Build Idris2 to ICP canister WASM

Usage: idris2-icwasm <command> [OPTIONS]

Commands:
  build       Compile Idris2 to IC WASM canister
  build-canister  Run shared canister build pipeline from config
  gen-entry   Generate canister_entry.c from a .did file

Build Options:
  --canister=NAME   Canister name (default: canister)
  --main=PATH       Main module path (default: src/Main.idr)
  --project=DIR     Project directory (default: .)
  --package=PKG     Additional package (can be repeated)
  -p=PKG            Short for --package
  --help, -h        Show this help

Gen-entry Options:
  --did=PATH        Path to .did file (required)
  --prefix=NAME     FFI prefix, e.g. "theworld" (required)
  --lib=NAME        Library name, e.g. "libic0" (informational)
  --init=FUNC       Init function name called in canister_init (optional)
  --out=PATH        Output C file path (default: canister_entry_gen.c)
  --timer-cmd=N     Generate canister_global_timer with CMD N (recommended)
  --heartbeat-cmd=N Generate canister_heartbeat with CMD N (deprecated, use --timer-cmd)
  --sql-stable      Emit sqlite_stable_save in pre_upgrade (auto if --init=sql_ffi_open)

Cmd-map Annotations:
  methodName=N @inject_time=SLOT        Inject ic0_time()/1e9 into arg slot
  methodName=N @inject_const=SLOT:VAL   Inject constant value into arg slot
  methodName=N @inject_time_plus=S:V    Inject ic0_time()/1e9 + V into arg slot
  methodName=N @result_fn=FUNC          Use custom result function for reply
  methodName=N @deferred_reply=FUNC     External hook performs async call and replies in callback

Build-canister Options:
  --config=PATH     Path to canister-build.toml (required)

Example:
  idris2-icwasm build --canister=my_canister --main=src/Main.idr
  idris2-icwasm build-canister --config=pkgs/Idris2TheWorld/canister-build.toml
  idris2-icwasm gen-entry --did=can.did --prefix=theworld --lib=libic0 --out=build/canister_entry_gen.c
"""

||| Resolve project directory to absolute path
resolveProjectDir : String -> IO String
resolveProjectDir dir = do
  if dir == "."
    then do
      Just cwd <- currentDir
        | Nothing => pure "."
      pure cwd
    else pure dir

-- =============================================================================
-- gen-entry options
-- =============================================================================

record GenEntryOptions where
  constructor MkGenEntryOptions
  didPath              : String
  ffiPfx               : String
  libName              : String
  initFn               : String
  outPath              : String
  cmdMapPath           : String     -- "" = auto-number from 0
  heartbeatCmd         : Maybe Nat  -- --heartbeat-cmd=N (deprecated)
  heartbeatCheckpoint  : Maybe Nat  -- --heartbeat-checkpoint=N (deprecated)
  timerCmd             : Maybe Nat  -- --timer-cmd=N
  sqlStable            : Maybe Bool -- --sql-stable (auto-detected from --init=sql_ffi_open)

defaultGenEntryOptions : GenEntryOptions
defaultGenEntryOptions = MkGenEntryOptions
  { didPath             = ""
  , ffiPfx              = ""
  , libName             = ""
  , initFn              = ""
  , outPath             = "canister_entry_gen.c"
  , cmdMapPath          = ""
  , heartbeatCmd        = Nothing
  , heartbeatCheckpoint = Nothing
  , timerCmd            = Nothing
  , sqlStable           = Nothing
  }

parseGenEntryArgs : List String -> GenEntryOptions
parseGenEntryArgs args = go defaultGenEntryOptions args
  where
    go : GenEntryOptions -> List String -> GenEntryOptions
    go opts [] = opts
    go opts (arg :: rest) =
      case parseKeyValue arg of
        Just ("--did",           val) => go ({ didPath      := val } opts) rest
        Just ("--prefix",        val) => go ({ ffiPfx       := val } opts) rest
        Just ("--lib",           val) => go ({ libName      := val } opts) rest
        Just ("--init",          val) => go ({ initFn       := val } opts) rest
        Just ("--out",           val) => go ({ outPath      := val } opts) rest
        Just ("--cmd-map",       val) => go ({ cmdMapPath   := val } opts) rest
        Just ("--heartbeat-cmd", val) => go ({ heartbeatCmd := parsePositive val } opts) rest
        Just ("--heartbeat-checkpoint", val) => go ({ heartbeatCheckpoint := parsePositive val } opts) rest
        Just ("--timer-cmd", val) => go ({ timerCmd := parsePositive val } opts) rest
        Just ("--sql-stable", _) => go ({ sqlStable := Just True } opts) rest
        _                             => if arg == "--sql-stable"
                                           then go ({ sqlStable := Just True } opts) rest
                                           else go opts rest

runGenEntry : List String -> IO ()
runGenEntry args = do
  let opts = parseGenEntryArgs args
  when (opts.didPath == "") $ do
    putStrLn "Error: --did=<path> is required"
    exitFailure
  when (opts.ffiPfx == "") $ do
    putStrLn "Error: --prefix=<name> is required"
    exitFailure
  -- Read .did file
  Right didContent <- readFile opts.didPath
    | Left err => do
        putStrLn $ "Error reading " ++ opts.didPath ++ ": " ++ show err
        exitFailure
  -- Read optional cmd-map file (with annotation support)
  cmdMapEntries <- if opts.cmdMapPath == ""
                     then pure []
                     else do
                       Right content <- readFile opts.cmdMapPath
                         | Left err => do
                             putStrLn $ "Error reading " ++ opts.cmdMapPath ++ ": " ++ show err
                             exitFailure
                       pure (parseCmdMapEntries content)
  -- Auto-detect --sql-stable: any --init function implies SQLite usage.
  -- You can't init a canister DB without needing stable persistence.
  -- Explicit --sql-stable=false is respected (escape hatch for non-SQLite init).
  let autoSqlStable = opts.initFn /= ""
      sqlStableFlag = case opts.sqlStable of
                        Just b  => b      -- explicit override
                        Nothing => autoSqlStable  -- auto: init → stable save
  let methods = parseDidFile didContent
      defs    = parseTypeDefinitions didContent
      genOpts = MkGenOptions opts.ffiPfx opts.libName opts.initFn opts.heartbeatCmd opts.heartbeatCheckpoint opts.timerCmd sqlStableFlag
      output  = generateCanisterEntryFull genOpts methods defs cmdMapEntries
  -- Write output file
  Right () <- writeFile opts.outPath output
    | Left err => do
        putStrLn $ "Error writing " ++ opts.outPath ++ ": " ++ show err
        exitFailure
  putStrLn $ "Generated " ++ opts.outPath ++
             " (" ++ show (length methods) ++ " methods)" ++
             maybe "" (\n => " [heartbeat CMD=" ++ show n ++ "]") opts.heartbeatCmd ++
             maybe "" (\n => " [timer CMD=" ++ show n ++ "]") opts.timerCmd ++
             (if sqlStableFlag then " [sql-stable]" else "")

-- =============================================================================
-- build-canister options
-- =============================================================================

record BuildCanisterOptions where
  constructor MkBuildCanisterOptions
  configPath : String

defaultBuildCanisterOptions : BuildCanisterOptions
defaultBuildCanisterOptions = MkBuildCanisterOptions ""

parseBuildCanisterArgs : List String -> BuildCanisterOptions
parseBuildCanisterArgs args = go defaultBuildCanisterOptions args
  where
    go : BuildCanisterOptions -> List String -> BuildCanisterOptions
    go opts [] = opts
    go opts (arg :: rest) =
      case parseKeyValue arg of
        Just ("--config", val) => go ({ configPath := val } opts) rest
        _ => go opts rest

shellQuote : String -> String
shellQuote str = "'" ++ go (unpack str) ++ "'"
  where
    go : List Char -> String
    go [] = ""
    go ('\'' :: rest) = "'\\''" ++ go rest
    go (c :: rest) = strCons c (go rest)

dirnamePath : String -> String
dirnamePath path =
  let rev = reverse (unpack path)
      (fileRev, rest) = break (== '/') rev in
  case rest of
    [] => "."
    (_ :: dirRev) => case reverse dirRev of
      [] => "/"
      chars => pack chars

pathExists : String -> IO Bool
pathExists path = do
  code <- system $ "test -f " ++ shellQuote path
  pure (code == 0)

dropLeadingSpaces : String -> String
dropLeadingSpaces str = pack (dropWhile (\c => c == ' ' || c == '\t') (unpack str))

stripQuotes : String -> Maybe String
stripQuotes str =
  case unpack (trim str) of
    ('"' :: rest) =>
      Just $ pack $ takeWhile (/= '"') rest
    ('\'' :: rest) =>
      Just $ pack $ takeWhile (/= '\'') rest
    _ => Nothing

extractTomlString : String -> String -> Maybe String
extractTomlString key content = go (lines content)
  where
    go : List String -> Maybe String
    go [] = Nothing
    go (line :: rest) =
      let trimmed = trim line in
      case break (== '=') trimmed of
        (lhs, rhs) =>
          if trim lhs == key && rhs /= ""
            then stripQuotes (dropLeadingSpaces (assert_total $ strTail rhs))
            else go rest

resolveAgainstDir : String -> String -> String
resolveAgainstDir base path =
  if isPrefixOf "/" path then path else base ++ "/" ++ path

resolveCommandPath : String -> IO (Maybe String)
resolveCommandPath prog = do
  let tmpFile = "/tmp/idris2-icwasm-command-path.txt"
  code <- system $ "command -v " ++ shellQuote prog ++ " > " ++ shellQuote tmpFile ++ " 2>/dev/null"
  if code /= 0
    then pure Nothing
    else do
      Right content <- readFile tmpFile
        | Left _ => pure Nothing
      pure $ Just (trim content)

resolveAbsoluteDir : String -> IO (Maybe String)
resolveAbsoluteDir path = do
  let tmpFile = "/tmp/idris2-icwasm-dir-path.txt"
      cmd = "python3 -c " ++
            shellQuote "import os, sys; print(os.path.dirname(os.path.abspath(sys.argv[1])))" ++
            " " ++ shellQuote path ++
            " > " ++ shellQuote tmpFile ++ " 2>/dev/null"
  code <- system cmd
  if code /= 0
    then pure Nothing
    else do
      Right content <- readFile tmpFile
        | Left _ => pure Nothing
      pure $ Just (trim content)

firstExisting : List String -> IO (Maybe String)
firstExisting [] = pure Nothing
firstExisting (path :: rest) = do
  exists <- pathExists path
  if exists then pure (Just path) else firstExisting rest

findBuildCanisterLauncher : String -> IO (Maybe String)
findBuildCanisterLauncher prog = do
  let exeDir = dirnamePath prog
  absExeDir <- resolveAbsoluteDir prog
  resolvedProg <- resolveCommandPath prog
  let resolvedExeDir = maybe "" dirnamePath resolvedProg
  absResolvedExeDir <- case resolvedProg of
    Just path => resolveAbsoluteDir path
    Nothing => pure Nothing
  Just cwd <- currentDir
    | Nothing => firstExisting
        [ exeDir ++ "/../../scripts/build-canister-from-config.sh"
        , maybe "" (\dir => dir ++ "/../../scripts/build-canister-from-config.sh") absExeDir
        , resolvedExeDir ++ "/../../scripts/build-canister-from-config.sh"
        , maybe "" (\dir => dir ++ "/../../scripts/build-canister-from-config.sh") absResolvedExeDir
        , "scripts/build-canister-from-config.sh"
        , "./scripts/build-canister-from-config.sh"
        ]
  firstExisting
    [ exeDir ++ "/../../scripts/build-canister-from-config.sh"
    , maybe "" (\dir => dir ++ "/../../scripts/build-canister-from-config.sh") absExeDir
    , resolvedExeDir ++ "/../../scripts/build-canister-from-config.sh"
    , maybe "" (\dir => dir ++ "/../../scripts/build-canister-from-config.sh") absResolvedExeDir
    , cwd ++ "/" ++ exeDir ++ "/../../scripts/build-canister-from-config.sh"
    , cwd ++ "/scripts/build-canister-from-config.sh"
    , cwd ++ "/pkgs/Idris2IcWasm/scripts/build-canister-from-config.sh"
    ]

findBuildCanisterLauncherFromConfig : String -> IO (Maybe String)
findBuildCanisterLauncherFromConfig configPath = do
  Right content <- readFile configPath
    | Left _ => pure Nothing
  configDir <- resolveAbsoluteDir configPath
  let localBinary = extractTomlString "local_idris2_icwasm" content
      supportDir = extractTomlString "icwasm_support" content
  localBinaryDir <- case (configDir, localBinary) of
    (Just dir, Just path) => resolveAbsoluteDir (resolveAgainstDir dir path)
    _ => pure Nothing
  supportRoot <- case (configDir, supportDir) of
    (Just dir, Just path) => do
      supportParent <- resolveAbsoluteDir (resolveAgainstDir dir path)
      case supportParent of
        Just p => resolveAbsoluteDir p
        Nothing => pure Nothing
    _ => pure Nothing
  firstExisting $
    catMaybes
      [ map (\dir => dir ++ "/../../scripts/build-canister-from-config.sh") localBinaryDir
      , map (\dir => dir ++ "/scripts/build-canister-from-config.sh") supportRoot
      ]

runBuildCanister : String -> List String -> IO ()
runBuildCanister prog args = do
  let opts = parseBuildCanisterArgs args
  when (opts.configPath == "") $ do
    putStrLn "Error: --config=<path> is required"
    exitFailure
  launcherFromConfig <- findBuildCanisterLauncherFromConfig opts.configPath
  launcher <- the (IO (Maybe String)) $
    case launcherFromConfig of
      Just path => pure (Just path)
      Nothing => findBuildCanisterLauncher prog
  case launcher of
    Nothing => do
      putStrLn "Error: build-canister launcher not found."
      putStrLn "Run from the Idris2IcWasm checkout or point canister-build.toml at the local Idris2IcWasm paths."
      exitFailure
    Just launcherPath => do
      let cmd = "IDRIS2_ICWASM_BINARY=" ++ shellQuote prog ++
                " bash " ++ shellQuote launcherPath ++
                " --config=" ++ shellQuote opts.configPath
      code <- system cmd
      if code == 0 then exitSuccess else exitFailure

-- =============================================================================
-- Main
-- =============================================================================

main : IO ()
main = do
  args <- getArgs
  case args of
    [] => putStrLn usage
    (prog :: rest) =>
      case rest of
        [] => putStrLn usage
        ("build" :: rest) => do
          let opts = parseArgs rest
          if opts.showHelp
            then putStrLn usage
            else do
              absProjectDir <- resolveProjectDir opts.projectDir
              let buildOpts = MkBuildOptions
                    absProjectDir
                    opts.canisterName
                    opts.mainModule
                    opts.packages
                    True   -- generateSourceMap
                    False  -- instrumentBranchProbes
                    False  -- forTestBuild (CLI doesn't use test builds)
                    Nothing -- testModulePath (CLI doesn't use test builds)
              result <- buildCanisterAuto buildOpts
              putStrLn $ show result
              case result of
                BuildSuccess _ => exitSuccess
                BuildError _ => exitFailure
        ("build-canister" :: rest) => runBuildCanister prog rest
        ("gen-entry" :: rest) => runGenEntry rest
        ("--help" :: _) => putStrLn usage
        ("-h" :: _) => putStrLn usage
        _ => do
          putStrLn "Unknown command. Use 'idris2-icwasm build', 'idris2-icwasm build-canister', 'idris2-icwasm gen-entry', or 'idris2-icwasm --help'"
          exitFailure
