||| CLI for idris2-icwasm build tool
module CLI

import System
import System.Directory
import System.File
import Data.String
import Data.List
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

Example:
  idris2-icwasm build --canister=my_canister --main=src/Main.idr
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
-- Main
-- =============================================================================

main : IO ()
main = do
  args <- getArgs
  case drop 1 args of  -- drop program name
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
                False  -- forTestBuild (CLI doesn't use test builds)
                Nothing -- testModulePath (CLI doesn't use test builds)
          result <- buildCanisterAuto buildOpts
          putStrLn $ show result
          case result of
            BuildSuccess _ => exitSuccess
            BuildError _ => exitFailure
    ("gen-entry" :: rest) => runGenEntry rest
    ("--help" :: _) => putStrLn usage
    ("-h" :: _) => putStrLn usage
    _ => do
      putStrLn "Unknown command. Use 'idris2-icwasm build', 'idris2-icwasm gen-entry', or 'idris2-icwasm --help'"
      exitFailure
