||| CLI for idris2-wasm build tool
module CLI

import System
import System.Directory
import Data.String
import Data.List
import WasmBuilder.WasmBuilder

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
idris2-wasm - Build Idris2 to ICP canister WASM

Usage: idris2-wasm build [OPTIONS]

Options:
  --canister=NAME   Canister name (default: canister)
  --main=PATH       Main module path (default: src/Main.idr)
  --project=DIR     Project directory (default: .)
  --package=PKG     Additional package (can be repeated)
  -p=PKG            Short for --package
  --help, -h        Show this help

Example:
  idris2-wasm build --canister=my_canister --main=src/Main.idr
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
    ("--help" :: _) => putStrLn usage
    ("-h" :: _) => putStrLn usage
    _ => do
      putStrLn "Unknown command. Use 'idris2-wasm build' or 'idris2-wasm --help'"
      exitFailure
