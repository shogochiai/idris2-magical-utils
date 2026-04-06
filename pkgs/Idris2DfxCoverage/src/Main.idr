||| idris2-dfx-cov CLI
||| Path-level semantic coverage analysis for DFX/ICP projects.
module Main

import DfxCoverage.PathCoverage
import DfxCoverage.PathRuntime
import DfxCoverage.DumpcasesParser
import DfxCoverage.CanisterCall
import DfxCoverage.Exclusions

import Coverage.Standardization.Types
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

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing Text False False Nothing Nothing Nothing Nothing Nothing Nothing "local" False

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
      result <- runProjectMethod projectDir canisterRef method network
      case result of
        Right _ => go rest (method :: successes) errors
        Left err => go rest successes (err :: errors)

findIpkgInDir : String -> IO (Maybe String)
findIpkgInDir dir = do
  Right entries <- listDir dir
    | Left _ => pure Nothing
  let ipkgs = filter (isSuffixOf ".ipkg") entries
  let nonTemp = filter (not . isPrefixOf "temp-") ipkgs
  case nonTemp of
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

runDumppathsJson : String -> IO (Either String String)
runDumppathsJson ipkgPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  idris2Cmd <- resolveIdris2Command
  supported <- supportsDumppathsJson idris2Cmd
  if not supported
     then pure $ Left "Current Idris2 does not support --dumppaths-json. Set IDRIS2_BIN to your forked compiler."
     else do
       t <- time
       let outPath = "/tmp/idris2_dfx_cov_paths_" ++ show t ++ ".json"
       let logPath = "/tmp/idris2_dfx_cov_paths_" ++ show t ++ ".log"
       pkgPath <- discoverPackagePath
       let envPrefix = if null pkgPath then "" else "IDRIS2_PACKAGE_PATH=\"" ++ pkgPath ++ "\" "
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

runPathFullPipelineArtifacts : Options -> IO (Either String (String, List PathRuntimeHit))
runPathFullPipelineArtifacts opts = do
  ipkgResult <- resolveIpkgForPaths opts
  case ipkgResult of
    Left err => pure $ Left err
    Right ipkgPath => do
      let (projectDir, ipkgName) = splitPath ipkgPath
      dumppathsResult <-
        case opts.dumppathsJson of
          Just path => do
            Right content <- readFile path
              | Left err => pure $ Left $ "Failed to read dumppaths JSON: " ++ show err
            pure $ Right content
          Nothing => runDumppathsJson ipkgPath
      case dumppathsResult of
        Left err => pure $ Left err
        Right dumppathsContent => do
          staticResult <- runAndAnalyzeDumpcases projectDir ipkgName
          case staticResult of
            Left err => pure $ Left err
            Right staticAnalysis => do
              let defaultCanister = pack (takeWhile (/= '.') (unpack ipkgName))
              canister <- case opts.canisterName of
                Just name => pure name
                Nothing => detectCanisterName projectDir defaultCanister
              let deployOpts =
                    { canisterName := canister
                    , network := opts.network
                    , dfxPath := "dfx"
                    , projectDir := projectDir
                    , instrumentBranchProbes := True
                    , forTestBuild := True
                    } defaultDeployOptions
              deployResult <- ensureDeployed deployOpts
              case deployResult of
                Left err => pure $ Left err
                Right _ => do
                  callResult <- runProjectMethods
                                  projectDir
                                  canister
                                  ["runTests", "runMinimalTests", "runTrivialTest"]
                                  opts.network
                  case callResult of
                    Left err => pure $ Left err
                    Right _ => do
                      let branchProbeMapPath = projectDir ++ "/build/idris2-branch-probes.csv"
                      hitsResult <- analyzePathHitsFromBranchProbeFiles
                                     dumppathsContent
                                     staticAnalysis
                                     branchProbeMapPath
                                     (Just projectDir)
                                     canister
                                     opts.network
                      pure $ map (\hits => (dumppathsContent, hits)) hitsResult

runPaths : Options -> IO ()
runPaths opts = do
  artifactsResult <-
    if opts.fullPipeline
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
  let opts = parseArgs (drop 1 args) defaultOptions
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
