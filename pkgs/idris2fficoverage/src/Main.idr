module Main

import FfiCoverage.Model
import Integration.Model
import Tests.AllTests
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Directory

%default covering

data OutputFormat = Text | Json

record Options where
  constructor MkOptions
  command : String
  target : String
  steps : List Nat
  format : OutputFormat
  tracePath : Maybe String
  registryPath : Maybe String
  expectPr : Maybe String
  expectNetwork : Maybe String
  expectCanister : Maybe String
  help : Bool

defaultOptions : Options
defaultOptions = MkOptions "help" "." [] Text Nothing Nothing Nothing Nothing Nothing False

parseStepsToken : String -> List Nat
parseStepsToken raw =
  mapMaybe (\s => parseInteger {a=Integer} s >>= \n => if n > 0 then Just (cast n) else Nothing)
           (forget (split (== ',') raw))

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("ask" :: rest) opts = parseArgs rest ({ command := "ask" } opts)
parseArgs ("dump-paths" :: rest) opts = parseArgs rest ({ command := "dump-paths" } opts)
parseArgs ("dump-s" :: rest) opts = parseArgs rest ({ command := "dump-s" } opts)
parseArgs ("--help" :: rest) opts = parseArgs rest ({ help := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ help := True } opts)
parseArgs ("--format" :: "json" :: rest) opts = parseArgs rest ({ format := Json } opts)
parseArgs ("--format" :: "text" :: rest) opts = parseArgs rest ({ format := Text } opts)
parseArgs ("--json" :: rest) opts = parseArgs rest ({ format := Json } opts)
parseArgs ("--text" :: rest) opts = parseArgs rest ({ format := Text } opts)
parseArgs ("--trace" :: path :: rest) opts = parseArgs rest ({ tracePath := Just path } opts)
parseArgs ("--registry" :: path :: rest) opts = parseArgs rest ({ registryPath := Just path } opts)
parseArgs ("--expect-pr" :: pr :: rest) opts = parseArgs rest ({ expectPr := Just pr } opts)
parseArgs ("--expect-network" :: network :: rest) opts = parseArgs rest ({ expectNetwork := Just network } opts)
parseArgs ("--expect-canister" :: canister :: rest) opts = parseArgs rest ({ expectCanister := Just canister } opts)
parseArgs ("--steps" :: raw :: rest) opts = parseArgs rest ({ steps := parseStepsToken raw } opts)
parseArgs (arg :: rest) opts =
  if isPrefixOf "--steps=" arg
    then parseArgs rest ({ steps := parseStepsToken (substr 8 (length arg) arg) } opts)
  else if isPrefixOf "-" arg
    then parseArgs rest opts
  else parseArgs rest ({ target := arg } opts)

helpText : String
helpText = """
idris2-ffi-cov - Integration FFI path coverage

USAGE:
  idris2-ffi-cov ask [repo-root] [--steps=1,2,3,4] [--trace <file>] [--registry <jsonl>] [--format json|text]
                    [--expect-pr N] [--expect-network ic] [--expect-canister id]
  idris2-ffi-cov dump-paths [repo-root] [--registry <jsonl>] [--format json|text]

STEPS:
  1 root SPEC.toml integration path declarations
  2 TaskTree integration route references
  3 declaration/evidence/idempotency classification
  4 trace coverage over declared Integration FFI paths

DEFAULT TRACE PATHS:
  .luci/integration/traces/integration.jsonl
  .luci/integration/traces/integration.trace
  .lazy/integration-trace.txt
  .luci/integration-trace.txt

DEFAULT REGISTRY PATHS:
  ci/integration-registry.jsonl
  INTEGRATION.jsonl
  .lazy/integration-registry.jsonl
"""

readMaybe : String -> IO String
readMaybe path = do
  Right content <- readFile path
    | Left _ => pure ""
  pure content

existsFile : String -> IO Bool
existsFile path = do
  r <- System.File.Meta.exists path
  pure r

taskTreeFiles : String -> IO (List String)
taskTreeFiles root = do
  let out = "/tmp/idris2-ffi-cov-tasktrees.txt"
  _ <- system $ "find " ++ root ++ " -path '*/docs/tasktrees/*.toml' -o -path '*/docs_archive/tasktrees/*.toml' 2>/dev/null > " ++ out
  content <- readMaybe out
  pure (filter (/= "") (lines content))

readMany : List String -> IO String
readMany [] = pure ""
readMany (path :: rest) = do
  here <- readMaybe path
  there <- readMany rest
  pure (here ++ "\n" ++ there)

defaultTracePath : String -> IO (Maybe String)
defaultTracePath root = do
  let candidates =
        [ root ++ "/.luci/integration/traces/integration.jsonl"
        , root ++ "/.luci/integration/traces/integration.trace"
        , root ++ "/.lazy/integration-trace.txt"
        , root ++ "/.luci/integration-trace.txt"
        ]
  findFirst candidates
  where
    findFirst : List String -> IO (Maybe String)
    findFirst [] = pure Nothing
    findFirst (p :: ps) = do
      ok <- existsFile p
      if ok then pure (Just p) else findFirst ps

defaultRegistryPath : String -> IO (Maybe String)
defaultRegistryPath root = do
  let candidates =
        [ root ++ "/ci/integration-registry.jsonl"
        , root ++ "/INTEGRATION.jsonl"
        , root ++ "/.lazy/integration-registry.jsonl"
        ]
  findFirst candidates
  where
    findFirst : List String -> IO (Maybe String)
    findFirst [] = pure Nothing
    findFirst (p :: ps) = do
      ok <- existsFile p
      if ok then pure (Just p) else findFirst ps

loadRegistry : Options -> IO (List FfiPath)
loadRegistry opts = do
  selectedPath <- case opts.registryPath of
                    Just p => pure (Just p)
                    Nothing => defaultRegistryPath opts.target
  case selectedPath of
    Nothing => pure FfiCoverage.Model.defaultRegistry
    Just p => do
      content <- readMaybe p
      let parsed = FfiCoverage.Model.registryFromJsonLines content
      if null parsed
        then pure FfiCoverage.Model.defaultRegistry
        else pure parsed

selected : Nat -> List Nat -> Bool
selected = stepSelected

containsJsonStringField : String -> String -> String -> Bool
containsJsonStringField field value line =
  isInfixOf ("\"" ++ field ++ "\":\"" ++ value ++ "\"") line

containsJsonNatField : String -> String -> String -> Bool
containsJsonNatField field value line =
  isInfixOf ("\"" ++ field ++ "\":" ++ value) line

lineMatchesExpectedContext : Options -> String -> Bool
lineMatchesExpectedContext opts line =
  maybe True (\pr => containsJsonNatField "prNumber" pr line) opts.expectPr
    && maybe True (\network => containsJsonStringField "network" network line) opts.expectNetwork
    && maybe True (\canister => containsJsonStringField "canister" canister line) opts.expectCanister

filterTraceContext : Options -> String -> String
filterTraceContext opts trace =
  unlines (filter (lineMatchesExpectedContext opts) (lines trace))

syntheticPassedTraceEvent : FfiPath -> String
syntheticPassedTraceEvent path =
  "{\"schema\":\"etherclaw.integration.trace.v1\","
    ++ "\"opId\":\"" ++ somePathId path ++ "\","
    ++ "\"outcome\":\"pass\","
    ++ "\"evidenceIds\":" ++ FfiCoverage.Model.renderStringArray (someEvidenceIds path)
    ++ "}"

syntheticTraceForRefs : List FfiPath -> List String -> String
syntheticTraceForRefs registry refs =
  unlines (map syntheticPassedTraceEvent (pathsFromRefs refs registry))

runAsk : Options -> IO ()
runAsk opts = do
  let root = opts.target
  registry <- loadRegistry opts
  rootSpec <- readMaybe (root ++ "/SPEC.toml")
  taskFiles <- taskTreeFiles root
  taskText <- readMany taskFiles
  mTrace <- case opts.tracePath of
              Just p => pure (Just p)
              Nothing => defaultTracePath root
  traceText <- case mTrace of
                 Just p => readMaybe p
                 Nothing => pure ""
  let requiredRefs = requiredRefsForStepsWithRegistry opts.steps registry rootSpec taskText
  let step1Status = if selected 1 opts.steps
                      then if rootSpec == "" then StepNoInput else StepOK
                      else StepSkipped
  let step2Status = if selected 2 opts.steps
                      then if taskText == "" then StepNoInput else StepOK
                      else StepSkipped
  let step3Status = if selected 3 opts.steps then StepOK else StepSkipped
  let step4Status = if selected 4 opts.steps
                      then if traceText == "" then StepNoInput else StepOK
                      else StepSkipped
  let effectiveTrace = if selected 4 opts.steps then filterTraceContext opts traceText else syntheticTraceForRefs registry requiredRefs
  let report = buildReportWithRegistry registry requiredRefs effectiveTrace
        [ ("root-spec", step1Status)
        , ("tasktree-routes", step2Status)
        , ("ffi-contracts", step3Status)
        , ("trace-coverage", step4Status)
        ]
  case opts.format of
    Json => putStrLn (renderReportJson report)
    Text => putStr (renderReportText report)
  when (gapCount report > 0 || not (null (incompleteStatuses report))) exitFailure

runDumpPaths : Options -> IO ()
runDumpPaths opts = do
  registry <- loadRegistry opts
  case opts.format of
    Json => putStrLn $ "{\"paths\":" ++ FfiCoverage.Model.renderStringArray (map somePathId registry) ++ "}"
    Text => putStr (unlines (map somePathId registry))

runDumpS : Options -> IO ()
runDumpS opts = do
  registry <- loadRegistry opts
  putStrLn "# Integration FFI Coverage Specs"
  putStrLn ""
  putStrLn "Package: integration"
  putStrLn ""
  putStrLn "## Specs"
  putStr (unlines (map renderOne (zip [1..length registry] registry)))
  putStrLn ""
  putStrLn $ "Total: " ++ show (length registry) ++ " specs"
  where
    pad3 : Nat -> String
    pad3 n =
      if n < 10 then "00" ++ show n
      else if n < 100 then "0" ++ show n
      else show n

    renderOne : (Nat, FfiPath) -> String
    renderOne (idx, path) =
      "- [REQ_INTFFI_" ++ pad3 idx ++ "] " ++ somePathId path
        ++ " SHALL be declared as an Integration FFI path with boundary "
        ++ show (someBoundary path) ++ ", risk " ++ show (someRisk path)
        ++ ", adapter `" ++ someAdapter path ++ "`, and evidence "
        ++ show (someEvidenceIds path)

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs args defaultOptions
  let (_, ffiCoverageTestFailures) = runAllTestsPure
  when (ffiCoverageTestFailures > 0) $ do
    putStrLn $ "idris2-ffi-coverage internal tests failed: " ++ show ffiCoverageTestFailures
    exitFailure
  if opts.help
    then putStr helpText
    else case opts.command of
      "ask" => runAsk opts
      "dump-paths" => runDumpPaths opts
      "dump-s" => runDumpS opts
      _ => putStr helpText
