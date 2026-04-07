||| Unified test runner with coverage collection
||| REQ_COV_UNI_001 - REQ_COV_UNI_003
module Coverage.UnifiedRunner

import Coverage.Types
import public Coverage.Collector
import public Coverage.DumpcasesParser
import public Coverage.Core.RuntimeHit
import public Coverage.Core.PathCoverage
import System
import System.Clock
import System.File
import System.Directory
import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default covering

-- =============================================================================
-- Temporary File Generation
-- =============================================================================

||| Generate unique identifier from timestamp
getUniqueId : IO String
getUniqueId = do
  t <- clockTime Monotonic
  pure $ "test_" ++ show (seconds t) ++ "_" ++ show (nanoseconds t `mod` 100000)

||| Join strings with separator
joinStrings : String -> List String -> String
joinStrings sep [] = ""
joinStrings sep [x] = x
joinStrings sep (x :: xs) = x ++ sep ++ joinStrings sep xs

||| Shell prelude for using the installed Idris2 app resolved from a neutral directory.
||| This avoids local pack.toml interference when running inside target projects.
installedIdrisPrelude : String
installedIdrisPrelude =
  "APP=\"$(cd /tmp && pack app-path idris2)\""
  ++ " && export IDRIS2_PACKAGE_PATH=\"$(cd /tmp && pack package-path)\""
  ++ " && export IDRIS2_LIBS=\"$(cd /tmp && pack libs-path)\""
  ++ " && export IDRIS2_DATA=\"$(cd /tmp && pack data-path)\""

trimLog : String -> String
trimLog = trim

toAbsolutePath : String -> IO String
toAbsolutePath path =
  if isPrefixOf "/" path
     then pure path
     else do
       Just cwd <- currentDir
         | Nothing => pure path
       pure (cwd ++ "/" ++ path)

removeFileIfExistsSafe : String -> IO ()
removeFileIfExistsSafe path = do
  _ <- removeFile path
  pure ()

buildPrelude : Maybe String -> String
buildPrelude idris2Override =
  case idris2Override of
    Just app =>
      "APP=\"" ++ app ++ "\""
      ++ " && export IDRIS2_PACKAGE_PATH=\"$(cd /tmp && pack package-path)\""
      ++ " && export IDRIS2_LIBS=\"$(cd /tmp && pack libs-path)\""
      ++ " && export IDRIS2_DATA=\"$(cd /tmp && pack data-path)\""
    Nothing => installedIdrisPrelude

||| Build an ipkg, preferring direct Idris2 and falling back to pack.
||| Returns captured failure logs so downstream callers can surface the real cause.
buildIpkgWithClean : Bool -> String -> String -> IO (Either String ())
buildIpkgWithClean cleanFirst projectDir ipkgName = do
  idris2Override <- getEnv "IDRIS2_BIN"
  absProjectDir <- toAbsolutePath projectDir
  let directLog = absProjectDir ++ "/.idris2-coverage-build-direct.log"
  let packLog = absProjectDir ++ "/.idris2-coverage-build-pack.log"
  removeFileIfExistsSafe directLog
  removeFileIfExistsSafe packLog

  let appPrelude = buildPrelude idris2Override
  when cleanFirst $
    do let cleanCmd = appPrelude
                   ++ " && cd " ++ absProjectDir
                   ++ " && \"$APP\" --clean " ++ ipkgName ++ " > /dev/null 2>&1"
       _ <- system cleanCmd
       pure ()
  let directCmd = appPrelude
               ++ " && cd " ++ absProjectDir
               ++ " && \"$APP\" --build " ++ ipkgName ++ " > " ++ directLog ++ " 2>&1"
  directExit <- system directCmd
  if directExit == 0
     then do
       removeFileIfExistsSafe directLog
       pure $ Right ()
     else do
       directMsg <- readFile directLog
       case idris2Override of
         Just _ => do
           removeFileIfExistsSafe directLog
           let directSummary = either (\err => "unable to read direct-build log: " ++ show err)
                                      trimLog directMsg
           pure $ Left $ "direct idris2 build failed: " ++ directSummary
         Nothing => do
           let packCmd = "cd " ++ absProjectDir ++ " && pack build " ++ ipkgName ++ " > " ++ packLog ++ " 2>&1"
           packExit <- system packCmd
           if packExit == 0
              then do
                removeFileIfExistsSafe directLog
                removeFileIfExistsSafe packLog
                pure $ Right ()
              else do
                packMsg <- readFile packLog
                removeFileIfExistsSafe directLog
                removeFileIfExistsSafe packLog
                let directSummary = either (\err => "unable to read direct-build log: " ++ show err)
                                           trimLog directMsg
                let packSummary = either (\err => "unable to read pack-build log: " ++ show err)
                                         trimLog packMsg
                pure $ Left $
                  "direct idris2 build failed: " ++ directSummary ++ "\n"
                  ++ "pack fallback failed: " ++ packSummary

buildIpkg : String -> String -> IO (Either String ())
buildIpkg = buildIpkgWithClean False

||| Generate temporary test runner source code
||| The test modules must export a `runAllTests : IO ()` function
generateTempRunner : String -> List String -> String
generateTempRunner modName testModules = unlines
  [ "module " ++ modName
  , ""
  , unlines (map (\m => "import " ++ m) testModules)
  , ""
  , "main : IO ()"
  , "main = do"
  , unlines (map (\m => "  " ++ m ++ ".runAllTests") testModules)
  ]

||| Generate a temporary wrapper that imports all project modules.
||| This forces Idris2 to emit case trees for library packages with no executable.
generateImportWrapper : String -> List String -> String
generateImportWrapper modName modules = unlines
  [ "module " ++ modName
  , ""
  , unlines (map (\m => "import " ++ m) modules)
  , ""
  , "main : IO ()"
  , "main = pure ()"
  ]

||| Generate temporary .ipkg file
||| @depends - Additional package dependencies (e.g., from target project's ipkg)
||| @sourcedir - Source directory (from target project's ipkg, defaults to "src")
||| @dumpcasesPath - Optional path for --dumpcases output (Nothing = profile only)
generateTempIpkgWithOpts : String -> String -> List String -> String -> List String -> String -> Maybe String -> Maybe String -> String
generateTempIpkgWithOpts pkgName mainMod modules execName depends sourcedir opts builddir =
  let allDepends = "base, contrib, idris2-coverage" ++
        (if null depends then "" else ", " ++ joinStrings ", " depends)
      optsLine = case opts of
        Just optStr => ["opts = \"" ++ optStr ++ "\""]
        Nothing => []
      builddirLine = case builddir of
        Just dir => ["builddir = \"" ++ dir ++ "\""]
        Nothing => []
  in unlines
    ( [ "package " ++ pkgName ]
   ++ optsLine
   ++ builddirLine
   ++ [ "sourcedir = \"" ++ sourcedir ++ "\""
      , "main = " ++ mainMod
      , "executable = " ++ execName
      , "depends = " ++ allDepends
      , "modules = " ++ joinStrings ", " modules
      ])

generateTempIpkg : String -> String -> List String -> String -> List String -> String -> Maybe String -> String
generateTempIpkg pkgName mainMod modules execName depends sourcedir dumpcasesPath =
  let opts = case dumpcasesPath of
        Just path => Just ("--profile --dumpcases " ++ path)
        Nothing => Just "--profile"
  in generateTempIpkgWithOpts pkgName mainMod modules execName depends sourcedir opts Nothing

||| Generate pack.toml with idris2-coverage dependency from GitHub
|||
||| This function creates a pack.toml that references idris2-coverage from GitHub,
||| ensuring portability across different development environments.
|||
||| If the target project already has a pack.toml with custom dependencies,
||| those are preserved and merged with the idris2-coverage reference.
|||
||| @projectPackToml - Content of project's existing pack.toml (empty string if none)
||| @return - Complete pack.toml content with idris2-coverage dependency
generateTempPackToml : String -> String
generateTempPackToml projectPackToml =
  let coverageDef = unlines
        [ "# Auto-generated: idris2-coverage dependency for test profiling"
        , "# This enables Coverage.Profiler to track function hits during test execution"
        , "[custom.all.idris2-coverage]"
        , "type   = \"github\""
        , "url    = \"git@github.com:shogochiai/idris2-magical-utils.git\""
        , "commit = \"latest\""
        , "ipkg   = \"pkgs/Idris2Coverage/idris2-coverage.ipkg\""
        ]
  in if projectPackToml == ""
       then coverageDef
       else projectPackToml ++ "\n\n" ++ coverageDef

||| Read project's existing pack.toml content if it exists
|||
||| This preserves any custom package dependencies the target project may have.
||| For example, if the project depends on non-public packages via GitHub URLs,
||| those dependencies will be merged into the generated pack.toml.
|||
||| @projectDir - Directory containing the project
||| @return - Content of pack.toml, or empty string if not found
readProjectPackToml : String -> IO String
readProjectPackToml projectDir = do
  let packPath = projectDir ++ "/pack.toml"
  Right content <- readFile packPath
    | Left _ => pure ""
  pure content

||| Check if a file exists
fileExists : String -> IO Bool
fileExists path = do
  Right _ <- readFile path
    | Left _ => pure False
  pure True

||| Write pack.toml only if it doesn't exist, return True if we created it
writePackTomlIfMissing : String -> String -> IO (Either String Bool)
writePackTomlIfMissing packTomlPath packTomlContent = do
  exists <- fileExists packTomlPath
  if exists
    then pure $ Right False  -- Didn't create, already exists
    else do
      Right () <- writeFile packTomlPath packTomlContent
        | Left err => pure $ Left $ "Failed to write pack.toml: " ++ show err
      pure $ Right True  -- Created it

-- =============================================================================
-- Test Output Parsing
-- =============================================================================

||| Safe tail of string - returns empty string if input is empty
safeTail : String -> String
safeTail s = if s == "" then "" else assert_total (strTail s)

||| Parse test output format: [PASS] TestName or [FAIL] TestName: message
covering
parseTestOutput : String -> List TestResult
parseTestOutput output =
  mapMaybe parseLine (lines output)
  where
    covering
    parseLine : String -> Maybe TestResult
    parseLine line =
      let trimmed = trim line
      in if isPrefixOf "[PASS]" trimmed
           then Just $ MkTestResult (trim $ substr 6 (length trimmed) trimmed) True Nothing
         else if isPrefixOf "[FAIL]" trimmed
           then
             let rest = trim $ substr 6 (length trimmed) trimmed
                 (name, msg) = break (== ':') rest
             in Just $ MkTestResult (trim name) False
                  (if msg == "" then Nothing else Just (trim $ safeTail msg))
         else Nothing

-- =============================================================================
-- File Cleanup
-- =============================================================================

||| Remove a file if it exists (ignore errors)
removeFileIfExists : String -> IO ()
removeFileIfExists path = do
  _ <- removeFile path
  pure ()

||| Remove pack.toml only if we created it
cleanupPackToml : String -> Bool -> IO ()
cleanupPackToml packTomlPath True = removeFileIfExists packTomlPath
cleanupPackToml _ False = pure ()

||| Clean up temporary files
cleanupTempFiles : String -> String -> String -> String -> IO ()
cleanupTempFiles tempIdr tempIpkg ssHtml profileHtml = do
  removeFileIfExists tempIdr
  removeFileIfExists tempIpkg
  removeFileIfExists ssHtml
  removeFileIfExists profileHtml

-- =============================================================================
-- Ipkg Parsing
-- =============================================================================

||| Parse depends from ipkg content, handling multi-line continuation
||| Returns list of package names from "depends = pkg1, pkg2, ..." lines
||| Supports continuation lines starting with ',' or whitespace
parseIpkgDepends : String -> List String
parseIpkgDepends content =
  let ls = lines content
      (_, fromDepends) = break (isPrefixOf "depends" . trim) ls
  in case fromDepends of
       [] => []
       (firstLine :: rest) =>
         let continuations = takeWhile isContinuation rest
             allLines = firstLine :: continuations
             joined = fastConcat $ intersperse " " (map trim allLines)
             afterEquals = trim $ snd $ break (== '=') joined
             pkgStr = if isPrefixOf "=" afterEquals
                        then trim (substr 1 (length afterEquals) afterEquals)
                        else afterEquals
         in map trim $ filter (/= "") $ forget $ split (== ',') pkgStr
  where
    isContinuation : String -> Bool
    isContinuation s =
      let trimmed = ltrim s
      in not (null trimmed) &&
         (isPrefixOf "," trimmed) &&
         not (isInfixOf "=" trimmed)

||| Parse sourcedir from ipkg content (defaults to "src")
parseIpkgSourcedir : String -> String
parseIpkgSourcedir content =
  let ls = lines content
      sourcedirLines = filter (isPrefixOf "sourcedir") (map trim ls)
  in case sourcedirLines of
       [] => "src"
       (line :: _) =>
         let afterEquals = trim $ snd $ break (== '=') line
             stripped = if isPrefixOf "=" afterEquals
                          then trim (substr 1 (length afterEquals) afterEquals)
                          else afterEquals
         in trim $ pack $ filter (/= '"') (unpack stripped)

||| Parse modules from ipkg content, handling multi-line continuation.
parseIpkgModules : String -> List String
parseIpkgModules content =
  let ls = lines content
      moduleLines = collectModuleLines ls False
      joined = fastConcat $ intersperse " " moduleLines
      afterEq = case break (== '=') (unpack joined) of
                  (_, rest) => pack $ drop 1 rest
      parts = forget $ split (== ',') afterEq
  in map (trim . pack . filter isModuleChar . unpack . trim) parts
  where
    isModuleChar : Char -> Bool
    isModuleChar c = isAlphaNum c || c == '.' || c == '_'

    collectModuleLines : List String -> Bool -> List String
    collectModuleLines [] _ = []
    collectModuleLines (l :: ls) False =
      if isInfixOf "modules" l && isInfixOf "=" l
         then l :: collectModuleLines ls True
         else collectModuleLines ls False
    collectModuleLines (l :: ls) True =
      let trimmed = trim l
      in if null trimmed
            then collectModuleLines ls True
            else if isPrefixOf "," trimmed || isPrefixOf " " l || isPrefixOf "\t" l
                    then l :: collectModuleLines ls True
                    else []

||| Find and read first matching ipkg file content
||| Prefers files without -minimal, -test, -temp prefixes
findIpkgContent : String -> IO (Maybe String)
findIpkgContent projectDir = do
  -- Try to find any .ipkg file in the directory
  Right entries <- listDir projectDir
    | Left _ => pure Nothing
  let ipkgFiles = filter (isSuffixOf ".ipkg") entries
  -- Filter out minimal/test/temp ipkg files
  let isMainIpkg = \f => not (isInfixOf "-minimal" f || isInfixOf "-test" f || isPrefixOf "temp-" f)
  let mainIpkgs = filter isMainIpkg ipkgFiles
  -- Prefer main ipkg, fall back to any ipkg
  let chosen = case mainIpkgs of
                 [] => ipkgFiles
                 xs => xs
  case chosen of
    [] => pure Nothing
    (f :: _) => do
      Right content <- readFile (projectDir ++ "/" ++ f)
        | Left _ => pure Nothing
      pure (Just content)

||| Read depends from project's ipkg file
public export
readProjectDepends : String -> IO (List String)
readProjectDepends projectDir = do
  Just content <- findIpkgContent projectDir
    | Nothing => pure []
  pure $ parseIpkgDepends content

||| Read sourcedir from project's ipkg file (defaults to "src")
public export
readProjectSourcedir : String -> IO String
readProjectSourcedir projectDir = do
  Just content <- findIpkgContent projectDir
    | Nothing => pure "src"
  pure $ parseIpkgSourcedir content

||| Split "path/to/project.ipkg" into ("path/to", "project.ipkg")
splitIpkgPathLocal : String -> (String, String)
splitIpkgPathLocal path =
  let parts = forget $ split (== '/') path
  in case parts of
       [] => (".", path)
       [x] => (".", x)
       _ => case initLast parts of
              Nothing => (".", path)
              Just (dirParts, lastPart) =>
                (fastConcat $ intersperse "/" dirParts, lastPart)
  where
    initLast : List a -> Maybe (List a, a)
    initLast [] = Nothing
    initLast [x] = Just ([], x)
    initLast (x :: xs) = case initLast xs of
      Nothing => Just ([], x)
      Just (ys, z) => Just (x :: ys, z)

||| Collect dumpcases for a project by generating a temporary executable wrapper.
||| This avoids the "already built package emits no dumpcases" problem.
public export
runProjectDumpcasesWithTempIpkg : (ipkgPath : String) -> IO (Either String String)
runProjectDumpcasesWithTempIpkg ipkgPath = do
  let (projectDir, _) = splitIpkgPathLocal ipkgPath
  Right ipkgContent <- readFile ipkgPath
    | Left err => pure $ Left $ "Failed to read ipkg: " ++ show err

  let projectDepends = parseIpkgDepends ipkgContent
  let sourcedir = parseIpkgSourcedir ipkgContent
  let projectModules = parseIpkgModules ipkgContent

  case projectModules of
    [] => pure $ Left "No modules found in ipkg"
    _ => do
      uid <- getUniqueId
      let tempModName = "TempDumpcases_" ++ uid
      let tempExecName = "temp-dumpcases-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let tempIpkgName = tempExecName ++ ".ipkg"
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml
      let dumpcasesPath = "/tmp/idris2_dumpcases_project_" ++ uid ++ ".txt"

      let runnerSource = generateImportWrapper tempModName projectModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp wrapper: " ++ show err

      let allModules = tempModName :: projectModules
      let ipkgContent = generateTempIpkg tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just dumpcasesPath)
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      buildResult <- buildIpkgWithClean True projectDir tempIpkgName
      case buildResult of
        Left err => do
          removeFileIfExists tempIdrPath
          removeFileIfExists tempIpkgPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath
          pure $ Left $ "Build with --dumpcases failed: " ++ err
        Right () => do
          Right dumpContent <- readFile dumpcasesPath
            | Left err => do
                removeFileIfExists tempIdrPath
                removeFileIfExists tempIpkgPath
                cleanupPackToml packTomlPath createdPackToml
                removeFileIfExists dumpcasesPath
                pure $ Left $ "Failed to read dumpcases output: " ++ show err

          removeFileIfExists tempIdrPath
          removeFileIfExists tempIpkgPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath

          if null (trim dumpContent)
             then pure $ Left "No dumpcases output generated"
             else pure $ Right dumpContent

-- =============================================================================
-- Main Entry Point
-- =============================================================================

||| Extended report with semantic coverage from --dumpcases on test binary
public export
record TestCoverageReportExt where
  constructor MkTestCoverageReportExt
  baseReport     : TestCoverageReport
  testCoverage   : TestCoverage           -- From test binary's --dumpcases
  functionHits   : List FunctionRuntimeHit -- Per-function runtime coverage (NEW)

public export
Show TestCoverageReportExt where
  show r = show r.baseReport ++ " | Test: " ++ show r.testCoverage.executedCanonical
        ++ "/" ++ show r.testCoverage.totalCanonical
        ++ " | " ++ show (length r.functionHits) ++ " functions"

||| REQ_COV_UNI_001: Run tests with profiling and return combined report
||| REQ_COV_UNI_002: Clean up all temporary files
||| REQ_COV_UNI_003: Exclude test modules from coverage calculation
|||
||| @projectDir - Path to project root (containing .ipkg)
||| @testModules - List of test module names (e.g., ["Module.Tests.AllTests"])
||| @timeout - Max seconds for build+run (default 120)
export
runTestsWithCoverage : (projectDir : String)
                     -> (testModules : List String)
                     -> (timeout : Nat)
                     -> IO (Either String TestCoverageReport)
runTestsWithCoverage projectDir testModules timeout = do
  -- Validate inputs
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ => do
      -- Read project dependencies and sourcedir
      projectDepends <- readProjectDepends projectDir
      sourcedir <- readProjectSourcedir projectDir

      -- Generate unique names
      uid <- getUniqueId
      let tempModName = "TempTestRunner_" ++ uid
      let tempExecName = "temp-test-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      -- Chez Scheme profiler generates .ss.html in the current working directory (where executable runs)
      let ssHtmlPath = projectDir ++ "/" ++ tempExecName ++ ".ss.html"
      let profileHtmlPath = projectDir ++ "/profile.html"
      let execPath = projectDir ++ "/build/exec/" ++ tempExecName

      -- Generate temp runner source
      let runnerSource = generateTempRunner tempModName testModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      -- Generate temp .ipkg (test modules only - Coverage.* comes from idris2-coverage package)
      let allModules = tempModName :: testModules
      let ipkgContent = generateTempIpkg tempExecName tempModName allModules tempExecName projectDepends sourcedir Nothing
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml

      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      -- Write pack.toml for idris2-coverage resolution (only if not present)
      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      -- Prefer the installed Idris2 app, but keep pack as fallback for projects
      -- that still rely on pack-managed dependency resolution.
      buildResult <- buildIpkg projectDir (tempExecName ++ ".ipkg")
      case buildResult of
        Left err => do
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml
          pure $ Left $ "Build failed: " ++ err
        Right () => do
          -- Run executable and capture output
          -- Use relative path from projectDir (./build/exec/...) since we cd there
          let relExecPath = "./build/exec/" ++ tempExecName
          let runCmd = "cd " ++ projectDir ++ " && " ++ relExecPath ++ " 2>&1"
          runResult <- system runCmd
          -- Note: test failures shouldn't fail the whole run

          -- Read test output (need to capture it properly)
          -- For now, we'll read from a temp output file
          -- Use relative path for shell command (after cd) but absolute for Idris readFile
          let relOutputFile = "./temp_test_output_" ++ uid ++ ".txt"
          let absOutputFile = projectDir ++ "/temp_test_output_" ++ uid ++ ".txt"
          _ <- system $ "cd " ++ projectDir ++ " && " ++ relExecPath ++ " > " ++ relOutputFile ++ " 2>&1"

          Right testOutput <- readFile absOutputFile
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                pure $ Left "Failed to read test output"

          removeFileIfExists absOutputFile

          -- Parse test results
          let testResults = parseTestOutput testOutput
          let passedCount = length $ filter (.passed) testResults
          let failedCount = length $ filter (not . (.passed)) testResults

          -- Read and parse coverage data
          Right ssHtml <- readFile ssHtmlPath
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                -- Return results without coverage if .ss.html not found
                t <- clockTime UTC
                let timestamp = show (seconds t)
                let emptyBranch = MkBranchCoverageSummary 0 0 0 0.0 []
                pure $ Right $ MkTestCoverageReport testResults
                  (length testResults) passedCount failedCount emptyBranch timestamp

          -- Read Scheme source for function definitions
          let ssPath = projectDir ++ "/build/exec/" ++ tempExecName ++ "_app/" ++ tempExecName ++ ".ss"
          Right ssContent <- readFile ssPath
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                pure $ Left "Failed to read .ss file"

          -- Parse coverage
          -- Note: Test module exclusion disabled due to Chez Scheme linking issues
          -- REQ_COV_UNI_003 temporarily not enforced
          let funcDefs = parseSchemeDefs ssContent
          let branchPoints = parseBranchCoverage ssHtml
          let branchSummary = summarizeBranchCoverageWithFunctions funcDefs branchPoints

          -- Get timestamp
          t <- clockTime UTC
          let timestamp = show (seconds t)

          -- REQ_COV_UNI_002: Clean up
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml

          pure $ Right $ MkTestCoverageReport
            testResults
            (length testResults)
            passedCount
            failedCount
            branchSummary
            timestamp

-- =============================================================================
-- Extended Entry Point with --dumpcases on Test Binary
-- =============================================================================

||| Run tests with semantic coverage from test binary's --dumpcases
||| This is the key function for accurate coverage: analyze the SAME binary that runs
|||
||| @projectDir - Path to project root (containing .ipkg)
||| @testModules - List of test module names
||| @timeout - Max seconds for build+run
export
runTestsWithTestCoverage : (projectDir : String)
                          -> (testModules : List String)
                          -> (timeout : Nat)
                          -> IO (Either String TestCoverage)
runTestsWithTestCoverage projectDir testModules timeout = do
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ => do
      -- Read project dependencies and sourcedir
      projectDepends <- readProjectDepends projectDir
      sourcedir <- readProjectSourcedir projectDir

      -- Generate unique names
      uid <- getUniqueId
      let tempModName = "TempTestRunner_" ++ uid
      let tempExecName = "temp-test-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let tempIpkgName = tempExecName ++ ".ipkg"
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml
      let ssHtmlPath = projectDir ++ "/" ++ tempExecName ++ ".ss.html"
      let profileHtmlPath = projectDir ++ "/profile.html"
      let dumpcasesPath = "/tmp/idris2_dumpcases_test_" ++ uid ++ ".txt"

      -- Generate temp runner source
      let runnerSource = generateTempRunner tempModName testModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      -- Generate temp .ipkg
      let allModules = tempModName :: testModules
      let ipkgContent = generateTempIpkg tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just dumpcasesPath)
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      -- Write pack.toml for idris2-coverage resolution (only if not present)
      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      -- Build with --dumpcases on test binary
      -- Use pack build to ensure proper package resolution (avoids ambiguous identifier issues)
      -- The --dumpcases flag is passed via opts in the ipkg file
      putStrLn $ "Dumping case trees to " ++ dumpcasesPath
      buildResult <- buildIpkg projectDir tempIpkgName
      case buildResult of
        Left err => do
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath
          pure $ Left $ "Build with --dumpcases failed: " ++ err
        Right () => do
          -- Parse --dumpcases output from test binary
          Right dumpContent <- readFile dumpcasesPath
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                cleanupPackToml packTomlPath createdPackToml
                pure $ Left "Failed to read dumpcases output"

          let funcs = parseDumpcasesFile dumpContent
          let analysis = aggregateAnalysis funcs

          -- Run executable with profiler
          let relExecPath = "./build/exec/" ++ tempExecName
          let relOutputFile = "./temp_test_output_" ++ uid ++ ".txt"
          let absOutputFile = projectDir ++ "/temp_test_output_" ++ uid ++ ".txt"
          _ <- system $ "cd " ++ projectDir ++ " && " ++ relExecPath ++ " > " ++ relOutputFile ++ " 2>&1"

          -- Read .ss.html for profiler hits
          Right ssHtml <- readFile ssHtmlPath
            | Left _ => do
                -- Return static analysis with 0 executed if no profiler output
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                cleanupPackToml packTomlPath createdPackToml
                removeFileIfExists dumpcasesPath
                removeFileIfExists absOutputFile
                pure $ Right $ MkTestCoverage "test-binary" analysis.totalCanonical analysis.totalExcluded 0

          -- Parse profiler output for executed branches
          let branchPoints : List BranchPoint = parseBranchCoverage ssHtml
          let executed : Nat = length $ filter (\bp => bp.coveredBranches > 0) branchPoints

          -- Cleanup
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath
          removeFileIfExists absOutputFile

          pure $ Right $ MkTestCoverage
            "test-binary"
            analysis.totalCanonical
            analysis.totalExcluded
            (cast executed)

-- =============================================================================
-- Extended Entry Point with Per-Function Runtime Hits
-- =============================================================================

||| Run tests and return per-function runtime coverage data
||| This is the recommended API for accurate severity calculation
|||
||| @projectDir - Path to project root (containing .ipkg)
||| @testModules - List of test module names
||| @timeout - Max seconds for build+run
export
runTestsWithFunctionHits : (projectDir : String)
                          -> (testModules : List String)
                          -> (timeout : Nat)
                          -> IO (Either String (List FunctionRuntimeHit))
runTestsWithFunctionHits projectDir testModules timeout = do
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ => do
      -- Read project dependencies and sourcedir
      projectDepends <- readProjectDepends projectDir
      sourcedir <- readProjectSourcedir projectDir

      -- Generate unique names
      uid <- getUniqueId
      let tempModName = "TempTestRunner_" ++ uid
      let tempExecName = "temp-test-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let tempIpkgName = tempExecName ++ ".ipkg"
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml
      let ssHtmlPath = projectDir ++ "/" ++ tempExecName ++ ".ss.html"
      let profileHtmlPath = projectDir ++ "/profile.html"
      let dumpcasesPath = "/tmp/idris2_dumpcases_fh_" ++ uid ++ ".txt"
      let ssPath = projectDir ++ "/build/exec/" ++ tempExecName ++ "_app/" ++ tempExecName ++ ".ss"

      -- Generate temp runner source
      let runnerSource = generateTempRunner tempModName testModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      -- Generate temp .ipkg
      let allModules = tempModName :: testModules
      let ipkgContent = generateTempIpkg tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just dumpcasesPath)
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      -- Write pack.toml for idris2-coverage resolution (only if not present)
      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      -- Build with --dumpcases on test binary
      -- Use pack build to ensure proper package resolution (avoids ambiguous identifier issues)
      -- The --dumpcases flag is passed via opts in the ipkg file
      buildResult <- buildIpkg projectDir tempIpkgName
      case buildResult of
        Left err => do
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath
          pure $ Left $ "Build with --dumpcases failed: " ++ err
        Right () => do
          -- Parse --dumpcases output for static analysis
          Right dumpContent <- readFile dumpcasesPath
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                cleanupPackToml packTomlPath createdPackToml
                pure $ Left "Failed to read dumpcases output"

          let funcs = parseDumpcasesFile dumpContent

          -- Run executable with profiler
          let relExecPath = "./build/exec/" ++ tempExecName
          _ <- system $ "cd " ++ projectDir ++ " && " ++ relExecPath ++ " 2>&1"

          -- Read .ss.html for profiler hits
          Right ssHtml <- readFile ssHtmlPath
            | Left _ => do
                -- Return static-only data with 0 executed
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                cleanupPackToml packTomlPath createdPackToml
                removeFileIfExists dumpcasesPath
                let staticHits = map (\f => MkFunctionRuntimeHit f.fullName f.fullName
                                      (countCanonical f.cases) 0 0 0) funcs
                pure $ Right staticHits

          -- Read .ss for function definitions
          Right ssContent <- readFile ssPath
            | Left _ => do
                cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
                cleanupPackToml packTomlPath createdPackToml
                removeFileIfExists dumpcasesPath
                pure $ Left "Failed to read .ss file"

          -- Match functions with profiler data
          let functionHits = matchAllFunctionsWithCoverage funcs ssHtml ssContent

          -- Cleanup
          cleanupTempFiles tempIdrPath tempIpkgPath ssHtmlPath profileHtmlPath
          cleanupPackToml packTomlPath createdPackToml
          removeFileIfExists dumpcasesPath

          pure $ Right functionHits
  where
    countCanonical : List CompiledCase -> Nat
    countCanonical = length . filter (\c => c.kind == Canonical)

parsePathHitLineLocal : String -> Maybe PathRuntimeHit
parsePathHitLineLocal line =
  let trimmed = trim line in
  if null trimmed || isPrefixOf "#" trimmed
     then Nothing
     else case forget (split (== ',') trimmed) of
            [pathId] => Just (MkPathRuntimeHit (trim pathId) 1)
            [pathId, countStr] =>
              let parsed = fromMaybe 1 (parsePositive (trim countStr))
              in Just (MkPathRuntimeHit (trim pathId) parsed)
            _ => Just (MkPathRuntimeHit trimmed 1)

||| Build and run test modules with forked Idris2 path instrumentation enabled.
||| Returns static dumppaths JSON plus runtime path hits from the executed test binary.
export
runTestsWithPathCoverageArtifacts : (projectDir : String)
                                 -> (projectModules : List String)
                                 -> (testModules : List String)
                                 -> (timeout : Nat)
                                 -> IO (Either String (String, List PathRuntimeHit))
runTestsWithPathCoverageArtifacts projectDir projectModules testModules timeout = do
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ => do
      projectDepends <- readProjectDepends projectDir
      sourcedir <- readProjectSourcedir projectDir

      uid <- getUniqueId
      let tempModName = "TempPathRunner_" ++ uid
      let tempExecName = "temp-paths-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let tempIpkgName = tempExecName ++ ".ipkg"
      let tempBuildDir = ".idris2-coverage-build-" ++ uid
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml
      let dumppathsPath = "/tmp/idris2_dumppaths_runtime_" ++ uid ++ ".json"
      let pathHitsPath = "/tmp/idris2_pathhits_runtime_" ++ uid ++ ".txt"
      let relExecPath = "./" ++ tempBuildDir ++ "/exec/" ++ tempExecName

      let runnerSource = generateTempRunner tempModName testModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      let allModules = nub (tempModName :: (projectModules ++ testModules))
      let opts = "--dumppaths-json " ++ dumppathsPath ++ " --dumppathshits " ++ pathHitsPath
      let ipkgContent = generateTempIpkgWithOpts tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just opts) (Just tempBuildDir)
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      buildResult <- buildIpkgWithClean True projectDir tempIpkgName
      case buildResult of
        Left err => do
          removeFileIfExists tempIdrPath
          removeFileIfExists tempIpkgPath
          cleanupPackToml packTomlPath createdPackToml
          _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
          removeFileIfExists dumppathsPath
          removeFileIfExists pathHitsPath
          pure $ Left $ "Build with path instrumentation failed: " ++ err
        Right () => do
          Right dumppathsContent <- readFile dumppathsPath
            | Left err => do
                removeFileIfExists tempIdrPath
                removeFileIfExists tempIpkgPath
                cleanupPackToml packTomlPath createdPackToml
                _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
                removeFileIfExists dumppathsPath
                removeFileIfExists pathHitsPath
                pure $ Left $ "Failed to read dumppaths JSON: " ++ show err

          _ <- system $ "cd " ++ projectDir ++ " && " ++ relExecPath ++ " > /dev/null 2>&1"

          hitsContent <- readFile pathHitsPath
          let hits = case hitsContent of
                       Left _ => []
                       Right content => mapMaybe parsePathHitLineLocal (lines content)

          removeFileIfExists tempIdrPath
          removeFileIfExists tempIpkgPath
          cleanupPackToml packTomlPath createdPackToml
          _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
          removeFileIfExists dumppathsPath
          removeFileIfExists pathHitsPath

          pure $ Right (dumppathsContent, hits)
