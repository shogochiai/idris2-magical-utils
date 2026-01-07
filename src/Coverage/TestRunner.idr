||| Test execution with profiling
||| REQ_COV_RUN_001 - REQ_COV_RUN_004
module Coverage.TestRunner

import Coverage.Types
import Coverage.Collector
import Data.List
import Data.List1
import Data.String
import System
import System.File
import System.Directory
import System.Clock

%default total

-- =============================================================================
-- Test File Discovery
-- =============================================================================

||| REQ_COV_RUN_001: Discover test files matching glob pattern
||| Simple glob matching (supports * wildcard)
||| Simplified: checks if pattern parts are present in order
export
matchGlob : String -> String -> Bool
matchGlob pattern path =
  let parts = forget $ split (== '*') pattern
  in matchParts parts path
  where
    matchParts : List String -> String -> Bool
    matchParts [] _ = True
    matchParts [p] s = isSuffixOf p s || p == ""
    matchParts (p :: ps) s =
      if p == ""
         then matchParts ps s
         else if isPrefixOf p s
                 then matchParts ps (pack $ drop (length p) (unpack s))
                 else False

-- =============================================================================
-- Test Execution Result
-- =============================================================================

||| Result of running a single test with profiling
public export
record TestProfileResult where
  constructor MkTestProfileResult
  testId     : String           -- Test file path or ID
  testPassed : Bool
  profileHits : List ProfileHit

public export
Show TestProfileResult where
  show r = "TestResult(\{r.testId}, passed=\{show r.testPassed}, hits=\{show $ length r.profileHits})"

-- =============================================================================
-- Test Compilation
-- =============================================================================

||| REQ_COV_RUN_002: Compile test with idris2 --profile
export
covering
compileWithProfile : (testFile : String) -> (outputName : String) -> IO (Either String String)
compileWithProfile testFile outputName = do
  -- Build command: idris2 --profile -o <output> <testFile>
  let cmd = "idris2 --profile -o \{outputName} \{testFile} 2>&1"
  exitCode <- system cmd
  if exitCode == 0
     then pure $ Right outputName
     else pure $ Left "Compilation failed for \{testFile}"

-- =============================================================================
-- Test Execution
-- =============================================================================

||| REQ_COV_RUN_003: Execute compiled test and capture profile.html
export
covering
executeWithProfile : (execPath : String) -> (workDir : String) -> IO (Either String (Bool, String))
executeWithProfile execPath workDir = do
  -- Run the executable
  let cmd = "cd \{workDir} && ./build/exec/\{execPath} 2>&1"
  exitCode <- system cmd
  let testPassed = exitCode == 0
  -- Profile HTML should be in workDir
  let profilePath = workDir ++ "/profile.html"
  exists <- do
    Right _ <- readFile profilePath
      | Left _ => pure False
    pure True
  if exists
     then pure $ Right (testPassed, profilePath)
     else pure $ Left "profile.html not found after execution"

-- =============================================================================
-- Combined Test Run
-- =============================================================================

||| Run a single test with profiling and collect results
||| REQ_COV_RUN_004: Associate profile results with test ID
export
covering
runTestWithProfile : (testFile : String) -> (workDir : String) -> IO (Either String TestProfileResult)
runTestWithProfile testFile workDir = do
  -- Generate output name from test file
  let baseName = pack $ reverse $ takeWhile (/= '/') $ reverse $ unpack testFile
  let outputName = pack $ takeWhile (/= '.') $ unpack baseName

  -- Compile
  Right _ <- compileWithProfile testFile outputName
    | Left err => pure $ Left err

  -- Execute
  Right (passed, profilePath) <- executeWithProfile outputName workDir
    | Left err => pure $ Left err

  -- Collect profile data
  let schemePath = workDir ++ "/build/exec/" ++ outputName ++ "_app/" ++ outputName ++ ".ss"
  Right hits <- collectFromFiles profilePath schemePath
    | Left err => pure $ Left err

  pure $ Right $ MkTestProfileResult testFile passed hits

-- =============================================================================
-- Batch Test Execution
-- =============================================================================

||| Run multiple tests and collect all profile results
export
covering
runAllTests : (testFiles : List String) -> (workDir : String) -> IO (List (Either String TestProfileResult))
runAllTests testFiles workDir =
  traverse (\f => runTestWithProfile f workDir) testFiles

-- =============================================================================
-- Test File Listing
-- =============================================================================

||| List test files in a directory matching pattern
||| Finds *AllTests.idr files in */Tests/* directories
export
covering
findTestFiles : (baseDir : String) -> (pattern : String) -> IO (List String)
findTestFiles baseDir pattern = do
  t <- clockTime UTC
  let uid = show (seconds t)
  let tmpFile = "/tmp/idris2_find_tests_" ++ uid ++ ".txt"
  let cmd = "find " ++ baseDir ++ " -name '*AllTests.idr' -path '*/Tests/*' > " ++ tmpFile ++ " 2>/dev/null"
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure []
  _ <- removeFile tmpFile
  let files = filter (not . null) $ map trim $ lines content
  pure files

||| Convert file path to module name
||| e.g., "src/Coverage/Tests/AllTests.idr" -> "Coverage.Tests.AllTests"
||| Strips common source directories (src/, lib/) and .idr extension
export
filePathToModule : String -> String
filePathToModule path =
  let noExt = if isSuffixOf ".idr" path
                 then pack $ reverse $ drop 4 $ reverse $ unpack path
                 else path
      parts = forget $ split (== '/') noExt
      -- Skip common source directories
      stripped = dropSourceDirs parts
  in joinBy "." stripped
  where
    dropSourceDirs : List String -> List String
    dropSourceDirs [] = []
    dropSourceDirs (x :: xs) =
      if x == "src" || x == "lib" || x == "." || x == ""
         then dropSourceDirs xs
         else x :: xs

||| Discover test modules from filesystem
||| Returns module names like "Coverage.Tests.AllTests"
export
covering
discoverTestModules : String -> IO (List String)
discoverTestModules baseDir = do
  files <- findTestFiles baseDir "*AllTests.idr"
  -- Convert absolute paths to relative by stripping baseDir prefix
  let relativePaths = map (stripPrefix baseDir) files
  pure $ map filePathToModule relativePaths
  where
    stripPrefix : String -> String -> String
    stripPrefix pre path =
      if isPrefixOf pre path
         then let stripped = pack $ drop (length pre) (unpack path)
              in if isPrefixOf "/" stripped
                    then pack $ drop 1 (unpack stripped)
                    else stripped
         else path
