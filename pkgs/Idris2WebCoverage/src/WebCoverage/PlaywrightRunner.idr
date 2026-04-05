||| Playwright Coverage Runner
|||
||| Executes JavaScript with V8 coverage collection via Playwright browser.
||| Uses system calls to run Node.js Playwright script (no FFI required).
module WebCoverage.PlaywrightRunner

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Directory
import Execution.Standardization.Web

import WebCoverage.Types

%default covering

shellQuote : String -> String
shellQuote s = "'" ++ concatMap escape (unpack s) ++ "'"
  where
    escape : Char -> String
    escape '\'' = "'\\''"
    escape c = singleton c

getUniqueId : IO String
getUniqueId = do
  Right h <- popen "date +%s%N 2>/dev/null || date +%s" Read
    | Left _ => pure "0"
  Right s <- fGetLine h
    | Left _ => do _ <- pclose h; pure "0"
  _ <- pclose h
  pure (trim s)

runShellCapture : String -> IO (Either String String)
runShellCapture cmd = do
  uid <- getUniqueId
  let outputPath = "/tmp/idris2_web_cov_shell_" ++ uid ++ ".log"
  let wrapped = "(" ++ cmd ++ ") > " ++ shellQuote outputPath ++ " 2>/dev/null"
  exitCode <- system wrapped
  Right output <- readFile outputPath
    | Left err => do
        _ <- removeFile outputPath
        pure $ Left $ "Failed to read shell output: " ++ show err
  _ <- removeFile outputPath
  if exitCode == 0
     then pure $ Right (trim output)
     else pure $ Left (trim output)

-- =============================================================================
-- Playwright Script Generation
-- =============================================================================

||| Generate inline Playwright script for coverage collection
playwrightScript : String -> String -> String
playwrightScript jsPath outputPath = unlines
  [ "const { chromium } = require('playwright');"
  , ""
  , "(async () => {"
  , "  const browser = await chromium.launch({ headless: true });"
  , "  const page = await browser.newPage();"
  , ""
  , "  await page.setContent('<html><head></head><body><div id=\"app\"></div></body></html>');"
  , ""
  , "  await page.coverage.startJSCoverage({ resetOnNavigation: false, reportAnonymousScripts: true });"
  , ""
  , "  await page.addScriptTag({ path: '" ++ jsPath ++ "' });"
  , ""
  , "  await new Promise(r => setTimeout(r, 1000));"
  , ""
  , "  const coverage = await page.coverage.stopJSCoverage();"
  , "  await browser.close();"
  , ""
  , "  const fs = require('fs');"
  , "  fs.writeFileSync('" ++ outputPath ++ "', JSON.stringify(coverage));"
  , "})().catch(e => { console.error(e); process.exit(1); });"
  ]

-- =============================================================================
-- V8 JSON Parsing (same as V8Runner)
-- =============================================================================

-- Find pattern in char list
findPattern : List Char -> List Char -> Maybe (List Char)
findPattern _ [] = Nothing
findPattern pattern cs@(_ :: rest) =
  if take (length pattern) cs == pattern
    then Just (drop (length pattern) cs)
    else findPattern pattern rest

-- Take digits from char list
takeDigits : List Char -> Nat
takeDigits cs =
  let digits = takeWhile isDigit (dropWhile (not . isDigit) cs)
  in fromMaybe 0 (parsePositive (pack digits))

-- Parse V8 coverage ranges from JSON
parseV8Ranges : String -> List V8Range
parseV8Ranges json = extractRanges (unpack json)
  where
    extractRanges : List Char -> List V8Range
    extractRanges [] = []
    extractRanges cs =
      case findPattern (unpack "\"startOffset\":") cs of
        Nothing => []
        Just afterStart =>
          let startNum = takeDigits afterStart
          in case findPattern (unpack "\"endOffset\":") afterStart of
               Nothing => []
               Just afterEnd =>
                 let endNum = takeDigits afterEnd
                 in case findPattern (unpack "\"count\":") afterEnd of
                      Nothing => []
                      Just afterCount =>
                        let countNum = takeDigits afterCount
                            range = MkV8Range (jsByteOffsetFromNat startNum) (jsByteOffsetFromNat endNum) countNum
                        in range :: extractRanges afterCount

-- Parse script name from URL field
parseScriptUrl : String -> Maybe String
parseScriptUrl json =
  case findPattern (unpack "\"url\":\"") (unpack json) of
    Nothing => Nothing
    Just afterUrl =>
      let url = takeWhile (/= '"') afterUrl
      in Just (pack url)

-- =============================================================================
-- Playwright Coverage Collection
-- =============================================================================

resolveNodeModulesPath : String -> IO (Either String String)
resolveNodeModulesPath projectDir = do
  envPath <- getEnv "IDRIS2_WEB_COVERAGE_NODE_MODULES"
  globalRoot <- runShellCapture "npm root -g 2>/dev/null || true"
  let candidates =
        filter (/= "")
          ([fromMaybe "" envPath, projectDir ++ "/node_modules"]
            ++ [either (const "") id globalRoot])
  tryCandidates candidates
  where
    tryCandidates : List String -> IO (Either String String)
    tryCandidates [] =
      pure $ Left $
        "Playwright runtime not available. Install `playwright` in the target project, "
        ++ "set IDRIS2_WEB_COVERAGE_NODE_MODULES, or make it available via `npm root -g`."
    tryCandidates (candidate :: rest) = do
      let resolveCmd =
            "NODE_PATH=" ++ shellQuote candidate
            ++ " node -e " ++ shellQuote "require.resolve('playwright')"
            ++ " >/dev/null 2>&1"
      exitCode <- system resolveCmd
      if exitCode == 0
         then pure $ Right candidate
         else tryCandidates rest

||| Run JavaScript file in Playwright browser and collect V8 coverage
|||
||| @jsPath - Path to compiled JavaScript file
||| @timeout - Timeout in seconds
export
runDomTestCoverage : (projectDir : String) -> (jsPath : String) -> IO (Either String V8ScriptCoverage)
runDomTestCoverage projectDir jsPath = do
  putStrLn "    [Playwright] Starting browser coverage collection..."
  -- Generate unique ID for temp files
  uid <- getUniqueId
  let scriptPath = "/tmp/playwright_cov_" ++ uid ++ ".js"
  let outputPath = "/tmp/playwright_cov_" ++ uid ++ ".json"
  let logPath = "/tmp/playwright_cov_" ++ uid ++ ".log"

  -- Write Playwright script
  let script = playwrightScript jsPath outputPath
  Right () <- writeFile scriptPath script
    | Left err => pure $ Left $ "Failed to write script: " ++ show err

  Right nodeModulesPath <- resolveNodeModulesPath projectDir
    | Left err => do
        cleanup scriptPath outputPath logPath
        pure $ Left err

  let cmd =
        "cd " ++ shellQuote projectDir
        ++ " && NODE_PATH=" ++ shellQuote nodeModulesPath
        ++ " node " ++ shellQuote scriptPath
        ++ " > " ++ shellQuote logPath ++ " 2>&1"
  exitCode <- system cmd
  if exitCode /= 0
     then do
       runLog <- readFile logPath
       cleanup scriptPath outputPath logPath
       pure $ Left $
         "Playwright coverage failed: "
         ++ either (\_ => "unable to read Playwright log") trim runLog
     else do
       -- Read coverage output
       Right covJson <- readFile outputPath
         | Left _ => do
             cleanup scriptPath outputPath logPath
             pure $ Left "Failed to read coverage output"

       -- Cleanup temp files
       cleanup scriptPath outputPath logPath

       -- Parse coverage JSON
       let ranges = parseV8Ranges covJson
       let scriptName = fromMaybe jsPath (parseScriptUrl covJson)
       let func = MkV8FunctionCoverage "_aggregate_" ranges False
       pure $ Right $ MkV8ScriptCoverage scriptName [func]
  where
    cleanup : String -> String -> String -> IO ()
    cleanup s o l = do
      _ <- removeFile s
      _ <- removeFile o
      _ <- removeFile l
      pure ()

||| Collect Playwright coverage (alias for runDomTestCoverage)
export
collectPlaywrightCoverage : (jsPath : String) -> (timeout : Nat)
                          -> IO (Either String V8ScriptCoverage)
collectPlaywrightCoverage jsPath _ = runDomTestCoverage "." jsPath
