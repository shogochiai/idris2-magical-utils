||| Playwright Coverage API
|||
||| High-level API for collecting V8 coverage from browser execution.
||| Designed for idris2-dom-mvc testing.
module Playwright.Coverage

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Playwright.Types
import Playwright.FFI

%default covering

-- =============================================================================
-- Coverage JSON Parsing
-- =============================================================================

-- Simple JSON number extraction
takeDigits : List Char -> Nat
takeDigits cs =
  let digits = takeWhile isDigit (dropWhile (not . isDigit) cs)
  in fromMaybe 0 (parsePositive (pack digits))

-- Find pattern in char list
findPattern : List Char -> List Char -> Maybe (List Char)
findPattern _ [] = Nothing
findPattern pattern cs@(_ :: rest) =
  if take (length pattern) cs == pattern
    then Just (drop (length pattern) cs)
    else findPattern pattern rest

||| Parse coverage ranges from JSON string
parseCoverageRanges : String -> List CoverageRange
parseCoverageRanges json = extractRanges (unpack json)
  where
    extractRanges : List Char -> List CoverageRange
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
                            range = MkCoverageRange startNum endNum countNum
                        in range :: extractRanges afterCount

||| Parse coverage entries from raw JSON
export
parseCoverageEntries : String -> List CoverageEntry
parseCoverageEntries json =
  -- Simplified: create single aggregate entry with all ranges
  let ranges = parseCoverageRanges json
      funcs = [MkCoverageFunction "_aggregate_" ranges False]
  in [MkCoverageEntry "script" "0" funcs]

-- =============================================================================
-- High-Level Coverage API
-- =============================================================================

||| Configuration for coverage collection
public export
record CoverageConfig where
  constructor MkCoverageConfig
  headless : Bool        -- Run browser in headless mode
  scriptPath : String    -- Path to compiled JS to test
  timeout : Nat          -- Timeout in milliseconds

||| Default coverage configuration
export
defaultConfig : String -> CoverageConfig
defaultConfig scriptPath = MkCoverageConfig True scriptPath 30000

||| Result of coverage collection
public export
record CoverageResult where
  constructor MkCoverageResult
  entries : List CoverageEntry
  totalRanges : Nat
  executedRanges : Nat
  coverageRatio : Double

export
Show CoverageResult where
  show r = "CoverageResult(" ++ show r.executedRanges ++ "/" ++ show r.totalRanges
         ++ " = " ++ show (r.coverageRatio * 100.0) ++ "%)"

||| Calculate coverage statistics from entries
calcStats : List CoverageEntry -> CoverageResult
calcStats entries =
  let allRanges = concatMap (.ranges) (concatMap (.functions) entries)
      totalCnt = length allRanges
      execCnt = length (filter (\r => r.count > 0) allRanges)
      ratio = if totalCnt == 0 then 0.0 else cast execCnt / cast totalCnt
  in MkCoverageResult entries totalCnt execCnt ratio

||| Run JavaScript file in browser and collect coverage
|||
||| This is the main entry point for coverage collection.
||| 1. Launches headless Chromium
||| 2. Creates a page with minimal HTML
||| 3. Starts V8 coverage
||| 4. Injects and runs the test script
||| 5. Collects coverage data
||| 6. Closes browser
export
collectBrowserCoverage : CoverageConfig -> IO (Either String CoverageResult)
collectBrowserCoverage config = do
  -- Launch browser
  browser <- launchBrowser

  -- Create page
  page <- newPage browser

  -- Set minimal HTML content
  setContent page "<!DOCTYPE html><html><head></head><body></body></html>"

  -- Start coverage collection
  startCoverage page

  -- Add the test script
  addScriptTag page config.scriptPath

  -- Wait a bit for script execution (simple approach)
  -- In production, would use page.waitForFunction or similar

  -- Stop coverage and get results
  rawJson <- stopCoverageRaw page

  -- Close browser
  closeBrowser browser

  -- Parse and return results
  let entries = parseCoverageEntries rawJson
  pure $ Right (calcStats entries)

||| Convenience function: collect coverage and return ratio
export
getCoverageRatio : String -> IO (Either String Double)
getCoverageRatio scriptPath = do
  result <- collectBrowserCoverage (defaultConfig scriptPath)
  pure $ map (.coverageRatio) result

-- =============================================================================
-- DOM Test Helpers
-- =============================================================================

||| Run a DOM test function and collect coverage
|||
||| For idris2-dom-mvc style tests where the compiled JS
||| defines a `main` function that runs tests.
export
runDomTestWithCoverage : (scriptPath : String) -> IO (Either String CoverageResult)
runDomTestWithCoverage = collectBrowserCoverage . defaultConfig

||| Evaluate custom JS in page context with coverage
export
evalWithCoverage : (jsCode : String) -> IO (Either String (String, CoverageResult))
evalWithCoverage jsCode = do
  browser <- launchBrowser
  page <- newPage browser
  setContent page "<!DOCTYPE html><html><head></head><body></body></html>"
  startCoverage page

  -- Evaluate custom JS
  result <- evaluate page jsCode

  rawJson <- stopCoverageRaw page
  closeBrowser browser

  let entries = parseCoverageEntries rawJson
  pure $ Right (result, calcStats entries)
