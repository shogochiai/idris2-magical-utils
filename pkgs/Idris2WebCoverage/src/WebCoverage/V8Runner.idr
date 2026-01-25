||| V8 Coverage Runner
|||
||| Executes JavaScript with V8 coverage collection via Node.js.
module WebCoverage.V8Runner

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Directory
import WebCoverage.Types

%default covering

-- =============================================================================
-- V8 Coverage Execution
-- =============================================================================

||| Run JavaScript file with V8 coverage and return raw JSON
|||
||| @jsPath - Path to compiled JavaScript file
||| @timeout - Timeout in seconds
export
runV8Coverage : (jsPath : String) -> (timeout : Nat) -> IO (Either String String)
runV8Coverage jsPath timeout = do
  -- Create temp directory for V8 coverage output
  uid <- getUniqueId
  let covDir = "/tmp/idris2-web-cov-" ++ uid
  _ <- system $ "mkdir -p " ++ covDir

  -- Run with NODE_V8_COVERAGE environment variable
  -- macOS doesn't have timeout, run directly without timeout
  let cmd = "env NODE_V8_COVERAGE=" ++ covDir ++ " node " ++ jsPath ++ " 2>&1"
  _ <- system cmd

  -- Read coverage JSON files
  Right entries <- listDir covDir
    | Left _ => do
        _ <- system $ "rm -rf " ++ covDir
        pure $ Left "Failed to list coverage directory"

  let jsonFiles = filter (isSuffixOf ".json") entries
  case jsonFiles of
    [] => do
      _ <- system $ "rm -rf " ++ covDir
      pure $ Left "No V8 coverage data generated"
    (f :: _) => do
      Right content <- readFile (covDir ++ "/" ++ f)
        | Left err => do
            _ <- system $ "rm -rf " ++ covDir
            pure $ Left $ "Failed to read coverage: " ++ show err
      _ <- system $ "rm -rf " ++ covDir
      pure $ Right content
  where
    getUniqueId : IO String
    getUniqueId = do
      Right h <- popen "date +%s%N 2>/dev/null || date +%s" Read
        | Left _ => pure "0"
      Right s <- fGetLine h
        | Left _ => do _ <- pclose h; pure "0"
      _ <- pclose h
      pure (trim s)

-- =============================================================================
-- V8 JSON Parsing Helpers
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

-- Count substring occurrences
countSubstr : String -> String -> Nat
countSubstr needle haystack = go 0 (unpack haystack)
  where
    needleChars : List Char
    needleChars = unpack needle
    needleLen : Nat
    needleLen = length needleChars

    go : Nat -> List Char -> Nat
    go n [] = n
    go n cs@(_ :: rest) =
      if take needleLen cs == needleChars
        then go (S n) rest
        else go n rest

-- =============================================================================
-- V8 JSON Parsing
-- =============================================================================

||| Parse V8 function name from JSON block
parseV8FuncName : String -> Maybe String
parseV8FuncName json =
  case findPattern (unpack "\"functionName\":") (unpack json) of
    Nothing => Nothing
    Just afterKey =>
      let trimmed = dropWhile (\c => c == ' ' || c == '\n' || c == '\t') afterKey
      in case trimmed of
           ('"' :: rest) =>
             let name = takeWhile (/= '"') rest
             in Just (pack name)
           _ => Nothing

||| Parse V8 coverage ranges from JSON
parseV8Ranges : String -> List V8Range
parseV8Ranges json =
  if countSubstr "\"startOffset\"" json == 0 then []
  else extractRanges (unpack json)
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
                            range = MkV8Range startNum endNum countNum
                        in range :: extractRanges afterCount

-- Helper: count characters until pattern is found
countUntilPattern : List Char -> List Char -> Nat
countUntilPattern _ [] = 0
countUntilPattern pattern cs@(_ :: rest) =
  if take (length pattern) cs == pattern
    then 0
    else S (countUntilPattern pattern rest)

-- Extract ranges until we hit the next scriptId
extractScriptRanges : List Char -> List V8Range
extractScriptRanges cs = go cs []
  where
    go : List Char -> List V8Range -> List V8Range
    go [] acc = reverse acc
    go chars acc =
      -- Stop if we hit another scriptId (next script)
      case findPattern (unpack "\"scriptId\":") chars of
        Just _ =>
          case findPattern (unpack "\"startOffset\":") chars of
            Nothing => reverse acc
            Just afterStart =>
              -- Check if startOffset comes BEFORE next scriptId
              let distToStart = countUntilPattern (unpack "\"startOffset\":") chars
                  distToScript = countUntilPattern (unpack "\"scriptId\":") chars
              in if distToStart < distToScript
                   then let startNum = takeDigits afterStart
                        in case findPattern (unpack "\"endOffset\":") afterStart of
                             Nothing => reverse acc
                             Just afterEnd =>
                               let endNum = takeDigits afterEnd
                               in case findPattern (unpack "\"count\":") afterEnd of
                                    Nothing => reverse acc
                                    Just afterCount =>
                                      let countNum = takeDigits afterCount
                                          range = MkV8Range startNum endNum countNum
                                      in go afterCount (range :: acc)
                   else reverse acc
        Nothing =>
          -- No more scriptId, extract all remaining ranges
          case findPattern (unpack "\"startOffset\":") chars of
            Nothing => reverse acc
            Just afterStart =>
              let startNum = takeDigits afterStart
              in case findPattern (unpack "\"endOffset\":") afterStart of
                   Nothing => reverse acc
                   Just afterEnd =>
                     let endNum = takeDigits afterEnd
                     in case findPattern (unpack "\"count\":") afterEnd of
                          Nothing => reverse acc
                          Just afterCount =>
                            let countNum = takeDigits afterCount
                                range = MkV8Range startNum endNum countNum
                            in go afterCount (range :: acc)

||| Parse V8 script coverage from JSON
||| Extracts all ranges for the target script
export
parseV8ScriptCoverage : String -> String -> Maybe V8ScriptCoverage
parseV8ScriptCoverage json targetScript =
  if not (isInfixOf targetScript json) then Nothing
  else
    -- Find the section for our target script and extract ranges
    case findTargetScriptRanges (unpack json) (unpack targetScript) of
      [] => Just $ MkV8ScriptCoverage targetScript []
      ranges =>
        let aggregateFunc = MkV8FunctionCoverage "_aggregate_" ranges False
        in Just $ MkV8ScriptCoverage targetScript [aggregateFunc]
  where
    -- Find the target script section and extract all ranges from it
    findTargetScriptRanges : List Char -> List Char -> List V8Range
    findTargetScriptRanges [] _ = []
    findTargetScriptRanges cs target =
      -- Look for target script in URL field
      case findPattern (unpack "\"url\":\"file:") cs of
        Nothing => []
        Just afterUrl =>
          -- Check if this URL contains our target
          if isInfixOf (pack target) (pack (take 500 afterUrl))
            then extractScriptRanges afterUrl
            else findTargetScriptRanges afterUrl target

-- =============================================================================
-- High-Level API
-- =============================================================================

||| Run JavaScript and collect V8 coverage data
export
collectV8Coverage : (jsPath : String) -> (timeout : Nat)
                  -> IO (Either String V8ScriptCoverage)
collectV8Coverage jsPath timeout = do
  result <- runV8Coverage jsPath timeout
  case result of
    Left err => pure $ Left err
    Right json =>
      let scriptName = getScriptName jsPath
      in case parseV8ScriptCoverage json scriptName of
           Nothing => pure $ Left "Failed to parse V8 coverage"
           Just cov => pure $ Right cov
  where
    getScriptName : String -> String
    getScriptName path =
      let parts = forget $ split (== '/') path
      in fromMaybe path (last' parts)
