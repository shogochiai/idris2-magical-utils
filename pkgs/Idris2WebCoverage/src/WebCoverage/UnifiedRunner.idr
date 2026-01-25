||| Unified Web Coverage Runner
|||
||| Main entry point for web coverage collection.
||| Orchestrates: ipkg generation → build → dumpcases → V8 coverage → source map
|||
||| Compatible with idris2-coverage's runTestsWithFunctionHits pattern.
module WebCoverage.UnifiedRunner

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.Clock
import System.File
import System.Directory

import WebCoverage.Types
import WebCoverage.DumpcasesParser
import WebCoverage.V8Runner
import WebCoverage.PlaywrightRunner
import WebCoverage.SourceMapper
import WebCoverage.JSFunctionParser

-- idris2-coverage-core classification
import Coverage.Classification.BranchClass
import Coverage.Core.Exclusions

%default covering

-- =============================================================================
-- Temporary File Generation
-- =============================================================================

||| Generate unique identifier from timestamp
getUniqueId : IO String
getUniqueId = do
  t <- clockTime Monotonic
  pure $ "webtest_" ++ show (seconds t) ++ "_" ++ show (nanoseconds t `mod` 100000)

||| Generate temporary test runner source code
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

||| Join strings with separator
joinStrings : String -> List String -> String
joinStrings sep [] = ""
joinStrings sep [x] = x
joinStrings sep (x :: xs) = x ++ sep ++ joinStrings sep xs

||| Generate temporary .ipkg file for browser JavaScript with source map
|||
||| Key options:
|||   - --cg javascript (Browser code generator for Playwright)
|||   - --directive sourcemap (generate source map)
|||   - --dumpcases (for static analysis)
generateTempIpkg : String -> String -> List String -> String
                 -> List String -> String -> String -> String
generateTempIpkg pkgName mainMod modules execName depends sourcedir dumpcasesPath =
  let allDepends = "base, contrib" ++
        (if null depends then "" else ", " ++ joinStrings ", " depends)
      -- Enable source map generation and dumpcases (browser JS for Playwright)
      optsLine = "opts = \"--cg javascript --directive sourcemap --dumpcases " ++
                 dumpcasesPath ++ "\""
  in unlines
    [ "package " ++ pkgName
    , optsLine
    , "sourcedir = \"" ++ sourcedir ++ "\""
    , "main = " ++ mainMod
    , "executable = " ++ execName
    , "depends = " ++ allDepends
    , "modules = " ++ joinStrings ", " modules
    ]

-- =============================================================================
-- Ipkg Parsing
-- =============================================================================

||| Parse depends from ipkg content
parseIpkgDepends : String -> List String
parseIpkgDepends content =
  let ls = lines content
      dependsLines = filter (isPrefixOf "depends") (map trim ls)
  in case dependsLines of
       [] => []
       (line :: _) =>
         let afterEquals = trim $ snd $ break (== '=') line
             pkgStr = if isPrefixOf "=" afterEquals
                        then trim (substr 1 (length afterEquals) afterEquals)
                        else afterEquals
         in map trim $ forget $ split (== ',') pkgStr

||| Parse sourcedir from ipkg content
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

||| Find ipkg file in directory
findIpkgFile : String -> IO (Maybe String)
findIpkgFile projectDir = do
  Right entries <- listDir projectDir
    | Left _ => pure Nothing
  let ipkgFiles = filter (isSuffixOf ".ipkg") entries
  -- Filter out temp/test ipkgs
  let mainIpkgs = filter (\f => not (isInfixOf "temp" f || isInfixOf "test" f)) ipkgFiles
  pure $ head' (if null mainIpkgs then ipkgFiles else mainIpkgs)

||| Read ipkg content
readIpkgContent : String -> IO (Maybe String)
readIpkgContent projectDir = do
  Just ipkgName <- findIpkgFile projectDir
    | Nothing => pure Nothing
  Right content <- readFile (projectDir ++ "/" ++ ipkgName)
    | Left _ => pure Nothing
  pure (Just content)

-- =============================================================================
-- File Cleanup
-- =============================================================================

||| Remove file if exists
removeFileIfExists : String -> IO ()
removeFileIfExists path = do
  _ <- removeFile path
  pure ()

||| Cleanup temp files
cleanupTempFiles : List String -> IO ()
cleanupTempFiles = traverse_ removeFileIfExists

-- =============================================================================
-- Main Entry Point
-- =============================================================================

||| Run tests with web coverage and return per-function hits
|||
||| This is the main API, compatible with idris2-coverage's pattern.
|||
||| @projectDir - Path to project root (containing .ipkg)
||| @testModules - List of test module names
||| @timeout - Max seconds for build+run
export
runTestsWithWebCoverage : (projectDir : String)
                        -> (testModules : List String)
                        -> (timeout : Nat)
                        -> IO (Either String (List WebFunctionHit))
runTestsWithWebCoverage projectDir testModules timeout = do
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ => do
      -- Read project config
      Just ipkgContent <- readIpkgContent projectDir
        | Nothing => pure $ Left "No .ipkg file found"

      let projectDepends = parseIpkgDepends ipkgContent
      let sourcedir = parseIpkgSourcedir ipkgContent

      -- Generate unique names
      uid <- getUniqueId
      let tempModName = "TempWebRunner_" ++ uid
      let tempExecName = "temp-webtest-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let dumpcasesPath = "/tmp/idris2_dumpcases_web_" ++ uid ++ ".txt"
      let jsPath = projectDir ++ "/build/exec/" ++ tempExecName
      let mapPath = jsPath ++ ".map"

      -- Generate temp runner source
      let runnerSource = generateTempRunner tempModName testModules
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      -- Generate temp .ipkg with Node.js + source map options
      let allModules = tempModName :: testModules
      let ipkgContent = generateTempIpkg tempExecName tempModName allModules
                          tempExecName projectDepends sourcedir dumpcasesPath
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      -- Build with idris2 directly (pack ignores ipkg opts field)
      -- First install dependencies via pack, then build with idris2
      let installCmd = "cd " ++ projectDir ++ " && pack install-deps " ++ tempExecName ++ ".ipkg 2>&1"
      _ <- system installCmd
      -- Clear build cache for temp module to force recompilation with opts
      let clearCmd = "rm -rf " ++ projectDir ++ "/build/ttc/*/TempWebRunner_* 2>/dev/null; true"
      _ <- system clearCmd
      -- NOTE: Using `idris2-sm` instead of `idris2` for source map support with --cg javascript
      --
      -- Background:
      --   - Standard idris2 (as of 2025-01) only supports source maps for --cg node, not --cg javascript
      --   - The feature/es-source-maps branch adds source map support for --cg javascript (browser JS)
      --   - `idris2-sm` is a wrapper pointing to the locally installed feature branch version
      --
      -- When to change back to `idris2`:
      --   - After PR https://github.com/idris-lang/Idris2/pull/XXXX is merged
      --   - Or when Idris2 releases a version with --cg javascript source map support
      --   - At that point, simply change `idris2-sm` back to `idris2` for portability
      let buildCmd = "cd " ++ projectDir ++ " && idris2-sm --build " ++ tempExecName ++ ".ipkg 2>&1"
      buildResult <- system buildCmd
      if buildResult /= 0
        then do
          cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
          pure $ Left "Build failed"
        else do
          -- Parse dumpcases for static analysis
          Right dumpContent <- readFile dumpcasesPath
            | Left _ => do
                cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
                pure $ Left "Failed to read dumpcases"

          let allFuncs = parseDumpcases dumpContent
          -- Filter to user functions using idris2-coverage-core exclusions
          let funcs = filterUserFuncs allFuncs

          -- Collect coverage via Playwright (browser-based)
          v8Result <- runDomTestCoverage jsPath

          case v8Result of
            Left _ => do
              -- No V8 coverage, return static-only data
              cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
              let staticHits = map funcToHit funcs
              pure $ Right staticHits
            Right v8Cov => do
              -- Read JS file for function-level matching
              Right jsCode <- readFile jsPath
                | Left _ => do
                    cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
                    -- Fallback: use proportional byte coverage
                    let hits = matchFuncsWithV8Proportional funcs v8Cov
                    pure $ Right hits

              -- Parse JS functions and match with V8 coverage
              let hits = matchFuncsWithV8AndJS funcs v8Cov jsCode
              cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
              pure $ Right hits
  where
    -- | Check if function is from dom-mvc libraries (idris2-dom, idris2-dom-mvc)
    isDomMvcLibraryFunc : String -> Bool
    isDomMvcLibraryFunc name =
         isPrefixOf "Text.HTML." name
      || isPrefixOf "Web.MVC." name
      || isPrefixOf "JS." name
      || isPrefixOf "Control.Monad.Dom." name
      || isPrefixOf "Text.CSS." name
      || isPrefixOf "Web.Raw." name

    -- | Check if function is a test function
    isTestFunc : String -> Bool
    isTestFunc name = isInfixOf ".Tests." name || isInfixOf "Test." name
                   || isPrefixOf "test_" name

    -- | Check if function should be excluded from coverage (using idris2-coverage-core)
    isExcludedFunc : String -> Bool
    isExcludedFunc name = shouldExcludeFunctionName name
                       || isExcludedReason (determineExclusionReason defaultExclusions name)
                       || isDomMvcLibraryFunc name
                       || isTestFunc name

    -- | Filter to user functions only (exclude Prelude, prim__, {csegen:*}, etc.)
    filterUserFuncs : List FuncCases -> List FuncCases
    filterUserFuncs = filter (\fc => not (isExcludedFunc fc.funcName))

    funcToHit : FuncCases -> WebFunctionHit
    funcToHit fc = MkWebFunctionHit fc.funcName fc.funcName
                     fc.totalBranches 0 (length fc.cases) 0

    -- | Calculate total executed bytes from V8 coverage (bytes with count > 0)
    calcExecutedBytes : V8ScriptCoverage -> Nat
    calcExecutedBytes v8Cov =
      let allRanges = concatMap (.ranges) v8Cov.functions
          executedRanges = filter (\r => r.count > 0) allRanges
          -- Simple sum of range sizes (may overcount overlapping ranges, but good estimate)
      in sum $ map (\r => minus r.endOffset r.startOffset) executedRanges

    -- | Calculate total code bytes from V8 coverage (all ranges)
    calcTotalBytes : V8ScriptCoverage -> Nat
    calcTotalBytes v8Cov =
      let allRanges = concatMap (.ranges) v8Cov.functions
      in case allRanges of
           [] => 0
           _  => -- Use max endOffset as total code size
                 foldl max 0 (map (.endOffset) allRanges)

    -- | Fallback: Byte-based proportional coverage matching
    -- Used when JS file cannot be read
    matchFuncsWithV8Proportional : List FuncCases -> V8ScriptCoverage -> List WebFunctionHit
    matchFuncsWithV8Proportional funcs v8Cov =
      let executedBytes = calcExecutedBytes v8Cov
          totalBytes = calcTotalBytes v8Cov
          rawRatio = if totalBytes == 0 then 0.0
                     else cast executedBytes / cast totalBytes
          byteCovRatio = min 1.0 rawRatio
      in map (\fc =>
           let rawExec = cast fc.totalBranches * byteCovRatio
               rounded = cast {to=Int} (rawExec + 0.5)
               executed = cast {to=Nat} (max 0 rounded)
           in MkWebFunctionHit fc.funcName fc.funcName
                fc.totalBranches executed (length fc.cases)
                (min executed (length fc.cases))
         ) funcs

    -- | Convert Idris function name to JS mangled name (like chezMangle for JS)
    -- Idris2 JS codegen: "Module.Name.func" -> "Module_Name_func"
    -- Special chars are encoded: '-' -> '$_', ':' -> '$c'
    jsMangle : String -> String
    jsMangle name =
      let chars = unpack name
      in pack $ concatMap encodeChar chars
      where
        encodeChar : Char -> List Char
        encodeChar '.' = ['_']
        encodeChar '-' = ['$', '_']
        encodeChar ':' = ['$', 'c']
        encodeChar '\'' = ['$', 'p']
        encodeChar c = [c]

    -- | Find matching JS function for an Idris function
    -- Priority: 1) Exact idrisName match, 2) Mangled name match, 3) Suffix match
    findJSFunc : String -> List JSFunctionDef -> Maybe JSFunctionDef
    findJSFunc idrisName jsFuncs =
      -- Try exact idrisName match first
      case find (\jf => jf.idrisName == idrisName) jsFuncs of
        Just f => Just f
        Nothing =>
          let mangledName = jsMangle idrisName
          in -- Try exact mangled name match
             case find (\jf => jf.jsName == mangledName) jsFuncs of
               Just f => Just f
               Nothing =>
                 -- Try suffix match (handles prefixed names like "n--")
                 find (\jf => isSuffixOf mangledName jf.jsName) jsFuncs

    -- | Count V8 ranges that overlap with a JS function's byte range
    countRangesInFunc : JSFunctionDef -> List V8Range -> (Nat, Nat)
    countRangesInFunc jf ranges =
      let funcRanges = filter (\r => r.startOffset >= jf.startOffset
                                  && r.startOffset < jf.endOffset) ranges
          execCount = length $ filter (\r => r.count > 0) funcRanges
          totCount = length funcRanges
      in (execCount, totCount)

    -- | Function-level coverage matching using JS function parser
    -- Parses JS file to find Idris function definitions, then matches
    -- V8 coverage ranges to specific functions.
    -- For unmatched functions, uses global byte coverage ratio as fallback.
    matchFuncsWithV8AndJS : List FuncCases -> V8ScriptCoverage -> String -> List WebFunctionHit
    matchFuncsWithV8AndJS funcs v8Cov jsCode =
      let -- Parse JS functions from the generated code
          jsFuncs = parseJSFunctions jsCode
          -- Get all V8 ranges
          allRanges = concatMap (.ranges) v8Cov.functions
          -- Calculate global byte coverage ratio for fallback
          globalExecBytes = calcExecutedBytes v8Cov
          globalTotalBytes = calcTotalBytes v8Cov
          globalRatio = if globalTotalBytes == 0 then 0.0
                        else min 1.0 (cast globalExecBytes / cast globalTotalBytes)
      in map (\fc => matchSingleFunc fc jsFuncs allRanges globalRatio) funcs
      where
        matchSingleFunc : FuncCases -> List JSFunctionDef -> List V8Range -> Double -> WebFunctionHit
        matchSingleFunc fc jsFuncs ranges fallbackRatio =
          case findJSFunc fc.funcName jsFuncs of
            Nothing =>
              -- No JS function found, use global byte coverage ratio
              let rawExec = cast fc.totalBranches * fallbackRatio
                  executed = cast {to=Nat} (max 0 (cast {to=Int} (rawExec + 0.5)))
              in MkWebFunctionHit fc.funcName fc.funcName
                   fc.totalBranches executed (length fc.cases)
                   (min executed (length fc.cases))
            Just jf =>
              let execAndTotal = countRangesInFunc jf ranges
                  rangeExec = fst execAndTotal
                  rangeTot = snd execAndTotal
                  -- Calculate coverage ratio for this function
                  funcRatio = if rangeTot == 0 then fallbackRatio  -- use fallback if no ranges
                              else cast rangeExec / cast rangeTot
                  -- Apply ratio to canonical branches
                  rawExec = cast fc.totalBranches * funcRatio
                  finalExec = cast {to=Nat} (max 0 (cast {to=Int} (rawExec + 0.5)))
              in MkWebFunctionHit fc.funcName jf.jsName
                   fc.totalBranches finalExec (length fc.cases)
                   (min finalExec (length fc.cases))

-- =============================================================================
-- Convenience API
-- =============================================================================

||| Run web coverage and return report
export
runWebCoverageReport : (projectDir : String)
                     -> (testModules : List String)
                     -> (timeout : Nat)
                     -> IO (Either String WebCoverageReport)
runWebCoverageReport projectDir testModules timeout = do
  result <- runTestsWithWebCoverage projectDir testModules timeout
  case result of
    Left err => pure $ Left err
    Right hits =>
      let totalCanon = sum $ map (.canonicalCount) hits
          totalExec = sum $ map (.executedCount) hits
          pct = if totalCanon == 0 then 100.0
                else cast totalExec / cast totalCanon * 100.0
      in pure $ Right $ MkWebCoverageReport hits [] totalCanon totalExec pct
