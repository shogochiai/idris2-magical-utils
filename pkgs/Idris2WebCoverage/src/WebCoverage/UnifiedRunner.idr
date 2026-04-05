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
import Execution.Standardization.Web

-- idris2-coverage-core classification
import Coverage.Classification.BranchClass
import Coverage.Core.Exclusions
import Coverage.Core.ObligationMap
import Coverage.DumpcasesParser as CovDump

%default covering

installedIdrisPrelude : String
installedIdrisPrelude =
  "export IDRIS2_PACKAGE_PATH=\"$(cd /tmp && pack package-path)\""
  ++ " && export IDRIS2_LIBS=\"$(cd /tmp && pack libs-path)\""
  ++ " && export IDRIS2_DATA=\"$(cd /tmp && pack data-path)\""

removeFileIfExistsSafe : String -> IO ()
removeFileIfExistsSafe path = do
  _ <- removeFile path
  pure ()

buildTempWebIpkg : String -> String -> IO (Either String ())
buildTempWebIpkg projectDir ipkgName = do
  let logPath = projectDir ++ "/.idris2-web-coverage-build.log"
  removeFileIfExistsSafe logPath
  let clearCmd = "rm -rf " ++ projectDir ++ "/build/ttc/*/TempWebRunner_* 2>/dev/null; true"
  _ <- system clearCmd
  let buildCmd = installedIdrisPrelude
              ++ " && cd " ++ projectDir
              ++ " && idris2-sm --build " ++ ipkgName ++ " > " ++ logPath ++ " 2>&1"
  buildExit <- system buildCmd
  if buildExit == 0
     then do
       removeFileIfExistsSafe logPath
       pure $ Right ()
     else do
       logContent <- readFile logPath
       removeFileIfExistsSafe logPath
       pure $ Left $
         either (\err => "unable to read build log: " ++ show err)
                trim
                logContent

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
-- Static Obligation Helpers
-- =============================================================================

isDomMvcLibraryFunc : String -> Bool
isDomMvcLibraryFunc name =
     isPrefixOf "Text.HTML." name
  || isPrefixOf "Web.MVC." name
  || isPrefixOf "JS." name
  || isPrefixOf "Control.Monad.Dom." name
  || isPrefixOf "Text.CSS." name
  || isPrefixOf "Web.Raw." name

isTestFunc : String -> Bool
isTestFunc name = isInfixOf ".Tests." name || isInfixOf "Test." name
               || isPrefixOf "test_" name

isExcludedFunc : String -> Bool
isExcludedFunc name = shouldExcludeFunctionName name
                   || isExcludedReason (determineExclusionReason defaultExclusions name)
                   || isDomMvcLibraryFunc name
                   || isTestFunc name

filterUserFuncs : List FuncCases -> List FuncCases
filterUserFuncs = filter (\fc => not (isExcludedFunc fc.funcName))

filterUserCompiledFuncs : List CovDump.CompiledFunction -> List CovDump.CompiledFunction
filterUserCompiledFuncs = filter (\f => not (isExcludedFunc f.fullName))

canonicalBranchCount : StaticFunctionAnalysis -> Nat
canonicalBranchCount sfa =
  length $ filter (\cb => cb.branchClass == BCCanonical) sfa.branches

totalBranchCount : StaticFunctionAnalysis -> Nat
totalBranchCount = length . (.branches)

lookupStaticFunction : List StaticFunctionAnalysis -> String -> Maybe StaticFunctionAnalysis
lookupStaticFunction analyses name = find (\sfa => sfa.fullName == name) analyses

analysisToFunctionObligation : StaticFunctionAnalysis -> CoverageObligation
analysisToFunctionObligation sfa =
  MkCoverageObligation
    sfa.fullName
    ElaboratedCaseTree
    FunctionLevel
    sfa.fullName
    Nothing
    (functionObligationClass sfa.branches)

normalizeHitWithStatic : List StaticFunctionAnalysis -> WebFunctionHit -> WebFunctionHit
normalizeHitWithStatic analyses hit =
  case lookupStaticFunction analyses hit.funcName of
    Nothing => hit
    Just sfa =>
      let canonical = canonicalBranchCount sfa
          executed = min hit.executedCount canonical
      in MkWebFunctionHit
           hit.funcName
           hit.jsName
           canonical
           executed
           (totalBranchCount sfa)
           (min executed (totalBranchCount sfa))

coveredFunctionObligationIds : List CoverageObligation -> List WebFunctionHit -> List String
coveredFunctionObligationIds obligations hits =
  let obligationMap =
        buildFunctionObligationMap obligations (\ob => [runtimeFunctionName ob.obligationId])
      runtimeUnits =
        map (runtimeFunctionName . (.funcName)) $
          filter (\h => h.executedCount > 0) hits
  in resolveCoveredDenominatorIds obligations obligationMap runtimeUnits

buildWebCoverageReport : List WebFunctionHit -> List StaticFunctionAnalysis -> WebCoverageReport
buildWebCoverageReport hits analyses =
  let obligations = map analysisToFunctionObligation analyses
      coveredIds = coveredFunctionObligationIds obligations hits
      excludedIds =
        map (.obligationId) $
          filter (\ob => mustBeExcluded ob.classification) obligations
      unknownIds =
        map (.obligationId) $
          filter (\ob => ob.classification == UnknownClassification) obligations
      denominatorIds =
        map (.obligationId) $
          filter (\ob => countsAsDenominator ob.classification) obligations
      measurement =
        MkCoverageMeasurement denominatorIds coveredIds excludedIds unknownIds
      denominatorCount = length $ filter (\ob => countsAsDenominator ob.classification) obligations
      coveredCount = length coveredIds
      pct = if denominatorCount == 0
               then 100.0
               else cast coveredCount / cast denominatorCount * 100.0
      totalCanonical = sum $ map (.canonicalCount) hits
      totalExecuted = sum $ map (\h => min h.executedCount h.canonicalCount) hits
  in MkWebCoverageReport
       hits
       obligations
       measurement
       coveredIds
       []
       denominatorCount
       coveredCount
       totalCanonical
       totalExecuted
       pct
       standardFunctionCoverageModelName
       defaultWebExecutionProfileName
       standardUnknownPolicyName
       (isStandardFunctionCoverageClaimAdmissible analyses)

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
runTestsWithWebCoverageDetailed : (projectDir : String)
                               -> (testModules : List String)
                               -> (timeout : Nat)
                               -> IO (Either String (List WebFunctionHit, List StaticFunctionAnalysis))
runTestsWithWebCoverageDetailed projectDir testModules timeout = do
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

      buildResult <- buildTempWebIpkg projectDir (tempExecName ++ ".ipkg")
      case buildResult of
        Left err => do
          cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
          pure $ Left $ "Build failed: " ++ err
        Right () => do
          -- Parse dumpcases for static analysis
          Right dumpContent <- readFile dumpcasesPath
            | Left _ => do
                cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
                pure $ Left "Failed to read dumpcases"

          let compiledFuncs = filterUserCompiledFuncs (CovDump.parseDumpcasesFile dumpContent)
          let staticFunctions = map CovDump.toStaticFunctionAnalysis compiledFuncs
          let funcs = filterUserFuncs (parseDumpcases dumpContent)

          -- Collect coverage via Playwright (browser-based)
          v8Result <- runDomTestCoverage projectDir jsPath

          case v8Result of
            Left err => do
              cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
              pure $ Left err
            Right v8Cov => do
              -- Read JS file for function-level matching
              Right jsCode <- readFile jsPath
                | Left _ => do
                    cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
                    -- Fallback: use proportional byte coverage
                    let hits = map (normalizeHitWithStatic staticFunctions) (matchFuncsWithV8Proportional funcs v8Cov)
                    pure $ Right (hits, staticFunctions)

              -- Parse JS functions and match with V8 coverage
              let hits = map (normalizeHitWithStatic staticFunctions) (matchFuncsWithV8AndJS funcs v8Cov jsCode)
              cleanupTempFiles [tempIdrPath, tempIpkgPath, dumpcasesPath]
              pure $ Right (hits, staticFunctions)
  where
    funcToHit : FuncCases -> WebFunctionHit
    funcToHit fc = MkWebFunctionHit fc.funcName fc.funcName
                     fc.totalBranches 0 (length fc.cases) 0

    -- | Calculate total executed bytes from V8 coverage (bytes with count > 0)
    calcExecutedBytes : V8ScriptCoverage -> Nat
    calcExecutedBytes v8Cov =
      let allRanges = concatMap (.ranges) v8Cov.functions
          executedRanges = filter (\r => r.count > 0) allRanges
          -- Simple sum of range sizes (may overcount overlapping ranges, but good estimate)
      in sum $ map (\r => toNatJsByteLength (offsetMinus r.endOffset r.startOffset)) executedRanges

    -- | Calculate total code bytes from V8 coverage (all ranges)
    calcTotalBytes : V8ScriptCoverage -> Nat
    calcTotalBytes v8Cov =
      let allRanges = concatMap (.ranges) v8Cov.functions
      in case allRanges of
           [] => 0
           _  => -- Use max endOffset as total code size
                 toNatJsByteOffset (foldl maxOffset zeroJsByteOffset (map (.endOffset) allRanges))

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
      let funcRanges = filter (\r => offsetGE r.startOffset jf.startOffset
                                  && offsetLT r.startOffset jf.endOffset) ranges
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

||| Run tests with web coverage and return per-function hits
|||
||| Compatibility API that projects the detailed report down to runtime hits.
export
runTestsWithWebCoverage : (projectDir : String)
                        -> (testModules : List String)
                        -> (timeout : Nat)
                        -> IO (Either String (List WebFunctionHit))
runTestsWithWebCoverage projectDir testModules timeout = do
  result <- runTestsWithWebCoverageDetailed projectDir testModules timeout
  pure $ map fst result

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
  result <- runTestsWithWebCoverageDetailed projectDir testModules timeout
  case result of
    Left err => pure $ Left err
    Right (hits, analyses) => pure $ Right $ buildWebCoverageReport hits analyses
