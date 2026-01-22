||| Parser for Idris2 --dumpcases output
||| Extracts semantic coverage information (canonical vs impossible cases)
module EvmCoverage.DumpcasesParser

import EvmCoverage.Types
import Data.List
import Data.List1
import Data.String
import Data.Maybe
import System
import System.Clock
import System.File

%default covering

-- =============================================================================
-- Helper Functions (module level)
-- =============================================================================

||| Find substring in list of chars
findSubstr : List Char -> List Char -> Nat -> Maybe Nat
findSubstr needle [] _ = Nothing
findSubstr needle haystack@(_ :: rest) idx =
  if isPrefixOf needle haystack
    then Just idx
    else findSubstr needle rest (S idx)

||| Extract text between two delimiters
extractBetween : String -> String -> String -> Maybe String
extractBetween start end str =
  let chars = unpack str
      startChars = unpack start
      endChars = unpack end
  in do startIdx <- findSubstr startChars chars 0
        let afterStart = drop (startIdx + length startChars) chars
        endIdx <- findSubstr endChars afterStart 0
        pure $ pack $ take endIdx afterStart

||| Count occurrences of substring
countOccurrences : String -> String -> Nat
countOccurrences needle haystack = go 0 (unpack haystack)
  where
    needleChars : List Char
    needleChars = unpack needle

    go : Nat -> List Char -> Nat
    go acc [] = acc
    go acc rest@(_ :: xs) =
      if isPrefixOf needleChars rest
        then go (S acc) (drop (length needleChars) rest)
        else go acc xs

-- =============================================================================
-- CRASH Reason Classification
-- =============================================================================

||| Parse CRASH reason from dumpcases output
||| Examples:
|||   CRASH "No clauses in AbsurdTest.absurdFunc"
|||   CRASH "Unhandled input for Main.partial at ..."
|||   CRASH "Nat case not covered for ..."
export
parseCrashReason : String -> BranchClass
parseCrashReason msg =
  if isInfixOf "No clauses" msg then BCExcludedNoClauses
  else if isInfixOf "Unhandled input" msg then BCBugUnhandledInput
  else if isInfixOf "Nat case" msg then BCOptimizerNat
  else BCUnknownCrash msg

-- =============================================================================
-- Module Filtering (EVM-specific)
-- =============================================================================

||| Check if function belongs to project (not runtime/stdlib)
||| Based on idris2-coverage/exclusions/base.txt patterns
isProjectFunction : String -> Bool
isProjectFunction name =
  let n = case strUncons name of
            Just ('.', rest) => rest
            _ => name
  in not (isExcluded n)
  where
    -- Compiler-generated MN names
    isCompilerGenerated : String -> Bool
    isCompilerGenerated s =
      isInfixOf "{csegen:" s ||
      isInfixOf "{eta:" s ||
      isInfixOf "{arg:" s ||
      isInfixOf "{lazyArg:" s ||
      isInfixOf "{conArg:" s ||
      isInfixOf "{case" s

    -- Standard library modules
    isStdLib : String -> Bool
    isStdLib s =
      isPrefixOf "Prelude." s ||
      isPrefixOf "Data." s ||
      isPrefixOf "System." s ||
      isPrefixOf "Control." s ||
      isPrefixOf "Decidable." s ||
      isPrefixOf "Language." s ||
      isPrefixOf "Debug." s ||
      isPrefixOf "PrimIO." s ||
      isPrefixOf "_builtin." s ||
      isPrefixOf "Builtin." s

    -- Builtins and primitives
    isPrimitive : String -> Bool
    isPrimitive s = isPrefixOf "prim__" s

    -- Test modules (not deployed to EVM)
    isTestModule : String -> Bool
    isTestModule s = isInfixOf ".Tests." s || isInfixOf "Test." s

    -- Schema/Storages modules (pure storage layout, not business logic)
    isSchemaModule : String -> Bool
    isSchemaModule s = isInfixOf ".Storages." s || isInfixOf ".Schema" s

    isExcluded : String -> Bool
    isExcluded s =
      isCompilerGenerated s ||
      isStdLib s ||
      isPrimitive s ||
      isTestModule s ||
      isSchemaModule s

-- =============================================================================
-- Line Parsing
-- =============================================================================

||| Count arguments from the right side of a function definition
countArgs : String -> Nat
countArgs s =
  case extractBetween "[" "]" s of
    Nothing => 0
    Just argsStr => length $ forget $ split (== ',') argsStr

||| Split list into init and last
splitInitLast : List a -> Maybe (List a, a)
splitInitLast [] = Nothing
splitInitLast [x] = Just ([], x)
splitInitLast (x :: xs) = do
  (rest, lastItem) <- splitInitLast xs
  Just (x :: rest, lastItem)

||| Parse function definition line
||| Format: "Module.funcName = [arg0, arg1]: body"
parseFunctionDef : String -> Maybe (String, String, Nat)
parseFunctionDef line =
  case break (== '=') line of
    (name, rest) =>
      if null rest then Nothing
      else
        let trimmedName = trim name
            parts = forget $ split (== '.') trimmedName
        in case parts of
             [] => Nothing
             [single] => Just ("", single, countArgs rest)
             _ => case splitInitLast parts of
                    Just (modParts, fName) =>
                      Just (concat $ intersperse "." modParts, fName, countArgs rest)
                    Nothing => Nothing

||| Check if line contains a case expression
||| Format: "(%case expr [(pattern body), ...] default)"
isCaseLine : String -> Bool
isCaseLine line = isInfixOf "%case" line || isInfixOf "%concase" line

||| Check if line contains CRASH
||| Format: "CRASH \"message\""
isCrashLine : String -> Bool
isCrashLine line = isInfixOf "CRASH" line || isInfixOf "blodwen-error-quit" line

||| Extract CRASH message from line
extractCrashMsg : String -> String
extractCrashMsg line =
  case extractBetween "\"" "\"" line of
    Just msg => msg
    Nothing => "Unknown CRASH"

-- =============================================================================
-- Counting Branches in Case Expression
-- =============================================================================

||| Count branches in a %case expression
||| Looks for %concase and %constcase patterns
countCaseBranches : String -> Nat
countCaseBranches line =
  let conCases = countOccurrences "%concase" line
      constCases = countOccurrences "%constcase" line
  in max 1 (conCases + constCases)  -- At least 1 branch

-- =============================================================================
-- Main Parser
-- =============================================================================

||| Make a canonical branch
makeBranch : String -> Nat -> Nat -> ClassifiedBranch
makeBranch fName baseIdx i =
  let bid = MkBranchId "" fName (baseIdx + i)
  in MkClassifiedBranch bid BCCanonical ("case branch " ++ show i)

||| Parse branches from function body lines
parseBranches : String -> List String -> Nat -> List ClassifiedBranch
parseBranches _ [] _ = []
parseBranches fName (l :: ls) idx =
  if isCrashLine l
    then let crashClass = parseCrashReason (extractCrashMsg l)
             bid = MkBranchId "" fName idx
             branch = MkClassifiedBranch bid crashClass l
         in branch :: parseBranches fName ls (S idx)
    else if isCaseLine l
      then let numBranches = countCaseBranches l
               indices = take numBranches [0..]
               newBranches = map (makeBranch fName idx) indices
           in newBranches ++ parseBranches fName ls (idx + numBranches)
      else parseBranches fName ls idx

||| Parse a single function from dumpcases output
||| Note: In dumpcases output, each function definition is a single line
parseFunction : String -> List String -> (String, List ClassifiedBranch)
parseFunction funcLine bodyLines =
  case parseFunctionDef funcLine of
    Nothing => ("", [])
    Just (modName, fName, arity) =>
      let fullName = if null modName then fName else modName ++ "." ++ fName
          -- Count case branches in the function line itself (not bodyLines)
          numBranches = countCaseBranches funcLine
          indices = take numBranches [0..]
          branches = map (makeBranch fullName 0) indices
      in (fullName, branches)

||| Parse function list helper
parseFunctions : List String -> List ClassifiedBranch -> List ClassifiedBranch
parseFunctions [] acc = acc
parseFunctions (l :: rest) acc =
  if isInfixOf " = [" l && not (isPrefixOf "{" (trim l))
    then let (funcName, branches) = parseFunction l rest
         in parseFunctions rest (acc ++ branches)
    else parseFunctions rest acc

||| Parse entire dumpcases file content
export
parseDumpcases : String -> List ClassifiedBranch
parseDumpcases content =
  let allBranches = parseFunctions (lines content) []
  in filter (\b => isProjectFunction b.branchId.funcName) allBranches

||| Extract directory and filename from path
splitPath : String -> (String, String)
splitPath path =
  let chars = unpack path
      revChars = reverse chars
  in case break (== '/') revChars of
       (revName, []) => (".", pack (reverse revName))
       (revName, _ :: rest) => (pack (reverse rest), pack (reverse revName))

||| Generate unique ID for temp files
generateUid : IO String
generateUid = do
  t <- time
  pure $ show t

||| Read ipkg and extract modules list
parseModulesFromIpkg : String -> List String
parseModulesFromIpkg content =
  let ls = lines content
      -- Find lines after "modules =" and extract module names
      inModules = dropWhile (not . isInfixOf "modules") ls
  in case inModules of
       [] => []
       (_ :: rest) => mapMaybe extractModule (take 50 rest)
  where
    extractModule : String -> Maybe String
    extractModule line =
      let trimmed = trim line
      in if null trimmed || isPrefixOf "depends" trimmed || isPrefixOf "main" trimmed
            || isPrefixOf "executable" trimmed || isPrefixOf "sourcedir" trimmed
            || isPrefixOf "package" trimmed || isPrefixOf "version" trimmed
           then Nothing
           else Just $ trim $ pack $ filter (\c => c /= ',') (unpack trimmed)

||| Extract sourcedir value from ipkg content
extractSourcedir : String -> String
extractSourcedir content =
  case find (isInfixOf "sourcedir") (lines content) of
    Nothing => ""
    Just line =>
      case break (== '=') (unpack line) of
        (_, []) => ""
        (_, _ :: rest) => trim $ pack $ filter (\c => c /= '"') rest

||| Extract multi-line field from ipkg content (e.g., depends, modules)
||| Captures the field and all continuation lines (starting with spaces/comma)
extractMultiLineField : String -> String -> String
extractMultiLineField fieldName content =
  let ls = lines content
      (before, fromField) = break (isInfixOf fieldName) ls
  in case fromField of
       [] => ""
       (firstLine :: rest) =>
         let continuations = takeWhile isContinuation rest
             allLines = firstLine :: continuations
         in unlines allLines
  where
    isContinuation : String -> Bool
    isContinuation s =
      let trimmed = ltrim s
      in not (null s) &&
         (isPrefixOf "," trimmed || isPrefixOf " " s) &&
         not (isInfixOf "=" trimmed && not (isInfixOf "," (pack $ take 1 $ unpack trimmed)))

-- =============================================================================
-- IO Function Extraction from .idr Files
-- =============================================================================

||| Represents an exported IO function found in a module
record ExportedIOFunc where
  constructor MkExportedIOFunc
  moduleName : String
  funcName : String
  argTypes : List String  -- List of argument type strings (e.g., ["Integer", "List Integer"])

||| Get the return type part of a signature (after the last -> or after :)
||| e.g., "A -> B -> IO ()" returns "IO ()"
|||       "IO Integer" returns "IO Integer"
|||       "List (String, IO Bool)" returns "List (String, IO Bool)"
getReturnType : String -> String
getReturnType sig =
  let parts = forget $ split (== ':') sig
  in case parts of
       [_, afterColon] =>
         -- Split by " -> " and take the last part
         let arrowParts = forget $ split (\c => c == '-') afterColon
         in case reverse arrowParts of
              [] => ""
              (lastPart :: _) =>
                -- lastPart might start with ">" if it was "-> X"
                let stripped = trim lastPart
                    len = length stripped
                in if isPrefixOf ">" stripped && len > 0
                     then trim $ substr 1 len stripped
                     else stripped
       _ => ""

||| Check if return type is an IO type (not List (IO x) etc.)
||| Must start with "IO " or "IO(" to be considered an IO function
isIOReturnType : String -> Bool
isIOReturnType retType =
  let trimmed = trim retType
  in isPrefixOf "IO " trimmed || isPrefixOf "IO(" trimmed

||| Helper to split by a multi-char delimiter
splitByDelim : List Char -> List Char -> List Char -> List (List Char)
splitByDelim _ [] acc = [reverse acc]
splitByDelim delim rest@(c :: cs) acc =
  if isPrefixOf delim rest
    then reverse acc :: splitByDelim delim (drop (length delim) rest) []
    else splitByDelim delim cs (c :: acc)

||| Split string by " -> " delimiter
splitByArrow : String -> List String
splitByArrow s =
  let chars = unpack s
  in map (trim . pack) $ splitByDelim (unpack " -> ") chars []

||| Safe init for lists (all but last element)
initList : List a -> List a
initList [] = []
initList [_] = []
initList (x :: xs) = x :: initList xs

||| Extract argument types from a type signature
||| e.g., "funcName : Integer -> List Integer -> IO ()" returns ["Integer", "List Integer"]
||| e.g., "funcName : IO ()" returns []
extractArgTypes : String -> List String
extractArgTypes sig =
  -- Find ':' and get everything after it
  let colonIdx = findSubstr (unpack ":") (unpack sig) 0
  in case colonIdx of
       Nothing => []
       Just idx =>
         -- Get the type part after ':'
         let afterColon = pack $ drop (idx + 1) (unpack sig)
             parts = splitByArrow afterColon
         in case parts of
              [] => []
              [_] => []  -- Only return type, no args
              xs => initList xs  -- All but the last (return type)

||| Parse a function signature line (without export keyword) to check if it's an IO function
||| Only matches functions where the RETURN TYPE is IO, not functions containing IO in args
||| Looks for patterns like:
|||   funcName : IO Type           → argTypes = [], OK
|||   funcName : Integer -> IO ()  → argTypes = ["Integer"], OK
|||   funcName : List (IO Bool)    → NOT matched (return type is List, not IO)
parseIOFuncSignature : String -> String -> Maybe ExportedIOFunc
parseIOFuncSignature modName line =
  let trimmed = trim line
  in if not (isInfixOf " : " trimmed)
       then Nothing
       else
         let retType = getReturnType trimmed
         in if not (isIOReturnType retType)
              then Nothing
              else
                -- Extract function name (before :)
                let parts = words trimmed
                in case parts of
                     (fname :: ":" :: _) =>
                       let args = extractArgTypes trimmed
                       in Just $ MkExportedIOFunc modName fname args
                     _ => Nothing

||| Check if a line is an export visibility modifier
||| Returns True for "export" or "public export" on its own line
isExportLine : String -> Bool
isExportLine line =
  let trimmed = trim line
  in trimmed == "export" || trimmed == "public export"

||| Scan an .idr file and extract all exported IO functions
||| Handles Idris2's multi-line export pattern:
|||   export
|||   funcName : IO Type
||| @filePath - Full path to the .idr file
||| @moduleName - The module name (e.g., "Main.Functions.Vote")
scanModuleForIOFuncs : String -> String -> IO (List ExportedIOFunc)
scanModuleForIOFuncs filePath moduleName = do
  Right content <- readFile filePath
    | Left _ => pure []
  let ls = lines content
  -- Scan pairs of adjacent lines: if line N is "export", check line N+1 for IO signature
  pure $ scanPairs ls
  where
    scanPairs : List String -> List ExportedIOFunc
    scanPairs [] = []
    scanPairs [_] = []
    scanPairs (l1 :: l2 :: rest) =
      if isExportLine l1
        then case parseIOFuncSignature moduleName l2 of
               Just func => func :: scanPairs (l2 :: rest)
               Nothing => scanPairs (l2 :: rest)
        else scanPairs (l2 :: rest)

||| Convert module name to file path
||| e.g., "Main.Functions.Vote" -> "Main/Functions/Vote.idr"
moduleToPath : String -> String
moduleToPath modName =
  let parts = forget $ split (== '.') modName
  in (concat $ intersperse "/" parts) ++ ".idr"

||| Generate a dummy value for a given type
||| Common EVM types: Integer, String, Bool, List, tuples
generateDummyValue : String -> String
generateDummyValue ty =
  let trimmed = trim ty
  in if isPrefixOf "Integer" trimmed || isPrefixOf "Nat" trimmed || isPrefixOf "Int" trimmed
       then "0"
     else if isPrefixOf "String" trimmed
       then "\"\""
     else if isPrefixOf "Bool" trimmed
       then "True"
     else if isPrefixOf "List" trimmed
       then "[]"
     else if isPrefixOf "Maybe" trimmed
       then "Nothing"
     else if isPrefixOf "(" trimmed  -- Tuple types like (Integer, Integer)
       then "(0, 0)"  -- Default tuple with 0s
     else "0"  -- Default to 0 for unknown types (common in EVM)

||| Generate all dummy arguments from argument type list
generateDummyArgs : List String -> String
generateDummyArgs [] = ""
generateDummyArgs (ty :: rest) =
  " " ++ generateDummyValue ty ++ generateDummyArgs rest

||| Generate a call expression for an IO function
||| Functions with args get type-appropriate dummy values
generateFuncCall : ExportedIOFunc -> String
generateFuncCall func =
  let qualifiedName = func.moduleName ++ "." ++ func.funcName
      args = generateDummyArgs func.argTypes
  in "  _ <- " ++ qualifiedName ++ args

||| Generate the DumpcasesWrapper content that imports all modules
||| and calls all exported IO functions
|||
||| This enables comprehensive coverage analysis by ensuring all
||| exported IO functions are reachable from the entry point.
||| Without this, library packages with no main function would have
||| no coverage data for their business logic functions.
generateComprehensiveWrapper : List String -> List ExportedIOFunc -> String
generateComprehensiveWrapper modules funcs =
  let imports = map (\m => "import " ++ m) modules
      funcCalls = map generateFuncCall funcs
      -- Group calls for readability
      header = unlines
        [ "-- ==================================================================="
        , "-- AUTO-GENERATED by idris2-evm-coverage"
        , "-- This wrapper imports all modules and calls all exported IO functions"
        , "-- to enable comprehensive coverage analysis via --dumpcases."
        , "-- ==================================================================="
        , ""
        , "module DumpcasesWrapper"
        , ""
        ]
      importSection = unlines imports
      mainSection = unlines
        [ ""
        , "main : IO ()"
        , "main = do"
        , "  -- Calling all exported IO functions to include them in dumpcases analysis"
        ]
      callsSection = if null funcCalls
                       then "  pure ()  -- No IO functions found"
                       else unlines funcCalls ++ "\n  pure ()"
  in header ++ importSection ++ mainSection ++ callsSection

||| Run idris2-yul --dumpcases and parse output
||| EVM coverage requires idris2-yul compiler for EVM FFI support
||| Creates a temporary executable ipkg since --dumpcases only works with executables
|||
||| AUTO-GENERATION STRATEGY:
||| Instead of only calling runTests (which misses actual contract functions),
||| this scans all modules for exported IO functions and generates a wrapper
||| that calls them all. This ensures comprehensive coverage analysis.
export
runDumpcasesAndParse : String -> String -> IO (Either String (List ClassifiedBranch))
runDumpcasesAndParse ipkgPath outputPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  uid <- generateUid

  -- Read original ipkg to get modules and depends
  Right ipkgContent <- readFile ipkgPath
    | Left err => pure $ Left $ "Failed to read ipkg: " ++ show err

  -- Extract sourcedir and create temp main in correct location
  let sourcedir = extractSourcedir ipkgContent
  let modules = parseModulesFromIpkg ipkgContent
  let tempMainName = "DumpcasesWrapper"
  let tempMainDir = if null sourcedir then projectDir else projectDir ++ "/" ++ sourcedir
  let tempMainPath = tempMainDir ++ "/" ++ tempMainName ++ ".idr"

  -- NEW: Scan all module files to find exported IO functions
  -- This enables comprehensive coverage instead of just test coverage
  let baseDir = if null sourcedir then projectDir else projectDir ++ "/" ++ sourcedir
  allIOFuncs <- traverse (\m => scanModuleForIOFuncs (baseDir ++ "/" ++ moduleToPath m) m) modules
  let ioFuncs = concat allIOFuncs

  -- Generate comprehensive wrapper that calls all exported IO functions
  let tempMainContent = generateComprehensiveWrapper modules ioFuncs

  Right () <- writeFile tempMainPath tempMainContent
    | Left err => pure $ Left $ "Failed to create temp main: " ++ show err

  -- Create temporary ipkg with executable
  let tempIpkgName = "dumpcases-temp-" ++ uid ++ ".ipkg"
  let tempIpkgPath = projectDir ++ "/" ++ tempIpkgName

  -- Extract multi-line depends section from original ipkg
  let dependsLines = extractMultiLineField "depends" ipkgContent
  let dependsSection = if null dependsLines then "depends = base" else dependsLines
  let sourcedirLine = case find (isInfixOf "sourcedir") (lines ipkgContent) of
                        Just l => l
                        Nothing => ""

  -- Get modules string for ipkg
  let modulesStr = concat $ intersperse "\n        , " modules

  let tempIpkgContent = unlines
        [ "package dumpcases-temp"
        , "version = \"0.0.1\""
        , sourcedirLine
        , dependsSection
        , "main = " ++ tempMainName
        , "executable = \"dumpcases-temp\""
        , "opts = \"--dumpcases " ++ outputPath ++ "\""
        , "modules = " ++ tempMainName
        , "        , " ++ modulesStr
        ]

  Right () <- writeFile tempIpkgPath tempIpkgContent
    | Left err => do
        _ <- system $ "rm -f " ++ tempMainPath
        pure $ Left $ "Failed to create temp ipkg: " ++ show err

  -- Run pack build (uses opts from ipkg for --dumpcases)
  -- Note: For EVM contracts, codegen may fail (EVM FFI needs idris2-yul), but
  -- --dumpcases outputs case trees DURING type-checking before codegen.
  -- So we check if output exists first, ignoring exit code if output is valid.
  let cmd = "cd " ++ projectDir ++ " && pack build " ++ tempIpkgName ++ " 2>&1"
  _ <- system cmd  -- Exit code may be non-zero for EVM FFI, but dumpcases may still succeed

  -- DEBUG: Don't cleanup temp files to inspect them
  -- _ <- system $ "rm -f " ++ tempMainPath ++ " " ++ tempIpkgPath

  -- Check if dumpcases output exists (may succeed even if codegen fails)
  Right content <- readFile outputPath
    | Left err => pure $ Left $ "Failed to read dumpcases: " ++ show err
  if null (trim content)
    then pure $ Left "No dumpcases output generated (type-checking may have failed)"
    else pure $ Right $ parseDumpcases content

-- =============================================================================
-- Static Analysis
-- =============================================================================

||| Check if branch class is unknown
isUnknownClass : ClassifiedBranch -> Bool
isUnknownClass cb = case cb.branchClass of
                      BCUnknownCrash _ => True
                      _ => False

||| Analyze dumpcases output and classify all branches
export
analyzeStatic : List ClassifiedBranch -> StaticBranchAnalysis
analyzeStatic branches =
  let canonical = filter (\b => isCanonical b.branchClass) branches
      excluded  = filter (\b => b.branchClass == BCExcludedNoClauses) branches
      bugs      = filter (\b => b.branchClass == BCBugUnhandledInput) branches
      optimizer = filter (\b => b.branchClass == BCOptimizerNat) branches
      unknown   = filter isUnknownClass branches
      compiler  = filter (\b => b.branchClass == BCCompilerGenerated) branches
  in MkStaticBranchAnalysis
       branches
       (length canonical)
       (length excluded)
       (length bugs)
       (length optimizer)
       (length unknown)
       (length compiler)

-- =============================================================================
-- Top-K Targets with Severity
-- =============================================================================

||| Group branches by function name
groupByFunc : List ClassifiedBranch -> List (String, List ClassifiedBranch)
groupByFunc branches = go branches []
  where
    insertOrUpdate : String -> ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    insertOrUpdate fn b [] = [(fn, [b])]
    insertOrUpdate fn b ((k, vs) :: rest) =
      if k == fn then (k, b :: vs) :: rest
      else (k, vs) :: insertOrUpdate fn b rest

    go : List ClassifiedBranch -> List (String, List ClassifiedBranch) -> List (String, List ClassifiedBranch)
    go [] acc = acc
    go (b :: bs) acc = go bs (insertOrUpdate b.branchId.funcName b acc)

||| Extract module name from function name
extractModuleName : String -> String
extractModuleName fn =
  let parts = forget $ split (== '.') fn
  in case init' parts of
       Nothing => ""
       Just modParts => fastConcat $ intersperse "." modParts

||| Convert function branches to HighImpactTarget (using shared core type)
funcToTarget : (String, List ClassifiedBranch) -> HighImpactTarget
funcToTarget (fn, branches) =
  let canonical = filter (\b => isCanonical b.branchClass) branches
      canonicalCount = length canonical
      modName = extractModuleName fn
  in mkUntestedTarget fn modName canonicalCount 0

||| Get top K high-impact targets from branches, sorted by severity
||| @k - Maximum number of targets to return
||| @branches - All classified branches
export
topKTargetsFromBranches : Nat -> List ClassifiedBranch -> List HighImpactTarget
topKTargetsFromBranches k branches =
  let grouped = groupByFunc branches
      targets = map funcToTarget grouped
      -- Filter out functions with 0 canonical branches
      nonEmpty = filter (\t => t.branchCount > 0) targets
  in Coverage.Core.HighImpact.topKTargets k nonEmpty
