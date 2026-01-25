||| Dumpcases Runner - Backend-agnostic --dumpcases execution
|||
||| Provides an interface for running `idris2 --dumpcases` with different
||| code generators (Chez, RefC, EVM).
|||
||| Usage:
|||   result <- runDumpcasesWithBackend ChezBackend projectDir ipkgName
|||   result <- runDumpcasesWithBackend RefcBackend projectDir ipkgName
|||   result <- runDumpcasesWithBackend EvmBackend projectDir ipkgName
module Coverage.Core.DumpcasesRunner

import Data.List
import Data.Maybe
import Data.String
import System
import System.File

%default covering

-- =============================================================================
-- Backend Configuration
-- =============================================================================

||| Code generator backend type
public export
data Backend
  = ChezBackend    -- Default Chez Scheme backend
  | RefcBackend    -- RefC backend (for WASM/DFX)
  | EvmBackend     -- EVM backend (for Ethereum)
  | CustomBackend String  -- Custom codegen name

public export
Show Backend where
  show ChezBackend = "chez"
  show RefcBackend = "refc"
  show EvmBackend = "evm"
  show (CustomBackend name) = name

||| Get the --codegen flag for a backend
||| Chez doesn't need explicit --codegen flag
codegenFlag : Backend -> String
codegenFlag ChezBackend = ""
codegenFlag RefcBackend = "--codegen refc "
codegenFlag EvmBackend = "--codegen evm "
codegenFlag (CustomBackend name) = "--codegen " ++ name ++ " "

-- =============================================================================
-- Build Command Generation
-- =============================================================================

||| Generate idris2 --dumpcases build command
|||
||| @backend - Which code generator to use
||| @projectDir - Project root directory
||| @ipkgName - Package file name (e.g., "myproject.ipkg")
||| @outputPath - Where to write dumpcases output
public export
buildDumpcasesCommand : Backend -> String -> String -> String -> String
buildDumpcasesCommand backend projectDir ipkgName outputPath =
  "cd " ++ projectDir ++ " && idris2 --dumpcases " ++ outputPath ++ " " ++
  codegenFlag backend ++ "--build " ++ ipkgName ++ " 2>&1"

||| Default output path for temporary dumpcases file
public export
defaultOutputPath : String
defaultOutputPath = "/tmp/idris2_dumpcases_output.txt"

||| Backend-specific output path
public export
backendOutputPath : Backend -> String
backendOutputPath ChezBackend = "/tmp/idris2_dumpcases_chez.txt"
backendOutputPath RefcBackend = "/tmp/idris2_dumpcases_refc.txt"
backendOutputPath EvmBackend = "/tmp/idris2_dumpcases_evm.txt"
backendOutputPath (CustomBackend name) = "/tmp/idris2_dumpcases_" ++ name ++ ".txt"

-- =============================================================================
-- Dumpcases Execution
-- =============================================================================

||| Run idris2 --dumpcases with specified backend
|||
||| @backend - Which code generator to use
||| @projectDir - Project root directory (containing .ipkg)
||| @ipkgName - Package file name
||| @outputPath - Where to write dumpcases output
|||
||| Returns: Either error message or dumpcases content
public export
runDumpcases : (backend : Backend)
            -> (projectDir : String)
            -> (ipkgName : String)
            -> (outputPath : String)
            -> IO (Either String String)
runDumpcases backend projectDir ipkgName outputPath = do
  let cmd = buildDumpcasesCommand backend projectDir ipkgName outputPath
  _ <- system cmd
  -- Note: We ignore exit code because dumpcases output is written BEFORE
  -- C compilation. On macOS with Homebrew, RefC backend often fails to find
  -- gmp.h during C compilation, but the dumpcases output is already generated.
  -- We check if the output file exists and has content instead.
  result <- readFile outputPath
  case result of
    Left err => pure $ Left $ "Failed to read dumpcases output: " ++ show err
    Right content =>
      if null (trim content)
        then pure $ Left "No dumpcases output generated (build may have failed)"
        else pure $ Right content

||| Run dumpcases with default output path for backend
public export
runDumpcasesDefault : Backend -> String -> String -> IO (Either String String)
runDumpcasesDefault backend projectDir ipkgName =
  runDumpcases backend projectDir ipkgName (backendOutputPath backend)

-- =============================================================================
-- Convenience Functions for Each Backend
-- =============================================================================

||| Run dumpcases with Chez backend (default)
public export
runDumpcasesChez : String -> String -> IO (Either String String)
runDumpcasesChez = runDumpcasesDefault ChezBackend

||| Run dumpcases with RefC backend (for DFX/WASM)
public export
runDumpcasesRefc : String -> String -> IO (Either String String)
runDumpcasesRefc = runDumpcasesDefault RefcBackend

||| Run dumpcases with EVM backend
public export
runDumpcasesEvm : String -> String -> IO (Either String String)
runDumpcasesEvm = runDumpcasesDefault EvmBackend

-- =============================================================================
-- Pack Build Support
-- =============================================================================

||| Parse modules from ipkg file content
parseModules : String -> List String
parseModules content =
  let ls = lines content
      -- Find lines that contain module names (after "modules = " or continuation lines)
      modulesSection = findModulesSection ls
  in mapMaybe extractModuleName modulesSection
  where
    isModulesStart : String -> Bool
    isModulesStart l = isPrefixOf "modules" (trim l)

    isContinuation : String -> Bool
    isContinuation l =
      let t = trim l
      in not (null t) && not (isPrefixOf "[" t) && not (elem '=' (unpack t))

    extractModuleName : String -> Maybe String
    extractModuleName l =
      let t = trim l
          -- Remove "modules = " prefix if present
          afterEq = case break (== '=') (unpack t) of
                      (_, []) => t
                      (_, '=' :: rest) => trim (pack rest)
                      _ => t
          -- Remove commas
          noComma = pack $ filter (/= ',') (unpack afterEq)
          cleaned = trim noComma
      in if null cleaned || isPrefixOf "--" cleaned || isPrefixOf "[" cleaned
           then Nothing
           else Just cleaned

    collectModules : List String -> List String
    collectModules [] = []
    collectModules (l :: ls) =
      let t = trim l
      in if null t || isPrefixOf "[" t
           then []
           else l :: (if isContinuation (fromMaybe "" (head' ls)) then collectModules ls else [])

    findModulesSection : List String -> List String
    findModulesSection [] = []
    findModulesSection (l :: ls) =
      if isModulesStart l
        then collectModules (l :: ls)
        else findModulesSection ls

||| Run dumpcases using pack by modifying the original ipkg
|||
||| Creates a modified ipkg with a dummy main module and --dumpcases option,
||| then compiles to get case trees for all modules.
public export
runDumpcasesWithPack : (backend : Backend)
                    -> (projectDir : String)
                    -> (ipkgName : String)
                    -> (outputPath : String)
                    -> IO (Either String String)
runDumpcasesWithPack backend projectDir ipkgName outputPath = do
  -- Read the original ipkg file
  let ipkgPath = projectDir ++ "/" ++ ipkgName
  ipkgResult <- readFile ipkgPath
  case ipkgResult of
    Left err => pure $ Left $ "Failed to read ipkg file: " ++ show err
    Right ipkgContent => do
      let modules = parseModules ipkgContent
      if null modules
        then pure $ Left "No modules found in ipkg file"
        else do
          -- Parse sourcedir from ipkg (default to ".")
          let srcDir = parseSourceDir ipkgContent

          -- Create a dummy main module that imports ALL modules
          -- If there's a Test module, try to call runTests from it
          let dummyModName = "DumpcasesMain"
          let importLines = map (\m => "import " ++ m) modules
          let testMod = findTestModule modules
          let mainBody = case testMod of
                           Just tm => tm ++ ".runTests"
                           Nothing => "pure ()"
          let dummyMain = unlines $
                [ "module " ++ dummyModName
                , ""
                ] ++ importLines ++
                [ ""
                , "main : IO ()"
                , "main = " ++ mainBody
                ]
          let dummyPath = if srcDir == "."
                            then projectDir ++ "/" ++ dummyModName ++ ".idr"
                            else projectDir ++ "/" ++ srcDir ++ "/" ++ dummyModName ++ ".idr"

          -- Create modified ipkg that includes all modules plus dummy main
          let cgOpt = case backend of
                        ChezBackend => ""
                        RefcBackend => " --codegen refc"
                        EvmBackend => " --codegen evm"
                        CustomBackend name => " --codegen " ++ name
          let modifiedIpkg = addDumpcasesOpts ipkgContent dummyModName outputPath cgOpt
          let modifiedIpkgPath = projectDir ++ "/dumpcases_temp.ipkg"

          -- Write temporary files
          dummyResult <- writeFile dummyPath dummyMain
          case dummyResult of
            Left err => pure $ Left $ "Failed to write dummy main: " ++ show err
            Right () => do
              ipkgWriteResult <- writeFile modifiedIpkgPath modifiedIpkg
              case ipkgWriteResult of
                Left err => do
                  _ <- system $ "rm -f " ++ dummyPath
                  pure $ Left $ "Failed to write modified ipkg: " ++ show err
                Right () => do
                  -- Run pack build with modified ipkg
                  let cmd = "cd " ++ projectDir ++ " && pack build dumpcases_temp.ipkg 2>&1"
                  exitCode <- system cmd

                  -- Clean up temporary files
                  _ <- system $ "rm -f " ++ dummyPath ++ " " ++ modifiedIpkgPath
                  _ <- system $ "rm -rf " ++ projectDir ++ "/build/exec/dumpcases_*"

                  if exitCode /= 0
                    then pure $ Left $ "pack build failed with exit code " ++ show exitCode
                    else do
                      result <- readFile outputPath
                      case result of
                        Left err => pure $ Left $ "Failed to read dumpcases output: " ++ show err
                        Right content =>
                          if null (trim content)
                            then pure $ Left "No dumpcases output generated"
                            else pure $ Right content
  where
    findTestModule : List String -> Maybe String
    findTestModule [] = Nothing
    findTestModule (m :: ms) =
      if isInfixOf "Test" m || isInfixOf "test" m
        then Just m
        else findTestModule ms

    parseSourceDir : String -> String
    parseSourceDir content =
      let ls = lines content
      in fromMaybe "." (findSourceDir ls)
      where
        stripQuotes : String -> String
        stripQuotes s =
          let t = trim s
              chars = unpack t
          in case chars of
               ('"' :: rest) =>
                 -- Remove trailing quote
                 pack (reverse (drop 1 (reverse rest)))
               _ => t

        findSourceDir : List String -> Maybe String
        findSourceDir [] = Nothing
        findSourceDir (l :: ls) =
          let t = trim l
          in if isPrefixOf "sourcedir" t
               then case break (== '=') (unpack t) of
                      (_, '=' :: rest) => Just (stripQuotes (pack rest))
                      _ => findSourceDir ls
               else findSourceDir ls

    addModule : String -> List String -> List String
    addModule modName [] = []
    addModule modName (l :: ls) =
      if isPrefixOf "modules" (trim l)
        then (l ++ ", " ++ modName) :: addModule modName ls
        else l :: addModule modName ls

    -- Add main, executable, and opts to existing ipkg content
    addDumpcasesOpts : String -> String -> String -> String -> String
    addDumpcasesOpts content modName outPath cgOpt =
      let ls = lines content
          -- Add DumpcasesMain to modules
          modsAdded = addModule modName ls
          -- Add main, executable, and opts
          extras = [ ""
                   , "main = " ++ modName
                   , "executable = dumpcases_runner"
                   , "opts = \"--dumpcases " ++ outPath ++ cgOpt ++ "\""
                   ]
      in unlines (modsAdded ++ extras)
