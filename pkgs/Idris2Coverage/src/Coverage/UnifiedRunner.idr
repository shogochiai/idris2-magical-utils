||| Unified test runner with coverage collection
||| REQ_COV_UNI_001 - REQ_COV_UNI_003
module Coverage.UnifiedRunner

import Coverage.Types
import public Coverage.Collector
import public Coverage.DumpcasesParser
import public Coverage.Core.RuntimeHit
import public Coverage.Core.PathCoverage
import Coverage.Core.DumppathsJson
import Coverage.Exclusions
import Coverage.PathCoverage
import Coverage.Standardization.Types
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

splitPath : String -> (String, String)
splitPath path =
  let parts = forget $ split (== '/') path in
  case reverse parts of
    [] => (".", path)
    [name] => (".", name)
    (name :: revDirs) => (joinBy "/" (reverse revDirs), name)

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
       putStrLn "    Cleaning temp path coverage build..."
       _ <- system cleanCmd
       putStrLn "    Clean complete."
       pure ()
  let directCmd = appPrelude
               ++ " && cd " ++ absProjectDir
               ++ " && \"$APP\" --build " ++ ipkgName ++ " > " ++ directLog ++ " 2>&1"
  putStrLn "    Running direct path coverage build..."
  directExit <- system directCmd
  putStrLn $ "    Direct path coverage build exit: " ++ show directExit
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

||| Parent directory of a path (drop the last "/segment"). "" or "/" at the root.
parentOf : String -> String
parentOf path =
  let segs = forget (split (== '/') path)
      kept = case reverse segs of
               (_ :: rest) => reverse rest
               []          => []
  in case kept of
       []   => ""
       [""] => "/"
       _    => fastConcat (intersperse "/" kept)

||| Rewrite each `path = "rel"` in pack.toml content so a relative dep path
||| resolves from `baseDir` regardless of where the temp pack.toml is written.
||| pack resolves a local dep's `path` relative to the pack.toml that declares it;
||| when we INHERIT an ancestor pack.toml into a package-dir temp pack.toml, its
||| relative paths (e.g. "../idris2-magical-utils/pkgs/Foo") would otherwise
||| resolve from the wrong dir and dependency resolution fails. Absolutizing
||| against the ancestor dir makes them location-independent. Already-absolute
||| paths (leading "/") are left untouched.
absolutizePackPaths : (baseDir : String) -> (content : String) -> String
absolutizePackPaths baseDir content = unlines (map rewriteLine (lines content))
  where
    -- the value inside `path = "..."`, or Nothing if this isn't a path line
    pathValue : String -> Maybe String
    pathValue line =
      let t = trim line in
      if isPrefixOf "path" t
        then case break (== '=') (unpack t) of
               (_, eqRest) =>
                 let afterEq = trim (pack (drop 1 eqRest))
                 in if isPrefixOf "\"" afterEq
                      then Just (pack (takeWhile (/= '"') (drop 1 (unpack afterEq))))
                      else Nothing
        else Nothing

    rewriteLine : String -> String
    rewriteLine line =
      case pathValue line of
        Nothing  => line
        Just rel =>
          if isPrefixOf "/" rel
            then line                                    -- already absolute
            else "path = \"" ++ baseDir ++ "/" ++ rel ++ "\""

||| Read the project's pack.toml, preserving its custom dependency entries.
||| If the package has no pack.toml of its own, walk UP to the nearest ancestor
||| pack.toml and inherit it — pack resolves custom local deps (e.g.
||| idris2-react-native-lib, idris2-delivery-kind) from an ancestor pack.toml, so
||| the temp coverage build must too, else web/native apps and monorepo
||| sub-packages fail with "no matching version is installed". Inherited
||| relative `path =` entries are absolutized against the ancestor dir so they
||| resolve correctly from the package-dir temp pack.toml. Bounded walk.
export
readProjectPackToml : String -> IO String
readProjectPackToml projectDir = do
  absDir <- toAbsolutePath projectDir
  -- The package's own pack.toml: paths are already correct relative to it. But a
  -- monorepo sub-package's own pack.toml is often PARTIAL (e.g. just
  -- `[custom.all.etherclaw]` self-registration) while the real local-dep
  -- definitions (toml, lazyshared, idris2-test-suite, …) live in an ANCESTOR
  -- pack.toml. Stopping at the partial own file made installNeededDepsIntoFork
  -- find 0 local deps → the numerator build then failed "Module … not found".
  -- So MERGE: own (verbatim) ++ nearest ancestor (paths absolutized). Later
  -- duplicate [custom.all.X] blocks are harmless; localDepEntries dedups by name.
  ownContent <- readFile (projectDir ++ "/pack.toml")
  ancestor <- inheritFrom (parentOf absDir) 8
  case ownContent of
    Right own => pure $ if ancestor == "" then own else own ++ "\n\n" ++ ancestor
    Left _    => pure ancestor
  where
    inheritFrom : String -> Nat -> IO String
    inheritFrom _ Z = pure ""
    inheritFrom dir (S fuel) =
      if dir == "" || dir == "/"
        then do
          Right c <- readFile "/pack.toml"
            | Left _ => pure ""
          pure (absolutizePackPaths "/" c)
        else do
          Right c <- readFile (dir ++ "/pack.toml")
            | Left _ => inheritFrom (parentOf dir) fuel
          pure (absolutizePackPaths dir c)

||| The string value of a `key = "value"` toml line (already trimmed), or Nothing.
tomlStringField : (key : String) -> String -> Maybe String
tomlStringField key line =
  let t = trim line in
  if isPrefixOf key t
    then case break (== '=') (unpack t) of
           (_, eqRest) =>
             let afterEq = trim (pack (drop 1 eqRest))
             in if isPrefixOf "\"" afterEq
                  then Just (pack (takeWhile (/= '"') (drop 1 (unpack afterEq))))
                  else Nothing
    else Nothing

||| Extract (depName, absolutePath, ipkgFileName) for every `type = "local"` custom
||| dep in pack.toml content. Paths are expected already absolutized (see
||| absolutizePackPaths). Used to install local deps into the FORKED compiler's
||| package path before a direct `idris2 --build` (which reads neither pack.toml
||| nor pack's store — only ~/.idris2 and ./depends).
export
localDepEntries : String -> List (String, String, String)
localDepEntries content = go (lines content) Nothing Nothing Nothing Nothing []
  where
    -- the dep NAME from a `[custom.all.<name>]` (or `[custom.<profile>.<name>]`)
    -- header — the last dot-segment, trailing ']' stripped.
    blockName : String -> Maybe String
    blockName line =
      let t = trim line in
      if isPrefixOf "[custom." t
        then let inner = pack (filter (\c => c /= '[' && c /= ']') (unpack t))
             in case reverse (forget (split (== '.') inner)) of
                  (n :: _) => Just (trim n)
                  []       => Nothing
        else Nothing

    -- track current block's name/type/path/ipkg; flush (name,path,ipkg) when type=local
    go : List String -> (mname : Maybe String) -> (mty : Maybe String) -> (mpath : Maybe String)
       -> (mipkg : Maybe String) -> List (String, String, String) -> List (String, String, String)
    flush : Maybe String -> Maybe String -> Maybe String -> Maybe String
          -> List (String, String, String) -> List (String, String, String)
    flush (Just n) (Just "local") (Just p) (Just i) acc = (n, p, i) :: acc
    flush _ _ _ _ acc = acc

    go [] mname mty mpath mipkg acc = reverse (flush mname mty mpath mipkg acc)
    go (l :: ls) mname mty mpath mipkg acc =
      let t = trim l in
      case blockName l of
        Just n  => go ls (Just n) Nothing Nothing Nothing (flush mname mty mpath mipkg acc)
        Nothing =>
          if isPrefixOf "[" t
            -- a non-custom block ends the current custom block
            then go ls Nothing Nothing Nothing Nothing (flush mname mty mpath mipkg acc)
            else
              let mty'   = maybe mty   Just (tomlStringField "type" l)
                  mpath' = maybe mpath Just (tomlStringField "path" l)
                  mipkg' = maybe mipkg Just (tomlStringField "ipkg" l)
              in go ls mname mty' mpath' mipkg' acc

||| The coverage stack the temp ipkg always pulls in (generateTempIpkgWithOpts
||| hardcodes idris2-coverage; it depends on these). Always install these.
export
coverageStackDeps : List String
coverageStackDeps =
  [ "idris2-coverage", "idris2-coverage-core", "idris2-coverage-standardization" ]

||| Install ONLY the local deps this project needs into the FORKED compiler's
||| package path so a direct `idris2 --build` (the only path that honours
||| --dumppaths-json) can resolve them — fork reads ~/.idris2 / ./depends, not
||| pack.toml / pack's store. Filtered to (project ipkg `depends` ∪ coverage
||| stack) so we don't build unrelated heavyweight monorepo packages (e.g. the
||| canister). Best-effort + multi-round for dep ordering. No-op when IDRIS2_BIN
||| is unset (pack resolves deps itself then).
export
installNeededDepsIntoFork : (projectDepends : List String) -> (packTomlContent : String) -> IO ()
installNeededDepsIntoFork projectDepends packTomlContent = do
  Just idris2 <- getEnv "IDRIS2_BIN"
    | Nothing => pure ()
  let wanted = projectDepends ++ coverageStackDeps
  let deps = filter (\(n, _, _) => elem n wanted) (localDepEntries packTomlContent)
  -- Multi-round (no topo-sort): a dep whose own deps aren't installed yet fails
  -- this round but succeeds once they land in a later round (installs idempotent).
  let rounds = 4
  putStrLn $ "    [dep-install] " ++ show (length deps) ++ " local deps for forked compiler"
  for_ (replicate rounds ()) $ \_ => traverse_ (installOne idris2) deps
  where
    installOne : String -> (String, String, String) -> IO ()
    installOne idris2 (_, path, ipkg) = do
      -- Always build THEN install with the FORKED compiler, in the dep's own dir.
      -- Build is incremental (cached TTCs), and only a successful build installs a
      -- real package — so a dep whose own deps are missing this round simply fails
      -- here and is retried next round (idempotent). && chains so a failed build
      -- never installs a hollow package.
      _ <- system $ "cd " ++ path ++ " && " ++ idris2 ++ " --build " ++ ipkg
                 ++ " > /dev/null 2>&1 && " ++ idris2 ++ " --install " ++ ipkg
                 ++ " > /dev/null 2>&1"
      pure ()

||| Check if a file exists
fileExists : String -> IO Bool
fileExists path = do
  Right _ <- readFile path
    | Left _ => pure False
  pure True

||| Write pack.toml only if it doesn't exist, return True if we created it.
||| (Local-dep install into the forked compiler happens separately via
||| installNeededDepsIntoFork at the path-coverage entry points, where the
||| project's declared depends are known and can filter out unrelated heavyweight
||| monorepo packages.)
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
export
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
      -- strip inline `-- ...` comments PER LINE before joining, so a commented
      -- module (e.g. "-- , EtherClaw.Main  -- TEMP DISABLED") contributes nothing
      -- instead of being mangled into a fake module name.
      cleanedLines = map stripInlineComment moduleLines
      joined = fastConcat $ intersperse " " cleanedLines
      afterEq = case break (== '=') (unpack joined) of
                  (_, rest) => pack $ drop 1 rest
      parts = forget $ split (== ',') afterEq
      named = map (trim . pack . filter isModuleChar . unpack . trim) parts
  in filter isValidModule named
  where
    isModuleChar : Char -> Bool
    isModuleChar c = isAlphaNum c || c == '.' || c == '_'

    -- a real module name starts with an uppercase letter; drops empties and junk
    isValidModule : String -> Bool
    isValidModule m = case unpack m of
                        (c :: _) => isUpper c
                        []       => False

    stripInlineComment : String -> String
    stripInlineComment s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go []                = []
        go ['-']             = ['-']
        go ('-' :: '-' :: _) = []        -- start of a `--` comment: drop the rest
        go (c :: cs)         = c :: go cs

    collectModuleLines : List String -> Bool -> List String
    collectModuleLines [] _ = []
    collectModuleLines (l :: ls) False =
      if isInfixOf "modules" l && isInfixOf "=" l
         then l :: collectModuleLines ls True
         else collectModuleLines ls False
    collectModuleLines (l :: ls) True =
      let trimmed = trim l
      in if null trimmed || isPrefixOf "--" trimmed
            then collectModuleLines ls True       -- skip blank / fully-commented lines
            else if isPrefixOf "," trimmed || isPrefixOf " " l || isPrefixOf "\t" l
                    then l :: collectModuleLines ls True
                    else []

||| Find and read first matching ipkg file content
||| Prefers files without -minimal, -test, -temp prefixes
||| Lowercase + strip non-alphanumerics — for matching an ipkg basename to a dir
||| name regardless of case/separators (e.g. "EtherClaw" ~ "etherclaw.ipkg").
normIdent : String -> String
normIdent = pack . map toLower . filter isAlphaNum . unpack

||| The last path segment of a dir (its name).
dirBaseName : String -> String
dirBaseName dir =
  -- Drop empty AND "." segments: toAbsolutePath "." yields ".../EtherClaw/."
  -- whose naive last segment is "." → empty want → chooseMainIpkg name-match
  -- fails → arbitrary ipkg (release-bundle, depends lack toml) → numerator
  -- "Module Language.TOML not found". Filtering "." recovers "EtherClaw".
  case reverse (filter (\s => s /= "" && s /= ".") (forget (split (== '/') dir))) of
    (s :: _) => s
    []       => dir

||| Choose the project's main ipkg from the candidates. CRUCIAL fix: a package dir
||| can hold MANY ipkgs (etherclaw.ipkg + release-bundle.ipkg + various *-test.ipkg);
||| picking an arbitrary first one yields the WRONG dependency list (e.g.
||| release-bundle's deps lack `toml`, so the coverage build then fails "Module
||| Language.TOML not found"). Prefer, in order: (1) the ipkg whose basename matches
||| the dir name, (2) any non-test/minimal/temp ipkg, (3) any ipkg.
chooseMainIpkg : (projectDir : String) -> (ipkgFiles : List String) -> Maybe String
chooseMainIpkg projectDir ipkgFiles =
  let isAux = \f => isInfixOf "-minimal" f || isInfixOf "-test" f || isInfixOf "-tests" f || isPrefixOf "temp-" f
      mains = filter (not . isAux) ipkgFiles
      wantName = normIdent (dirBaseName projectDir)
      byName = filter (\f => normIdent (dropIpkgExt f) == wantName) mains
  in case byName of
       (f :: _) => Just f
       [] => case mains of
               (f :: _) => Just f
               [] => case ipkgFiles of
                       (f :: _) => Just f
                       [] => Nothing
  where
    dropIpkgExt : String -> String
    dropIpkgExt f = if isSuffixOf ".ipkg" f
                      then substr 0 (length f `minus` 5) f
                      else f

findIpkgContent : String -> IO (Maybe String)
findIpkgContent projectDir = do
  Right entries <- listDir projectDir
    | Left _ => pure Nothing
  let ipkgFiles = filter (isSuffixOf ".ipkg") entries
  -- chooseMainIpkg matches an ipkg basename against the DIR NAME. A bare "."
  -- projectDir (the common case when the target is a relative `foo.ipkg`) has
  -- dirBaseName "." → empty want → the name-match fails and we'd pick an
  -- arbitrary non-aux ipkg (e.g. release-bundle.ipkg, whose depends LACK toml →
  -- numerator build "Module Language.TOML not found"). Absolutize first so
  -- dirBaseName sees the real package dir name (EtherClaw → etherclaw.ipkg).
  absDir <- toAbsolutePath projectDir
  case chooseMainIpkg absDir ipkgFiles of
    Nothing => pure Nothing
    Just f => do
      Right content <- readFile (projectDir ++ "/" ++ f)
        | Left _ => pure Nothing
      pure (Just content)

||| Find the project's main ipkg PATH (not content), same selection as
||| findIpkgContent. Used by the chunked path-coverage denominator.
findProjectIpkgPath : String -> IO (Maybe String)
findProjectIpkgPath projectDir = do
  Right entries <- listDir projectDir
    | Left _ => pure Nothing
  let ipkgFiles = filter (isSuffixOf ".ipkg") entries
  -- Same absolutize-before-name-match fix as findIpkgContent (see note there).
  absDir <- toAbsolutePath projectDir
  case chooseMainIpkg absDir ipkgFiles of
    Nothing => pure Nothing
    Just f => pure (Just (projectDir ++ "/" ++ f))

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

jsonString : String -> String
jsonString s =
  "\"" ++ fastConcat (map escapeChar (unpack s)) ++ "\""
  where
    escapeChar : Char -> String
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = singleton c

maybeNatJson : Maybe Nat -> String
maybeNatJson Nothing = "null"
maybeNatJson (Just n) = show n

maybeStringJson : Maybe String -> String
maybeStringJson Nothing = "null"
maybeStringJson (Just s) = jsonString s

obligationClassName : ObligationClass -> String
obligationClassName ReachableObligation = "ReachableObligation"
obligationClassName LogicallyUnreachable = "LogicallyUnreachable"
obligationClassName UserAdmittedPartialGap = "UserAdmittedPartialGap"
obligationClassName CompilerInsertedArtifact = "CompilerInsertedArtifact"
obligationClassName ExternalEffectBoundary = "ExternalEffectBoundary"
obligationClassName UnknownClassification = "UnknownClassification"
obligationClassName StubbedReach = "StubbedReach"

pathStepToJson : PathStep -> String
pathStepToJson step =
  "{"
    ++ "\"node_id\":" ++ jsonString step.nodeId ++ ","
    ++ "\"branch_index\":" ++ show step.branchIndex ++ ","
    ++ "\"origin\":" ++ jsonString step.origin ++ ","
    ++ "\"case_index\":" ++ maybeNatJson step.caseIndex ++ ","
    ++ "\"branch_label\":" ++ maybeStringJson step.branchLabel ++ ","
    ++ "\"source_span\":" ++ maybeStringJson step.sourceSpan
    ++ "}"

pathToJson : PathObligation -> String
pathToJson path =
  "{"
    ++ "\"path_id\":" ++ jsonString path.pathId ++ ","
    ++ "\"classification\":" ++ jsonString (obligationClassName path.classification) ++ ","
    ++ "\"terminal_kind\":" ++ jsonString path.terminalKind ++ ","
    ++ "\"terminal_clause_id\":" ++ maybeNatJson path.terminalClauseId ++ ","
    ++ "\"steps\":[" ++ fastConcat (intersperse "," (map pathStepToJson path.steps)) ++ "],"
    ++ "\"source_span_union\":" ++ maybeStringJson path.sourceSpanUnion ++ ","
    ++ "\"path_length\":" ++ show path.pathLength
    ++ "}"

pathObligationFunctionToJson : PathObligation -> String
pathObligationFunctionToJson path =
  "{"
    ++ "\"function_name\":" ++ jsonString path.functionName ++ ","
    ++ "\"paths\":[" ++ pathToJson path ++ "]"
    ++ "}"

pathObligationsToDumppathsJson : List PathObligation -> String
pathObligationsToDumppathsJson paths =
  "{"
    ++ "\"compiler_version\":\"chunked-static-fallback\","
    ++ "\"export_kind\":\"canonical_intrafunction_paths\","
    ++ "\"path_schema_version\":1,"
    ++ "\"functions\":["
    ++ fastConcat (intersperse "," (map pathObligationFunctionToJson paths))
    ++ "]}"

chunkList : Nat -> List a -> List (List a)
chunkList _ [] = []
chunkList Z xs = [xs]
chunkList size xs =
  let here = take size xs
      rest = drop size xs
  in if null here then [] else here :: chunkList size rest

runStaticDumppathsJsonWhole : String -> IO (Either String String)
runStaticDumppathsJsonWhole ipkgPath = do
  let (projectDir, ipkgName) = splitPath ipkgPath
  uid <- getUniqueId
  idris2Override <- getEnv "IDRIS2_BIN"
  absProjectDir <- toAbsolutePath projectDir
  let dumppathsPath = "/tmp/idris2_static_dumppaths_" ++ uid ++ ".json"
  let logPath = "/tmp/idris2_static_dumppaths_" ++ uid ++ ".log"
  let appPrelude = buildPrelude idris2Override
  let cmd = appPrelude
         ++ " && cd " ++ absProjectDir
         ++ " && \"$APP\" --dumppaths-json " ++ dumppathsPath
         ++ " --build " ++ ipkgName
         ++ " > " ++ logPath ++ " 2>&1"
  _ <- system cmd
  contentResult <- readFile dumppathsPath
  logResult <- readFile logPath
  removeFileIfExistsSafe dumppathsPath
  removeFileIfExistsSafe logPath
  case contentResult of
    Right content =>
      if null (trim content)
         then pure $ Left "Static dumppaths JSON was empty"
         else pure $ Right content
    Left err =>
      let logTail = case logResult of
                      Left _ => ""
                      Right logContent => unlines (reverse (take 20 (reverse (lines logContent))))
      in pure $ Left $ "Failed to read static dumppaths JSON: " ++ show err
                    ++ if null logTail then "" else "\nBuild log tail:\n" ++ logTail

runStaticDumppathsJsonChunk : String -> String -> String -> List String -> String -> List String -> Nat -> IO (Either String (List PathObligation))
runStaticDumppathsJsonChunk projectDir sourcedir tempBuildDir projectDepends packTomlContent modules idx = do
  uid <- getUniqueId
  idris2Override <- getEnv "IDRIS2_BIN"
  absProjectDir <- toAbsolutePath projectDir
  let tempModName = "TempStaticDumppaths_" ++ uid ++ "_" ++ show idx
  let tempExecName = "temp-static-dumppaths-" ++ uid ++ "-" ++ show idx
  let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
  let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
  let tempIpkgName = tempExecName ++ ".ipkg"
  let dumppathsPath = "/tmp/idris2_static_dumppaths_chunk_" ++ uid ++ "_" ++ show idx ++ ".json"
  let logPath = "/tmp/idris2_static_dumppaths_chunk_" ++ uid ++ "_" ++ show idx ++ ".log"
  let packTomlPath = projectDir ++ "/pack.toml"
  let runnerSource = generateImportWrapper tempModName modules
  Right () <- writeFile tempIdrPath runnerSource
    | Left err => pure $ Left $ "Failed to write static chunk wrapper: " ++ show err
  let allModules = tempModName :: modules
  -- Reuse the PROJECT's existing build/ttc as the build dir so the chunk does NOT
  -- recompile the transitive dependency closure of its modules from scratch (the
  -- closure can be ~the whole package on densely-interconnected projects, which
  -- OOM-kills one chunk's single compiler process — chunking gives no memory
  -- benefit if every chunk rebuilds everything). With the project's build/ as
  -- builddir, Idris hash-checks and reuses the already-compiled module TTCs
  -- (from a prior `pack build`), compiling only the tiny temp wrapper. Falls back
  -- to a fresh tempBuildDir transparently if the project hasn't been built yet.
  hasPrebuilt <- exists (projectDir ++ "/build/ttc")
  let buildDirForChunk = if hasPrebuilt then "build" else tempBuildDir
  let ipkgContent = generateTempIpkgWithOpts tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just ("--dumppaths-json " ++ dumppathsPath)) (Just buildDirForChunk)
  Right () <- writeFile tempIpkgPath ipkgContent
    | Left err => do
        removeFileIfExists tempIdrPath
        pure $ Left $ "Failed to write static chunk ipkg: " ++ show err
  Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
    | Left err => do
        removeFileIfExists tempIdrPath
        removeFileIfExists tempIpkgPath
        pure $ Left err
  let appPrelude = buildPrelude idris2Override
  let cmd = appPrelude
         ++ " && cd " ++ absProjectDir
         ++ " && \"$APP\" --build " ++ tempIpkgName
         ++ " > " ++ logPath ++ " 2>&1"
  _ <- system cmd
  contentResult <- readFile dumppathsPath
  logResult <- readFile logPath
  removeFileIfExists tempIdrPath
  removeFileIfExists tempIpkgPath
  cleanupPackToml packTomlPath createdPackToml
  removeFileIfExistsSafe dumppathsPath
  removeFileIfExistsSafe logPath
  case contentResult of
    Left err =>
      let logTail = case logResult of
                      Left _ => ""
                      Right logContent => unlines (reverse (take 12 (reverse (lines logContent))))
      in pure $ Left $ "Static chunk " ++ show idx ++ " failed to produce dumppaths JSON: " ++ show err
                    ++ if null logTail then "" else "\nBuild log tail:\n" ++ logTail
    Right content =>
      if null (trim content)
         then pure $ Left $ "Static chunk " ++ show idx ++ " produced empty dumppaths JSON"
         else case parseDumppathsJson content of
                Left err => pure $ Left $ "Static chunk " ++ show idx ++ " produced unparsable dumppaths JSON: " ++ err
                Right paths => pure $ Right paths

runStaticDumppathsJsonChunks : String -> String -> IO (Either String String)
runStaticDumppathsJsonChunks ipkgPath wholeErr = do
  let (projectDir, _) = splitPath ipkgPath
  Right ipkgContent <- readFile ipkgPath
    | Left err => pure $ Left $ wholeErr ++ "\nChunked fallback could not read ipkg: " ++ show err
  let projectDepends = parseIpkgDepends ipkgContent
  let sourcedir = parseIpkgSourcedir ipkgContent
  let projectModules = parseIpkgModules ipkgContent
  if null projectModules
     then pure $ Left $ wholeErr ++ "\nChunked fallback found no modules in ipkg."
     -- Chunks run SEQUENTIALLY in fixed groups of 8 (chunkList 8 below), so peak
     -- memory is bounded by one chunk's transitive build, NOT the module total —
     -- a large package is exactly the case chunking is FOR. The ceiling only
     -- bounds wall-clock (chunk count = ceil(n/8)); 400 modules = ~50 chunks.
     -- (Was 80, which wrongly skipped chunking for big packages like EtherClaw
     -- (158 mods) and left only the OOM-prone whole-package attempt.)
     else if length projectModules > 400
             then pure $ Left $
               wholeErr ++ "\nChunked fallback skipped: package has "
               ++ show (length projectModules)
               ++ " modules, which exceeds the local resource-ceiling threshold."
     else do
       uid <- getUniqueId
       let tempBuildDir = ".idris2-static-dumppaths-build-" ++ uid
       projectPackToml <- readProjectPackToml projectDir
       let packTomlContent = generateTempPackToml projectPackToml
       putStrLn $ "    Static whole-package fallback failed; trying chunked static path obligations (" ++ show (length projectModules) ++ " modules)..."
       chunkResults <- runChunks projectDir sourcedir tempBuildDir projectDepends packTomlContent (chunkList 8 projectModules) 0 []
       _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
       case chunkResults of
         Left err => pure $ Left $ wholeErr ++ "\nChunked fallback failed: " ++ err
         Right paths =>
           -- Apply the standard path-coverage exclusions to the denominator (the
           -- UnifiedRunner path previously skipped them, so harness/runtime-only
           -- obligations were wrongly counted). idris2FullExclusions drops the
           -- compiler/builtin/test noise; the extra TempStaticDumppaths pattern drops
           -- the per-chunk generated wrapper Mains (`TempStaticDumppaths_test_*.main`),
           -- which are this chunked-build's own scaffolding, not product code.
           let excl     = MkLoadedExclusions
                            (prefixPattern "TempStaticDumppaths" "Chunked-dumppaths build scaffolding (generated per-chunk Main wrapper)"
                              :: idris2FullExclusions) "builtin"
               filtered = filterPathObligations excl emptyExclusionConfig (nub paths) in
           if null filtered
              then pure $ Left $ wholeErr ++ "\nChunked fallback produced zero path obligations."
              else pure $ Right $ pathObligationsToDumppathsJson filtered
  where
    runChunks : String -> String -> String -> List String -> String -> List (List String) -> Nat -> List PathObligation -> IO (Either String (List PathObligation))
    runChunks _ _ _ _ _ [] _ acc = pure $ Right acc
    runChunks projectDir sourcedir tempBuildDir deps packTomlContent (mods :: rest) idx acc = do
      result <- runStaticDumppathsJsonChunk projectDir sourcedir tempBuildDir deps packTomlContent mods idx
      -- Clean ONLY the per-chunk temp build dir between chunks (bounds disk on
      -- large packages). NEVER touch the project's real build/ — when chunks reuse
      -- it (buildDirForChunk="build") deleting it would force a full recompile and
      -- defeat the dependency-reuse that keeps each chunk's memory bounded.
      _ <- system $ "rm -rf " ++ projectDir ++ "/" ++ tempBuildDir
      case result of
        Left err => pure $ Left err
        Right paths => runChunks projectDir sourcedir tempBuildDir deps packTomlContent rest (idx + 1) (paths ++ acc)

||| Build an ipkg with forked Idris2 `--dumppaths-json` and return the static
||| path-obligation JSON. This is a conservative fallback for projects whose
||| runtime-instrumented test runner is too large to link or execute locally:
||| the denominator remains real, while runtime hits are intentionally empty.
||| Module-count above which we skip the whole-package dumppaths attempt and go
||| straight to chunked obligations. The whole-package `--dumppaths-json` builds
||| every module into ONE process and is the memory hog that the OS OOM-kills on
||| large packages (e.g. EtherClaw at ~140 modules); attempting it first just
||| wastes minutes and risks the kill before the (light, sequential) chunked path
||| ever runs. Below this, the whole attempt is cheap and yields a single clean JSON.
staticWholeModuleCeiling : Nat
staticWholeModuleCeiling = 40

export
runStaticDumppathsJson : String -> IO (Either String String)
runStaticDumppathsJson ipkgPath = do
  -- Install the project's custom LOCAL deps into the forked compiler's package
  -- path FIRST. runStaticDumppathsJsonWhole builds the project's REAL ipkg with a
  -- direct `idris2 --build` (the only path that honours --dumppaths-json), which
  -- resolves deps from ~/.idris2 / ./depends only — not pack.toml / pack store.
  -- Without this, web/native apps and monorepo sub-packages that inherit custom
  -- deps from an ancestor pack.toml (e.g. idris2-delivery-kind) fail dep
  -- resolution. No-op when IDRIS2_BIN is unset.
  let (staticProjectDir, _) = splitPath ipkgPath
  staticPackToml <- readProjectPackToml staticProjectDir
  staticDepends  <- readProjectDepends staticProjectDir
  installNeededDepsIntoFork staticDepends staticPackToml
  -- Peek the module count: large packages skip the OOM-prone whole-package build
  -- and chunk directly (chunks run sequentially in groups of 8 — bounded memory).
  modCount <- do
    Right c <- readFile ipkgPath
      | Left _ => pure 0
    pure (length (parseIpkgModules c))
  if modCount > staticWholeModuleCeiling
    then do
      putStrLn $ "    Large package (" ++ show modCount
              ++ " modules > " ++ show staticWholeModuleCeiling
              ++ "); skipping whole-package dumppaths and chunking directly..."
      runStaticDumppathsJsonChunks ipkgPath
        ("Whole-package dumppaths skipped for large package (" ++ show modCount ++ " modules).")
    else do
      whole <- runStaticDumppathsJsonWhole ipkgPath
      case whole of
        Right content => pure $ Right content
        Left err => runStaticDumppathsJsonChunks ipkgPath err

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

      buildResult <- buildIpkgWithClean False projectDir tempIpkgName
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

||| Forward declaration so the chunked dispatcher (runTestsWithFunctionHits'
||| where-block) can reference this single-build helper, whose definition lives
||| after the dispatcher. Idris resolves the name from this signature.
runFunctionHitsOnce : String -> List String -> Nat -> IO (Either String (List FunctionRuntimeHit))

||| Test-module count above which function-hits coverage is built+run in
||| sequential chunks instead of one whole-suite process (the 8GB-host OOM fix).
functionHitsChunkTestModuleCeiling : Nat
functionHitsChunkTestModuleCeiling = 12

||| Number of test modules built+run together in one function-hits chunk process.
functionHitsChunkSize : Nat
functionHitsChunkSize = 8

||| Run tests and return per-function runtime coverage data
||| This is the recommended API for accurate severity calculation
|||
||| @projectDir - Path to project root (containing .ipkg)
||| @testModules - List of test module names
||| @timeout - Max seconds for build+run
|||
||| For large test suites (> functionHitsChunkTestModuleCeiling test modules) the
||| build+run is split into sequential per-chunk processes to bound peak memory
||| (the 8GB-host OOM fix): a single build+run that links and executes the FULL
||| suite holds every module's closures live at once. Per-function hits from
||| separate chunks are merged by funcName (static fields = max, runtime fields =
||| max, so a function exercised in any chunk reports its best observed coverage
||| and is never double-counted). Small suites keep the original single build+run.
export
runTestsWithFunctionHits : (projectDir : String)
                          -> (testModules : List String)
                          -> (timeout : Nat)
                          -> IO (Either String (List FunctionRuntimeHit))
runTestsWithFunctionHits projectDir testModules timeout =
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ =>
      if length testModules > functionHitsChunkTestModuleCeiling
        then runChunkedFunctionHits projectDir testModules timeout
        else runFunctionHitsOnce projectDir testModules timeout
  where
    ||| Merge per-function hits across chunks by funcName. Static fields
    ||| (canonicalCount, totalExprs) are identical wherever the function appears
    ||| so max is a safe pick; runtime fields (executedCount, coveredExprs) take
    ||| the max across chunks — a function exercised by tests in any chunk reports
    ||| its best observed coverage, and is never double-counted by summing.
    mergeFunctionHit : FunctionRuntimeHit -> FunctionRuntimeHit -> FunctionRuntimeHit
    mergeFunctionHit a b =
      MkFunctionRuntimeHit a.funcName a.schemeFunc
        (max a.canonicalCount b.canonicalCount)
        (max a.executedCount b.executedCount)
        (max a.totalExprs b.totalExprs)
        (max a.coveredExprs b.coveredExprs)

    insertHit : List FunctionRuntimeHit -> FunctionRuntimeHit -> List FunctionRuntimeHit
    insertHit [] h = [h]
    insertHit (x :: xs) h =
      if x.funcName == h.funcName
        then mergeFunctionHit x h :: xs
        else x :: insertHit xs h

    mergeAllHits : List FunctionRuntimeHit -> List FunctionRuntimeHit -> List FunctionRuntimeHit
    mergeAllHits acc [] = acc
    mergeAllHits acc (h :: hs) = mergeAllHits (insertHit acc h) hs

    runChunks : List (List String) -> Nat -> List FunctionRuntimeHit -> IO (Either String (List FunctionRuntimeHit))
    runChunks [] _ acc = pure $ Right acc
    runChunks (mods :: rest) idx acc = do
      putStrLn $ "    Function-hits chunk " ++ show idx ++ " (" ++ show (length mods) ++ " test modules)..."
      chunkResult <- runFunctionHitsOnce projectDir mods timeout
      case chunkResult of
        Left err => pure $ Left $ "Function-hits chunk " ++ show idx ++ " failed: " ++ err
        Right hits => runChunks rest (idx + 1) (mergeAllHits acc hits)

    runChunkedFunctionHits : String -> List String -> Nat -> IO (Either String (List FunctionRuntimeHit))
    runChunkedFunctionHits projectDir testModules timeout = do
      putStrLn $ "    Large test suite (" ++ show (length testModules)
              ++ " test modules > " ++ show functionHitsChunkTestModuleCeiling
              ++ "); chunking function-hits coverage to bound memory..."
      runChunks (chunkList functionHitsChunkSize testModules) 0 []

-- One build+run of function-hits coverage over the given test modules.
-- (Type signature forward-declared above so the chunked dispatcher can call it.)
-- Builds the test exe with --dumpcases (static denominator) + profiler, runs it
-- once, returns per-function hits.
runFunctionHitsOnce projectDir testModules timeout = do
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

||| Normalize a recorded path-id's record-projection segments so they match the
||| dumppaths denominator's bare-field form. The fork compiler's recordPathHit names
||| a record field accessor with Idris's projection DISPLAY syntax `(.field)`, but
-- NOTE: the old `normalizeProjectionName` polyfill (which stripped the `(.field)`
-- projection wrapper so a recorded `(.chainId)` would join a BARE-`chainId`
-- denominator) was removed once the fork compiler was fixed to emit record-field
-- accessors as a SINGLE canonical `(.field)` form on BOTH sides (the bare selector
-- wrapper is dropped from the dumppaths denominator; see isRecordSelectorWrapper in
-- Compiler.Common). The two sides now agree natively, so the recorded id must be
-- used verbatim — re-stripping `(.field)`→`field` here would re-break the join.

||| Split off ONLY a trailing ",<digits>" count, leaving the rest as the path-id.
||| The path-id itself contains commas (where-bound helpers: "…registryReqId,afterFirst,go#p0"),
||| so splitting on EVERY comma truncated the name (same class as the dfx comma bug).
||| The recorded line is "<path-id>,<count>"; the count is the final comma-field IFF
||| it is all digits — otherwise the whole line is the path-id (no count).
splitTrailingCount : String -> (String, Nat)
splitTrailingCount s =
  case reverse (forget (split (== ',') s)) of
    (lastSeg :: restRev@(_ :: _)) =>
      let lt = trim lastSeg in
      if lt /= "" && all isDigit (unpack lt)
        then (trim (joinBy "," (reverse restRev)), fromMaybe 1 (parsePositive lt))
        else (trim s, 1)
    _ => (trim s, 1)

parsePathHitLineLocal : String -> Maybe PathRuntimeHit
parsePathHitLineLocal line =
  let trimmed = trim line in
  if null trimmed || isPrefixOf "#" trimmed
     then Nothing
     else let (pathId, cnt) = splitTrailingCount trimmed
          in Just (MkPathRuntimeHit pathId cnt)

||| Tests per intra-module slice. The exe is built ONCE and run repeatedly with
||| IDRIS2COV_TEST_OFFSET/_LIMIT windows of this size — each a fresh process whose
||| heap is reclaimed, so a single huge test module (EtherClaw's ~1100 tests)
||| never executes in one OOM-prone process. Test modules that honour the env
||| (via Idris2.TestSuite.Runner) get real intra-module chunking; modules that
||| ignore it run everything on the first slice and the loop detects that and
||| stops (see runExeSlices).
intraSliceLimit : Nat
intraSliceLimit = 100

||| Safety ceiling on the offset walk (slices), so a runner that neither honours
||| nor signals end can't loop forever. 200 slices × 100 = 20k tests.
intraSliceMaxSlices : Nat
intraSliceMaxSlices = 200

||| Per-slice wall-clock timeout (seconds). A test that blocks in a syscall
||| (e.g. an isolated Boundary.Sys runProc on a pipe that never returns) would
||| otherwise hang the whole walk. On timeout the slice is killed (perl alarm),
||| its partial write-on-first-hit hits are kept, and the walk advances to the
||| next window. Generous so a slow-but-progressing instrumented slice finishes.
intraSliceTimeoutSecs : Nat
intraSliceTimeoutSecs = 150

||| Parse "Results: P passed, F failed" → (P, F). Nothing if absent. Pulls the
||| whitespace-separated tokens of the LAST Results line that parse as Nat.
parseResultsPF : String -> Maybe (Nat, Nat)
parseResultsPF out =
  case lastResultsLine (filter (isInfixOf "Results:") (lines out)) of
    Nothing => Nothing
    Just line =>
      case mapMaybe (parsePositive {a = Nat}) (words line) of
        (p :: f :: _) => Just (p, f)
        (p :: [])     => Just (p, 0)
        []            => Nothing
  where
    lastResultsLine : List String -> Maybe String
    lastResultsLine []        = Nothing
    lastResultsLine [x]       = Just x
    lastResultsLine (_ :: xs) = lastResultsLine xs

||| Run the built path-coverage exe in IDRIS2COV_TEST_OFFSET/_LIMIT slices,
||| accumulating runtime path hits across slices (each a fresh process → bounded
||| memory). `--dumppathshits` rewrites the hits file per run, so we read+accumulate
||| after each slice. Termination, robust for BOTH env-aware and env-ignoring exes:
|||   * a slice reporting 0 passed + 0 failed = past the end → stop;
|||   * a slice at offset>0 whose (passed+failed) equals the first slice's count =
|||     the exe IGNORES the window (ran everything again) → stop, keep slice-0 hits;
|||   * the offset walk is bounded by intraSliceMaxSlices.
runExeSlices : (projectDir : String) -> (relExecPath : String) -> (pathHitsPath : String)
            -> IO (List PathRuntimeHit)
runExeSlices projectDir relExecPath pathHitsPath = go 0 0 Nothing [] intraSliceMaxSlices
  where
    -- Env-gated per-slice trace. Set IDRIS2COV_SLICE_DEBUG=1 to diagnose a
    -- non-deterministic numerator: prints offset, exit code, parsed (passed,
    -- failed), this slice's hit count, accumulated hit count, and which
    -- termination branch the walk took. Off by default (no output pollution).
    sliceDebug : String -> IO ()
    sliceDebug msg = do
      dbg <- getEnv "IDRIS2COV_SLICE_DEBUG"
      case dbg of
        Just v => if trim v == "" || trim v == "0" then pure () else putStrLn ("    [slice] " ++ msg)
        Nothing => pure ()

    -- offset, sliceIdx, firstCount (passed+failed of slice 0), accHits, fuel
    go : Nat -> Nat -> Maybe Nat -> List PathRuntimeHit -> Nat -> IO (List PathRuntimeHit)
    go _ _ _ acc Z = pure acc
    go offset idx firstCount acc (S fuel) = do
      removeFileIfExists pathHitsPath
      let outPath = pathHitsPath ++ ".out"
      -- Per-slice timeout + stdin from /dev/null. A test in some window can block
      -- the whole walk: either reading stdin (getLine → /dev/null gives EOF) or,
      -- worse, blocking in a Boundary.Sys/runProc syscall on a pipe/process that
      -- never returns when the test runs in isolation (observed: a slice sat in
      -- state UN ~0% CPU indefinitely, 0 test lines, even with stdin redirected).
      -- macOS has no `timeout`, so wrap in perl's alarm(): a hung slice is killed
      -- after intraSliceTimeoutSecs and the walk continues with the hits recorded
      -- so far (write-on-first-hit means partial hits are already on disk). Slow-
      -- but-progressing slices finish well under the limit.
      sliceExit <- system $ "cd " ++ projectDir
                 ++ " && IDRIS2COV_TEST_OFFSET=" ++ show offset
                 ++ " IDRIS2COV_TEST_LIMIT=" ++ show intraSliceLimit
                 ++ " perl -e 'alarm " ++ show intraSliceTimeoutSecs
                 ++ "; exec @ARGV' " ++ relExecPath
                 ++ " < /dev/null > " ++ outPath ++ " 2>&1"
      out <- readFile outPath
      removeFileIfExists outPath
      hitsContent <- readFile pathHitsPath
      let sliceHits = case hitsContent of
                        Left _ => []
                        Right c => mapMaybe parsePathHitLineLocal (lines c)
      let pf : Maybe (Nat, Nat)
          pf = case out of
                 Left _  => Nothing
                 Right o => parseResultsPF o
      let count : Nat
          count = case pf of
                    Just (p, f) => p + f
                    Nothing     => 0
      let acc' = sliceHits ++ acc
      -- A slice killed by the perl alarm (SIGALRM → exit 142) has NO "Results:"
      -- line (count==0) but is NOT the end of the suite — a later window may still
      -- have tests. Keep its partial hits and ADVANCE past it rather than
      -- terminating. Bounded by fuel/intraSliceMaxSlices.
      let timedOut = (sliceExit == 142 || sliceExit == 124) && isNothing pf
      -- A slice that exited NON-ZERO with NO "Results:" line (e.g. SIGKILL/OOM
      -- exit 137, a segfault, or any crash that aborts mid-suite) is ALSO not a
      -- genuine past-the-end: terminating here would keep only this slice's
      -- partial hits and silently drop every later window — the flaky-numerator
      -- bug (covered measured as 4131 vs 507 across runs depended on whether
      -- slice 0 happened to finish cleanly). Treat it like a timeout: keep the
      -- partial hits already written (write-on-first-hit) and ADVANCE. Only a
      -- CLEAN exit (0) with count==0 means we have genuinely walked off the end.
      let crashedMidSlice = (sliceExit /= 0) && isNothing pf && not timedOut
      sliceDebug $ "offset=" ++ show offset ++ " idx=" ++ show idx
                ++ " exit=" ++ show sliceExit
                ++ " pf=" ++ show pf ++ " count=" ++ show count
                ++ " sliceHits=" ++ show (length sliceHits)
                ++ " accHits=" ++ show (length acc')
                ++ " firstCount=" ++ show firstCount
                ++ " timedOut=" ++ show timedOut
                ++ " crashedMidSlice=" ++ show crashedMidSlice
      if timedOut || crashedMidSlice
        then do sliceDebug "branch=ADVANCE(timeout/crash)"
                go (offset + intraSliceLimit) (S idx) firstCount acc' fuel
        else if count == 0
          then do sliceDebug "branch=STOP(clean-empty past-the-end)"; pure acc'  -- clean empty slice → past the end → done
          else case firstCount of
               Nothing =>
                 -- Slice 0. Distinguish env-AWARE from env-IGNORING by slice-0's
                 -- count: an env-aware exe returns exactly the window size
                 -- (intraSliceLimit) when there are ≥ that many tests; an exe that
                 -- ignores the window runs the WHOLE suite, so count > limit.
                 -- (A suite SMALLER than one window gives count < limit and the
                 --  next slice returns 0 → natural stop.)
                 if count > intraSliceLimit
                   then do sliceDebug "branch=STOP(slice0 count>limit env-ignored)"; pure acc'  -- exe ignored window, ran everything: slice-0 IS the full set
                   else do sliceDebug "branch=ADVANCE(slice0 env-aware)"
                           go (offset + intraSliceLimit) (S idx) (Just count) acc' fuel
               Just _ =>
                 -- Env-aware: keep walking windows until an empty slice (count==0,
                 -- handled above) or a short final slice (count < limit) ends it.
                 -- Each full slice legitimately has count == intraSliceLimit, so we
                 -- must NOT treat "count == firstCount" as the ignore signal (that
                 -- false-positived and stopped after one window — undercounting).
                 if count < intraSliceLimit
                   then do sliceDebug "branch=STOP(short final window)"; pure acc'  -- last (partial) window consumed → done
                   else do sliceDebug "branch=ADVANCE(env-aware full window)"
                           go (offset + intraSliceLimit) (S idx) firstCount acc' fuel

||| Test-module count above which the runtime path-coverage exe is built and run
||| in sequential chunks instead of one whole-suite process. A single exe that
||| links + executes the FULL test suite (e.g. EtherClaw's ~1100 tests across
||| ~140 modules) is the 8GB-host OOM hog: the whole-suite compiler/runtime
||| process holds every module's closures live at once. Chunking by test module
||| keeps peak memory bounded by one chunk's transitive build+run — each chunk is
||| a separate process, so its heap is reclaimed before the next chunk starts.
||| Runtime path hits are keyed by stable compiler-exported path id, so hits from
||| separate chunks aggregate by plain concatenation (coverage = union of covered
||| ids; see coveredPathIds / buildPathCoverageResultFromHits).
runtimeChunkTestModuleCeiling : Nat
runtimeChunkTestModuleCeiling = 12

||| Number of test modules built+run together in one runtime chunk process.
runtimeChunkSize : Nat
runtimeChunkSize = 8

||| Run ONE runtime path-hits chunk: build a temp exe over (tempRunner + this
||| chunk's test modules), run it with --dumppathshits, return the chunk's hits.
||| Each chunk is its own process so the OS reclaims its heap between chunks —
||| this is what bounds peak memory for large suites. The temp build dir is
||| per-chunk and removed by the caller; the project's real build/ is never
||| touched. Returns [] on a chunk that fails to produce hits (the denominator
||| stays real via the static path; a hit-less chunk just contributes no hits).
runRuntimePathHitsChunk : (projectDir : String)
                       -> (sourcedir : String)
                       -> (projectDepends : List String)
                       -> (packTomlContent : String)
                       -> (chunkTestModules : List String)
                       -> (idx : Nat)
                       -> IO (List PathRuntimeHit)
runRuntimePathHitsChunk projectDir sourcedir projectDepends packTomlContent chunkTestModules idx = do
  uid <- getUniqueId
  let tempModName = "TempPathRunnerChunk_" ++ uid
  let tempExecName = "temp-paths-chunk-" ++ uid
  let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
  let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
  let tempIpkgName = tempExecName ++ ".ipkg"
  let tempBuildDir = ".idris2-coverage-runtime-" ++ uid
  let packTomlPath = projectDir ++ "/pack.toml"
  let dumppathsPath = "/tmp/idris2_dumppaths_chunk_" ++ uid ++ "_" ++ show idx ++ ".json"
  let pathHitsPath = "/tmp/idris2_pathhits_chunk_" ++ uid ++ "_" ++ show idx ++ ".txt"
  let relExecPath = "./" ++ tempBuildDir ++ "/exec/" ++ tempExecName

  let cleanup : IO ()
      cleanup = do
        removeFileIfExists tempIdrPath
        removeFileIfExists tempIpkgPath
        _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
        removeFileIfExists dumppathsPath
        removeFileIfExists pathHitsPath

  let runnerSource = generateTempRunner tempModName chunkTestModules
  Right () <- writeFile tempIdrPath runnerSource
    | Left _ => pure []

  let allModules = nub (tempModName :: chunkTestModules)
  -- NUMERATOR only: --dumppathshits (lightweight hit recording). The denominator
  -- (--dumppaths-json path enumeration) already comes from the chunked static
  -- path, and emitting it here too would re-instrument every module = the heavy
  -- compile that OOMs a many-module package (EtherClaw ~108 modules). Recording
  -- hits is cheap; enumerating paths is what's expensive.
  let opts = "--dumppathshits " ++ pathHitsPath
  let ipkgContent = generateTempIpkgWithOpts tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just opts) (Just tempBuildDir)
  Right () <- writeFile tempIpkgPath ipkgContent
    | Left _ => do removeFileIfExists tempIdrPath; pure []

  Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
    | Left _ => do removeFileIfExists tempIdrPath; removeFileIfExists tempIpkgPath; pure []

  buildResult <- buildIpkgWithClean False projectDir tempIpkgName
  case buildResult of
    Left err => do
      putStrLn $ "    Runtime chunk " ++ show idx ++ " build failed: " ++ err
      cleanupPackToml packTomlPath createdPackToml
      cleanup
      pure []
    Right () => do
      -- Run the built exe in IDRIS2COV_TEST_OFFSET/_LIMIT slices (≈100 tests per
      -- fresh process) instead of one whole-suite invocation. A single process
      -- that runs ALL of EtherClaw's ~1100 tests under path instrumentation grows
      -- unboundedly and gets SIGKILL'd (RUN_EXIT=137) before it can dump hits →
      -- 0 covered. Slicing keeps each run's peak memory bounded and accumulates+
      -- dedups hits across slices. Requires the test module to honour the env
      -- window (runTestSuiteMain does); env-ignoring exes are handled by
      -- runExeSlices' terminator (keeps slice-0 hits).
      hits <- runExeSlices projectDir relExecPath pathHitsPath
      cleanupPackToml packTomlPath createdPackToml
      cleanup
      pure hits

||| Sequentially run runtime path-hits chunks, accumulating hits. Chunks run one
||| at a time (NOT in parallel) so peak memory is one chunk's build+run, not the
||| sum — bounded memory is the entire point. Hits accumulate across chunks.
runRuntimePathHitsChunks : (projectDir : String)
                        -> (sourcedir : String)
                        -> (projectDepends : List String)
                        -> (packTomlContent : String)
                        -> (chunks : List (List String))
                        -> (idx : Nat)
                        -> (acc : List PathRuntimeHit)
                        -> IO (List PathRuntimeHit)
runRuntimePathHitsChunks _ _ _ _ [] _ acc = pure acc
runRuntimePathHitsChunks projectDir sourcedir deps packTomlContent (mods :: rest) idx acc = do
  putStrLn $ "    Runtime path-hits chunk " ++ show idx ++ " (" ++ show (length mods) ++ " test modules)..."
  hits <- runRuntimePathHitsChunk projectDir sourcedir deps packTomlContent mods idx
  runRuntimePathHitsChunks projectDir sourcedir deps packTomlContent rest (idx + 1) (hits ++ acc)

||| Build and run test modules with forked Idris2 path instrumentation enabled.
||| Returns static dumppaths JSON plus runtime path hits from the executed test binary.
|||
||| For large test suites (> runtimeChunkTestModuleCeiling test modules) the
||| runtime hits are collected in sequential per-chunk processes to keep peak
||| memory bounded (the 8GB-host OOM fix), while the denominator JSON comes from
||| the already-chunked static path (runStaticDumppathsJson). Small suites keep
||| the original single whole-suite build+run.
export
runTestsWithPathCoverageArtifacts : (projectDir : String)
                                 -> (projectModules : List String)
                                 -> (testModules : List String)
                                 -> (timeout : Nat)
                                 -> IO (Either String (String, List PathRuntimeHit))
runTestsWithPathCoverageArtifacts projectDir projectModules testModules timeout =
  case testModules of
    [] => pure $ Left "No test modules specified"
    _ =>
      -- Chunk when EITHER the test-module count OR the PROJECT-module count is
      -- large. A package like EtherClaw has few test modules (1) but many project
      -- modules (~108): building all of them with --dumppaths-json in ONE process
      -- OOM-kills the COMPILER (exit 137) before any test runs. The whole-suite
      -- path is only safe for genuinely small packages on both axes.
      if length testModules > runtimeChunkTestModuleCeiling
           || length projectModules > staticWholeModuleCeiling
        then runChunkedPathCoverage projectDir projectModules testModules
        else runWholePathCoverage projectDir projectModules testModules timeout
  where
    ||| Large-package path: denominator from the chunked static path (groups of 8
    ||| modules — bounds COMPILER memory), runtime hits from sequential per-test-
    ||| chunk exes (bounds RUN memory). OOM-safe on both axes.
    runChunkedPathCoverage : String -> List String -> List String -> IO (Either String (String, List PathRuntimeHit))
    runChunkedPathCoverage projectDir projectModules testModules = do
      putStrLn $ "    Large package (" ++ show (length testModules)
              ++ " test / " ++ show (length projectModules)
              ++ " project modules); chunking path coverage to bound memory..."
      projectDepends <- readProjectDepends projectDir
      sourcedir <- readProjectSourcedir projectDir
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml

      -- Denominator: the full static dumppaths JSON over the project ipkg. This
      -- path is already chunked internally (runStaticDumppathsJsonChunks) so it
      -- never builds the whole package in one process.
      Just projectIpkgPath <- findProjectIpkgPath projectDir
        | Nothing => pure $ Left "Chunked path coverage: no project ipkg found for static denominator"
      denomResult <- runStaticDumppathsJson projectIpkgPath
      case denomResult of
        Left err => pure $ Left $ "Chunked path coverage: static denominator failed: " ++ err
        Right denomJson => do
          -- Numerator: runtime path hits, collected one test-module chunk per
          -- process so peak memory stays bounded.
          let chunks = chunkList runtimeChunkSize testModules
          hits <- runRuntimePathHitsChunks projectDir sourcedir projectDepends packTomlContent chunks 0 []
          pure $ Right (denomJson, hits)

    ||| The original whole-suite single build+run, kept for small suites where the
    ||| memory cost is negligible and one clean process is simplest.
    runWholePathCoverage : String -> List String -> List String -> Nat -> IO (Either String (String, List PathRuntimeHit))
    runWholePathCoverage projectDir projectModules testModules timeout = do
      putStrLn "    Preparing path coverage runner..."
      projectDepends <- readProjectDepends projectDir
      putStrLn $ "    Project dependencies: " ++ show projectDepends
      sourcedir <- readProjectSourcedir projectDir
      putStrLn $ "    Project sourcedir: " ++ sourcedir

      uid <- getUniqueId
      putStrLn $ "    Path coverage runner id: " ++ uid
      let tempModName = "TempPathRunner_" ++ uid
      let tempExecName = "temp-paths-" ++ uid
      let tempIdrPath = projectDir ++ "/" ++ sourcedir ++ "/" ++ tempModName ++ ".idr"
      let tempIpkgPath = projectDir ++ "/" ++ tempExecName ++ ".ipkg"
      let tempIpkgName = tempExecName ++ ".ipkg"
      let tempBuildDir = ".idris2-coverage-build-" ++ uid
      let packTomlPath = projectDir ++ "/pack.toml"
      projectPackToml <- readProjectPackToml projectDir
      let packTomlContent = generateTempPackToml projectPackToml
      -- Install the project's needed local deps into the forked compiler so the
      -- direct --dumppaths-json build resolves them (fork ignores pack.toml).
      installNeededDepsIntoFork projectDepends packTomlContent
      let dumppathsPath = "/tmp/idris2_dumppaths_runtime_" ++ uid ++ ".json"
      let pathHitsPath = "/tmp/idris2_pathhits_runtime_" ++ uid ++ ".txt"
      let relExecPath = "./" ++ tempBuildDir ++ "/exec/" ++ tempExecName

      let runnerSource = generateTempRunner tempModName testModules
      putStrLn $ "    Writing temp path runner: " ++ tempIdrPath
      Right () <- writeFile tempIdrPath runnerSource
        | Left err => pure $ Left $ "Failed to write temp runner: " ++ show err

      let allModules = nub (tempModName :: (projectModules ++ testModules))
      let opts = "--dumppaths-json " ++ dumppathsPath ++ " --dumppathshits " ++ pathHitsPath
      let ipkgContent = generateTempIpkgWithOpts tempExecName tempModName allModules tempExecName projectDepends sourcedir (Just opts) (Just tempBuildDir)
      putStrLn $ "    Writing temp path ipkg: " ++ tempIpkgPath
      Right () <- writeFile tempIpkgPath ipkgContent
        | Left err => do
            removeFileIfExists tempIdrPath
            pure $ Left $ "Failed to write temp ipkg: " ++ show err

      putStrLn "    Ensuring path coverage pack.toml..."
      Right createdPackToml <- writePackTomlIfMissing packTomlPath packTomlContent
        | Left err => do
            removeFileIfExists tempIdrPath
            removeFileIfExists tempIpkgPath
            pure $ Left err

      putStrLn "    Building temp path coverage runner..."
      buildResult <- buildIpkgWithClean False projectDir tempIpkgName
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

          -- Run the exe in intra-module offset/limit slices (each a fresh process)
          -- so a single huge test module never executes in one OOM-prone process.
          -- Modules using Idris2.TestSuite.Runner honour the env and chunk for
          -- real; others run everything on slice 0 and the loop stops. Hits
          -- accumulate across slices (keyed by stable path id → concat-safe).
          hits <- runExeSlices projectDir relExecPath pathHitsPath

          removeFileIfExists tempIdrPath
          removeFileIfExists tempIpkgPath
          cleanupPackToml packTomlPath createdPackToml
          _ <- system $ "cd " ++ projectDir ++ " && rm -rf " ++ tempBuildDir
          removeFileIfExists dumppathsPath
          removeFileIfExists pathHitsPath

          pure $ Right (dumppathsContent, hits)
