||| RefC C branch probe instrumentation for DFX branch-level coverage.
|||
||| This module rewrites generated RefC C to insert probe function calls at
||| materialized C branch sites. The probes become normal WASM functions, so
||| ic-wasm's existing `__get_profiling` function-level tracing can be reused
||| as a branch-hit transport.
module WasmBuilder.BranchProbe

import Data.List
import Data.Maybe
import Data.String
import System
import System.File
import WasmBuilder.SourceMap.SourceMap

%default covering

public export
data BranchSiteKind
  = SiteIf
  | SiteSwitch
  | SiteCase
  | SiteDefault

public export
Show BranchSiteKind where
  show SiteIf = "if"
  show SiteSwitch = "switch"
  show SiteCase = "case"
  show SiteDefault = "default"

public export
record CFuncRange where
  constructor MkCFuncRange
  cName : String
  idrisName : String
  startLine : Nat
  endLine : Nat

public export
record BranchProbeSite where
  constructor MkBranchProbeSite
  probeIndex : Nat
  probeName : String
  cFuncName : String
  idrisName : String
  ordinalInFunc : Nat
  lineNumber : Nat
  kind : BranchSiteKind

trimPrefix : String -> String -> Bool
trimPrefix pfx s = isPrefixOf pfx (trim s)

moduleToSourcePath : String -> String
moduleToSourcePath modName = joinBy "/" (splitOnDot (unpack modName) [] []) ++ ".idr"
  where
    splitOnDot : List Char -> List Char -> List String -> List String
    splitOnDot [] acc results = reverse (pack (reverse acc) :: results)
    splitOnDot ('.' :: rest) acc results = splitOnDot rest [] (pack (reverse acc) :: results)
    splitOnDot (c :: rest) acc results = splitOnDot rest (c :: acc) results

identChar : Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

takeWhileStr : (Char -> Bool) -> String -> String
takeWhileStr p = pack . takeWhile p . unpack

isTopLevelDefLine : String -> Bool
isTopLevelDefLine line =
  let raw = line
      trimmed = trim raw
      startsIndented =
        case unpack raw of
          (' ' :: _) => True
          ('\t' :: _) => True
          _ => False
      blocked =
        startsIndented ||
        null trimmed ||
        isPrefixOf "--" trimmed ||
        isPrefixOf "|||" trimmed ||
        isPrefixOf "module " trimmed ||
        isPrefixOf "import " trimmed ||
        isPrefixOf "%default" trimmed ||
        isPrefixOf "%language" trimmed ||
        isPrefixOf "data " trimmed ||
        isPrefixOf "record " trimmed ||
        isPrefixOf "interface " trimmed ||
        isPrefixOf "namespace " trimmed ||
        isPrefixOf "parameters " trimmed ||
        isPrefixOf "mutual" trimmed
      name = takeWhileStr identChar trimmed
      rest = trim (substr (length name) (length trimmed) trimmed)
  in not blocked && not (null name) && (isPrefixOf ":" rest || isPrefixOf "=" rest)

qualifyIdrisName : String -> String -> String
qualifyIdrisName moduleName localName =
  if isInfixOf "." localName then localName else moduleName ++ "." ++ localName

inferTopLevelIdrisName : String -> IdrisLoc -> IO (Maybe String)
inferTopLevelIdrisName projectDir loc = do
  let path = projectDir ++ "/src/" ++ moduleToSourcePath loc.file
  Right content <- readFile path
    | Left _ => pure Nothing
  let indexed = zip [1 .. length (lines content)] (lines content)
  let defs =
        mapMaybe
          (\(lineNum, line) =>
            if isTopLevelDefLine line
               then let localName = takeWhileStr identChar (trim line)
                    in Just (lineNum, qualifyIdrisName loc.file localName)
               else Nothing)
          indexed
  pure (pickLast defs)
  where
    pickLast : List (Nat, String) -> Maybe String
    pickLast defs = go Nothing defs
      where
        go : Maybe String -> List (Nat, String) -> Maybe String
        go acc [] = acc
        go acc ((lineNum, fullName) :: rest) =
          if lineNum <= loc.startLine
             then go (Just fullName) rest
             else go acc rest

findNearestLoc : Nat -> List CToIdrisMapping -> Maybe IdrisLoc
findNearestLoc targetLine mappings = go Nothing mappings
  where
    go : Maybe IdrisLoc -> List CToIdrisMapping -> Maybe IdrisLoc
    go acc [] = acc
    go acc (m :: rest) =
      if m.cLine > targetLine
         then acc
         else go (Just m.idrisLoc) rest

detectBranchSiteKind : String -> Maybe BranchSiteKind
detectBranchSiteKind line =
  let t = trim line in
  if trimPrefix "if (" t || trimPrefix "if(" t || trimPrefix "} else if (" t || trimPrefix "} else if(" t
     then Just SiteIf
  else if trimPrefix "switch (" t || trimPrefix "switch(" t
     then Just SiteSwitch
  else if trimPrefix "case " t
     then Just SiteCase
  else if trimPrefix "default:" t
     then Just SiteDefault
  else Nothing

findFuncForLine : Nat -> List CFuncRange -> Maybe CFuncRange
findFuncForLine lineNum = find (\f => lineNum >= f.startLine && lineNum <= f.endLine)

buildFuncRanges : List CFunctionInfo -> List CFuncRange
buildFuncRanges [] = []
buildFuncRanges [f] = [MkCFuncRange f.cName f.idrisName f.lineStart 1000000]
buildFuncRanges (f :: rest@(g :: _)) =
  MkCFuncRange f.cName f.idrisName f.lineStart (minus g.lineStart 1) :: buildFuncRanges rest

sortFuncsByLine : List CFunctionInfo -> List CFunctionInfo
sortFuncsByLine [] = []
sortFuncsByLine (x :: xs) = insert x (sortFuncsByLine xs)
  where
    insert : CFunctionInfo -> List CFunctionInfo -> List CFunctionInfo
    insert e [] = [e]
    insert e (y :: ys) =
      if e.lineStart <= y.lineStart
         then e :: y :: ys
         else y :: insert e ys

countFuncSites : String -> List BranchProbeSite -> Nat
countFuncSites idrisName = length . filter (\s => s.idrisName == idrisName)

mkProbeName : Nat -> String
mkProbeName idx = "__dfxcov_probe_" ++ show idx

gatherProbeSites : String -> List BranchProbeSite
gatherProbeSites content =
  let funcs = buildFuncRanges (sortFuncsByLine (parseFunctionDefs content))
      indexed = zip [1 .. length (lines content)] (lines content)
  in go funcs indexed [] 0
  where
    go : List CFuncRange -> List (Nat, String) -> List BranchProbeSite -> Nat -> List BranchProbeSite
    go _ [] acc _ = reverse acc
    go funcs ((lineNum, line) :: rest) acc nextIdx =
      case (findFuncForLine lineNum funcs, detectBranchSiteKind line) of
        (Just fn, Just kind) =>
          let ordinal = countFuncSites fn.idrisName acc
              site = MkBranchProbeSite nextIdx (mkProbeName nextIdx) fn.cName fn.idrisName ordinal lineNum kind
          in go funcs rest (site :: acc) (S nextIdx)
        _ => go funcs rest acc nextIdx

probeCallLine : BranchProbeSite -> String
probeCallLine site =
  "  extern void " ++ site.probeName ++ "(void); " ++
  site.probeName ++ "(); /* dfxcov:" ++ site.idrisName ++ "#" ++ show site.ordinalInFunc ++ " */"

emitInstrumentedLines : List String -> List BranchProbeSite -> List String
emitInstrumentedLines src sites =
  let indexed = zip [1 .. length src] src in
  concatMap inject indexed
  where
    siteAt : Nat -> Maybe BranchProbeSite
    siteAt lineNum = find (\s => s.lineNumber == lineNum) sites

    inject : (Nat, String) -> List String
    inject (lineNum, line) =
      case siteAt lineNum of
        Nothing => [line]
        Just site =>
          case site.kind of
            SiteCase => [line, probeCallLine site]
            SiteDefault => [line, probeCallLine site]
            _ => [probeCallLine site, line]

generateProbeC : List BranchProbeSite -> String
generateProbeC sites =
  let probeCount = length sites
      bufferSize = if probeCount == 0 then 16 else (probeCount * 16) + 16
      header =
        [ "#include <stdint.h>"
        , "#include <stdio.h>"
        , "#include <string.h>"
        , "static volatile uint32_t __dfxcov_sink = 0;"
        , "static uint32_t __dfxcov_counters[" ++ show (max 1 probeCount) ++ "] = {0};"
        , "static char __dfxcov_hits_buffer[" ++ show bufferSize ++ "] = {0};"
        , ""
        , "uint32_t __dfxcov_probe_total(void) { return " ++ show probeCount ++ "; }"
        , ""
        , "const char* __dfxcov_format_hits(void) {"
        , "  size_t pos = 0;"
        , "  __dfxcov_hits_buffer[0] = '\\0';"
        , "  for (uint32_t i = 0; i < " ++ show probeCount ++ "; ++i) {"
        , "    if (__dfxcov_counters[i] == 0) continue;"
        , "    int written = snprintf("
        , "      __dfxcov_hits_buffer + pos,"
        , "      sizeof(__dfxcov_hits_buffer) - pos,"
        , "      pos == 0 ? \"%u\" : \",%u\","
        , "      i);"
        , "    if (written < 0) break;"
        , "    if ((size_t) written >= sizeof(__dfxcov_hits_buffer) - pos) {"
        , "      pos = sizeof(__dfxcov_hits_buffer) - 1;"
        , "      break;"
        , "    }"
        , "    pos += (size_t) written;"
        , "  }"
        , "  return __dfxcov_hits_buffer;"
        , "}"
        , ""
        , "void __dfxcov_reset_hits(void) {"
        , "  memset(__dfxcov_counters, 0, sizeof(__dfxcov_counters));"
        , "}"
        , ""
        ]
      defs =
        map (\s =>
          "__attribute__((used,noinline)) void "
          ++ s.probeName
          ++ "(void) { __dfxcov_sink += "
          ++ show (S s.probeIndex)
          ++ "; __dfxcov_counters["
          ++ show s.probeIndex
          ++ "] += 1; }"
        ) sites
  in unlines (header ++ defs ++ [""])

generateProbeDecls : List BranchProbeSite -> List String
generateProbeDecls sites =
  map (\s => "void " ++ s.probeName ++ "(void);") sites

generateProbeCsv : List BranchProbeSite -> String
generateProbeCsv sites =
  let header = "probe_index,probe_name,idris_function,ordinal,line,kind"
      rows =
        map (\s =>
          show s.probeIndex ++ "," ++ s.probeName ++ "," ++ s.idrisName ++ "," ++ show s.ordinalInFunc ++ "," ++ show s.lineNumber ++ "," ++ show s.kind
        ) sites
  in unlines (header :: rows)

public export
record BranchProbeArtifacts where
  constructor MkBranchProbeArtifacts
  instrumentedCPath : String
  probeCPath : String
  csvPath : String
  sites : List BranchProbeSite

export
instrumentRefCCFile : String -> String -> String -> IO (Either String BranchProbeArtifacts)
instrumentRefCCFile cFile buildDir projectDir = do
  _ <- system $ "mkdir -p " ++ buildDir
  Right content <- readFile cFile
    | Left err => pure $ Left $ "Failed to read RefC C file: " ++ show err
  let rawSites = gatherProbeSites content
  let mappings = parseRefCComments cFile content
  sites <- traverse (enrichSite mappings) rawSites
  let instrumentedContent =
        unlines (generateProbeDecls sites ++ [""] ++ emitInstrumentedLines (lines content) sites)
  let probeC = generateProbeC sites
  let csv = generateProbeCsv sites
  let instrumentedPath = buildDir ++ "/dfxcov_instrumented.c"
  let probePath = buildDir ++ "/dfxcov_probes.c"
  let csvPath = buildDir ++ "/idris2-branch-probes.csv"
  Right () <- writeFile instrumentedPath instrumentedContent
    | Left err => pure $ Left $ "Failed to write instrumented RefC C file: " ++ show err
  Right () <- writeFile probePath probeC
    | Left err => pure $ Left $ "Failed to write branch probe C file: " ++ show err
  Right () <- writeFile csvPath csv
    | Left err => pure $ Left $ "Failed to write branch probe map: " ++ show err
  pure $ Right $ MkBranchProbeArtifacts instrumentedPath probePath csvPath sites
  where
    enrichSite : List CToIdrisMapping -> BranchProbeSite -> IO BranchProbeSite
    enrichSite mappings site =
      case findNearestLoc site.lineNumber mappings of
        Nothing => pure site
        Just loc => do
          mFullName <- inferTopLevelIdrisName projectDir loc
          pure $
            case mFullName of
              Just fullName => { idrisName := fullName } site
              Nothing => { idrisName := loc.file } site
