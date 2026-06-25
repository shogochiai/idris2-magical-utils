||| Runtime path-hit extraction for DFX/ICP profiling output.
module DfxCoverage.PathRuntime

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.SortedMap
import System
import System.File

import DfxCoverage.IcWasm.ProfilingParser
import DfxCoverage.IcWasm.IcpPublicNameParser
import DfxCoverage.SourceMap.BranchProbeMap
import DfxCoverage.DumpcasesParser
import Coverage.Core.PathCoverage
import Coverage.Core.DumppathsJson
import Coverage.Core.Types
import public Coverage.Core.RuntimeHit

%default covering

countOccurrences : List String -> SortedMap String Nat
countOccurrences = foldl addOne empty
  where
    addOne : SortedMap String Nat -> String -> SortedMap String Nat
    addOne acc key =
      let current = fromMaybe 0 (lookup key acc)
      in insert key (S current) acc

export
extractPathIdFromPublicName : String -> Maybe String
extractPathIdFromPublicName name =
  let trimmed = trim name in
  if isPrefixOf "path:" trimmed
     then Just (substr 5 (length trimmed `minus` 5) trimmed)
  else if isPrefixOf "cov_path:" trimmed
     then Just (substr 9 (length trimmed `minus` 9) trimmed)
  else if isPrefixOf "__cov_path__" trimmed
     then Just (substr 12 (length trimmed `minus` 12) trimmed)
  else Nothing

export
pathHitsFromResolvedNames : List String -> List PathRuntimeHit
pathHitsFromResolvedNames names =
  map (\(pid, count) => MkPathRuntimeHit pid count) (Data.SortedMap.toList (countOccurrences names))

export
pathHitsFromProfilingResult : ProfilingResult -> IcpFuncNames -> List PathRuntimeHit
pathHitsFromProfilingResult profiling names =
  let executedIds = getExecutedFuncIds profiling
      resolved = resolveFuncIds executedIds names
      pathNames = mapMaybe (\(_, fname) => extractPathIdFromPublicName fname) resolved
  in pathHitsFromResolvedNames pathNames

export
analyzePathHitsFromProfilingText : String -> IcpFuncNames -> List PathRuntimeHit
analyzePathHitsFromProfilingText content names =
  pathHitsFromProfilingResult (parseProfilingOutput content) names

export
analyzePathHitsFromFiles : String -> String -> IO (Either String (List PathRuntimeHit))
analyzePathHitsFromFiles profilingOutputPath publicNamesWasm = do
  Right content <- readFile profilingOutputPath
    | Left err => pure $ Left $ "Failed to read profiling output: " ++ show err
  Right names <- extractIcpFuncNames publicNamesWasm
    | Left err => pure $ Left err
  pure $ Right $ analyzePathHitsFromProfilingText content names

branchProbeDidPath : String
branchProbeDidPath = "/tmp/dfx_branch_probes.did"

branchProbeOutPath : String -> String
branchProbeOutPath canisterRef = "/tmp/dfx_branch_probes_" ++ canisterRef ++ ".txt"

branchProbePrefix : Maybe String -> String
branchProbePrefix (Just dir) = "cd " ++ dir ++ " && "
branchProbePrefix Nothing = ""

parseBranchProbeIndices : String -> List Nat
parseBranchProbeIndices s = parseNums (unpack s)
  where
    parseNatToken : String -> Maybe Nat
    parseNatToken token =
      case parseInteger token of
        Just n => if n < 0 then Nothing else Just (cast n)
        Nothing => Nothing

    parseNums : List Char -> List Nat
    parseNums [] = []
    parseNums cs =
      let (digits, rest) = span isDigit cs
      in case digits of
           [] => case rest of
                   [] => []
                   (_ :: xs) => parseNums xs
           _ =>
             case parseNatToken (pack digits) of
               Just n => n :: parseNums rest
               Nothing => parseNums rest

export
getBranchProbeIndices : Maybe String -> String -> String -> IO (Either String (List Nat))
getBranchProbeIndices mProjectDir canisterRef network = do
  Right () <- writeFile branchProbeDidPath "service : { __get_branch_probes : () -> (text) query; }\n"
    | Left err => pure $ Left $ "Failed to write branch probe did: " ++ show err
  let outPath = branchProbeOutPath canisterRef
  exitCode <- system $
    branchProbePrefix mProjectDir ++
    "dfx canister call " ++ canisterRef ++
    " __get_branch_probes --query --candid " ++ branchProbeDidPath ++
    " --network " ++ network ++
    " > " ++ outPath ++ " 2>&1"
  if exitCode /= 0
     then do
       errText <- readFile outPath
       _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ outPath
       case errText of
         Right content => pure $ Left $ "__get_branch_probes call failed: " ++ trim content
         Left _ => pure $ Left "__get_branch_probes call failed"
     else do
       Right content <- readFile outPath
         | Left err => do
             _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ outPath
             pure $ Left $ "Failed to read branch probe output: " ++ show err
       _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ outPath
       pure $ Right $ nub (parseBranchProbeIndices content)

-- ============================================================================
-- Canonical path-id hits (`--path-hits` / `__get_path_hits`) — IDENTITY JOIN
-- ============================================================================
-- The forked compiler's --dumppathshits pass injects prim__recordPathHit
-- "<fn>#p<n>" at every CaseTree leaf; RefC lowers it to idris2_recordPathHit
-- (pathcov.c) which records the canonical path-id string. The canister query
-- __get_path_hits replies the comma-separated recorded ids. Those ids are the
-- SAME strings as dumppaths path_id, so the numerator is an exact set
-- intersection — no source-map, ordinal, or span heuristic.

pathHitsDidPath : String
pathHitsDidPath = "/tmp/dfx_path_hits.did"

pathHitsOutPath : String -> String
pathHitsOutPath canisterRef = "/tmp/dfx_path_hits_" ++ canisterRef ++ ".txt"

||| Strip dfx candid text wrapping: an outer `( ... )` and the surrounding
||| double-quotes around the string body. Keeps inner content verbatim.
stripCandidText : String -> String
stripCandidText s =
  let cs = unpack s
      -- drop outer parens
      noParen = case cs of
                  ('(' :: rest) => case reverse rest of
                                     (')' :: revBody) => reverse revBody
                                     _ => rest
                  _ => cs
      t = trim (pack noParen)
      -- drop surrounding double-quotes
      tc = unpack t
      noQuote = case tc of
                  ('"' :: rest) => case reverse rest of
                                     ('"' :: revBody) => reverse revBody
                                     _ => rest
                  _ => tc
  in pack noQuote

||| Split a comma-separated reply into trimmed, non-empty path-id tokens.
parsePathIdList : String -> List String
parsePathIdList s =
  -- Split on SEMICOLON, not comma: path-ids for `where`-bound / lifted helpers embed
  -- a comma in their name (e.g. "…balancedObject,go#p0"). A comma split truncated
  -- those to "…balancedObject" (dropping ",go#pN"), so they never joined and showed
  -- as permanently-missing. The canister join (__dfxcov_format_path_hits in
  -- pathcov.c) emits ';' between ids accordingly. Fall back to ',' only for a legacy
  -- WASM that still comma-joined (no ';' present in the reply).
  let sep   = if isInfixOf ";" s then ';' else ','
      raw    = forget (split (== sep) s)
  in filter (/= "") (map trim raw)

||| Query the live canister's __get_path_hits and return the recorded canonical
||| path-ids. A trailing "__TRUNCATED__" token (pathcov saturation marker) is
||| dropped but logged by the caller via `pathHitsSaturated`.
export
getPathHitIds : Maybe String -> String -> String -> IO (Either String (List String))
getPathHitIds mProjectDir canisterRef network = do
  Right () <- writeFile pathHitsDidPath "service : { __get_path_hits : () -> (text) query; }\n"
    | Left err => pure $ Left $ "Failed to write path hits did: " ++ show err
  let outPath = pathHitsOutPath canisterRef
  exitCode <- system $
    branchProbePrefix mProjectDir ++
    "dfx canister call " ++ canisterRef ++
    " __get_path_hits --query --candid " ++ pathHitsDidPath ++
    " --network " ++ network ++
    " > " ++ outPath ++ " 2>&1"
  if exitCode /= 0
     then do
       errText <- readFile outPath
       _ <- system $ "rm -f " ++ pathHitsDidPath ++ " " ++ outPath
       case errText of
         Right content => pure $ Left $ "__get_path_hits call failed: " ++ trim content
         Left _ => pure $ Left "__get_path_hits call failed"
     else do
       Right content <- readFile outPath
         | Left err => do
             _ <- system $ "rm -f " ++ pathHitsDidPath ++ " " ++ outPath
             pure $ Left $ "Failed to read path hits output: " ++ show err
       _ <- system $ "rm -f " ++ pathHitsDidPath ++ " " ++ outPath
       -- dfx wraps the text reply as `("a,b,c")` (possibly multi-line). Strip the
       -- candid wrapping (outer parens + the surrounding double-quotes) before
       -- splitting so the first/last ids aren't contaminated with punctuation.
       let body = stripCandidText (trim content)
       pure $ Right $ nub (parsePathIdList body)

||| IDENTITY JOIN: a path obligation is covered iff its path_id was recorded.
||| (The recorded ids may include dfx-reply punctuation tokens; we intersect
||| against the actual dumppaths path_ids so only real obligations count.)
export
analyzePathHitsFromPathIds : String -> List String -> Either String (List PathRuntimeHit)
analyzePathHitsFromPathIds dumppathsContent recordedIds = do
  paths <- parseDumppathsJson dumppathsContent
  let covered = filter (\p => elem p.pathId recordedIds) paths
  pure (map (\p => MkPathRuntimeHit p.pathId 1) covered)

terminalBranchId : PathObligation -> Maybe String
terminalBranchId path =
  case last' path.steps of
    Nothing => Nothing
    Just step =>
      case step.caseIndex of
        Nothing => Nothing
        Just caseIdx =>
          Just $ show $ MkBranchId path.moduleName (getSimpleFuncName path.functionName) caseIdx step.branchIndex

export
pathHitsFromCoveredBranchIds : List String -> List PathObligation -> List PathRuntimeHit
pathHitsFromCoveredBranchIds coveredBranchIds paths =
  mapMaybe toHit paths
  where
    toHit : PathObligation -> Maybe PathRuntimeHit
    toHit path =
      case terminalBranchId path of
        Just branchId =>
          if elem branchId coveredBranchIds
             then Just (MkPathRuntimeHit path.pathId 1)
             else Nothing
        Nothing => Nothing

export
pathHitsFromCoveredBranchIdsInContent : List String -> String -> Either String (List PathRuntimeHit)
pathHitsFromCoveredBranchIdsInContent coveredBranchIds dumppathsContent = do
  paths <- parseDumppathsJson dumppathsContent
  pure $ pathHitsFromCoveredBranchIds coveredBranchIds paths

||| DIRECT probe→path mapping that bypasses the static-analysis branch-id bridge.
||| The old path (coveredMaterializedBranchIds → reachableBranchIdsForFunction →
||| branchMatchesNodeId) collapses 2775 hits → 0 because it joins through a THIRD
||| derivation (dumpcases StaticBranchAnalysis) whose function-name and branch-id
||| forms don't match the probe CSV / dumppaths nodeIds. Instead: probe entries
||| (idrisName, ordinalInFunc) and path obligations (functionName) both derive from
||| the SAME source, so within a function the N-th hit ordinal maps to the N-th
||| path obligation. Group both by function name and align by ordinal.
hitProbeOrdinalsByFunction : List BranchProbeEntry -> List Nat -> List (String, List Nat)
hitProbeOrdinalsByFunction entries hitProbeIndices =
  let hit = filter (\e => elem e.probeIndex hitProbeIndices) entries
      fns = nub (map (.idrisName) hit)
  in map (\fn => (fn, nub (map (.ordinalInFunc) (filter (\e => e.idrisName == fn) hit)))) fns

pathsByFunction : List PathObligation -> String -> List PathObligation
pathsByFunction paths fn = filter (\p => p.functionName == fn) paths

||| A path "owns" a source span if any step's sourceSpan, or its sourceSpanUnion,
||| equals it. This is the by-id join key (mangling-independent).
pathHasSpan : String -> PathObligation -> Bool
pathHasSpan span path =
  span /= ""
    && ( path.sourceSpanUnion == Just span
      || any (\st => st.sourceSpan == Just span) path.steps )

||| BY-ID span mapping (preferred): for each hit probe carrying an idrisSpan, every
||| path obligation owning that exact source span is covered. Independent of RefC
||| name mangling and ordinal alignment.
hitsBySpan : List PathObligation -> List BranchProbeEntry -> List Nat -> List PathRuntimeHit
hitsBySpan paths entries hitProbeIndices =
  let hitSpans = nub (filter (/= "") (map (.idrisSpan)
                       (filter (\e => elem e.probeIndex hitProbeIndices) entries)))
  in [ MkPathRuntimeHit p.pathId 1
     | p <- paths, span <- hitSpans, pathHasSpan span p ]

export
analyzePathHitsFromBranchProbeCoverageDirect : String -> List BranchProbeEntry -> List Nat -> Either String (List PathRuntimeHit)
analyzePathHitsFromBranchProbeCoverageDirect dumppathsContent probeEntries hitProbeIndices = do
  paths <- parseDumppathsJson dumppathsContent
  -- Prefer the by-id span mapping; if probes carry no spans (legacy CSV), fall
  -- back to the function+ordinal alignment.
  let spanHits = nub (hitsBySpan paths probeEntries hitProbeIndices)
  let hitByFn = hitProbeOrdinalsByFunction probeEntries hitProbeIndices
  let ordHits = nub (concatMap (mapFnHits paths) hitByFn)
  -- Union both (span is precise; ordinal adds functions without spans).
  pure (nub (spanHits ++ ordHits))
  where
    mapFnHits : List PathObligation -> (String, List Nat) -> List PathRuntimeHit
    mapFnHits paths (fn, ords) =
      let fnPaths = pathsByFunction paths fn
      in mapMaybe (\ord => map (\p => MkPathRuntimeHit p.pathId 1) (indexOrd ord fnPaths)) ords

export
analyzePathHitsFromBranchProbeCoverage : String -> StaticBranchAnalysis -> List BranchProbeEntry -> List Nat -> Either String (List PathRuntimeHit)
analyzePathHitsFromBranchProbeCoverage dumppathsContent staticAnalysis probeEntries hitProbeIndices =
  -- Prefer the DIRECT mapping; fall back to the legacy static-analysis bridge only
  -- if the direct one yields nothing (e.g. no function-name overlap).
  case analyzePathHitsFromBranchProbeCoverageDirect dumppathsContent probeEntries hitProbeIndices of
    Right hits@(_ :: _) => Right hits
    _ =>
      let coveredBranchIds = coveredMaterializedBranchIds staticAnalysis probeEntries hitProbeIndices
      in pathHitsFromCoveredBranchIdsInContent coveredBranchIds dumppathsContent

export
analyzePathHitsFromBranchProbeFiles : String -> StaticBranchAnalysis -> String -> Maybe String -> String -> String -> IO (Either String (List PathRuntimeHit))
analyzePathHitsFromBranchProbeFiles dumppathsContent staticAnalysis branchProbeMapPath mProjectDir canisterRef network = do
  Right probeEntries <- readBranchProbeMap branchProbeMapPath
    | Left err => pure $ Left err
  Right hitProbeIndices <- getBranchProbeIndices mProjectDir canisterRef network
    | Left err => do
        putStrLn $ "    [numerator] getBranchProbeIndices FAILED: " ++ err
        pure $ Left err
  -- Breakdown of where the 2775 hits collapse: hitEntries (probes that fired) →
  -- coveredBranchIds (mapped to static branch-ids by function+ordinal) → path hits.
  let hitEntries = filter (\e => elem e.probeIndex hitProbeIndices) probeEntries
  let fnsFound = length (filter (\nm => not (null (reachableBranchIdsForFunction staticAnalysis nm)))
                           (nub (map (.idrisName) hitEntries)))
  let coveredBranchIds = coveredMaterializedBranchIds staticAnalysis probeEntries hitProbeIndices
  let hits = analyzePathHitsFromBranchProbeCoverage dumppathsContent staticAnalysis probeEntries hitProbeIndices
  putStrLn $ "    [numerator] probeEntries=" ++ show (length probeEntries)
          ++ " hitProbeIndices=" ++ show (length hitProbeIndices)
          ++ " hitEntries=" ++ show (length hitEntries)
          ++ " hitEntries-with-fn=" ++ show fnsFound
          ++ " coveredBranchIds=" ++ show (length coveredBranchIds)
          ++ " -> mapped hits=" ++ show (either (const 0) length hits)
  pure hits
