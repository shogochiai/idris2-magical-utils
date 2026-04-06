||| Runtime path-hit extraction for DFX/ICP profiling output.
module DfxCoverage.PathRuntime

import Data.List
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

export
analyzePathHitsFromBranchProbeCoverage : String -> StaticBranchAnalysis -> List BranchProbeEntry -> List Nat -> Either String (List PathRuntimeHit)
analyzePathHitsFromBranchProbeCoverage dumppathsContent staticAnalysis probeEntries hitProbeIndices =
  let coveredBranchIds = coveredMaterializedBranchIds staticAnalysis probeEntries hitProbeIndices
  in pathHitsFromCoveredBranchIdsInContent coveredBranchIds dumppathsContent

export
analyzePathHitsFromBranchProbeFiles : String -> StaticBranchAnalysis -> String -> Maybe String -> String -> String -> IO (Either String (List PathRuntimeHit))
analyzePathHitsFromBranchProbeFiles dumppathsContent staticAnalysis branchProbeMapPath mProjectDir canisterRef network = do
  Right probeEntries <- readBranchProbeMap branchProbeMapPath
    | Left err => pure $ Left err
  Right hitProbeIndices <- getBranchProbeIndices mProjectDir canisterRef network
    | Left err => pure $ Left err
  pure $ analyzePathHitsFromBranchProbeCoverage dumppathsContent staticAnalysis probeEntries hitProbeIndices
