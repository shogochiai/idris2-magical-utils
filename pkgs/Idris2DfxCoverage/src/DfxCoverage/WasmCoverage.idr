||| Unified WASM Coverage API compatibility layer.
module DfxCoverage.WasmCoverage

import Data.List
import Data.Maybe
import Data.String
import System
import System.File

import Coverage.Core.Result
import Coverage.Core.RuntimeHit
import Coverage.Core.Types
import Coverage.Core.ObligationMap
import Coverage.Classification.BranchClass
import Coverage.Standardization.Types
import public Coverage.Core.HighImpact as HI

import DfxCoverage.IcWasm.ProfilingParser
import DfxCoverage.IcWasm.IcpPublicNameParser as IPN
import DfxCoverage.SourceMap.SourceMapParser
import DfxCoverage.SourceMap.BranchProbeMap
import DfxCoverage.DumpcasesParser as DP

%default covering

public export
record WasmCoverageResult where
  constructor MkWasmCoverageResult
  coreResult : CoverageResult
  runtimeHits : List FunctionRuntimeHit
  highImpacts : List HI.HighImpactTarget
  measurement : CoverageMeasurement
  branchMeasurement : Maybe CoverageMeasurement
  materializedBranchIds : List String
  cycleAnalysis : CycleAnalysis
  wasmFuncCount : Nat
  sourceMapPath : Maybe String
  dumpcasesStats : Maybe DumpcasesStats

public export
Show WasmCoverageResult where
  show r = "WASM Coverage: " ++ show (coveredCount r.coreResult) ++ "/" ++
           show (totalCount r.coreResult) ++ " (" ++
           show (coveragePercent r.coreResult) ++ "%)" ++
           "\n  High Impact Targets: " ++ show (length r.highImpacts) ++
           "\n  Measurement denominator: " ++ show (length r.measurement.denominatorIds) ++
           ", covered: " ++ show (length r.measurement.coveredIds) ++
           maybe "" (\bm => "\n  Branch measurement denominator: " ++
                            show (length bm.denominatorIds) ++
                            ", covered: " ++ show (length bm.coveredIds))
                     r.branchMeasurement ++
           "\n  Total Cycles: " ++ show (totalCycles r.cycleAnalysis)

public export
record NameResolver where
  constructor MkNameResolver
  funcIdToWasm : Nat -> Maybe String
  wasmToIdris : String -> Maybe String
  targetNames : List String

export
buildNameResolver : IPN.IcpFuncNames -> Maybe SourceMapV3 -> NameResolver
buildNameResolver funcNames mSourceMap =
  let funcIdLookup = \idx => IPN.lookupFuncName idx funcNames
      wasmLookup = case mSourceMap of
                     Just sm => buildNameLookup sm
                     Nothing => \_ => Nothing
      targets = case mSourceMap of
                  Just sm => getProjectFunctions sm
                  Nothing => []
  in MkNameResolver funcIdLookup wasmLookup targets

export
resolveToIdris : NameResolver -> Nat -> Maybe String
resolveToIdris nr funcId = do
  wasmName <- nr.funcIdToWasm funcId
  case nr.wasmToIdris wasmName of
    Just idrisName => Just idrisName
    Nothing => Just (wasmToIdrisName wasmName)

export
runtimeUnitsForFuncId : NameResolver -> Nat -> List RuntimeUnitRef
runtimeUnitsForFuncId nr funcId =
  case nr.funcIdToWasm funcId of
    Nothing => []
    Just wasmName =>
      let generated = generatedFunctionName wasmName in
      case resolveToIdris nr funcId of
        Just idrisName => [generated, runtimeFunctionName idrisName]
        Nothing => [generated]

export
lookupCanonicalCount : List FuncCases -> String -> Nat
lookupCanonicalCount fcs idrisName =
  case Data.List.find (\fc => fc.funcName == idrisName) fcs of
    Just fc => fc.totalBranches
    Nothing => 1

export
buildCanonicalLookup : Maybe (List FuncCases) -> (String -> Nat)
buildCanonicalLookup Nothing = const 1
buildCanonicalLookup (Just fcs) = lookupCanonicalCount fcs

export
toCoverageResult : ProfilingResult -> NameResolver -> CoverageResult
toCoverageResult profiling nr =
  let executedIds = getExecutedFuncIds profiling
      targets = nr.targetNames
      obligationMap = buildObligationMap $
        map (\name => (name, [runtimeFunctionName name, generatedFunctionName (idrisToWasmName name)])) targets
      executedObligationIds =
        resolveRuntimeUnits obligationMap $
          concatMap (runtimeUnitsForFuncId nr) executedIds
  in buildCoverageResult targets executedObligationIds

export
buildCoverageMeasurement : CoverageResult -> CoverageMeasurement
buildCoverageMeasurement cr =
  MkCoverageMeasurement cr.targetFunctions cr.coveredFunctions [] []

export
buildSemanticFunctionMeasurement : NameResolver
                               -> StaticBranchAnalysis
                               -> List Nat
                               -> CoverageMeasurement
buildSemanticFunctionMeasurement nr staticAnalysis executedFuncIds =
  let obligations = map staticFunctionToCoverageObligation staticAnalysis.functions
      denominatorIds = map (.obligationId) $ filter (\ob => countsAsDenominator ob.classification) obligations
      excludedIds = map (.obligationId) $ filter (\ob => mustBeExcluded ob.classification) obligations
      unknownIds = map (.obligationId) $ filter (\ob => ob.classification == UnknownClassification) obligations
      obligationMap = buildObligationMap $
        map (\ob => (ob.obligationId, [runtimeFunctionName ob.obligationId, generatedFunctionName (idrisToWasmName ob.obligationId)])) obligations
      runtimeUnits = concatMap (runtimeUnitsForFuncId nr) executedFuncIds
      coveredIds = resolveCoveredDenominatorIds obligations obligationMap runtimeUnits
  in MkCoverageMeasurement denominatorIds coveredIds excludedIds unknownIds

coveredFunctionIdsFromBranchProbes : List CoverageObligation
                                  -> List BranchProbeEntry
                                  -> List Nat
                                  -> List String
coveredFunctionIdsFromBranchProbes obligations probeEntries hitProbeIndices =
  let denominatorNames = map (.obligationId) $ filter (\ob => countsAsDenominator ob.classification) obligations
      hitNames = nub $ map (.idrisName) $ filter (\e => elem e.probeIndex hitProbeIndices) probeEntries
  in filter (\name => elem name denominatorNames) hitNames

branchProbeDidPath : String
branchProbeDidPath = "/tmp/dfx_branch_probes.did"

branchProbeOutPath : String -> String
branchProbeOutPath canisterId = "/tmp/dfx_branch_probes_" ++ canisterId ++ ".txt"

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
      let (digits, rest) = span isDigit cs in
      case digits of
        [] => case rest of
                [] => []
                (_ :: xs) => parseNums xs
        _ =>
          case parseNatToken (pack digits) of
            Just n => n :: parseNums rest
            Nothing => parseNums rest

getBranchProbeIndices : Maybe String -> String -> String -> IO (Either String (List Nat))
getBranchProbeIndices mProjectDir canisterId network = do
  Right () <- writeFile branchProbeDidPath "service : { __get_branch_probes : () -> (text) query; }\n"
    | Left err => pure $ Left $ "Failed to write branch probe did: " ++ show err
  exitCode <- system $
    branchProbePrefix mProjectDir ++
    "dfx canister call " ++ canisterId ++
    " __get_branch_probes --query --candid " ++ branchProbeDidPath ++
    " --network " ++ network ++
    " > " ++ branchProbeOutPath canisterId ++ " 2>&1"
  if exitCode /= 0
     then do
       errText <- readFile (branchProbeOutPath canisterId)
       _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ branchProbeOutPath canisterId
       case errText of
         Right content => pure $ Left $ "__get_branch_probes call failed: " ++ trim content
         Left _ => pure $ Left "__get_branch_probes call failed"
     else do
       Right content <- readFile (branchProbeOutPath canisterId)
         | Left err => do
             _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ branchProbeOutPath canisterId
             pure $ Left $ "Failed to read branch probe output: " ++ show err
       _ <- system $ "rm -f " ++ branchProbeDidPath ++ " " ++ branchProbeOutPath canisterId
       pure $ Right $ nub (parseBranchProbeIndices content)

export
buildMaterializedBranchMeasurement : StaticBranchAnalysis
                                  -> List BranchProbeEntry
                                  -> List Nat
                                  -> CoverageMeasurement
buildMaterializedBranchMeasurement staticAnalysis entries hitProbeIndices =
  let denominatorIds = materializedBranchIds staticAnalysis entries
      coveredIds = coveredMaterializedBranchIds staticAnalysis entries hitProbeIndices
  in MkCoverageMeasurement denominatorIds coveredIds [] []

export
toFunctionRuntimeHit : Nat -> String -> Nat -> Nat -> FunctionRuntimeHit
toFunctionRuntimeHit _ idrisName hitCount canonCount =
  MkFunctionRuntimeHit idrisName (idrisToWasmName idrisName) canonCount hitCount 0 0

export
buildRuntimeHits : ProfilingResult -> NameResolver -> (String -> Nat) -> List FunctionRuntimeHit
buildRuntimeHits profiling nr canonLookup =
  let funcCycles = (analyzeCycles profiling).funcCycles in
  mapMaybe toHit funcCycles
  where
    toHit : FuncCycleEntry -> Maybe FunctionRuntimeHit
    toHit entry = do
      idrisName <- resolveToIdris nr entry.funcId
      let canonCount = canonLookup idrisName
      Just $ toFunctionRuntimeHit entry.funcId idrisName entry.callCount canonCount

export
buildHighImpactTargets : CoverageResult -> List HI.HighImpactTarget
buildHighImpactTargets cr =
  map (\funcName => HI.mkUntestedTarget funcName "" 1 0) (uncoveredFunctions cr)

public export
record WasmCoverageOptions where
  constructor MkWasmCoverageOptions
  wasmPath : String
  canisterId : String
  network : String
  sourceMapPath : Maybe String
  projectDir : Maybe String
  ipkgName : Maybe String
  branchProbeMapPath : Maybe String
  harnessDumpcasesPath : Maybe String

export
runWasmCoverageFromProfiling : WasmCoverageOptions -> ProfilingResult -> IO (Either String WasmCoverageResult)
runWasmCoverageFromProfiling opts profiling = do
  branchProbeHits <- case opts.branchProbeMapPath of
    Nothing => pure Nothing
    Just _ => do
      result <- getBranchProbeIndices opts.projectDir opts.canisterId opts.network
      pure $ either (const Nothing) Just result

  Right funcNames <- IPN.extractIcpFuncNames opts.wasmPath
    | Left err => pure $ Left $ "Failed to parse WASM names: " ++ err

  mSourceMap <- case opts.sourceMapPath of
    Nothing => pure Nothing
    Just path => do
      result <- readSourceMap path
      pure $ either (const Nothing) Just result

  mDumpcases <- case opts.harnessDumpcasesPath of
    Just path => do
      Right content <- readFile path
        | Left _ =>
            case (opts.projectDir, opts.ipkgName) of
              (Just projDir, Just ipkg) => do
                funcsResult <- DP.runAndParseDumpcases projDir ipkg
                pure $ either (const Nothing) (\funcs => Just (funcs, DP.toStaticBranchAnalysis funcs)) funcsResult
              _ => pure Nothing
      let parsed = DP.parseDumpcases content
      let analysis = DP.toStaticBranchAnalysis parsed
      pure $ Just (parsed, analysis)
    Nothing =>
      case (opts.projectDir, opts.ipkgName) of
        (Just projDir, Just ipkg) => do
          funcsResult <- DP.runAndParseDumpcases projDir ipkg
          pure $ either (const Nothing) (\funcs => Just (funcs, DP.toStaticBranchAnalysis funcs)) funcsResult
        _ => pure Nothing

  let nameResolver = buildNameResolver funcNames mSourceMap

  mProbeMap <- case opts.branchProbeMapPath of
    Nothing => pure Nothing
    Just path => do
      result <- readBranchProbeMap path
      pure $ either (const Nothing) Just result

  let canonLookup = buildCanonicalLookup (map fst mDumpcases)
  let coreResult = toCoverageResult profiling nameResolver
  let runtimeHits = buildRuntimeHits profiling nameResolver canonLookup
  let highImpacts = buildHighImpactTargets coreResult
  let measurement0 =
        case mDumpcases of
          Just (_, staticAnalysis) =>
            buildSemanticFunctionMeasurement nameResolver staticAnalysis (getExecutedFuncIds profiling)
          Nothing => buildCoverageMeasurement coreResult
  let mStaticAnalysis = map snd mDumpcases
  let measurement =
        case mStaticAnalysis of
          Just staticAnalysis =>
            case (mProbeMap, branchProbeHits) of
              (Just probeEntries, Just hitProbeIndices) =>
                let obligations = map staticFunctionToCoverageObligation staticAnalysis.functions
                    probeCovered = coveredFunctionIdsFromBranchProbes obligations probeEntries hitProbeIndices
                in { coveredIds := nub (measurement0.coveredIds ++ probeCovered) } measurement0
              _ => measurement0
          Nothing => measurement0
  let branchMeasurement =
        case mStaticAnalysis of
          Just staticAnalysis =>
            case mProbeMap of
              Just probeEntries =>
                Just (buildMaterializedBranchMeasurement staticAnalysis probeEntries (fromMaybe [] branchProbeHits))
              Nothing => Nothing
          Nothing => Nothing
  let cycles = analyzeCycles profiling
  let wasmFuncCount = funcNames.count
  let mStats = map (DP.computeStats . fst) mDumpcases

  pure $ Right $ MkWasmCoverageResult
    coreResult runtimeHits highImpacts measurement branchMeasurement
    (maybe [] denominatorIds branchMeasurement)
    cycles wasmFuncCount opts.sourceMapPath mStats

export
runWasmCoverage : WasmCoverageOptions -> IO (Either String WasmCoverageResult)
runWasmCoverage opts = do
  Right profiling <- getProfilingData opts.canisterId opts.network
    | Left err => pure $ Left $ "Failed to get profiling data: " ++ err
  runWasmCoverageFromProfiling opts profiling

export
formatWasmCoverage : WasmCoverageResult -> String
formatWasmCoverage r =
  let cr = r.coreResult
      dumpcasesInfo =
        case r.dumpcasesStats of
          Nothing => []
          Just stats =>
            [ ""
            , "Dumpcases Analysis:"
            , "  Total functions: " ++ show stats.totalFunctions
            , "  Total branches: " ++ show stats.totalBranches
            , "  Included functions: " ++ show stats.includedFuncs
            ]
      header =
        [ "═══════════════════════════════════════════════════════════════"
        , "WASM Coverage Report (idris2-coverage-core compatible)"
        , "═══════════════════════════════════════════════════════════════"
        , ""
        , "Coverage: " ++ show (coveredCount cr) ++ "/" ++ show (totalCount cr) ++
          " functions (" ++ show (coveragePercent cr) ++ "%)"
        , "Measurement denominator IDs: " ++ show (length r.measurement.denominatorIds)
        , "Measurement covered IDs: " ++ show (length r.measurement.coveredIds)
        ]
      branchInfo =
        case r.branchMeasurement of
          Nothing => []
          Just bm =>
            [ "Materialized branch IDs: " ++ show (length bm.denominatorIds)
            , "Covered branch IDs: " ++ show (length bm.coveredIds)
            ]
      wasmInfo =
        [ ""
        , "WASM Statistics:"
        , "  Total WASM functions: " ++ show r.wasmFuncCount
        , "  Total cycles: " ++ formatCycles (totalCycles r.cycleAnalysis)
        ]
  in unlines (header ++ branchInfo ++ wasmInfo ++ dumpcasesInfo ++ [""]) ++
     (if null r.highImpacts
         then "✓ No high-impact coverage gaps"
         else "High Impact Targets (" ++ show (length r.highImpacts) ++ "):\n" ++
              unlines (map formatTarget (take 10 r.highImpacts))) ++
     "\n" ++
     (if null (topConsumers r.cycleAnalysis)
         then ""
         else "\nTop Cycle Consumers:\n" ++
              unlines (map formatCycleEntry (take 5 (topConsumers r.cycleAnalysis))))
  where
    formatTarget : HI.HighImpactTarget -> String
    formatTarget t = "  ⚠️ " ++ t.funcName ++ " [" ++ show t.kind ++ "]"

    formatCycleEntry : FuncCycleEntry -> String
    formatCycleEntry e = "  func[" ++ show e.funcId ++ "]: " ++
                         show e.callCount ++ " calls, " ++
                         formatCycles e.totalCycles ++ " cycles"
