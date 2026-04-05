||| Runtime Unit To Obligation Mapping
|||
||| Shared abstraction for mapping backend-observed runtime units back to
||| semantic coverage obligation identifiers.
module Coverage.Core.ObligationMap

import Data.List

import Coverage.Standardization.Types

%default total

public export
data RuntimeUnitKind
  = RuntimeFunctionName
  | GeneratedFunctionName
  | BackendSymbolName
  | ProfilingLabel
  | SourceSpanRef

public export
Eq RuntimeUnitKind where
  RuntimeFunctionName == RuntimeFunctionName = True
  GeneratedFunctionName == GeneratedFunctionName = True
  BackendSymbolName == BackendSymbolName = True
  ProfilingLabel == ProfilingLabel = True
  SourceSpanRef == SourceSpanRef = True
  _ == _ = False

public export
Show RuntimeUnitKind where
  show RuntimeFunctionName = "runtime-function-name"
  show GeneratedFunctionName = "generated-function-name"
  show BackendSymbolName = "backend-symbol-name"
  show ProfilingLabel = "profiling-label"
  show SourceSpanRef = "source-span"

public export
record RuntimeUnitRef where
  constructor MkRuntimeUnitRef
  kind : RuntimeUnitKind
  value : String

public export
Eq RuntimeUnitRef where
  a == b = a.kind == b.kind && a.value == b.value

public export
Show RuntimeUnitRef where
  show ref = show ref.kind ++ ":" ++ ref.value

public export
record ObligationMapEntry where
  constructor MkObligationMapEntry
  runtimeUnit : RuntimeUnitRef
  obligationId : String

public export
Eq ObligationMapEntry where
  a == b = a.runtimeUnit == b.runtimeUnit && a.obligationId == b.obligationId

public export
Show ObligationMapEntry where
  show entry = show entry.runtimeUnit ++ " -> " ++ entry.obligationId

public export
record ObligationMap where
  constructor MkObligationMap
  entries : List ObligationMapEntry

public export
Show ObligationMap where
  show om = "ObligationMap(" ++ show (length om.entries) ++ " entries)"

public export
runtimeFunctionName : String -> RuntimeUnitRef
runtimeFunctionName = MkRuntimeUnitRef RuntimeFunctionName

public export
generatedFunctionName : String -> RuntimeUnitRef
generatedFunctionName = MkRuntimeUnitRef GeneratedFunctionName

public export
backendSymbolName : String -> RuntimeUnitRef
backendSymbolName = MkRuntimeUnitRef BackendSymbolName

public export
profilingLabelRef : String -> RuntimeUnitRef
profilingLabelRef = MkRuntimeUnitRef ProfilingLabel

public export
sourceSpanRef : String -> RuntimeUnitRef
sourceSpanRef = MkRuntimeUnitRef SourceSpanRef

dedup : Eq a => List a -> List a
dedup [] = []
dedup (x :: xs) = if elem x xs then dedup xs else x :: dedup xs

public export
buildObligationMap : List (String, List RuntimeUnitRef) -> ObligationMap
buildObligationMap pairs =
  MkObligationMap $
    dedup $
      concatMap (\(obligationId, units) =>
        map (\unit => MkObligationMapEntry unit obligationId) units
      ) pairs

public export
buildFunctionObligationMap : List CoverageObligation
                          -> (CoverageObligation -> List RuntimeUnitRef)
                          -> ObligationMap
buildFunctionObligationMap obligations aliasBuilder =
  buildObligationMap $
    map (\ob => (ob.obligationId, aliasBuilder ob)) $
      filter (\ob => ob.granularity == FunctionLevel) obligations

public export
resolveRuntimeUnit : ObligationMap -> RuntimeUnitRef -> List String
resolveRuntimeUnit om unit =
  dedup $
    map (.obligationId) $
      filter (\entry => entry.runtimeUnit == unit) om.entries

public export
resolveRuntimeUnits : ObligationMap -> List RuntimeUnitRef -> List String
resolveRuntimeUnits om units = dedup $ concatMap (resolveRuntimeUnit om) units

public export
resolveCoveredDenominatorIds : List CoverageObligation
                            -> ObligationMap
                            -> List RuntimeUnitRef
                            -> List String
resolveCoveredDenominatorIds obligations om units =
  let denominatorIds =
        map (.obligationId) $
          filter (\ob => countsAsDenominator ob.classification) obligations
      covered = resolveRuntimeUnits om units
  in filter (\oid => elem oid denominatorIds) covered
