||| Branch probe map parser for DFX branch-level runtime coverage.
|||
||| Reads the CSV emitted by Idris2IcWasm branch probe instrumentation and
||| maps probe function names back to function-local branch ordinals.
module DfxCoverage.SourceMap.BranchProbeMap

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import Coverage.Classification.BranchClass
import Coverage.Standardization.Types
import DfxCoverage.DumpcasesParser

%default covering

public export
record BranchProbeEntry where
  constructor MkBranchProbeEntry
  probeIndex : Nat
  probeName : String
  idrisName : String
  ordinalInFunc : Nat
  lineNumber : Nat
  kind : String

splitCsv : String -> List String
splitCsv s = forget $ split (== ',') s

indexOrd : Nat -> List a -> Maybe a
indexOrd _ [] = Nothing
indexOrd Z (x :: _) = Just x
indexOrd (S k) (_ :: xs) = indexOrd k xs

export
parseEntry : String -> Maybe BranchProbeEntry
parseEntry line =
  case splitCsv (trim line) of
    [probeIndexStr, probeName, idrisName, ordinalStr, lineStr, kind] =>
      case (parsePositive probeIndexStr, parsePositive ordinalStr, parsePositive lineStr) of
        (Just probeIndex, Just ordinalInFunc, Just lineNumber) =>
          Just $ MkBranchProbeEntry probeIndex probeName idrisName ordinalInFunc lineNumber kind
        _ => Nothing
    [probeName, idrisName, ordinalStr, lineStr, kind] =>
      case (parsePositive ordinalStr, parsePositive lineStr) of
        (Just ordinalInFunc, Just lineNumber) =>
          Just $ MkBranchProbeEntry ordinalInFunc probeName idrisName ordinalInFunc lineNumber kind
        _ => Nothing
    _ => Nothing

export
readBranchProbeMap : String -> IO (Either String (List BranchProbeEntry))
readBranchProbeMap path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read branch probe map: " ++ show err
  let entries = mapMaybe parseEntry (drop 1 (lines content))
  pure $ Right entries

reachableBranchIdsForFunction : StaticBranchAnalysis -> String -> List String
reachableBranchIdsForFunction analysis idrisName =
  case Data.List.find sameName analysis.functions of
    Nothing => []
    Just fn =>
      map (show . (.branchId)) $
        filter (\b => countsAsDenominator (branchClassToObligationClass b.branchClass)) fn.branches
  where
    sameName : StaticFunctionAnalysis -> Bool
    sameName (MkStaticFunctionAnalysis fullName _) = fullName == idrisName

export
materializedBranchIds : StaticBranchAnalysis -> List BranchProbeEntry -> List String
materializedBranchIds analysis entries =
  nub $ concatMap perFunc groupedNames
  where
    groupedNames : List String
    groupedNames = nub (map (.idrisName) entries)

    sortEntries : List BranchProbeEntry -> List BranchProbeEntry
    sortEntries [] = []
    sortEntries (x :: xs) = insert x (sortEntries xs)
      where
        insert : BranchProbeEntry -> List BranchProbeEntry -> List BranchProbeEntry
        insert e [] = [e]
        insert e (y :: ys) =
          if e.ordinalInFunc <= y.ordinalInFunc
             then e :: y :: ys
             else y :: insert e ys

    perFunc : String -> List String
    perFunc idrisName =
      let staticIds = reachableBranchIdsForFunction analysis idrisName
          probes = sortEntries (filter (\e => e.idrisName == idrisName) entries)
      in mapMaybe (\entry => indexOrd entry.ordinalInFunc staticIds) probes

export
coveredMaterializedBranchIds : StaticBranchAnalysis -> List BranchProbeEntry -> List Nat -> List String
coveredMaterializedBranchIds analysis entries hitProbeIndices =
  nub $ mapMaybe toBranchId hitEntries
  where
    hitEntries : List BranchProbeEntry
    hitEntries = filter (\e => elem e.probeIndex hitProbeIndices) entries

    toBranchId : BranchProbeEntry -> Maybe String
    toBranchId entry =
      indexOrd entry.ordinalInFunc (reachableBranchIdsForFunction analysis entry.idrisName)
