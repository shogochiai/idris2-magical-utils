module FfiCoverage.Model

import Integration.Model
import Integration.Registry as IntegrationRegistry
import Integration.Trace
import Data.List
import Data.Maybe
import Data.String

%default covering

public export
data StepStatus = StepOK | StepSkipped | StepNoInput | StepError

public export
Eq StepStatus where
  StepOK == StepOK = True
  StepOK == StepSkipped = False
  StepOK == StepNoInput = False
  StepOK == StepError = False
  StepSkipped == StepOK = False
  StepSkipped == StepSkipped = True
  StepSkipped == StepNoInput = False
  StepSkipped == StepError = False
  StepNoInput == StepOK = False
  StepNoInput == StepSkipped = False
  StepNoInput == StepNoInput = True
  StepNoInput == StepError = False
  StepError == StepOK = False
  StepError == StepSkipped = False
  StepError == StepNoInput = False
  StepError == StepError = True

public export
Show StepStatus where
  show StepOK = "StepOK"
  show StepSkipped = "StepSkipped"
  show StepNoInput = "StepNoInput"
  show StepError = "StepError"

public export
stepStatusEqObserved : StepStatus -> StepStatus -> Bool
stepStatusEqObserved left right = left == right

public export
FfiPath : Type
FfiPath = SomeIntegrationOp

public export
record FfiCoverageReport where
  constructor MkFfiCoverageReport
  requiredPaths      : List String
  declaredPaths      : List FfiPath
  tracedPaths        : List FfiPath
  missingDeclaration : List String
  missingTrace       : List FfiPath
  missingEvidence    : List FfiPath
  unclassifiedMutation : List FfiPath
  stepStatuses       : List (String, StepStatus)

public export
defaultRegistry : List FfiPath
defaultRegistry = IntegrationRegistry.defaultRegistry

public export
parseBoundary : String -> Maybe Boundary
parseBoundary "LocalProof" = Just LocalProof
parseBoundary "GovernedSource" = Just GovernedSource
parseBoundary "GovernedCI" = Just GovernedCI
parseBoundary "GovernedReview" = Just GovernedReview
parseBoundary "GovernedPublication" = Just GovernedPublication
parseBoundary "InstanceGovernance" = Just InstanceGovernance
parseBoundary "GlobalRegistry" = Just GlobalRegistry
parseBoundary "HostService" = Just HostService
parseBoundary _ = Nothing

public export
parseRisk : String -> Maybe Risk
parseRisk "local-only" = Just LocalOnly
parseRisk "read-only" = Just ReadOnly
parseRisk "mutating" = Just Mutating
parseRisk "destructive" = Just Destructive
parseRisk _ = Nothing

mkSomeByTrust :
  TrustLevel -> String -> Boundary -> Risk -> String -> List EvidenceKind ->
  Maybe String -> ContextPolicy -> Maybe String -> Maybe String -> FfiPath
mkSomeByTrust UnsafeDiagnostic path boundary risk adapter evidence idempotency context recovery negative =
  MkSomeIntegrationOp UnsafeDiagnostic $
    mkDiagnosticOp path boundary risk adapter evidence idempotency context recovery negative
mkSomeByTrust Candidate path boundary risk adapter evidence idempotency context recovery negative =
  MkSomeIntegrationOp Candidate $
    mkCandidateOp path boundary risk adapter evidence idempotency context recovery negative
mkSomeByTrust ProofGrade path boundary risk adapter evidence idempotency context recovery negative =
  MkSomeIntegrationOp ProofGrade $
    mkProofOp path boundary risk adapter evidence idempotency context recovery negative

public export
parseRegistryLine : String -> Maybe FfiPath
parseRegistryLine line = do
  path <- parseJsonStringField "pathId" line
  trustText <- parseJsonStringField "trust" line
  trust <- parseTrustLevel trustText
  boundaryText <- parseJsonStringField "boundary" line
  boundary <- parseBoundary boundaryText
  riskText <- parseJsonStringField "risk" line
  risk <- parseRisk riskText
  adapter <- parseJsonStringField "adapter" line
  evidenceIds <- parseJsonStringArrayField "evidenceIds" line
  let evidence = map (\eid => evidenceKind eid eid) evidenceIds
  let idempotency = parseJsonStringField "idempotency" line
  let recovery = parseJsonStringField "recoveryPolicy" line
  let negative = parseJsonStringField "negativeTest" line
  Just (mkSomeByTrust trust path boundary risk adapter evidence idempotency defaultContextPolicy recovery negative)

public export
registryFromJsonLines : String -> List FfiPath
registryFromJsonLines text = mapMaybe parseRegistryLine (lines text)

public export
pathIsSideEffecting : FfiPath -> Bool
pathIsSideEffecting path = isSideEffecting (someRisk path)

public export
hasMutationClassification : FfiPath -> Bool
hasMutationClassification path =
  if pathIsSideEffecting path
    then not (null (someEvidenceIds path)) && isJust (someIdempotency path) && isJust (someRecoveryPolicy path)
    else True

stripTokenChars : List Char -> List Char
stripTokenChars [] = []
stripTokenChars (c :: rest) =
  if c == '"' || c == '\'' || c == ',' || c == ']' || c == '[' || c == ')' || c == '('
     || c == '{' || c == '}' || c == ';'
    then stripTokenChars rest
    else c :: stripTokenChars rest

cleanToken : String -> String
cleanToken token = pack (stripTokenChars (unpack (trim token)))

public export
extractIntegrationPathRefs : String -> List String
extractIntegrationPathRefs text =
  nub (filter (isPrefixOf "integration/") (map cleanToken (words text)))

public export
lookupPath : String -> List FfiPath -> Maybe FfiPath
lookupPath path registry = find (\p => somePathId p == path) registry

public export
pathsFromRefs : List String -> List FfiPath -> List FfiPath
pathsFromRefs refs registry = mapMaybe (\ref => FfiCoverage.Model.lookupPath ref registry) refs

public export
stepSelected : Nat -> List Nat -> Bool
stepSelected n [] = True
stepSelected n xs = elem n xs

public export
requiredRefsForStepsWithRegistry : List Nat -> List FfiPath -> String -> String -> List String
requiredRefsForStepsWithRegistry steps registry rootSpec taskText =
  let observedRootRefs = extractIntegrationPathRefs rootSpec
      observedTaskRefs = extractIntegrationPathRefs taskText
      observedRefs = nub (observedRootRefs ++ observedTaskRefs)
      rootRefs = if stepSelected 1 steps then observedRootRefs else []
      taskRefs = if stepSelected 2 steps then observedTaskRefs else []
  in if stepSelected 4 steps
       then if null observedRefs then map somePathId registry else observedRefs
       else nub (rootRefs ++ taskRefs)

public export
requiredRefsForSteps : List Nat -> String -> String -> List String
requiredRefsForSteps steps rootSpec taskText =
  requiredRefsForStepsWithRegistry steps FfiCoverage.Model.defaultRegistry rootSpec taskText

traceEventCovers : String -> FfiPath -> Bool
traceEventCovers line path = traceLineCovers path line

coveredByTrace : String -> FfiPath -> Bool
coveredByTrace trace path = any (\line => traceEventCovers line path) (lines trace)

public export
buildReportWithRegistry : List FfiPath -> List String -> String -> List (String, StepStatus) -> FfiCoverageReport
buildReportWithRegistry registry requiredRefs trace stepStatuses =
  let declared = pathsFromRefs requiredRefs registry
      traced = filter (coveredByTrace trace) declared
      missingDecl = filter (\ref => isNothing (FfiCoverage.Model.lookupPath ref registry)) requiredRefs
      missingTr = filter (\path => not (elem path traced)) declared
      missingEv = filter (\path => pathIsSideEffecting path && null (someEvidenceIds path)) declared
      unclassified = filter (\path => not (hasMutationClassification path)) declared
  in MkFfiCoverageReport requiredRefs declared traced missingDecl missingTr missingEv unclassified stepStatuses

public export
buildReport : List String -> String -> List (String, StepStatus) -> FfiCoverageReport
buildReport requiredRefs trace stepStatuses =
  buildReportWithRegistry FfiCoverage.Model.defaultRegistry requiredRefs trace stepStatuses

public export
gapCount : FfiCoverageReport -> Nat
gapCount r =
  length r.missingDeclaration
    + length r.missingTrace
    + length r.missingEvidence
    + length r.unclassifiedMutation

public export
coveragePercent : FfiCoverageReport -> Double
coveragePercent r =
  case length r.declaredPaths of
    0 => if null r.missingDeclaration then 100.0 else 0.0
    n => (cast (length r.tracedPaths) / cast n) * 100.0

public export
joinComma : List String -> String
joinComma [] = ""
joinComma [x] = x
joinComma (x :: xs) = x ++ "," ++ joinComma xs

public export
quoteJson : String -> String
quoteJson s = "\"" ++ pack (escape (unpack s)) ++ "\""
  where
    escape : List Char -> List Char
    escape [] = []
    escape ('"' :: rest) = '\\' :: '"' :: escape rest
    escape ('\\' :: rest) = '\\' :: '\\' :: escape rest
    escape ('\n' :: rest) = '\\' :: 'n' :: escape rest
    escape (c :: rest) = c :: escape rest

public export
renderStringArray : List String -> String
renderStringArray xs = "[" ++ joinComma (map quoteJson xs) ++ "]"

public export
renderPathJson : FfiPath -> String
renderPathJson p =
  "{"
    ++ "\"pathId\":" ++ quoteJson (somePathId p) ++ ","
    ++ "\"trust\":" ++ quoteJson (show (someTrust p)) ++ ","
    ++ "\"boundary\":" ++ quoteJson (show (someBoundary p)) ++ ","
    ++ "\"risk\":" ++ quoteJson (show (someRisk p)) ++ ","
    ++ "\"adapter\":" ++ quoteJson (someAdapter p) ++ ","
    ++ "\"evidence\":" ++ FfiCoverage.Model.renderStringArray (someEvidenceIds p) ++ ","
    ++ "\"idempotency\":" ++ maybe "null" quoteJson (someIdempotency p) ++ ","
    ++ "\"recoveryPolicy\":" ++ maybe "null" quoteJson (someRecoveryPolicy p)
  ++ "}"

public export
renderPathArray : List FfiPath -> String
renderPathArray paths = "[" ++ joinComma (map renderPathJson paths) ++ "]"

public export
renderStatusJson : (String, StepStatus) -> String
renderStatusJson (name, status) = quoteJson name ++ ":" ++ quoteJson (show status)

public export
renderIncompleteStatusJson : (String, StepStatus) -> String
renderIncompleteStatusJson (name, status) =
  "{\"pass\":" ++ quoteJson name ++ ",\"status\":" ++ quoteJson (show status) ++ "}"

public export
incompleteStatuses : FfiCoverageReport -> List (String, StepStatus)
incompleteStatuses r =
  filter (\(_, status) => status == StepNoInput || status == StepError) r.stepStatuses

public export
claimAdmissible : FfiCoverageReport -> Bool
claimAdmissible r = gapCount r == 0 && null (incompleteStatuses r)

public export
renderReportJson : FfiCoverageReport -> String
renderReportJson r =
  "{"
    ++ "\"stepStatuses\":{" ++ joinComma (map renderStatusJson r.stepStatuses) ++ "},"
    ++ "\"gaps\":" ++ FfiCoverage.Model.renderStringArray (r.missingDeclaration ++ map somePathId r.missingTrace) ++ ","
    ++ "\"gapCount\":" ++ show (gapCount r) ++ ","
    ++ "\"clean\":" ++ (if claimAdmissible r then "true" else "false") ++ ","
    ++ "\"incompleteSteps\":[" ++ joinComma (map renderIncompleteStatusJson (incompleteStatuses r)) ++ "],"
    ++ "\"coverage_percent\":" ++ show (coveragePercent r) ++ ","
    ++ "\"claim_admissible\":" ++ (if claimAdmissible r then "true" else "false") ++ ","
    ++ "\"missing_paths\":" ++ FfiCoverage.Model.renderStringArray (map somePathId r.missingTrace) ++ ","
    ++ "\"missingDeclaration\":" ++ FfiCoverage.Model.renderStringArray r.missingDeclaration ++ ","
    ++ "\"declaredPaths\":" ++ renderPathArray r.declaredPaths
  ++ "}"

public export
renderReportText : FfiCoverageReport -> String
renderReportText r =
  unlines $
    [ "=== Integration FFI Coverage ==="
    , "coverage_percent: " ++ show (coveragePercent r)
    , "claim_admissible: " ++ show (claimAdmissible r)
    , "Missing paths: " ++ show (length r.missingTrace)
    , "Missing declarations: " ++ show (length r.missingDeclaration)
    , ""
    , "Missing path ids:"
    ]
    ++ map (\p => "- " ++ somePathId p) r.missingTrace
    ++ ["", "Missing declarations:"]
    ++ map (\p => "- " ++ p) r.missingDeclaration
