module Tests.AllTests

import FfiCoverage.Model
import Integration.Model
import Data.List
import Data.String

%default covering

public export
record TestDef where
  constructor MkTestDef
  testId : String
  description : String
  run : () -> Bool

test : String -> String -> (() -> Bool) -> TestDef
test tid desc fn = MkTestDef tid desc fn

proofBundleTrace : String
proofBundleTrace =
  "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"trust\":\"proof-grade\",\"outcome\":\"pass\",\"evidenceIds\":[\"quality-bundle-json\",\"quality-bundle-junit\"]}"

unsafeTrustTrace : String
unsafeTrustTrace =
  "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"trust\":\"unsafe-diagnostic\",\"outcome\":\"pass\",\"evidenceIds\":[\"quality-bundle-json\",\"quality-bundle-junit\"]}"

missingEvidenceTrace : String
missingEvidenceTrace =
  "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"trust\":\"proof-grade\",\"outcome\":\"pass\",\"evidenceIds\":[]}"

stepStatusEqCases : List (StepStatus, StepStatus, Bool)
stepStatusEqCases =
  [ (StepOK, StepOK, True)
  , (StepOK, StepSkipped, False)
  , (StepOK, StepNoInput, False)
  , (StepOK, StepError, False)
  , (StepSkipped, StepOK, False)
  , (StepSkipped, StepSkipped, True)
  , (StepSkipped, StepNoInput, False)
  , (StepSkipped, StepError, False)
  , (StepNoInput, StepOK, False)
  , (StepNoInput, StepSkipped, False)
  , (StepNoInput, StepNoInput, True)
  , (StepNoInput, StepError, False)
  , (StepError, StepOK, False)
  , (StepError, StepSkipped, False)
  , (StepError, StepNoInput, False)
  , (StepError, StepError, True)
  ]

checkStepStatusEq : (StepStatus, StepStatus, Bool) -> Bool
checkStepStatusEq (left, right, expected) = stepStatusEqObserved left right == expected

proofBundlePath : Maybe FfiPath
proofBundlePath = FfiCoverage.Model.lookupPath "integration/governedci/proof/bundle" FfiCoverage.Model.defaultRegistry

publicVerifyPath : Maybe FfiPath
publicVerifyPath = FfiCoverage.Model.lookupPath "integration/governedci/public-verify/submit-request" FfiCoverage.Model.defaultRegistry

customRegistry : List FfiPath
customRegistry =
  FfiCoverage.Model.registryFromJsonLines "{\"pathId\":\"integration/custom/ci/proof\",\"trust\":\"proof-grade\",\"boundary\":\"GovernedCI\",\"risk\":\"mutating\",\"adapter\":\"scenario:custom\",\"evidenceIds\":[\"custom-receipt\"],\"idempotency\":\"custom-key\",\"recoveryPolicy\":\"custom recovery\",\"negativeTest\":\"custom negative\"}"

authorityScopedSpec : String
authorityScopedSpec =
  "integration/governedci/public-verify/submit-request integration/governedci/public-verify/query-receipt"

authorityScopedTask : String
authorityScopedTask =
  "integration/governedci/step3/gate integration/governedpublication/github/project-check integration/global-registry/upgrade/preflight"

||| REQ_FFICOV_REFS_001: Integration path references are extracted
test_REQ_FFICOV_REFS_001 : () -> Bool
test_REQ_FFICOV_REFS_001 () =
  extractIntegrationPathRefs "invariant = \"integration/governedci/proof/bundle\"" == ["integration/governedci/proof/bundle"]

||| REQ_FFICOV_REFS_002: Step4 coverage is scoped by declared Integration paths
test_REQ_FFICOV_REFS_002 : () -> Bool
test_REQ_FFICOV_REFS_002 () =
  let scoped = requiredRefsForSteps [4] authorityScopedSpec authorityScopedTask
      fallback = requiredRefsForSteps [4] "" ""
  in length scoped == 5
    && stepSelected 1 []
    && stepSelected 4 [4]
    && not (stepSelected 4 [1, 2, 3])
    && elem "integration/governedci/public-verify/submit-request" scoped
    && elem "integration/global-registry/upgrade/preflight" scoped
    && not (elem "integration/governedreview/private-review/create-request" scoped)
    && length fallback == length FfiCoverage.Model.defaultRegistry

||| REQ_FFICOV_REPORT_001: Proof-grade trace closes report gap
test_REQ_FFICOV_REPORT_001 : () -> Bool
test_REQ_FFICOV_REPORT_001 () =
  let r = buildReport ["integration/governedci/proof/bundle"] proofBundleTrace [("trace-coverage", StepOK)] in
  gapCount r == 0 && claimAdmissible r && coveragePercent r == 100.0

||| REQ_FFICOV_REPORT_002: Unknown path is missing declaration
test_REQ_FFICOV_REPORT_002 : () -> Bool
test_REQ_FFICOV_REPORT_002 () =
  let r = buildReport ["integration/unknown/path"] proofBundleTrace [("trace-coverage", StepOK)] in
  gapCount r == 1 && not (claimAdmissible r) && r.missingDeclaration == ["integration/unknown/path"]

||| REQ_FFICOV_REPORT_003: No input trace blocks admissible claim
test_REQ_FFICOV_REPORT_003 : () -> Bool
test_REQ_FFICOV_REPORT_003 () =
  let r = buildReport ["integration/governedci/proof/bundle"] "" [("trace-coverage", StepNoInput)] in
  not (claimAdmissible r) && length (incompleteStatuses r) == 1

||| REQ_FFICOV_REPORT_004: Unsafe trust trace does not close report gap
test_REQ_FFICOV_REPORT_004 : () -> Bool
test_REQ_FFICOV_REPORT_004 () =
  let r = buildReport ["integration/governedci/proof/bundle"] unsafeTrustTrace [("trace-coverage", StepOK)] in
  gapCount r == 1 && not (claimAdmissible r) && map somePathId r.missingTrace == ["integration/governedci/proof/bundle"]

||| REQ_FFICOV_REPORT_005: Missing evidence trace does not close report gap
test_REQ_FFICOV_REPORT_005 : () -> Bool
test_REQ_FFICOV_REPORT_005 () =
  let r = buildReport ["integration/governedci/proof/bundle"] missingEvidenceTrace [("trace-coverage", StepOK)] in
  gapCount r == 1 && not (claimAdmissible r)

||| REQ_FFICOV_REPORT_006: Destructive default op has mutation contract
test_REQ_FFICOV_REPORT_006 : () -> Bool
test_REQ_FFICOV_REPORT_006 () =
  let r = buildReport ["integration/global-registry/upgrade/apply"] "" [("trace-coverage", StepOK)] in
  null r.missingEvidence && null r.unclassifiedMutation

||| REQ_FFICOV_STATUS_001: Step status equality and display are total
test_REQ_FFICOV_STATUS_001 : () -> Bool
test_REQ_FFICOV_STATUS_001 () =
  all checkStepStatusEq stepStatusEqCases
    && show StepOK == "StepOK"
    && show StepSkipped == "StepSkipped"
    && show StepNoInput == "StepNoInput"
    && show StepError == "StepError"

||| REQ_FFICOV_RENDER_001: JSON string helpers cover arrays and escaped tokens
test_REQ_FFICOV_RENDER_001 : () -> Bool
test_REQ_FFICOV_RENDER_001 () =
  joinComma [] == ""
    && joinComma ["alpha"] == "alpha"
    && joinComma ["alpha", "beta"] == "alpha,beta"
    && quoteJson "quote\"slash\\line\nend" == "\"quote\\\"slash\\\\line\\nend\""
    && FfiCoverage.Model.renderStringArray [] == "[]"
    && FfiCoverage.Model.renderStringArray ["alpha"] == "[\"alpha\"]"
    && FfiCoverage.Model.renderStringArray ["alpha", "beta"] == "[\"alpha\",\"beta\"]"

||| REQ_FFICOV_RENDER_002: Report renderers expose path, status, gap, and admissibility fields
test_REQ_FFICOV_RENDER_002 : () -> Bool
test_REQ_FFICOV_RENDER_002 () =
  let clean = buildReport ["integration/governedci/proof/bundle"] proofBundleTrace [("trace-coverage", StepOK)]
      blocked = buildReport ["integration/unknown/path"] "" [("trace-coverage", StepNoInput), ("semantic", StepError), ("optional", StepSkipped)]
      cleanJson = renderReportJson clean
      blockedJson = renderReportJson blocked
      blockedText = renderReportText blocked
  in isInfixOf "\"clean\":true" cleanJson
    && isInfixOf "\"claim_admissible\":true" cleanJson
    && isInfixOf "\"declaredPaths\":[{\"pathId\":\"integration/governedci/proof/bundle\"" cleanJson
    && isInfixOf "\"clean\":false" blockedJson
    && isInfixOf "\"incompleteSteps\":[{\"pass\":\"trace-coverage\",\"status\":\"StepNoInput\"},{\"pass\":\"semantic\",\"status\":\"StepError\"}]" blockedJson
    && isInfixOf "Missing declarations: 1" blockedText

||| REQ_FFICOV_RENDER_003: Path renderers expose mutation metadata and empty path arrays
test_REQ_FFICOV_RENDER_003 : () -> Bool
test_REQ_FFICOV_RENDER_003 () =
  case publicVerifyPath of
    Nothing => False
    Just path =>
      pathIsSideEffecting path
        && hasMutationClassification path
        && isInfixOf "\"risk\":\"mutating\"" (renderPathJson path)
        && isInfixOf "\"idempotency\":\"governedci-public-verify-request\"" (renderPathJson path)
        && renderPathArray [] == "[]"
        && isInfixOf "\"pathId\":\"integration/governedci/public-verify/submit-request\"" (renderPathArray [path])

||| REQ_FFICOV_REPORT_007: Empty and undeclared reports handle coverage denominator edge cases
test_REQ_FFICOV_REPORT_007 : () -> Bool
test_REQ_FFICOV_REPORT_007 () =
  let emptyReport = buildReport [] "" []
      undeclaredReport = buildReport ["integration/unknown/path"] "" []
  in coveragePercent emptyReport == 100.0
    && coveragePercent undeclaredReport == 0.0
    && gapCount undeclaredReport == 1

||| REQ_FFICOV_REPORT_008: Read-only paths are mutation-classified without mutation contracts
test_REQ_FFICOV_REPORT_008 : () -> Bool
test_REQ_FFICOV_REPORT_008 () =
  case proofBundlePath of
    Nothing => False
    Just path =>
      not (pathIsSideEffecting path)
        && hasMutationClassification path
        && null (buildReport ["integration/governedci/proof/bundle"] "" [("trace-coverage", StepOK)]).unclassifiedMutation

||| REQ_FFICOV_REGISTRY_001: External registry controls declaration and coverage denominator
test_REQ_FFICOV_REGISTRY_001 : () -> Bool
test_REQ_FFICOV_REGISTRY_001 () =
  let refs = requiredRefsForStepsWithRegistry [4] customRegistry "" ""
      trace = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/custom/ci/proof\",\"trust\":\"proof-grade\",\"outcome\":\"pass\",\"evidenceIds\":[\"custom-receipt\"]}"
      clean = buildReportWithRegistry customRegistry refs trace [("trace-coverage", StepOK)]
      missing = buildReportWithRegistry customRegistry ["integration/governedci/proof/bundle"] trace [("trace-coverage", StepOK)]
  in refs == ["integration/custom/ci/proof"]
    && gapCount clean == 0
    && claimAdmissible clean
    && missing.missingDeclaration == ["integration/governedci/proof/bundle"]

allTests : List TestDef
allTests =
  [ test "REQ_FFICOV_REFS_001" "Integration path refs are extracted from SPEC text" test_REQ_FFICOV_REFS_001
  , test "REQ_FFICOV_REFS_002" "Step4 coverage is scoped by declared Integration paths" test_REQ_FFICOV_REFS_002
  , test "REQ_FFICOV_REPORT_001" "Proof-grade trace closes report gap" test_REQ_FFICOV_REPORT_001
  , test "REQ_FFICOV_REPORT_002" "Unknown path is a missing declaration" test_REQ_FFICOV_REPORT_002
  , test "REQ_FFICOV_REPORT_003" "No input trace blocks admissible claim" test_REQ_FFICOV_REPORT_003
  , test "REQ_FFICOV_REPORT_004" "Unsafe trust trace does not close report gap" test_REQ_FFICOV_REPORT_004
  , test "REQ_FFICOV_REPORT_005" "Missing evidence trace does not close report gap" test_REQ_FFICOV_REPORT_005
  , test "REQ_FFICOV_REPORT_006" "Destructive default op has evidence/idempotency/recovery contract" test_REQ_FFICOV_REPORT_006
  , test "REQ_FFICOV_STATUS_001" "Step status equality and display are total" test_REQ_FFICOV_STATUS_001
  , test "REQ_FFICOV_RENDER_001" "JSON string helpers cover arrays and escaped tokens" test_REQ_FFICOV_RENDER_001
  , test "REQ_FFICOV_RENDER_002" "Report renderers expose path, status, gap, and admissibility fields" test_REQ_FFICOV_RENDER_002
  , test "REQ_FFICOV_RENDER_003" "Path renderers expose mutation metadata and empty arrays" test_REQ_FFICOV_RENDER_003
  , test "REQ_FFICOV_REPORT_007" "Empty and undeclared reports handle denominator edge cases" test_REQ_FFICOV_REPORT_007
  , test "REQ_FFICOV_REPORT_008" "Read-only paths are mutation-classified without mutation contracts" test_REQ_FFICOV_REPORT_008
  , test "REQ_FFICOV_REGISTRY_001" "External registry controls declaration and coverage denominator" test_REQ_FFICOV_REGISTRY_001
  ]

export
runAllTestsPure : (Nat, Nat)
runAllTestsPure =
  let results = map (\t => t.run ()) allTests
      passed = length (filter id results)
      failed = length (filter not results)
  in (passed, failed)

export
runAllTests : IO ()
runAllTests = do
  let (passed, failed) = runAllTestsPure
  putStrLn $ "Tests: " ++ show passed ++ "/" ++ show (passed + failed) ++ " passed"
  if failed == 0
    then putStrLn "All tests PASSED!"
    else putStrLn $ show failed ++ " tests FAILED"
