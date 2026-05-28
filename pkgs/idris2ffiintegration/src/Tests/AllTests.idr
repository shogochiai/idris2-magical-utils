module Tests.AllTests

import Integration.Model
import Integration.Registry
import Integration.Trace
import Data.List
import Data.Maybe

%default covering

public export
record TestDef where
  constructor MkTestDef
  testId : String
  description : String
  run : () -> Bool

test : String -> String -> (() -> Bool) -> TestDef
test tid desc fn = MkTestDef tid desc fn

diagnosticOp : IntegrationOp UnsafeDiagnostic
diagnosticOp =
  mkDiagnosticOp
    "integration/example/diagnostic"
    HostService
    ReadOnly
    "shell:diagnostic-only"
    [evidenceKind "diagnostic-log" "non-proof diagnostic log"]
    Nothing
    defaultContextPolicy
    Nothing
    Nothing

candidateOp : IntegrationOp Candidate
candidateOp =
  mkCandidateOp
    "integration/example/candidate"
    GovernedCI
    Mutating
    "scenario:candidate"
    [evidenceKind "candidate-receipt" "candidate receipt"]
    (Just "candidate-idempotency")
    icCanisterContextPolicy
    (Just "candidate recovery")
    (Just "candidate negative test")

riskEqCases : List (Risk, Risk, Bool)
riskEqCases =
  [ (LocalOnly, LocalOnly, True)
  , (LocalOnly, ReadOnly, False)
  , (LocalOnly, Mutating, False)
  , (LocalOnly, Destructive, False)
  , (ReadOnly, LocalOnly, False)
  , (ReadOnly, ReadOnly, True)
  , (ReadOnly, Mutating, False)
  , (ReadOnly, Destructive, False)
  , (Mutating, LocalOnly, False)
  , (Mutating, ReadOnly, False)
  , (Mutating, Mutating, True)
  , (Mutating, Destructive, False)
  , (Destructive, LocalOnly, False)
  , (Destructive, ReadOnly, False)
  , (Destructive, Mutating, False)
  , (Destructive, Destructive, True)
  ]

trustEqCases : List (TrustLevel, TrustLevel, Bool)
trustEqCases =
  [ (UnsafeDiagnostic, UnsafeDiagnostic, True)
  , (UnsafeDiagnostic, Candidate, False)
  , (UnsafeDiagnostic, ProofGrade, False)
  , (Candidate, UnsafeDiagnostic, False)
  , (Candidate, Candidate, True)
  , (Candidate, ProofGrade, False)
  , (ProofGrade, UnsafeDiagnostic, False)
  , (ProofGrade, Candidate, False)
  , (ProofGrade, ProofGrade, True)
  ]

checkRiskEq : (Risk, Risk, Bool) -> Bool
checkRiskEq (left, right, expected) = riskEqObserved left right == expected

checkTrustEq : (TrustLevel, TrustLevel, Bool) -> Bool
checkTrustEq (left, right, expected) = trustEqObserved left right == expected

proofBundleOp : Maybe (IntegrationOp ProofGrade)
proofBundleOp =
  case lookupPath "integration/governedci/proof/bundle" defaultRegistry of
    Just (MkSomeIntegrationOp ProofGrade op) => Just op
    _ => Nothing

||| REQ_FFIINT_TRUST_001: ProofGrade pass with complete evidence is admissible
test_REQ_FFIINT_TRUST_001 : () -> Bool
test_REQ_FFIINT_TRUST_001 () =
  case proofBundleOp of
    Nothing => False
    Just op =>
      coverageAdmissible (proofTrace op ["quality-bundle-json", "quality-bundle-junit"] "sha256:ctx")

||| REQ_FFIINT_TRUST_002: Failed ProofGrade event is rejected
test_REQ_FFIINT_TRUST_002 : () -> Bool
test_REQ_FFIINT_TRUST_002 () =
  case proofBundleOp of
    Nothing => False
    Just op =>
      not (coverageAdmissible (observationTrace ProofGrade op "fail" ["quality-bundle-json", "quality-bundle-junit"] "sha256:ctx"))

||| REQ_FFIINT_TRUST_003: Missing evidence is rejected
test_REQ_FFIINT_TRUST_003 : () -> Bool
test_REQ_FFIINT_TRUST_003 () =
  case proofBundleOp of
    Nothing => False
    Just op =>
      not (coverageAdmissible (proofTrace op ["quality-bundle-json"] "sha256:ctx"))

||| REQ_FFIINT_TRUST_004: UnsafeDiagnostic is not coverage proof
test_REQ_FFIINT_TRUST_004 : () -> Bool
test_REQ_FFIINT_TRUST_004 () =
  not (coverageAdmissible (observationTrace UnsafeDiagnostic diagnosticOp "pass" ["diagnostic-log"] "sha256:diag"))

||| REQ_FFIINT_TRUST_005: Candidate is not coverage proof
test_REQ_FFIINT_TRUST_005 : () -> Bool
test_REQ_FFIINT_TRUST_005 () =
  not (coverageAdmissible (observationTrace Candidate candidateOp "pass" ["candidate-receipt"] "sha256:candidate"))

||| REQ_FFIINT_REGISTRY_001: Default registry size is stable
test_REQ_FFIINT_REGISTRY_001 : () -> Bool
test_REQ_FFIINT_REGISTRY_001 () =
  length defaultRegistry == 10

||| REQ_FFIINT_REGISTRY_002: Default registry is proof-grade
test_REQ_FFIINT_REGISTRY_002 : () -> Bool
test_REQ_FFIINT_REGISTRY_002 () =
  all (\op => someTrust op == ProofGrade) defaultRegistry

||| REQ_FFIINT_REGISTRY_003: Destructive operation has mutation contract
test_REQ_FFIINT_REGISTRY_003 : () -> Bool
test_REQ_FFIINT_REGISTRY_003 () =
  case lookupPath "integration/global-registry/upgrade/apply" defaultRegistry of
    Just op =>
      someRisk op == Destructive
        && someIdempotency op == Just "global-registry-upgrade"
        && isJust (someRecoveryPolicy op)
    Nothing => False

||| REQ_FFIINT_TRACE_001: Trace parser extracts typed fields
test_REQ_FFIINT_TRACE_001 : () -> Bool
test_REQ_FFIINT_TRACE_001 () =
  let line = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"trust\":\"proof-grade\",\"outcome\":\"pass\",\"evidenceIds\":[\"quality-bundle-json\",\"quality-bundle-junit\"]}" in
  case parseTraceEventLine line of
    Just ev =>
      ev.schema == "etherclaw.integration.trace.v1"
        && ev.opId == "integration/governedci/proof/bundle"
        && ev.trust == Just ProofGrade
        && ev.evidenceIds == ["quality-bundle-json", "quality-bundle-junit"]
    Nothing => False

||| REQ_FFIINT_TRACE_002: Unsafe trust trace does not cover proof operation
test_REQ_FFIINT_TRACE_002 : () -> Bool
test_REQ_FFIINT_TRACE_002 () =
  case lookupPath "integration/governedci/proof/bundle" defaultRegistry of
    Just op =>
      let line = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"trust\":\"unsafe-diagnostic\",\"outcome\":\"pass\",\"evidenceIds\":[\"quality-bundle-json\",\"quality-bundle-junit\"]}" in
      not (traceLineCovers op line)
    Nothing => False

||| REQ_FFIINT_TRACE_003: Missing evidence trace does not cover proof operation
test_REQ_FFIINT_TRACE_003 : () -> Bool
test_REQ_FFIINT_TRACE_003 () =
  case lookupPath "integration/governedci/proof/bundle" defaultRegistry of
    Just op =>
      let line = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/governedci/proof/bundle\",\"outcome\":\"pass\",\"evidenceIds\":[]}" in
      not (traceLineCovers op line)
    Nothing => False

||| REQ_FFIINT_MODEL_001: Risk and boundary enumerations have stable semantics
test_REQ_FFIINT_MODEL_001 : () -> Bool
test_REQ_FFIINT_MODEL_001 () =
  not (isSideEffecting LocalOnly)
    && not (isSideEffecting ReadOnly)
    && isSideEffecting Mutating
    && isSideEffecting Destructive
    && all checkRiskEq riskEqCases
    && all checkTrustEq trustEqCases
    && LocalOnly == LocalOnly
    && ReadOnly == ReadOnly
    && Mutating == Mutating
    && Destructive == Destructive
    && not (LocalOnly == ReadOnly)
    && not (ReadOnly == Mutating)
    && not (Mutating == Destructive)
    && not (Destructive == LocalOnly)
    && UnsafeDiagnostic == UnsafeDiagnostic
    && Candidate == Candidate
    && ProofGrade == ProofGrade
    && not (UnsafeDiagnostic == Candidate)
    && not (Candidate == ProofGrade)
    && not (ProofGrade == UnsafeDiagnostic)
    && show LocalProof == "LocalProof"
    && show GovernedSource == "GovernedSource"
    && show GovernedCI == "GovernedCI"
    && show GovernedReview == "GovernedReview"
    && show GovernedPublication == "GovernedPublication"
    && show InstanceGovernance == "InstanceGovernance"
    && show GlobalRegistry == "GlobalRegistry"
    && show HostService == "HostService"

||| REQ_FFIINT_RENDER_001: JSON render helpers cover empty, singleton, multiple, and escaped values
test_REQ_FFIINT_RENDER_001 : () -> Bool
test_REQ_FFIINT_RENDER_001 () =
  renderStringArray [] == "[]"
    && renderStringArray ["alpha"] == "[\"alpha\"]"
    && renderStringArray ["alpha", "beta"] == "[\"alpha\",\"beta\"]"
    && renderStringArray ["quote\"slash\\line\nend"] == "[\"quote\\\"slash\\\\line\\nend\"]"
    && renderBool True == "true"
    && renderBool False == "false"
    && renderContextPolicyJson workerContextPolicy
      == "{\"requirePrNumber\":true,\"requireNetwork\":true,\"requireCanister\":true,\"requireWorker\":true}"

||| REQ_FFIINT_RENDER_002: Operation and trace JSON rendering preserves public metadata
test_REQ_FFIINT_RENDER_002 : () -> Bool
test_REQ_FFIINT_RENDER_002 () =
  let candidate = MkSomeIntegrationOp Candidate candidateOp
      diagnostic = MkSomeIntegrationOp UnsafeDiagnostic diagnosticOp
      candidateJson = renderSomeOpJson candidate
      diagnosticJson = renderSomeOpJson diagnostic
      traceJson = renderTraceEventJson (observationTrace Candidate candidateOp "pass" ["candidate-receipt"] "sha256:candidate")
  in someAdapter candidate == "scenario:candidate"
    && someOpEqObserved candidate candidate
    && not (someOpEqObserved candidate diagnostic)
    && show (someBoundary candidate) == "GovernedCI"
    && someNegativeTest candidate == Just "candidate negative test"
    && someNegativeTest diagnostic == Nothing
    && candidateJson == "{\"pathId\":\"integration/example/candidate\",\"trust\":\"candidate\",\"boundary\":\"GovernedCI\",\"risk\":\"mutating\",\"adapter\":\"scenario:candidate\",\"evidenceIds\":[\"candidate-receipt\"],\"idempotency\":\"candidate-idempotency\",\"recoveryPolicy\":\"candidate recovery\",\"negativeTest\":\"candidate negative test\"}"
    && diagnosticJson == "{\"pathId\":\"integration/example/diagnostic\",\"trust\":\"unsafe-diagnostic\",\"boundary\":\"HostService\",\"risk\":\"read-only\",\"adapter\":\"shell:diagnostic-only\",\"evidenceIds\":[\"diagnostic-log\"],\"idempotency\":null,\"recoveryPolicy\":null,\"negativeTest\":null}"
    && traceJson == "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/example/candidate\",\"trust\":\"candidate\",\"outcome\":\"pass\",\"evidenceIds\":[\"candidate-receipt\"],\"contextDigest\":\"sha256:candidate\",\"coverageAdmissible\":false}"

||| REQ_FFIINT_REGISTRY_004: JSONL registry parser preserves operation contracts
test_REQ_FFIINT_REGISTRY_004 : () -> Bool
test_REQ_FFIINT_REGISTRY_004 () =
  let line = "{\"pathId\":\"integration/example/registry\",\"trust\":\"proof-grade\",\"boundary\":\"GovernedCI\",\"risk\":\"mutating\",\"adapter\":\"scenario:registry\",\"evidenceIds\":[\"registry-receipt\"],\"idempotency\":\"registry-key\",\"recoveryPolicy\":\"registry recovery\",\"negativeTest\":\"registry negative\"}" in
  case parseRegistryLine line of
    Just op =>
      somePathId op == "integration/example/registry"
        && someTrust op == ProofGrade
        && show (someBoundary op) == "GovernedCI"
        && someRisk op == Mutating
        && someEvidenceIds op == ["registry-receipt"]
        && someIdempotency op == Just "registry-key"
        && someRecoveryPolicy op == Just "registry recovery"
        && someNegativeTest op == Just "registry negative"
    Nothing => False

||| REQ_FFIINT_MODEL_002: Mutation contracts are required only for side-effecting operations
test_REQ_FFIINT_MODEL_002 : () -> Bool
test_REQ_FFIINT_MODEL_002 () =
  let unsafeMutating =
        mkCandidateOp
          "integration/example/unsafe-mutating"
          GovernedCI
          Mutating
          "scenario:unsafe"
          [evidenceKind "unsafe" "unsafe evidence"]
          Nothing
          icCanisterContextPolicy
          Nothing
          Nothing
      safeMutating =
        mkCandidateOp
          "integration/example/safe-mutating"
          GovernedCI
          Mutating
          "scenario:safe"
          [evidenceKind "safe" "safe evidence"]
          (Just "safe-idempotency")
          icCanisterContextPolicy
          (Just "safe recovery")
          Nothing
  in opHasMutationContract diagnosticOp
    && opHasMutationContract candidateOp
    && opHasMutationContract safeMutating
    && not (opHasMutationContract unsafeMutating)

||| REQ_FFIINT_TRACE_004: Trace parser covers candidate and malformed trust edge cases
test_REQ_FFIINT_TRACE_004 : () -> Bool
test_REQ_FFIINT_TRACE_004 () =
  let candidateLine = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/example/candidate\",\"trust\":\"candidate\",\"outcome\":\"pass\",\"evidenceIds\":[\"candidate-receipt\"]}"
      unknownTrustLine = "{\"schema\":\"etherclaw.integration.trace.v1\",\"opId\":\"integration/example/candidate\",\"trust\":\"future-trust\",\"outcome\":\"pass\",\"evidenceIds\":[\"candidate-receipt\"]}"
      malformedLine = "{\"schema\":\"etherclaw.integration.trace.v1\"}"
  in case (parseTraceEventLine candidateLine, parseTraceEventLine unknownTrustLine, parseTraceEventLine malformedLine) of
    (Just candidateEv, Just unknownEv, Nothing) =>
      candidateEv.trust == Just Candidate
        && unknownEv.trust == Nothing
        && traceLineCovers (MkSomeIntegrationOp Candidate candidateOp) unknownTrustLine == False
    _ => False

||| REQ_FFIINT_TRACE_005: Low-level trace field parsers handle missing and escaped JSON tokens
test_REQ_FFIINT_TRACE_005 : () -> Bool
test_REQ_FFIINT_TRACE_005 () =
  parseJsonStringField "schema" "" == Nothing
    && parseJsonStringField "schema" "{\"schema\":\"etherclaw.integration.trace.v1\"}"
      == Just "etherclaw.integration.trace.v1"
    && parseJsonStringArrayField "evidenceIds" "{}" == Nothing
    && parseJsonStringArrayField "evidenceIds" "{\"evidenceIds\":[]}" == Just []
    && parseJsonStringArrayField "evidenceIds" "{\"evidenceIds\":[\"a\\\\b\",\"c\\\"d\"]}"
      == Just ["a\\b", "c\"d"]
    && traceLineCovers (MkSomeIntegrationOp Candidate candidateOp) "{}" == False

||| REQ_FFIINT_TRACE_006: Trace parser primitives cover terminal parser states
test_REQ_FFIINT_TRACE_006 : () -> Bool
test_REQ_FFIINT_TRACE_006 () =
  prefixChars (unpack "ab") (unpack "ac") == False
    && prefixChars (unpack "a") [] == False
    && dropChars 1 [] == []
    && takeJsonStringChars [] == []
    && takeJsonStringChars (unpack "\\") == unpack "\\"
    && takeUntilChar ']' [] == []
    && findAfterChars (unpack "z") (unpack "abc") == Nothing
    && stripJsonStringToken "plain" == Nothing
    && parseTrustLevel "unsafe-diagnostic" == Just UnsafeDiagnostic
    && parseTrustLevel "candidate" == Just Candidate
    && parseTrustLevel "proof-grade" == Just ProofGrade
    && parseTrustLevel "future-trust" == Nothing

export
allTests : List TestDef
allTests =
  [ test "REQ_FFIINT_TRUST_001" "ProofGrade pass with full evidence is admissible" test_REQ_FFIINT_TRUST_001
  , test "REQ_FFIINT_TRUST_002" "Failed ProofGrade event is rejected" test_REQ_FFIINT_TRUST_002
  , test "REQ_FFIINT_TRUST_003" "Missing evidence is rejected" test_REQ_FFIINT_TRUST_003
  , test "REQ_FFIINT_TRUST_004" "UnsafeDiagnostic is not coverage-admissible" test_REQ_FFIINT_TRUST_004
  , test "REQ_FFIINT_TRUST_005" "Candidate is not coverage-admissible" test_REQ_FFIINT_TRUST_005
  , test "REQ_FFIINT_REGISTRY_001" "Default registry has expected size" test_REQ_FFIINT_REGISTRY_001
  , test "REQ_FFIINT_REGISTRY_002" "Default registry is proof-grade only" test_REQ_FFIINT_REGISTRY_002
  , test "REQ_FFIINT_REGISTRY_003" "Destructive upgrade op has mutation contract" test_REQ_FFIINT_REGISTRY_003
  , test "REQ_FFIINT_REGISTRY_004" "JSONL registry parser preserves operation contracts" test_REQ_FFIINT_REGISTRY_004
  , test "REQ_FFIINT_TRACE_001" "Trace parser extracts typed fields" test_REQ_FFIINT_TRACE_001
  , test "REQ_FFIINT_TRACE_002" "Unsafe trust trace does not cover proof op" test_REQ_FFIINT_TRACE_002
  , test "REQ_FFIINT_TRACE_003" "Missing evidence trace does not cover proof op" test_REQ_FFIINT_TRACE_003
  , test "REQ_FFIINT_MODEL_001" "Risk and boundary enumerations have stable semantics" test_REQ_FFIINT_MODEL_001
  , test "REQ_FFIINT_RENDER_001" "JSON render helpers cover arrays and escapes" test_REQ_FFIINT_RENDER_001
  , test "REQ_FFIINT_RENDER_002" "Operation and trace JSON rendering preserves metadata" test_REQ_FFIINT_RENDER_002
  , test "REQ_FFIINT_MODEL_002" "Mutation contracts are required only for side-effecting operations" test_REQ_FFIINT_MODEL_002
  , test "REQ_FFIINT_TRACE_004" "Trace parser covers candidate and malformed trust edge cases" test_REQ_FFIINT_TRACE_004
  , test "REQ_FFIINT_TRACE_005" "Trace field parsers handle missing and escaped tokens" test_REQ_FFIINT_TRACE_005
  , test "REQ_FFIINT_TRACE_006" "Trace parser primitives cover terminal parser states" test_REQ_FFIINT_TRACE_006
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
