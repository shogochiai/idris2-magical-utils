module Integration.Registry

import Integration.Model
import Integration.Trace
import Data.List
import Data.Maybe
import Data.String

%default covering

e : String -> String -> EvidenceKind
e = evidenceKind

mkSomeProof : IntegrationOp ProofGrade -> SomeIntegrationOp
mkSomeProof op = MkSomeIntegrationOp ProofGrade op

public export
defaultRegistry : List SomeIntegrationOp
defaultRegistry =
  [ mkSomeProof $ mkProofOp
      "integration/governedci/public-verify/submit-request"
      GovernedCI
      Mutating
      "scenario:governedci.publicVerify.submitRequest"
      [e "execution-request" "canister-authorized execution request"]
      (Just "governedci-public-verify-request")
      icCanisterContextPolicy
      (Just "query request status and avoid duplicate request ids")
      (Just "reject missing authority or malformed public verify request")
  , mkSomeProof $ mkProofOp
      "integration/governedci/public-verify/query-receipt"
      GovernedCI
      ReadOnly
      "scenario:governedci.publicVerify.queryReceipt"
      [e "execution-receipt" "canister execution receipt"]
      Nothing
      icCanisterContextPolicy
      Nothing
      (Just "missing receipt returns a typed not-found result")
  , mkSomeProof $ mkProofOp
      "integration/governedci/worker/health"
      HostService
      ReadOnly
      "scenario:governedci.worker.health"
      [e "worker-selftest-receipt" "Flux worker self-test receipt"]
      Nothing
      workerContextPolicy
      Nothing
      (Just "unreachable worker is reported as health failure")
  , mkSomeProof $ mkProofOp
      "integration/governedci/step3/gate"
      GovernedCI
      ReadOnly
      "scenario:governedci.step3.gate"
      [e "step3-receipt" "verify-only Step3 receipt"]
      Nothing
      icCanisterContextPolicy
      Nothing
      (Just "missing Step3 receipt fails closed")
  , mkSomeProof $ mkProofOp
      "integration/governedci/proof/bundle"
      LocalProof
      LocalOnly
      "scenario:governedci.proof.bundle"
      [ e "quality-bundle-json" "machine-readable quality proof bundle"
      , e "quality-bundle-junit" "JUnit projection of the quality proof bundle"
      ]
      Nothing
      defaultContextPolicy
      Nothing
      (Just "malformed bundle is rejected")
  , mkSomeProof $ mkProofOp
      "integration/governedsource/source-capsule/materialize"
      GovernedSource
      Mutating
      "scenario:governedsource.sourceCapsule.materialize"
      [e "materialization-receipt" "source capsule materialization receipt"]
      (Just "source-capsule-materialize")
      icCanisterContextPolicy
      (Just "materialization is keyed by source capsule digest")
      (Just "unauthorized materialization is rejected")
  , mkSomeProof $ mkProofOp
      "integration/governedreview/private-review/create-request"
      GovernedReview
      Mutating
      "scenario:governedreview.privateReview.createRequest"
      [e "review-request" "private review request receipt"]
      (Just "private-review-request")
      icCanisterContextPolicy
      (Just "review request ids are deterministic per governed source change")
      (Just "private content is never projected into public CI trace")
  , mkSomeProof $ mkProofOp
      "integration/governedpublication/github/project-check"
      GovernedPublication
      Mutating
      "scenario:governedpublication.github.projectCheck"
      [e "projection-receipt" "governed publication projection receipt"]
      (Just "github-check-projection")
      defaultContextPolicy
      (Just "projection is keyed by source authority receipt")
      (Just "projection refuses missing governed publication receipt")
  , mkSomeProof $ mkProofOp
      "integration/global-registry/upgrade/preflight"
      GlobalRegistry
      ReadOnly
      "scenario:globalRegistry.upgrade.preflight"
      [e "preflight-report" "read-only canister upgrade preflight report"]
      Nothing
      icCanisterContextPolicy
      Nothing
      (Just "unreachable canister fails preflight")
  , mkSomeProof $ mkProofOp
      "integration/global-registry/upgrade/apply"
      InstanceGovernance
      Destructive
      "scenario:globalRegistry.upgrade.apply"
      [ e "upgrade-receipt" "canister upgrade command receipt"
      , e "post-upgrade-preflight" "post-upgrade authority preflight report"
      ]
      (Just "global-registry-upgrade")
      icCanisterContextPolicy
      (Just "backup evidence and post-upgrade checks are required")
      (Just "apply refuses missing governance approval or missing backup path")
  ]

public export
lookupPath : String -> List SomeIntegrationOp -> Maybe SomeIntegrationOp
lookupPath path registry = find (\op => somePathId op == path) registry

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
  Maybe String -> ContextPolicy -> Maybe String -> Maybe String -> SomeIntegrationOp
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
parseRegistryLine : String -> Maybe SomeIntegrationOp
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
registryFromJsonLines : String -> List SomeIntegrationOp
registryFromJsonLines text = mapMaybe parseRegistryLine (lines text)
