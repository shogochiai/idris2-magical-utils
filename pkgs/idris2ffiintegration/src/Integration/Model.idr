module Integration.Model

import Data.List
import Data.Maybe
import Data.String

%default covering

public export
data TrustLevel
  = UnsafeDiagnostic
  | Candidate
  | ProofGrade

public export
Eq TrustLevel where
  UnsafeDiagnostic == UnsafeDiagnostic = True
  UnsafeDiagnostic == Candidate = False
  UnsafeDiagnostic == ProofGrade = False
  Candidate == UnsafeDiagnostic = False
  Candidate == Candidate = True
  Candidate == ProofGrade = False
  ProofGrade == UnsafeDiagnostic = False
  ProofGrade == Candidate = False
  ProofGrade == ProofGrade = True

public export
Show TrustLevel where
  show UnsafeDiagnostic = "unsafe-diagnostic"
  show Candidate = "candidate"
  show ProofGrade = "proof-grade"

public export
data Boundary
  = LocalProof
  | GovernedSource
  | GovernedCI
  | GovernedReview
  | GovernedPublication
  | InstanceGovernance
  | GlobalRegistry
  | HostService

public export
Show Boundary where
  show LocalProof = "LocalProof"
  show GovernedSource = "GovernedSource"
  show GovernedCI = "GovernedCI"
  show GovernedReview = "GovernedReview"
  show GovernedPublication = "GovernedPublication"
  show InstanceGovernance = "InstanceGovernance"
  show GlobalRegistry = "GlobalRegistry"
  show HostService = "HostService"

public export
data Risk
  = LocalOnly
  | ReadOnly
  | Mutating
  | Destructive

public export
Eq Risk where
  LocalOnly == LocalOnly = True
  LocalOnly == ReadOnly = False
  LocalOnly == Mutating = False
  LocalOnly == Destructive = False
  ReadOnly == LocalOnly = False
  ReadOnly == ReadOnly = True
  ReadOnly == Mutating = False
  ReadOnly == Destructive = False
  Mutating == LocalOnly = False
  Mutating == ReadOnly = False
  Mutating == Mutating = True
  Mutating == Destructive = False
  Destructive == LocalOnly = False
  Destructive == ReadOnly = False
  Destructive == Mutating = False
  Destructive == Destructive = True

public export
Show Risk where
  show LocalOnly = "local-only"
  show ReadOnly = "read-only"
  show Mutating = "mutating"
  show Destructive = "destructive"

public export
record ContextPolicy where
  constructor MkContextPolicy
  requirePrNumber : Bool
  requireNetwork : Bool
  requireCanister : Bool
  requireWorker : Bool

public export
record EvidenceKind where
  constructor MkEvidenceKind
  evidenceId : String
  description : String

public export
record IntegrationOp (trust : TrustLevel) where
  constructor MkIntegrationOp
  pathId : String
  boundary : Boundary
  risk : Risk
  adapter : String
  evidence : List EvidenceKind
  idempotency : Maybe String
  contextPolicy : ContextPolicy
  recoveryPolicy : Maybe String
  negativeTest : Maybe String

public export
data SomeIntegrationOp : Type where
  MkSomeIntegrationOp : (trust : TrustLevel) -> IntegrationOp trust -> SomeIntegrationOp

public export
Eq SomeIntegrationOp where
  (MkSomeIntegrationOp _ a) == (MkSomeIntegrationOp _ b) = a.pathId == b.pathId

public export
record TraceEvent (trust : TrustLevel) where
  constructor MkTraceEvent
  eventTrust : TrustLevel
  op : IntegrationOp trust
  outcome : String
  evidenceIds : List String
  contextDigest : String

public export
defaultContextPolicy : ContextPolicy
defaultContextPolicy = MkContextPolicy False False False False

public export
icCanisterContextPolicy : ContextPolicy
icCanisterContextPolicy = MkContextPolicy True True True False

public export
workerContextPolicy : ContextPolicy
workerContextPolicy = MkContextPolicy True True True True

public export
evidenceKind : String -> String -> EvidenceKind
evidenceKind = MkEvidenceKind

public export
mkProofOp :
  String -> Boundary -> Risk -> String -> List EvidenceKind -> Maybe String ->
  ContextPolicy -> Maybe String -> Maybe String -> IntegrationOp ProofGrade
mkProofOp = MkIntegrationOp

public export
mkCandidateOp :
  String -> Boundary -> Risk -> String -> List EvidenceKind -> Maybe String ->
  ContextPolicy -> Maybe String -> Maybe String -> IntegrationOp Candidate
mkCandidateOp = MkIntegrationOp

public export
mkDiagnosticOp :
  String -> Boundary -> Risk -> String -> List EvidenceKind -> Maybe String ->
  ContextPolicy -> Maybe String -> Maybe String -> IntegrationOp UnsafeDiagnostic
mkDiagnosticOp = MkIntegrationOp

public export
observationTrace : (trust : TrustLevel) -> IntegrationOp trust -> String -> List String -> String -> TraceEvent trust
observationTrace trust op outcome evidenceIds contextDigest =
  MkTraceEvent trust op outcome evidenceIds contextDigest

public export
proofTrace : IntegrationOp ProofGrade -> List String -> String -> TraceEvent ProofGrade
proofTrace op evidenceIds contextDigest = MkTraceEvent ProofGrade op "pass" evidenceIds contextDigest

public export
someTrust : SomeIntegrationOp -> TrustLevel
someTrust (MkSomeIntegrationOp trust _) = trust

public export
somePathId : SomeIntegrationOp -> String
somePathId (MkSomeIntegrationOp _ op) = op.pathId

public export
someBoundary : SomeIntegrationOp -> Boundary
someBoundary (MkSomeIntegrationOp _ op) = op.boundary

public export
someRisk : SomeIntegrationOp -> Risk
someRisk (MkSomeIntegrationOp _ op) = op.risk

public export
someAdapter : SomeIntegrationOp -> String
someAdapter (MkSomeIntegrationOp _ op) = op.adapter

public export
someEvidenceIds : SomeIntegrationOp -> List String
someEvidenceIds (MkSomeIntegrationOp _ op) = map evidenceId op.evidence

public export
someIdempotency : SomeIntegrationOp -> Maybe String
someIdempotency (MkSomeIntegrationOp _ op) = op.idempotency

public export
someRecoveryPolicy : SomeIntegrationOp -> Maybe String
someRecoveryPolicy (MkSomeIntegrationOp _ op) = op.recoveryPolicy

public export
someNegativeTest : SomeIntegrationOp -> Maybe String
someNegativeTest (MkSomeIntegrationOp _ op) = op.negativeTest

public export
isSideEffecting : Risk -> Bool
isSideEffecting Mutating = True
isSideEffecting Destructive = True
isSideEffecting _ = False

public export
opHasMutationContract : IntegrationOp trust -> Bool
opHasMutationContract op =
  if isSideEffecting op.risk
    then not (null op.evidence) && isJust op.idempotency && isJust op.recoveryPolicy
    else True

public export
riskEqObserved : Risk -> Risk -> Bool
riskEqObserved left right = left == right

public export
trustEqObserved : TrustLevel -> TrustLevel -> Bool
trustEqObserved left right = left == right

public export
someOpEqObserved : SomeIntegrationOp -> SomeIntegrationOp -> Bool
someOpEqObserved left right = left == right

public export
traceCoversEvidence : TraceEvent trust -> Bool
traceCoversEvidence ev =
  all (\eid => elem eid ev.evidenceIds) (map evidenceId ev.op.evidence)

public export
coverageAdmissible : TraceEvent trust -> Bool
coverageAdmissible ev =
  ev.eventTrust == ProofGrade
    && ev.outcome == "pass"
    && traceCoversEvidence ev
    && opHasMutationContract ev.op

joinComma : List String -> String
joinComma [] = ""
joinComma [x] = x
joinComma (x :: xs) = x ++ "," ++ joinComma xs

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
renderBool : Bool -> String
renderBool True = "true"
renderBool False = "false"

renderMaybeString : Maybe String -> String
renderMaybeString Nothing = "null"
renderMaybeString (Just value) = quoteJson value

public export
renderContextPolicyJson : ContextPolicy -> String
renderContextPolicyJson p =
  "{"
    ++ "\"requirePrNumber\":" ++ renderBool p.requirePrNumber ++ ","
    ++ "\"requireNetwork\":" ++ renderBool p.requireNetwork ++ ","
    ++ "\"requireCanister\":" ++ renderBool p.requireCanister ++ ","
    ++ "\"requireWorker\":" ++ renderBool p.requireWorker
  ++ "}"

public export
renderSomeOpJson : SomeIntegrationOp -> String
renderSomeOpJson some =
  "{"
    ++ "\"pathId\":" ++ quoteJson (somePathId some) ++ ","
    ++ "\"trust\":" ++ quoteJson (show (someTrust some)) ++ ","
    ++ "\"boundary\":" ++ quoteJson (show (someBoundary some)) ++ ","
    ++ "\"risk\":" ++ quoteJson (show (someRisk some)) ++ ","
    ++ "\"adapter\":" ++ quoteJson (someAdapter some) ++ ","
    ++ "\"evidenceIds\":" ++ renderStringArray (someEvidenceIds some) ++ ","
    ++ "\"idempotency\":" ++ renderMaybeString (someIdempotency some) ++ ","
    ++ "\"recoveryPolicy\":" ++ renderMaybeString (someRecoveryPolicy some) ++ ","
    ++ "\"negativeTest\":" ++ renderMaybeString (someNegativeTest some)
  ++ "}"

public export
renderTraceEventJson : TraceEvent trust -> String
renderTraceEventJson ev =
  "{"
    ++ "\"schema\":\"etherclaw.integration.trace.v1\","
    ++ "\"opId\":" ++ quoteJson ev.op.pathId ++ ","
    ++ "\"trust\":" ++ quoteJson (show ev.eventTrust) ++ ","
    ++ "\"outcome\":" ++ quoteJson ev.outcome ++ ","
    ++ "\"evidenceIds\":" ++ renderStringArray ev.evidenceIds ++ ","
    ++ "\"contextDigest\":" ++ quoteJson ev.contextDigest ++ ","
    ++ "\"coverageAdmissible\":" ++ renderBool (coverageAdmissible ev)
  ++ "}"
