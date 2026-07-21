module Coverage.Standardization.Step4Contract

%default total

||| The textual step4 contract a family producer emits. v2: RAW COUNTS ONLY —
||| the record deliberately has NO percent field. A producer that could fill in
||| its own percent could also lie with it (live incidents: a smoke script
||| echoed `coverage_percent: 100.0` with no path denominator; evm reported an
||| admissible-subset 50% for an honest 1.2%; `fromMaybe 100.0` renderers turned
||| unmeasured into perfect). The consumer computes any percentage in one place
||| as hit / (denominator + unknown); this record only carries the evidence.
||| Conservation the consumer re-checks: total = denominator+excluded+unknown,
||| hit <= denominator.
public export
record Step4Contract where
  constructor MkStep4Contract
  pathsTotal       : Nat
  pathsDenominator : Nat
  pathsHit         : Nat
  pathsExcluded    : Nat
  pathsUnknown     : Nat
  claimAdmissible  : Bool
  missingPaths     : Nat

boolText : Bool -> String
boolText True = "True"
boolText False = "False"

public export
renderStep4ContractLines : Step4Contract -> List String
renderStep4ContractLines contract =
  [ "paths_total: " ++ show contract.pathsTotal
  , "paths_denominator: " ++ show contract.pathsDenominator
  , "paths_hit: " ++ show contract.pathsHit
  , "paths_excluded: " ++ show contract.pathsExcluded
  , "paths_unknown: " ++ show contract.pathsUnknown
  , "claim_admissible: " ++ boolText contract.claimAdmissible
  , "Missing paths: " ++ show contract.missingPaths
  ]

public export
renderStep4ContractText : Step4Contract -> String
renderStep4ContractText contract =
  concat (map (++ "\n") (renderStep4ContractLines contract))

||| Honest "could not measure": zero evidence, inadmissible claim, one missing
||| obligation so a gap-counting consumer still fails the gate (unmeasured is
||| NEVER a pass, and with no counts it can never divide to 100%).
public export
unavailableStep4Contract : Step4Contract
unavailableStep4Contract = MkStep4Contract 0 0 0 0 0 False 1
