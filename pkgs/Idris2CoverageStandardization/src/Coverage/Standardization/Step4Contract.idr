module Coverage.Standardization.Step4Contract

%default total

public export
record Step4Contract where
  constructor MkStep4Contract
  coveragePercent : Double
  claimAdmissible : Bool
  missingPaths : Nat

boolText : Bool -> String
boolText True = "True"
boolText False = "False"

public export
renderStep4ContractLines : Step4Contract -> List String
renderStep4ContractLines contract =
  [ "coverage_percent: " ++ show contract.coveragePercent
  , "claim_admissible: " ++ boolText contract.claimAdmissible
  , "Missing paths: " ++ show contract.missingPaths
  ]

public export
renderStep4ContractText : Step4Contract -> String
renderStep4ContractText contract =
  "coverage_percent: " ++ show contract.coveragePercent ++ "\n"
  ++ "claim_admissible: " ++ boolText contract.claimAdmissible ++ "\n"
  ++ "Missing paths: " ++ show contract.missingPaths ++ "\n"

public export
unavailableStep4Contract : Step4Contract
unavailableStep4Contract = MkStep4Contract 0.0 False 1
