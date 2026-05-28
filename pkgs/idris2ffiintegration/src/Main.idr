module Main

import Integration.Model
import Integration.Registry
import Tests.AllTests
import Data.List
import Data.String
import System

%default covering

data OutputFormat = Text | Json

record Options where
  constructor MkOptions
  command : String
  format : OutputFormat
  help : Bool

defaultOptions : Options
defaultOptions = MkOptions "help" Text False

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("dump-registry" :: rest) opts = parseArgs rest ({ command := "dump-registry" } opts)
parseArgs ("self-check" :: rest) opts = parseArgs rest ({ command := "self-check" } opts)
parseArgs ("--format" :: "json" :: rest) opts = parseArgs rest ({ format := Json } opts)
parseArgs ("--json" :: rest) opts = parseArgs rest ({ format := Json } opts)
parseArgs ("--help" :: rest) opts = parseArgs rest ({ help := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ help := True } opts)
parseArgs (_ :: rest) opts = parseArgs rest opts

helpText : String
helpText = """
idris2-ffi-integration - typed Integration FFI operation model

USAGE:
  idris2-ffi-integration dump-registry [--format json]
  idris2-ffi-integration self-check [--format json]

CONTRACT:
  UnsafeDiagnostic and Candidate observations may be emitted, but only
  ProofGrade operations can produce coverage-admissible trace events.
"""

renderOpsJson : List SomeIntegrationOp -> String
renderOpsJson ops = "[" ++ joinComma (map renderSomeOpJson ops) ++ "]"
  where
    joinComma : List String -> String
    joinComma [] = ""
    joinComma [x] = x
    joinComma (x :: xs) = x ++ "," ++ joinComma xs

renderRegistryText : List SomeIntegrationOp -> String
renderRegistryText ops =
  concat (map (\line => line ++ "\n") (map renderOne ops))
  where
    renderOne : SomeIntegrationOp -> String
    renderOne op =
      somePathId op ++ " [" ++ show (someTrust op) ++ ", " ++ show (someRisk op) ++ "]"

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

selfCheckJson : String
selfCheckJson =
  case lookupPath "integration/governedci/proof/bundle" defaultRegistry of
    Just (MkSomeIntegrationOp ProofGrade proofOp) =>
      let proofEv = proofTrace proofOp ["quality-bundle-json", "quality-bundle-junit"] "sha256:ctx"
          failedProofEv = observationTrace ProofGrade proofOp "fail" ["quality-bundle-json", "quality-bundle-junit"] "sha256:ctx"
          missingEvidenceEv = proofTrace proofOp ["quality-bundle-json"] "sha256:ctx"
          diagnosticEv = observationTrace UnsafeDiagnostic diagnosticOp "pass" ["diagnostic-log"] "sha256:diag"
          candidateEv = observationTrace Candidate candidateOp "pass" ["candidate-receipt"] "sha256:candidate"
          ok = coverageAdmissible proofEv
            && not (coverageAdmissible failedProofEv)
            && not (coverageAdmissible missingEvidenceEv)
            && not (coverageAdmissible diagnosticEv)
            && not (coverageAdmissible candidateEv)
          (testPassed, testFailed) = runAllTestsPure
          allOk = ok && testFailed == 0
      in "{"
        ++ "\"status\":" ++ (if allOk then "\"clean\"" else "\"failed\"") ++ ","
        ++ "\"checks\":[\"proof-event-admissible\",\"failed-proof-rejected\",\"missing-evidence-rejected\",\"diagnostic-rejected\",\"candidate-rejected\"],"
        ++ "\"registrySize\":" ++ show (length defaultRegistry) ++ ","
        ++ "\"idrisTests\":{\"passed\":" ++ show testPassed ++ ",\"failed\":" ++ show testFailed ++ "}"
      ++ "}"
    _ => "{\"status\":\"failed\",\"reason\":\"proof-bundle op missing or not proof-grade\"}"

runSelfCheck : OutputFormat -> IO ()
runSelfCheck Json = do
  putStrLn selfCheckJson
  if isInfixOf "\"status\":\"clean\"" selfCheckJson then pure () else exitFailure
runSelfCheck Text = do
  putStrLn selfCheckJson
  if isInfixOf "\"status\":\"clean\"" selfCheckJson then pure () else exitFailure

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs args defaultOptions
  if opts.help
    then putStr helpText
    else case opts.command of
      "dump-registry" =>
        case opts.format of
          Json => putStrLn ("{\"registry\":" ++ renderOpsJson defaultRegistry ++ "}")
          Text => putStr (renderRegistryText defaultRegistry)
      "self-check" => runSelfCheck opts.format
      _ => putStr helpText
