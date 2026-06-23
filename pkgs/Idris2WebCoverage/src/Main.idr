||| idris2-web-cov — REAL path coverage for web/android/ios MVU apps.
|||
||| Runs the app's test universe in a real JS engine (node) with the forked
||| compiler's canonical path-id instrumentation, then identity-joins the
||| recorded ids against the dumppaths denominator. Emits the standard Step 4
||| contract lines (coverage_percent / claim_admissible / Missing paths), the
||| same surface LazyWeb Step 4 parses — mirroring idris2-evm-cov / dfx coverage.
module Main

import Data.List
import Data.String
import System

import Coverage.Core.PathCoverage
import Coverage.Core.DumppathsJson
import Coverage.Standardization.Step4Contract
import WebCoverage.PathRunner

%default covering

usage : String
usage = unlines
  [ "idris2-web-cov - REAL web/android/ios path coverage (canonical path-id)"
  , ""
  , "USAGE:"
  , "  idris2-web-cov paths <project-dir>"
  , ""
  , "Builds <project-dir>'s test ipkg with the forked compiler (--cg node"
  , "--dumppaths-json --dumppathshits), runs it under the __idris2_recordPathHit"
  , "harness in node, and identity-joins recorded path-ids against the dumppaths"
  , "denominator. Requires IDRIS2_BIN set to the forked compiler."
  , ""
  , "Emits: coverage_percent / claim_admissible / Missing paths."
  ]

runPaths : String -> IO ()
runPaths projectDir = do
  result <- runWebPathCoverage projectDir
  case result of
    Left err => do
      putStrLn $ "Error: " ++ err
      -- honest failure: emit unavailable contract (claim False, not silent 100%)
      putStr (renderStep4ContractText unavailableStep4Contract)
      exitWith (ExitFailure 1)
    Right (cov, _) => do
      let contract = resultToStep4Contract cov
      putStrLn "# Web Path Coverage Report (real JS-engine path-id)"
      putStrLn $ "coverage_model:   " ++ cov.coverageModel
      putStrLn ""
      putStr (renderStep4ContractText contract)
      putStrLn ""
      traverse_ (\p => putStrLn $ "- " ++ p.pathId) cov.missingPaths

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: "paths" :: dir :: _) => runPaths dir
    _ => putStr usage
