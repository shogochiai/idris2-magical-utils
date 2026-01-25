||| Self-analysis: Run idris2-coverage on itself
module SelfAnalyze

import Coverage.Types
import Coverage.Collector
import Coverage.Report
import Coverage.TestHint

import Data.List
import Data.String
import System.File

%default covering

main : IO ()
main = do
  putStrLn "=== Self-Coverage Analysis ==="
  putStrLn ""

  -- Read profile HTML
  Right html <- readFile "test-runner.ss.html"
    | Left err => putStrLn $ "Error reading HTML: " ++ show err

  -- Read Scheme source for function mapping
  Right ss <- readFile "build/exec/test-runner_app/test-runner.ss"
    | Left err => putStrLn $ "Error reading SS: " ++ show err

  -- Parse function definitions
  let funcDefs = parseSchemeDefs ss
  let prodFuncs = excludeTestModules funcDefs
  putStrLn $ "Found " ++ show (length funcDefs) ++ " function definitions (" ++ show (length prodFuncs) ++ " production)"

  -- Parse branch coverage
  let branchPoints = parseBranchCoverage html
  putStrLn $ "Found " ++ show (length branchPoints) ++ " branch points"

  -- Summarize with function mapping (excluding test modules per REQ_COV_AGG_005)
  let summary = summarizeBranchCoverageExcludingTests funcDefs branchPoints

  putStrLn ""
  putStrLn $ "=== Branch Coverage Summary ==="
  putStrLn $ "Total branch points: " ++ show summary.totalBranchPoints
  putStrLn $ "Total branches: " ++ show summary.totalBranches
  putStrLn $ "Covered branches: " ++ show summary.coveredBranches
  putStrLn $ "Branch coverage: " ++ show summary.branchPercent ++ "%"

  -- Generate hints
  let hints = generateBranchHints summary
  let hintSummary = summarizeBranchHints hints

  putStrLn ""
  putStrLn $ branchHintsToText hintSummary

  -- Show uncovered branches by module
  putStrLn ""
  putStrLn "=== Uncovered Branches by Function ==="
  let uncovered = take 20 summary.uncoveredBranches
  traverse_ showUncovered uncovered

  where
    showUncovered : (String, BranchPoint) -> IO ()
    showUncovered (func, bp) =
      putStrLn $ "  " ++ func ++ " (line " ++ show bp.line ++ "): "
              ++ show bp.branchType ++ " - "
              ++ show bp.coveredBranches ++ "/" ++ show bp.totalBranches ++ " covered"
