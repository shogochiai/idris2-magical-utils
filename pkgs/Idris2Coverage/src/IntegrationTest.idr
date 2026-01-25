module Main

import Coverage.Types
import Coverage.Collector
import Coverage.SourceAnalyzer
import Coverage.Aggregator
import Coverage.Report
import System.File

-- Test data
sampleScheme : String
sampleScheme = "(define Sample-add (lambda (x) x))\n(define Sample-mul (lambda (x) x))"

sampleHtml : String
sampleHtml = "<tr><td class=pc12>line 10 (5)</td></tr>"

sampleSource : String
sampleSource = "module Test\n\nexport\nadd : Int -> Int\nadd x = x"

-- Sample annotated HTML (like .ss.html)
sampleAnnotatedHtml : String
sampleAnnotatedHtml = """
<span class=pc2 title="line 10 char 1 count 5">code1</span>
<span class=pc2 title="line 10 char 20 count 3">code2</span>
<span class=pc1 title="line 11 char 1 count 0">uncovered</span>
<span class=pc2 title="line 12 char 1 count 1">code3</span>
"""

main : IO ()
main = do
  putStrLn "=== Coverage Integration Test ==="

  -- Test Types
  putStrLn $ "idrisToSchemeModule: " ++ idrisToSchemeModule "A.B"
  putStrLn $ "parseSchemeFunc: " ++ show (parseSchemeFunc "Sample-add")
  putStrLn $ "belongsToModule: " ++ show (belongsToModule "Sample-add" "Sample")

  -- Test Collector
  let defs = parseSchemeDefs sampleScheme
  putStrLn $ "parseSchemeDefs count: " ++ show (length defs)
  let hits = parseProfileHtml sampleHtml
  putStrLn $ "parseProfileHtml count: " ++ show (length hits)

  -- Test NEW: Expression-level coverage parsing
  let exprs = parseAnnotatedHtml sampleAnnotatedHtml
  putStrLn $ "parseAnnotatedHtml count: " ++ show (length exprs)
  let byLine = groupByLine exprs
  putStrLn $ "groupByLine count: " ++ show (length byLine)

  -- Test function coverage calculation
  let testDefs = [("TestFunc", 10)]
  let funcCov = calculateFunctionCoverage testDefs exprs
  putStrLn $ "calculateFunctionCoverage: " ++ show (length funcCov)
  case funcCov of
    [(name, exec, tot, pct)] =>
      putStrLn $ "  " ++ name ++ ": " ++ show exec ++ "/" ++ show tot ++ " (" ++ show pct ++ "%)"
    _ => putStrLn "  unexpected result"

  -- Test SourceAnalyzer
  let funcs = analyzeSource sampleSource
  putStrLn $ "analyzeSource count: " ++ show (length funcs)
  case extractModuleName sampleSource of
    Just m => putStrLn $ "extractModuleName: " ++ m
    Nothing => putStrLn "extractModuleName: Nothing"

  -- Test Report
  let fc = coveredFunction "Mod" "func" 10 ["t1"]
  let json = functionCoverageJson fc
  putStrLn $ "JSON generated: " ++ show (length json > 0)

  putStrLn "=== Done ==="
