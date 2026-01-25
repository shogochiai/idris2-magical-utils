||| Coverage report generation (JSON/Text)
||| REQ_COV_REP_001 - REQ_COV_REP_004
module Coverage.Report

import Coverage.Types
import Data.List
import Data.String
import System.File

%default total

-- =============================================================================
-- JSON Escaping
-- =============================================================================

||| Escape string for JSON
escapeJson : String -> String
escapeJson s = fastConcat $ map escapeChar (unpack s)
  where
    escapeChar : Char -> String
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar c    = singleton c

-- =============================================================================
-- JSON Generation
-- =============================================================================

||| Generate JSON for Maybe String
export
maybeStringJson : Maybe String -> String
maybeStringJson Nothing = "null"
maybeStringJson (Just s) = "\"\{escapeJson s}\""

||| Generate JSON for Maybe Nat
export
maybeNatJson : Maybe Nat -> String
maybeNatJson Nothing = "null"
maybeNatJson (Just n) = show n

||| Generate JSON for list of strings
stringListJson : List String -> String
stringListJson xs = "[" ++ fastConcat (intersperse ", " (map (\s => "\"\{escapeJson s}\"") xs)) ++ "]"

||| REQ_COV_REP_001: Generate JSON for FunctionCoverage
export
functionCoverageJson : FunctionCoverage -> String
functionCoverageJson f =
  "    {\n" ++
  "      \"module\": \"" ++ escapeJson f.moduleName ++ "\",\n" ++
  "      \"name\": \"" ++ escapeJson f.name ++ "\",\n" ++
  "      \"signature\": " ++ maybeStringJson f.signature ++ ",\n" ++
  "      \"line_start\": " ++ maybeNatJson f.lineStart ++ ",\n" ++
  "      \"line_end\": " ++ maybeNatJson f.lineEnd ++ ",\n" ++
  "      \"covered_lines\": " ++ show f.coveredLines ++ ",\n" ++
  "      \"total_lines\": " ++ show f.totalLines ++ ",\n" ++
  "      \"coverage_percent\": " ++ show f.coveragePercent ++ ",\n" ++
  "      \"called_by_tests\": " ++ stringListJson f.calledByTests ++ "\n" ++
  "    }"

||| Generate JSON for ModuleCoverage
export
moduleCoverageJson : ModuleCoverage -> String
moduleCoverageJson m =
  "    {\n" ++
  "      \"path\": \"" ++ escapeJson m.path ++ "\",\n" ++
  "      \"functions_total\": " ++ show m.functionsTotal ++ ",\n" ++
  "      \"functions_covered\": " ++ show m.functionsCovered ++ ",\n" ++
  "      \"line_coverage_percent\": " ++ show m.lineCoveragePercent ++ "\n" ++
  "    }"

||| Generate JSON for ProjectCoverage
export
projectCoverageJson : ProjectCoverage -> String
projectCoverageJson p =
  let branchJson = case p.branchCoveragePercent of
                     Nothing => "null"
                     Just b => show b
  in "{\n" ++
     "    \"total_functions\": " ++ show p.totalFunctions ++ ",\n" ++
     "    \"covered_functions\": " ++ show p.coveredFunctions ++ ",\n" ++
     "    \"line_coverage_percent\": " ++ show p.lineCoveragePercent ++ ",\n" ++
     "    \"branch_coverage_percent\": " ++ branchJson ++ "\n" ++
     "  }"

||| REQ_COV_REP_003: Generate complete JSON report
export
coverageReportJson : CoverageReport -> String
coverageReportJson r =
  let functionsJson = fastConcat $ intersperse ",\n" $ map functionCoverageJson r.functions
      modulesJson = fastConcat $ intersperse ",\n" $ map moduleCoverageJson r.modules
  in "{\n" ++
     "  \"functions\": [\n" ++
     functionsJson ++ "\n" ++
     "  ],\n" ++
     "  \"modules\": [\n" ++
     modulesJson ++ "\n" ++
     "  ],\n" ++
     "  \"project\": " ++ projectCoverageJson r.project ++ "\n" ++
     "}"

-- =============================================================================
-- Branch Coverage JSON Generation
-- =============================================================================

||| Generate JSON for a single BranchPoint
export
branchPointJson : BranchPoint -> String
branchPointJson bp =
  let detailsJson = "[" ++ fastConcat (intersperse ", " (map pairJson bp.branchDetails)) ++ "]"
  in "    {\n" ++
     "      \"line\": " ++ show bp.line ++ ",\n" ++
     "      \"char\": " ++ show bp.char ++ ",\n" ++
     "      \"type\": \"" ++ show bp.branchType ++ "\",\n" ++
     "      \"total_branches\": " ++ show bp.totalBranches ++ ",\n" ++
     "      \"covered_branches\": " ++ show bp.coveredBranches ++ ",\n" ++
     "      \"details\": " ++ detailsJson ++ "\n" ++
     "    }"
  where
    pairJson : (String, Nat) -> String
    pairJson (label, cnt) = "{\"label\": \"" ++ escapeJson label ++ "\", \"count\": " ++ show cnt ++ "}"

||| Generate JSON for BranchCoverageSummary
export
branchCoverageSummaryJson : BranchCoverageSummary -> String
branchCoverageSummaryJson bcs =
  "{\n" ++
  "    \"total_branch_points\": " ++ show bcs.totalBranchPoints ++ ",\n" ++
  "    \"total_branches\": " ++ show bcs.totalBranches ++ ",\n" ++
  "    \"covered_branches\": " ++ show bcs.coveredBranches ++ ",\n" ++
  "    \"branch_percent\": " ++ show bcs.branchPercent ++ "\n" ++
  "  }"

||| Generate JSON for branch coverage with details
export
branchCoverageDetailJson : List BranchPoint -> String
branchCoverageDetailJson bps =
  let branchesJson = fastConcat $ intersperse ",\n" $ map branchPointJson bps
  in "{\n" ++
     "  \"branch_points\": [\n" ++
     branchesJson ++ "\n" ++
     "  ]\n" ++
     "}"

-- =============================================================================
-- Text Report Generation
-- =============================================================================

||| REQ_COV_REP_002: Generate human-readable text output
export
coverageReportText : CoverageReport -> String
coverageReportText r =
  let header = "=== Coverage Report ===\n\n"
      projectSection = formatProject r.project
      moduleSection = formatModules r.modules
      functionSection = formatFunctions r.functions
  in header ++ projectSection ++ "\n" ++ moduleSection ++ "\n" ++ functionSection
  where
    formatProject : ProjectCoverage -> String
    formatProject p =
      let branchStr = case p.branchCoveragePercent of
                        Nothing => "N/A"
                        Just b => show b ++ "%"
      in "Project Summary:\n" ++
         "  Functions: " ++ show p.coveredFunctions ++ "/" ++ show p.totalFunctions ++ " covered\n" ++
         "  Line Coverage: " ++ show p.lineCoveragePercent ++ "%\n" ++
         "  Branch Coverage: " ++ branchStr ++ "\n"

    formatModules : List ModuleCoverage -> String
    formatModules ms =
      "Modules:\n" ++ fastConcat (map formatModule ms)
      where
        formatModule : ModuleCoverage -> String
        formatModule m =
          "  \{m.path}: \{show m.functionsCovered}/\{show m.functionsTotal} (\{show m.lineCoveragePercent}%)\n"

    formatFunctions : List FunctionCoverage -> String
    formatFunctions fs =
      let uncovered = filter (\f => f.coveragePercent == 0.0) fs
      in if null uncovered
            then "All functions covered!\n"
            else "Uncovered Functions:\n" ++ fastConcat (map formatUncovered uncovered)
      where
        formatUncovered : FunctionCoverage -> String
        formatUncovered f =
          "  \{f.moduleName}.\{f.name} (line \{maybe "?" show f.lineStart})\n"

-- =============================================================================
-- Report Output
-- =============================================================================

||| Output format
public export
data OutputFormat = JSON | Text

public export
Show OutputFormat where
  show JSON = "json"
  show Text = "text"

||| Generate report in specified format
export
generateReport : OutputFormat -> CoverageReport -> String
generateReport JSON r = coverageReportJson r
generateReport Text r = coverageReportText r

||| REQ_COV_REP_004: Write output to specified file path
export
covering
writeReport : OutputFormat -> CoverageReport -> String -> IO (Either String ())
writeReport fmt report path = do
  let content = generateReport fmt report
  Right () <- writeFile path content
    | Left err => pure $ Left "Failed to write \{path}: \{show err}"
  pure $ Right ()

||| Write report to stdout
export
printReport : OutputFormat -> CoverageReport -> IO ()
printReport fmt report =
  putStrLn $ generateReport fmt report
