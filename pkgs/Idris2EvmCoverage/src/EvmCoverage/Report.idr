||| Report generation for coverage output
||| Supports JSON and text formats
module EvmCoverage.Report

import EvmCoverage.Types
import EvmCoverage.Aggregator
import Data.List
import Data.String
import Coverage.Standardization.Types as CovStd
import Coverage.Core.HighImpact as CoreHI

%default total

-- =============================================================================
-- JSON Output
-- =============================================================================

||| Escape string for JSON
escapeJson : String -> String
escapeJson s = pack $ concatMap escape (unpack s)
  where
    escape : Char -> List Char
    escape '"' = ['\\', '"']
    escape '\\' = ['\\', '\\']
    escape '\n' = ['\\', 'n']
    escape '\r' = ['\\', 'r']
    escape '\t' = ['\\', 't']
    escape c = [c]

||| Format gap as JSON object
gapToJson : CoverageGap -> String
gapToJson gap = """
    {
      "branch": "\{escapeJson $ show gap.branchId}",
      "reason": "\{escapeJson gap.reason}",
      "priority": \{show gap.priority}
    }
"""

||| Format gaps list as JSON array
gapsToJsonArray : List CoverageGap -> String
gapsToJsonArray [] = "[]"
gapsToJsonArray gaps =
  "[\n" ++ (concat $ intersperse ",\n" $ map gapToJson gaps) ++ "\n  ]"

targetSummaryToJson : HighImpactTarget -> String
targetSummaryToJson t = """
    {
      "function": "\{escapeJson t.funcName}",
      "module": "\{escapeJson t.moduleName}",
      "branch_count": \{show t.branchCount},
      "severity": "\{escapeJson $ CoreHI.showSeverity t.severity}",
      "kind": "\{escapeJson $ show t.kind}"
    }
"""

targetSummariesToJsonArray : List HighImpactTarget -> String
targetSummariesToJsonArray [] = "[]"
targetSummariesToJsonArray ts =
  "[\n" ++ (concat $ intersperse ",\n" $ map targetSummaryToJson ts) ++ "\n  ]"

summaryToJson : BranchFunctionSummary -> String
summaryToJson s =
  "{"
  ++ "\"function\": \"" ++ escapeJson s.functionName ++ "\", "
  ++ "\"canonical_branches\": " ++ show s.canonicalBranches ++ ", "
  ++ "\"hit_branches\": " ++ show s.hitBranches ++ ", "
  ++ "\"uncovered_branch_ids\": " ++ listToJson s.uncoveredBranchIds ++ ", "
  ++ "\"bug_branches\": " ++ show s.bugBranches ++ ", "
  ++ "\"unknown_branches\": " ++ show s.unknownBranches
  ++ "}"
  where
    listToJson : List String -> String
    listToJson xs = "[" ++ concat (intersperse ", " (map (\x => "\"" ++ escapeJson x ++ "\"") xs)) ++ "]"

summariesToJsonArray : List BranchFunctionSummary -> String
summariesToJsonArray [] = "[]"
summariesToJsonArray ss =
  "[" ++ concat (intersperse ", " (map summaryToJson ss)) ++ "]"

measurementToJson : CovStd.CoverageMeasurement -> String
measurementToJson m =
  "{\n"
  ++ "    \"denominator_ids\": " ++ listToJson m.denominatorIds ++ ",\n"
  ++ "    \"covered_ids\": " ++ listToJson m.coveredIds ++ ",\n"
  ++ "    \"excluded_ids\": " ++ listToJson m.excludedIds ++ ",\n"
  ++ "    \"unknown_ids\": " ++ listToJson m.unknownIds ++ "\n"
  ++ "  }"
  where
    listToJson : List String -> String
    listToJson xs = "[" ++ concat (intersperse ", " (map (\x => "\"" ++ escapeJson x ++ "\"") xs)) ++ "]"

||| Generate JSON report from aggregated coverage
export
toJson : String -> String -> AggregatedCoverage -> String
toJson timestamp target cov =
  let gaps = generateGaps cov
  in """
{
  "timestamp": "\{timestamp}",
  "target": "\{escapeJson target}",
  "coverage": {
    "canonical_total": \{show cov.canonicalTotal},
    "canonical_hit": \{show cov.canonicalHit},
    "percent": \{show cov.coveragePercent}
  },
  "execution": {
    "profile": "\{escapeJson cov.executionProfile}",
    "coverage_model": "\{escapeJson cov.coverageModel}",
    "unknown_policy": "\{escapeJson cov.unknownPolicy}",
    "claim_admissible": \{if cov.claimAdmissible then "true" else "false"}
  },
  "measurement": \{measurementToJson cov.measurement},
  "classification": {
    "bugs": \{show cov.bugsTotal},
    "unknown": \{show cov.unknownTotal}
  },
  "high_impact_targets": \{targetSummariesToJsonArray cov.highImpactTargets},
  "function_summaries": \{summariesToJsonArray cov.functionSummaries},
  "gaps": \{gapsToJsonArray gaps}
}
"""

-- =============================================================================
-- Text Output
-- =============================================================================

||| Generate text report header
reportHeader : String -> String
reportHeader target = """
================================================================================
EVM Coverage Report
================================================================================
Target: \{target}

"""

||| Format classification as text table
classificationTable : AggregatedCoverage -> String
classificationTable cov = """
## Branch Classification

| Category    | Count |
|-------------|-------|
| Canonical   | \{show cov.canonicalTotal} |
| Hit         | \{show cov.canonicalHit} |
| Bugs        | \{show cov.bugsTotal} |
| Unknown     | \{show cov.unknownTotal} |

Coverage: \{show cov.coveragePercent}%
Execution Profile: \{cov.executionProfile}
Coverage Model: \{cov.coverageModel}
Unknown Policy: \{cov.unknownPolicy}
Claim Admissible: \{show cov.claimAdmissible}
"""

||| Format uncovered branches
uncoveredSection : AggregatedCoverage -> String
uncoveredSection cov =
  if null cov.uncoveredBranches
    then "\n## Uncovered Branches\nNone - full coverage!\n"
    else let lines = map (\b => "  - " ++ show b.branchId) cov.uncoveredBranches
         in "\n## Uncovered Branches\n" ++ unlines lines

||| Format bugs section
bugsSection : List ClassifiedBranch -> String
bugsSection bugs =
  if null bugs
    then ""
    else let lines = map (\b => "  [BUG] " ++ show b.branchId ++ ": " ++ b.pattern) bugs
         in "\n## Potential Bugs (UnhandledInput)\n" ++ unlines lines

topTargetsSection : List HighImpactTarget -> String
topTargetsSection targets =
  if null targets
    then ""
    else "\n## Top Branch Targets\n"
      ++ unlines (map (\t => "  - " ++ t.funcName ++ " (" ++ show t.branchCount ++ " branches, " ++ showTargetSeverity t ++ ")")
                      (take 5 targets))
  where
    showTargetSeverity : HighImpactTarget -> String
    showTargetSeverity t = CoreHI.showSeverity t.severity

functionSummariesSection : List BranchFunctionSummary -> String
functionSummariesSection summaries =
  if null summaries
    then ""
    else "\n## Function Branch Summaries\n"
      ++ unlines
           (map (\s =>
             "  - " ++ s.functionName ++ ": " ++ show s.hitBranches ++ "/" ++ show s.canonicalBranches
             ++ ", uncovered=" ++ show (length s.uncoveredBranchIds)
             ++ ", bugs=" ++ show s.bugBranches
             ++ ", unknown=" ++ show s.unknownBranches)
             (take 8 summaries))

||| Generate full text report
export
toText : String -> AggregatedCoverage -> String
toText target cov =
  let bugs = filter (\b => isBug b.branchClass) cov.uncoveredBranches
  in reportHeader target
  ++ classificationTable cov
  ++ uncoveredSection cov
  ++ bugsSection bugs
  ++ topTargetsSection cov.highImpactTargets
  ++ functionSummariesSection cov.functionSummaries

-- =============================================================================
-- Summary Output (compact)
-- =============================================================================

||| One-line summary
export
toOneLine : AggregatedCoverage -> String
toOneLine cov =
  "Coverage: \{show cov.canonicalHit}/\{show cov.canonicalTotal} (\{show cov.coveragePercent}%) | Bugs: \{show cov.bugsTotal} | Profile: \{cov.executionProfile}"

||| Short summary (for CI)
export
toShortSummary : AggregatedCoverage -> String
toShortSummary cov = """
canonical: \{show cov.canonicalHit}/\{show cov.canonicalTotal}
percent: \{show cov.coveragePercent}
bugs: \{show cov.bugsTotal}
unknown: \{show cov.unknownTotal}
profile: \{cov.executionProfile}
model: \{cov.coverageModel}
claim_admissible: \{show cov.claimAdmissible}
"""

-- =============================================================================
-- Markdown Output
-- =============================================================================

||| Generate Markdown report
export
toMarkdown : String -> AggregatedCoverage -> String
toMarkdown target cov =
  let gaps = generateGaps cov
  in """
# EVM Coverage Report

**Target:** `\{target}`

## Summary

| Metric | Value |
|--------|-------|
| Canonical Branches | \{show cov.canonicalTotal} |
| Branches Hit | \{show cov.canonicalHit} |
| Coverage | \{show cov.coveragePercent}% |
| Bugs (UnhandledInput) | \{show cov.bugsTotal} |
| Unknown | \{show cov.unknownTotal} |
| Execution Profile | \{cov.executionProfile} |
| Coverage Model | \{cov.coverageModel} |
| Claim Admissible | \{show cov.claimAdmissible} |

## Gaps (\{show $ length gaps})

\{if null gaps then "_No gaps_" else unlines $ map showGap gaps}

## Top Branch Targets

\{if null cov.highImpactTargets
    then "_No high-impact branch targets_"
    else unlines $ map showTarget (take 5 cov.highImpactTargets)}

## Function Branch Summaries

\{if null cov.functionSummaries
    then "_No function-level branch summaries_"
    else unlines $ map showSummary (take 8 cov.functionSummaries)}
"""
  where
    showGap : CoverageGap -> String
    showGap g = "- **\{show g.branchId}**: \{g.reason} (priority: \{show g.priority})"

    showTarget : HighImpactTarget -> String
    showTarget t = "- **\{t.funcName}**: \{show t.branchCount} branches (`\{CoreHI.showSeverity t.severity}`)"

    showSummary : BranchFunctionSummary -> String
    showSummary s =
      "- **\{s.functionName}**: \{show s.hitBranches}/\{show s.canonicalBranches}, uncovered=\{show $ length s.uncoveredBranchIds}, bugs=\{show s.bugBranches}, unknown=\{show s.unknownBranches}"

-- =============================================================================
-- File Writing Helpers
-- =============================================================================

||| Generate filename with extension
export
reportFilename : String -> String -> String
reportFilename baseName ext = baseName ++ "." ++ ext

-- =============================================================================
-- Function Coverage Output (from SourceMap)
-- =============================================================================
-- NOTE: FunctionCoverage type removed - SourceMap module not available
-- functionCoverageToJson and functionCoverageToMarkdown removed

||| Available output formats
public export
data OutputFormat = JSON | Text | Markdown | OneLine

public export
Show OutputFormat where
  show JSON = "json"
  show Text = "text"
  show Markdown = "markdown"
  show OneLine = "oneline"

||| Generate report in specified format
export
generateReport : OutputFormat -> String -> String -> AggregatedCoverage -> String
generateReport JSON timestamp target cov = toJson timestamp target cov
generateReport Text _ target cov = toText target cov
generateReport Markdown _ target cov = toMarkdown target cov
generateReport OneLine _ _ cov = toOneLine cov
