||| Report generation for coverage output
||| Supports JSON and text formats
module EvmCoverage.Report

import EvmCoverage.Types
import EvmCoverage.Aggregator
import Data.List
import Data.String

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
  "classification": {
    "bugs": \{show cov.bugsTotal},
    "unknown": \{show cov.unknownTotal}
  },
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

||| Generate full text report
export
toText : String -> AggregatedCoverage -> String
toText target cov =
  let bugs = filter (\b => isBug b.branchClass) cov.uncoveredBranches
  in reportHeader target
  ++ classificationTable cov
  ++ uncoveredSection cov
  ++ bugsSection bugs

-- =============================================================================
-- Summary Output (compact)
-- =============================================================================

||| One-line summary
export
toOneLine : AggregatedCoverage -> String
toOneLine cov =
  "Coverage: \{show cov.canonicalHit}/\{show cov.canonicalTotal} (\{show cov.coveragePercent}%) | Bugs: \{show cov.bugsTotal}"

||| Short summary (for CI)
export
toShortSummary : AggregatedCoverage -> String
toShortSummary cov = """
canonical: \{show cov.canonicalHit}/\{show cov.canonicalTotal}
percent: \{show cov.coveragePercent}
bugs: \{show cov.bugsTotal}
unknown: \{show cov.unknownTotal}
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

## Gaps (\{show $ length gaps})

\{if null gaps then "_No gaps_" else unlines $ map showGap gaps}
"""
  where
    showGap : CoverageGap -> String
    showGap g = "- **\{show g.branchId}**: \{g.reason} (priority: \{show g.priority})"

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
