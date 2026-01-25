/**
 * Report formatting for idris2-web-cov
 * Compatible with idris2-coverage output format
 */

/**
 * Format coverage report for terminal output
 */
export function formatReport(result, options = {}) {
  const lines = [];

  lines.push('# Web Coverage Report');
  lines.push(`Coverage: ${result.totalCovered}/${result.totalLines} (${result.totalPercentage}%)`);
  lines.push(`Functions: ${result.coveredFunctions}/${result.totalFunctions}`);
  lines.push('');

  // Group by file type
  const idrisFiles = result.byFile.filter(f => f.source.endsWith('.idr'));
  const preludeFiles = result.byFile.filter(f => f.source.includes('Prelude'));
  const otherFiles = result.byFile.filter(f => !f.source.endsWith('.idr') && !f.source.includes('Prelude'));

  // Show user files first
  const userFiles = idrisFiles.filter(f => !f.source.includes('Prelude'));

  if (userFiles.length > 0) {
    lines.push('## User Files');
    for (const file of userFiles) {
      const status = file.percentage === 100 ? '✓' : file.percentage > 0 ? '◐' : '✗';
      lines.push(`  ${status} ${file.source}: ${file.covered}/${file.total} (${file.percentage}%)`);

      if (options.uncovered && file.uncoveredLines.length > 0) {
        lines.push(`    Uncovered lines: ${file.uncoveredLines.join(', ')}`);
      }
    }
    lines.push('');
  }

  // Summary of prelude/library coverage
  if (preludeFiles.length > 0) {
    const preludeCovered = preludeFiles.reduce((sum, f) => sum + f.covered, 0);
    const preludeTotal = preludeFiles.reduce((sum, f) => sum + f.total, 0);
    const preludePct = preludeTotal > 0 ? Math.round((preludeCovered / preludeTotal) * 100) : 0;
    lines.push(`## Prelude/Library: ${preludeCovered}/${preludeTotal} (${preludePct}%)`);
  }

  return lines.join('\n');
}

/**
 * Format coverage report as JSON
 * Compatible with idris2-coverage JSON output
 */
export function formatJson(result) {
  return JSON.stringify({
    timestamp: Date.now(),
    coverage: {
      lines: {
        covered: result.totalCovered,
        total: result.totalLines,
        percentage: result.totalPercentage
      },
      functions: {
        covered: result.coveredFunctions,
        total: result.totalFunctions
      }
    },
    files: result.byFile.map(f => ({
      source: f.source,
      lines: {
        covered: f.covered,
        total: f.total,
        percentage: f.percentage
      },
      coveredLines: f.coveredLines,
      uncoveredLines: f.uncoveredLines
    }))
  }, null, 2);
}
