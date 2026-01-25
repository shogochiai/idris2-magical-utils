#!/usr/bin/env node
/**
 * idris2-web-cov - V8 Coverage for Idris2 dom-mvc apps
 *
 * Usage:
 *   idris2-web-cov <project-path> [options]
 *   idris2-web-cov path/to/app.js --map path/to/app.js.map
 *
 * Compatible with idris2-coverage CLI interface for LazyWeb integration.
 */

import { program } from 'commander';
import { collectCoverage } from '../lib/coverage.mjs';
import { formatReport, formatJson } from '../lib/report.mjs';

program
  .name('idris2-web-cov')
  .description('V8 coverage for Idris2 dom-mvc apps with source map support')
  .version('0.1.0')
  .argument('<path>', 'Path to JS file or project directory')
  .option('-m, --map <path>', 'Path to source map file (default: <js-file>.map)')
  .option('-u, --url <url>', 'URL to load for browser testing (for dom-mvc apps)')
  .option('--html <path>', 'Path to HTML file to load')
  .option('--uncovered', 'Show only uncovered functions')
  .option('--json', 'Output in JSON format')
  .option('-t, --timeout <ms>', 'Timeout for page load in ms', '5000')
  .option('--headless', 'Run browser in headless mode (default: true)', true)
  .option('--node', 'Use Node.js V8 coverage instead of browser')
  .action(async (targetPath, options) => {
    try {
      const result = await collectCoverage(targetPath, options);

      if (options.json) {
        console.log(formatJson(result));
      } else {
        console.log(formatReport(result, options));
      }

      // Exit code: 0 if coverage > 0, 1 if no coverage data
      process.exit(result.totalFunctions > 0 ? 0 : 1);
    } catch (error) {
      console.error('Error:', error.message);
      if (process.env.DEBUG) {
        console.error(error.stack);
      }
      process.exit(2);
    }
  });

program.parse();
