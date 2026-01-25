/**
 * Coverage collection using V8 Coverage API
 */

import { chromium } from 'playwright';
import { SourceMapConsumer } from 'source-map';
import { readFileSync, existsSync, statSync, readdirSync } from 'fs';
import { spawn } from 'child_process';
import { mkdirSync, rmSync } from 'fs';
import path from 'path';

/**
 * Find JS and map files from a path
 */
function resolveFiles(targetPath) {
  const stat = statSync(targetPath);

  if (stat.isFile()) {
    const jsFile = targetPath;
    const mapFile = targetPath + '.map';
    return { jsFile, mapFile: existsSync(mapFile) ? mapFile : null };
  }

  // Directory - look for build output
  const buildDir = path.join(targetPath, 'build', 'exec');
  if (existsSync(buildDir)) {
    const files = readdirSync(buildDir);
    const jsFile = files.find(f => !f.endsWith('.map') && !f.startsWith('.'));
    if (jsFile) {
      const fullPath = path.join(buildDir, jsFile);
      const mapPath = fullPath + '.map';
      return {
        jsFile: fullPath,
        mapFile: existsSync(mapPath) ? mapPath : null
      };
    }
  }

  throw new Error(`Could not find JS file in ${targetPath}`);
}

/**
 * Collect coverage using Node.js V8
 */
async function collectNodeCoverage(jsFile, mapFile) {
  const covDir = `/tmp/idris2-web-cov-${Date.now()}`;
  mkdirSync(covDir, { recursive: true });

  // Run with V8 coverage
  const proc = spawn('node', [jsFile], {
    env: { ...process.env, NODE_V8_COVERAGE: covDir },
    stdio: 'pipe'
  });

  let stdout = '';
  let stderr = '';
  proc.stdout.on('data', d => stdout += d);
  proc.stderr.on('data', d => stderr += d);

  await new Promise(resolve => proc.on('close', resolve));

  // Read coverage
  const files = readdirSync(covDir).filter(f => f.endsWith('.json'));
  if (files.length === 0) {
    rmSync(covDir, { recursive: true });
    throw new Error('No coverage data generated');
  }

  const coverage = JSON.parse(readFileSync(path.join(covDir, files[0]), 'utf8'));
  rmSync(covDir, { recursive: true });

  // Find our script - look for the JS file in coverage results
  const basename = path.basename(jsFile);
  const scriptCov = coverage.result.find(r =>
    r.url && r.url.includes(basename)
  );

  return {
    coverage: scriptCov,
    jsCode: readFileSync(jsFile, 'utf8'),
    output: { stdout, stderr }
  };
}

/**
 * Collect coverage using Playwright browser
 */
async function collectBrowserCoverage(jsFile, mapFile, options) {
  const jsCode = readFileSync(jsFile, 'utf8');

  const browser = await chromium.launch({
    headless: options.headless !== false
  });
  const page = await browser.newPage();

  await page.coverage.startJSCoverage({ reportAnonymousScripts: true });

  // Determine what to load
  let htmlContent;
  if (options.url) {
    await page.goto(options.url, { timeout: parseInt(options.timeout) });
  } else if (options.html && existsSync(options.html)) {
    htmlContent = readFileSync(options.html, 'utf8');
    await page.setContent(htmlContent);
  } else {
    // Create minimal HTML to run the JS
    const cleanedJs = jsCode.replace(/^#!.*\n/, '');
    htmlContent = `<!DOCTYPE html>
<html><body>
<script>window.__coverage_output = [];</script>
<script>${cleanedJs}</script>
</body></html>`;
    await page.setContent(htmlContent);
  }

  await page.waitForTimeout(parseInt(options.timeout) || 1000);

  const coverage = await page.coverage.stopJSCoverage();
  await browser.close();

  // Find relevant script coverage
  const scriptCov = coverage.find(c =>
    c.source && (c.source.includes('__mainExpression') || c.source.includes('Idris'))
  );

  return {
    coverage: scriptCov,
    jsCode,
    output: { stdout: '', stderr: '' }
  };
}

/**
 * Map V8 coverage to Idris2 source using source map
 */
async function mapToSource(v8Coverage, jsCode, sourceMapJson) {
  const consumer = await new SourceMapConsumer(sourceMapJson);
  const jsLines = jsCode.split('\n');

  const sourceFiles = new Map(); // source -> { covered: Set, total: Set, functions: [] }

  // Initialize source files from map
  for (const source of sourceMapJson.sources) {
    sourceFiles.set(source, {
      covered: new Set(),
      total: new Set(),
      functions: []
    });
  }

  // Process coverage
  if (v8Coverage && v8Coverage.functions) {
    for (const func of v8Coverage.functions) {
      const isCovered = func.ranges.some(r => r.count > 0);

      for (const range of func.ranges) {
        // Find JS line from offset
        let charCount = 0;
        for (let i = 0; i < jsLines.length; i++) {
          if (charCount + jsLines[i].length >= range.startOffset) {
            // Find nearest mapping
            for (let j = i + 1; j >= Math.max(1, i - 15); j--) {
              const pos = consumer.originalPositionFor({ line: j, column: 0 });
              if (pos.source && pos.line) {
                const srcFile = sourceFiles.get(pos.source);
                if (srcFile) {
                  srcFile.total.add(pos.line);
                  if (range.count > 0) {
                    srcFile.covered.add(pos.line);
                  }
                  if (func.functionName) {
                    srcFile.functions.push({
                      name: func.functionName,
                      line: pos.line,
                      covered: isCovered
                    });
                  }
                }
                break;
              }
            }
            break;
          }
          charCount += jsLines[i].length + 1;
        }
      }
    }
  }

  consumer.destroy();
  return sourceFiles;
}

/**
 * Main coverage collection function
 */
export async function collectCoverage(targetPath, options = {}) {
  const { jsFile, mapFile } = resolveFiles(targetPath);

  if (!mapFile) {
    throw new Error(`Source map not found for ${jsFile}. Compile with --directive sourcemap`);
  }

  const sourceMapJson = JSON.parse(readFileSync(mapFile, 'utf8'));

  // Collect coverage
  const { coverage, jsCode, output } = options.node
    ? await collectNodeCoverage(jsFile, mapFile)
    : await collectBrowserCoverage(jsFile, mapFile, options);

  // Map to source
  const sourceFiles = await mapToSource(coverage, jsCode, sourceMapJson);

  // Calculate totals
  let totalCovered = 0;
  let totalLines = 0;
  const byFile = [];

  for (const [source, data] of sourceFiles) {
    const covered = data.covered.size;
    const total = data.total.size;
    totalCovered += covered;
    totalLines += total;

    if (total > 0) {
      byFile.push({
        source,
        covered,
        total,
        percentage: total > 0 ? Math.round((covered / total) * 100) : 0,
        coveredLines: [...data.covered].sort((a, b) => a - b),
        uncoveredLines: [...data.total].filter(l => !data.covered.has(l)).sort((a, b) => a - b),
        functions: data.functions
      });
    }
  }

  return {
    totalCovered,
    totalLines,
    totalPercentage: totalLines > 0 ? Math.round((totalCovered / totalLines) * 100) : 0,
    totalFunctions: coverage?.functions?.length || 0,
    coveredFunctions: coverage?.functions?.filter(f => f.ranges.some(r => r.count > 0)).length || 0,
    byFile,
    output
  };
}
