/**
 * Playwright Shim for Idris2
 *
 * Bridges Playwright's async API to Idris2's callback-based FFI.
 * Each function accepts an Idris2 callback as the last parameter.
 */

import { chromium } from 'playwright';

/**
 * Launch Chromium browser (headless)
 * @param {Function} callback - Idris2 callback (receives browser handle)
 */
export async function launchBrowser(callback) {
  try {
    const browser = await chromium.launch({ headless: true });
    callback(browser)();
  } catch (e) {
    console.error('launchBrowser error:', e.message);
    callback(null)();
  }
}

/**
 * Create a new page in the browser
 * @param {Browser} browser - Playwright browser instance
 * @param {Function} callback - Idris2 callback (receives page handle)
 */
export async function newPage(browser, callback) {
  try {
    const page = await browser.newPage();
    callback(page)();
  } catch (e) {
    console.error('newPage error:', e.message);
    callback(null)();
  }
}

/**
 * Start V8 JS coverage collection
 * @param {Page} page - Playwright page instance
 * @param {Function} callback - Idris2 callback (receives 0 on success)
 */
export async function startCoverage(page, callback) {
  try {
    await page.coverage.startJSCoverage({
      resetOnNavigation: false,
      reportAnonymousScripts: true
    });
    callback(0)();
  } catch (e) {
    console.error('startCoverage error:', e.message);
    callback(-1)();
  }
}

/**
 * Stop coverage and return JSON string
 * @param {Page} page - Playwright page instance
 * @param {Function} callback - Idris2 callback (receives JSON string)
 */
export async function stopCoverage(page, callback) {
  try {
    const coverage = await page.coverage.stopJSCoverage();
    const json = JSON.stringify(coverage);
    callback(json)();
  } catch (e) {
    console.error('stopCoverage error:', e.message);
    callback('[]')();
  }
}

/**
 * Add a script tag to load JS file
 * @param {Page} page - Playwright page instance
 * @param {string} path - Path to JS file
 * @param {Function} callback - Idris2 callback (receives 0 on success)
 */
export async function addScriptTag(page, path, callback) {
  try {
    await page.addScriptTag({ path: path });
    callback(0)();
  } catch (e) {
    console.error('addScriptTag error:', e.message);
    callback(-1)();
  }
}

/**
 * Set page HTML content
 * @param {Page} page - Playwright page instance
 * @param {string} html - HTML content
 * @param {Function} callback - Idris2 callback (receives 0 on success)
 */
export async function setContent(page, html, callback) {
  try {
    await page.setContent(html);
    callback(0)();
  } catch (e) {
    console.error('setContent error:', e.message);
    callback(-1)();
  }
}

/**
 * Evaluate JavaScript in page context
 * @param {Page} page - Playwright page instance
 * @param {string} js - JavaScript code to evaluate
 * @param {Function} callback - Idris2 callback (receives result as string)
 */
export async function evaluate(page, js, callback) {
  try {
    const result = await page.evaluate(js);
    callback(String(result))();
  } catch (e) {
    console.error('evaluate error:', e.message);
    callback('')();
  }
}

/**
 * Close the browser
 * @param {Browser} browser - Playwright browser instance
 * @param {Function} callback - Idris2 callback (receives 0 on success)
 */
export async function closeBrowser(browser, callback) {
  try {
    await browser.close();
    callback(0)();
  } catch (e) {
    console.error('closeBrowser error:', e.message);
    callback(-1)();
  }
}

/**
 * Navigate to a URL and return the HTTP response status code.
 * @param {Page} page - Playwright page instance
 * @param {string} url - URL to navigate to
 * @param {Function} callback - Idris2 callback (receives status code, or -1 on navigation failure)
 */
export async function goto(page, url, callback) {
  try {
    const response = await page.goto(url, { waitUntil: 'load' });
    callback(response ? response.status() : -1)();
  } catch (e) {
    console.error('goto error:', e.message);
    callback(-1)();
  }
}
