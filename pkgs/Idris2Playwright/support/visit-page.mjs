#!/usr/bin/env node
/**
 * Synchronous-from-Idris2 bridge for a single page visit.
 *
 * WHY THIS EXISTS: Idris2's Node backend `%foreign "node:lambda:..."` is a
 * plain synchronous JS function call (see Compiler/ES/Codegen.idr's
 * `makeForeign` -- only "lambda"/"support"/"stringIterator" foreign types
 * exist, none of them async/promise-aware). Idris2's IO monad on this
 * backend has no native way to await a Promise. Playwright's entire API is
 * Promise-based, so a direct FFI binding (the ORIGINAL idris2-playwright
 * design: an async shim function invoked with a callback) can never
 * actually resume Idris execution correctly -- there is no continuation to
 * invoke into. This script sidesteps the mismatch entirely: it is a
 * self-contained, one-shot subprocess that does ALL the async work
 * internally (launch, navigate, extract, close) and prints ONE JSON object
 * to stdout before exiting. The Idris side calls it via
 * child_process.execFileSync, which IS genuinely synchronous (blocks the
 * calling process until the child exits and returns its stdout) --
 * matching Idris2 IO's synchronous execution model exactly.
 *
 * Usage: node visit-page.mjs <url>
 * Prints: {"status":<int>,"title":<string>,"links":[<string>,...],"conduits":[<conduit>,...]}
 * status is -1 for a navigation-level failure (DNS/connection refused/etc),
 * never a JS exception escaping to stderr -- always valid JSON to stdout.
 *
 * `conduits`: WebsiteRender's derived landing page renders each install
 * conduit as a `.entry` div (`.entry.pending` for a NotBuilt/Unpinned/
 * DigestMismatch status note, plain `.entry` with an `a.btn[href]` + `.dg`
 * digest label for a Ready download button). This is the "no lies" check:
 * a Ready conduit CLAIMS a specific sha256(base64(bytes)) digest next to its
 * download link -- this script fetches that link's actual bytes and computes
 * the SAME digest convention (GlobalRegistry's govsrcDigest / hopPublishDerived
 * store it identically), so a caller can catch a Ready button whose declared
 * digest doesn't match what's actually served (drift the page itself doesn't
 * self-detect). Non-`.entry`-bearing pages (docs/spec/status pages) simply
 * produce an empty conduits array -- no extra network calls beyond the page
 * load itself.
 *
 * `headingEn`: the conduit's English heading text (the `[data-lang=en]` span
 * inside its `<h3>`, e.g. "Android app", "CLI install") -- present for BOTH
 * Ready and pending entries, since WebsiteRender always names the family in
 * the heading regardless of status. This is the "no silent gaps" check: a
 * real bug this session found live -- a project can have a genuinely BUILT
 * artifact (an APK sitting on disk) that was simply never included in a
 * release's unit list, so the deployed page shows "coming soon" forever even
 * though nothing is actually missing except the release step. Matching on
 * this label (not the CSS class, which is generic "entry pending" with no
 * family tag) lets a caller assert "family X's conduit MUST be Ready", using
 * the family name a human already reads on the page, not an internal tag.
 */
import { chromium } from 'playwright';
import crypto from 'crypto';

const url = process.argv[2];

function sha256Base64Digest(buf) {
  const b64 = buf.toString('base64');
  return crypto.createHash('sha256').update(b64, 'utf8').digest('hex');
}

async function main() {
  let browser;
  try {
    browser = await chromium.launch({ headless: true });
    const page = await browser.newPage();
    let status = -1;
    try {
      const response = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
      status = response ? response.status() : -1;
    } catch (navErr) {
      status = -1;
    }
    let title = '';
    let links = [];
    let conduits = [];
    if (status >= 200 && status < 400) {
      title = await page.title();
      links = await page.$$eval('a[href]', (as) => as.map((a) => a.getAttribute('href')));
      conduits = await page.$$eval('.entry', (divs) => divs.map((d) => {
        const btn = d.querySelector('a.btn[href]');
        const dg = d.querySelector('.dg');
        const dgText = dg ? dg.textContent.trim() : '';
        const headingEnEl = d.querySelector('h3 [data-lang="en"]');
        return {
          pending: d.classList.contains('pending'),
          href: btn ? btn.getAttribute('href') : null,
          declaredDigest: dgText.replace(/^\S+\s+/, ''),
          headingEn: headingEnEl ? headingEnEl.textContent.trim() : (d.querySelector('h3') ? d.querySelector('h3').textContent.trim() : ''),
        };
      }));
      for (const c of conduits) {
        if (c.href) {
          try {
            const resolvedUrl = new URL(c.href, url).toString();
            const resp = await page.context().request.get(resolvedUrl);
            const buf = await resp.body();
            c.actualDigest = sha256Base64Digest(buf);
            c.fetchOk = resp.ok();
          } catch (fetchErr) {
            c.actualDigest = '';
            c.fetchOk = false;
          }
        } else {
          c.actualDigest = '';
          c.fetchOk = true;  // no link to fetch (a pending entry): not a fetch failure
        }
      }
    }
    process.stdout.write(JSON.stringify({ status, title, links, conduits }));
  } catch (e) {
    process.stdout.write(JSON.stringify({ status: -1, title: '', links: [], conduits: [] }));
  } finally {
    if (browser) await browser.close();
  }
}

main();
