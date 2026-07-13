||| Synchronous, one-shot page-visit bridge.
|||
||| Idris2's Node backend `%foreign "node:lambda:..."` is a plain synchronous
||| JS function call (see idris2's Compiler/ES/Codegen.idr -- only
||| "lambda"/"support"/"stringIterator" foreign types exist; there is no
||| async/promise-aware variant). Idris2 IO on this backend has no native way
||| to await a Promise, so a DIRECT binding to Playwright's (entirely
||| Promise-based) API -- e.g. a callback-continuation shim, which is how
||| this library's OTHER primitives (launchBrowser/newPage/goto/evaluate in
||| Playwright.FFI) are built -- can never actually resume Idris execution
||| correctly: there is no continuation for Idris to invoke into, so those
||| calls return a value that isn't really the awaited result (confirmed:
||| `browser.newPage is not a function` on the object launchBrowser hands
||| back).
|||
||| This module sidesteps the mismatch entirely via
||| `child_process.execFileSync`, which genuinely blocks synchronously until
||| a child process exits -- matching Idris2 IO's synchronous execution
||| model exactly. `support/visit-page.mjs` is a self-contained subprocess
||| that does ALL the async Playwright work internally (launch, navigate,
||| extract title/links, close) in one browser session and prints one JSON
||| object to stdout before exiting.
module Playwright.Sync

import Data.String
import System.FFI

%default covering

||| Path to the visit-page helper script, overridable via the
||| PLAYWRIGHT_VISIT_HELPER env var (the generated JS output can end up
||| anywhere -- e.g. a different package's build/exec/ directory -- so a
||| relative path baked into the FFI string can't be trusted to resolve).
||| Falls back to "./support/visit-page.mjs" (relative to the CALLER's own
||| working directory) if unset.
%foreign "node:lambda:(url) => require('child_process').execFileSync(process.execPath, [process.env.PLAYWRIGHT_VISIT_HELPER || './support/visit-page.mjs', url], {encoding: 'utf8', maxBuffer: 20*1024*1024}).toString()"
prim__visitPageSync : String -> PrimIO String

||| Visit one URL synchronously (launches a fresh headless browser, navigates
||| to it, and closes it -- one full subprocess round-trip per call, since
||| there is no persistent-session mechanism that fits Idris2's synchronous
||| IO model). Returns the raw JSON: {"status":<int>,"title":<string>,
||| "links":[<string>,...]}. status is -1 for a navigation-level failure
||| (DNS/connection refused/timeout), never a thrown exception.
export
visitPageSync : (url : String) -> IO String
visitPageSync url = primIO (prim__visitPageSync url)
