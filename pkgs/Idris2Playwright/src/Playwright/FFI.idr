||| Playwright FFI
|||
||| Foreign function interface to Playwright via Node.js backend.
||| Uses a shim module for async handling.
module Playwright.FFI

import Playwright.Types
import System.FFI

%default covering

-- =============================================================================
-- Shim Path Configuration
-- =============================================================================

||| Path to the playwright shim module (relative to execution dir)
||| Users should set PLAYWRIGHT_SHIM_PATH env var or use default
shimPath : String
shimPath = "./playwright-shim.mjs"

-- =============================================================================
-- Low-Level FFI Declarations
-- =============================================================================

-- Note: These use synchronous wrappers in the shim that internally
-- handle Playwright's async API via blocking patterns.

%foreign "node:lambda:(callback) => require('./support/playwright-shim.mjs').launchBrowser(callback)"
prim__launchBrowser : PrimIO Browser

%foreign "node:lambda:(browser, callback) => require('./support/playwright-shim.mjs').newPage(browser, callback)"
prim__newPage : Browser -> PrimIO Page

%foreign "node:lambda:(page, callback) => require('./support/playwright-shim.mjs').startCoverage(page, callback)"
prim__startCoverage : Page -> PrimIO Int

%foreign "node:lambda:(page, callback) => require('./support/playwright-shim.mjs').stopCoverage(page, callback)"
prim__stopCoverage : Page -> PrimIO String

%foreign "node:lambda:(page, path, callback) => require('./support/playwright-shim.mjs').addScriptTag(page, path, callback)"
prim__addScriptTag : Page -> String -> PrimIO Int

%foreign "node:lambda:(page, html, callback) => require('./support/playwright-shim.mjs').setContent(page, html, callback)"
prim__setContent : Page -> String -> PrimIO Int

%foreign "node:lambda:(page, js, callback) => require('./support/playwright-shim.mjs').evaluate(page, js, callback)"
prim__evaluate : Page -> String -> PrimIO String

%foreign "node:lambda:(browser, callback) => require('./support/playwright-shim.mjs').closeBrowser(browser, callback)"
prim__closeBrowser : Browser -> PrimIO Int

%foreign "node:lambda:(page, url, callback) => require('./support/playwright-shim.mjs').goto(page, url, callback)"
prim__goto : Page -> String -> PrimIO Int

-- =============================================================================
-- Idris IO Wrappers
-- =============================================================================

||| Launch Chromium browser (headless by default)
export
launchBrowser : IO Browser
launchBrowser = primIO prim__launchBrowser

||| Create a new page in the browser
export
newPage : Browser -> IO Page
newPage browser = primIO (prim__newPage browser)

||| Start V8 JS coverage collection
export
startCoverage : Page -> IO ()
startCoverage page = do
  _ <- primIO (prim__startCoverage page)
  pure ()

||| Stop coverage and get raw JSON string
export
stopCoverageRaw : Page -> IO String
stopCoverageRaw page = primIO (prim__stopCoverage page)

||| Add a script tag to the page (loads JS file)
export
addScriptTag : Page -> (path : String) -> IO ()
addScriptTag page path = do
  _ <- primIO (prim__addScriptTag page path)
  pure ()

||| Set page HTML content
export
setContent : Page -> (html : String) -> IO ()
setContent page html = do
  _ <- primIO (prim__setContent page html)
  pure ()

||| Evaluate JavaScript in page context
export
evaluate : Page -> (js : String) -> IO String
evaluate page js = primIO (prim__evaluate page js)

||| Close browser
export
closeBrowser : Browser -> IO ()
closeBrowser browser = do
  _ <- primIO (prim__closeBrowser browser)
  pure ()

||| Navigate to a URL, returning the HTTP response status code (or -1 if the
||| navigation itself failed, e.g. DNS/connection error -- distinct from a
||| genuine HTTP error status like 404, which IS a successful navigation with
||| a non-2xx status).
export
goto : Page -> (url : String) -> IO Int
goto page url = primIO (prim__goto page url)
