# idris2-playwright

Minimal Playwright bindings for Idris2, designed for browser-based coverage collection.

## Purpose

Enable V8 coverage collection for Idris2 projects that compile to JavaScript and run in the browser (e.g., idris2-dom-mvc).

## Installation

```bash
# Install npm dependencies
npm install

# Build Idris2 package
pack build idris2-playwright.ipkg
```

## Usage

```idris
import Playwright.Coverage

main : IO ()
main = do
  result <- runDomTestWithCoverage "./build/exec/my-test.js"
  case result of
    Left err => putStrLn $ "Error: " ++ err
    Right cov => putStrLn $ "Coverage: " ++ show (cov.coverageRatio * 100) ++ "%"
```

## API

### Core Functions

- `launchBrowser : IO Browser` - Launch headless Chromium
- `newPage : Browser -> IO Page` - Create browser page
- `startCoverage : Page -> IO ()` - Start V8 coverage collection
- `stopCoverageRaw : Page -> IO String` - Stop and get coverage JSON
- `addScriptTag : Page -> String -> IO ()` - Load JS file in page
- `closeBrowser : Browser -> IO ()` - Close browser

### High-Level API

- `collectBrowserCoverage : CoverageConfig -> IO (Either String CoverageResult)`
- `runDomTestWithCoverage : String -> IO (Either String CoverageResult)`
- `getCoverageRatio : String -> IO (Either String Double)`

## Requirements

- Idris2 with Node.js backend (`--cg node`)
- Node.js 18+
- Playwright (installed via npm)

## License

MIT
