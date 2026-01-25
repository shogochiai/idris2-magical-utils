# idris2-web-coverage

V8 coverage for Idris2 dom-mvc apps with source map support.

## Prerequisites

1. Idris2 with source map support (feature/es-source-maps branch)
2. Node.js 18+
3. Playwright

## Installation

```bash
npm install
npx playwright install chromium
```

## Usage

```bash
# Run coverage on a compiled Idris2 JS file
./bin/idris2-web-cov.mjs path/to/app.js

# Or on a project directory (looks in build/exec/)
./bin/idris2-web-cov.mjs path/to/project/

# Use Node.js instead of browser (for non-DOM apps)
./bin/idris2-web-cov.mjs --node path/to/app.js

# Show uncovered lines
./bin/idris2-web-cov.mjs --uncovered path/to/app.js

# JSON output for CI
./bin/idris2-web-cov.mjs --json path/to/app.js

# For dom-mvc apps, specify HTML file or URL
./bin/idris2-web-cov.mjs --html index.html path/to/app.js
./bin/idris2-web-cov.mjs --url http://localhost:8080 path/to/app.js
```

## Compiling Idris2 with Source Maps

```bash
idris2 --cg node --directive sourcemap -o myapp MyApp.idr
```

This generates:
- `build/exec/myapp` - JavaScript file
- `build/exec/myapp.map` - Source Map v3

## Integration with LazyWeb

This tool is designed to be called from LazyWeb Step 4 (TestAndCoverage):

```bash
lazy web ask path/to/dom-mvc-project --steps=4
```

## Output Format

Compatible with idris2-coverage for unified tooling:

```
# Web Coverage Report
Coverage: 45/60 (75%)
Functions: 12/20

## User Files
  ✓ MyApp.idr: 15/15 (100%)
  ◐ Components/Button.idr: 8/12 (66%)
    Uncovered lines: 23, 45, 67

## Prelude/Library: 22/33 (66%)
```

## JSON Output

```json
{
  "timestamp": 1234567890,
  "coverage": {
    "lines": { "covered": 45, "total": 60, "percentage": 75 },
    "functions": { "covered": 12, "total": 20 }
  },
  "files": [...]
}
```
