Run:

```bash
cd /Users/bob/code/idris2-magical-utils
./pkgs/Idris2DfxCoverage/build/exec/idris2-dfx-cov paths \
  --dumppaths-json ./pkgs/Idris2DfxCoverage/fixtures/path-demo/golden.paths.json \
  --path-hits ./pkgs/Idris2DfxCoverage/fixtures/path-demo/sample.path-hits.txt
```

Expected output is in [expected.txt](./expected.txt).
