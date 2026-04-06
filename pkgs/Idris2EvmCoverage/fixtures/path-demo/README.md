Run:

```bash
cd /Users/bob/code/idris2-magical-utils
./pkgs/Idris2EvmCoverage/build/exec/idris2-evm-cov paths \
  --dumppaths-json ./pkgs/Idris2EvmCoverage/fixtures/path-demo/golden.paths.json \
  --path-hits ./pkgs/Idris2EvmCoverage/fixtures/path-demo/sample.path-hits.txt
```

Expected output is in [expected.txt](./expected.txt).
