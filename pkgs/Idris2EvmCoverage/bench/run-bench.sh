#!/bin/bash
# Benchmark runner for ProfileParser
# Usage: ./run-bench.sh <input_file> [runs]

set -e

INPUT="${1:-bench/fixtures/sample-1k.html}"
RUNS="${2:-3}"

echo "Benchmark: input=$INPUT runs=$RUNS"
echo "Input size: $(wc -c < "$INPUT") bytes"
echo ""

# Build the main executable if needed
if [ ! -f build/exec/idris2-evm-cov ]; then
    echo "Building idris2-evm-cov..."
    idris2 --build idris2-evm-coverage.ipkg
fi

# Create a temporary Idris2 script to run the parser
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

cat > "$TMPDIR/Bench.idr" << 'IDRIS'
module Bench

import System
import System.Clock
import System.File

import EvmCoverage.ProfileParser

getTimeNs : IO Integer
getTimeNs = do
  t <- clockTime Monotonic
  pure $ seconds t * 1000000000 + nanoseconds t

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, inputPath] => do
      Right content <- readFile inputPath
        | Left err => putStrLn $ "Error: " ++ show err
      start <- getTimeNs
      let spans = extractSpans content
      let count = length spans
      -- Force evaluation
      putStr $ show count ++ " spans, "
      end <- getTimeNs
      let elapsed = (end - start) `div` 1000000
      putStrLn $ show elapsed ++ " ms"
    _ => putStrLn "Usage: bench <input_file>"
IDRIS

# Build bench module
echo "Building bench module..."
cd "$TMPDIR"
idris2 -p contrib --source-dir . --source-dir /Users/bob/code/idris2-evm-coverage/src -o bench Bench.idr 2>&1

if [ ! -f build/exec/bench ]; then
    echo "Build failed!"
    exit 1
fi

echo ""
echo "Running $RUNS iterations..."
echo ""

for i in $(seq 1 $RUNS); do
    echo -n "Run $i: "
    ./build/exec/bench "/Users/bob/code/idris2-evm-coverage/$INPUT"
done
