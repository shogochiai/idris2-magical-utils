#!/bin/bash
# Detect potential exclusion leaks after Idris2 update
# Usage: ./scripts/detect-leaks.sh [project_path] [top_n]

PROJECT=${1:-.}
TOP=${2:-1000}

echo "Analyzing $PROJECT with top $TOP targets..."
echo ""

# Run analysis and capture only JSON (filter out build messages)
# sed extracts from first '{' to end of output
JSON_OUTPUT=$(./build/exec/idris2-cov --json --top $TOP "$PROJECT" 2>&1 | \
  sed -n '/^{/,$p')

if [ -z "$JSON_OUTPUT" ]; then
  echo "ERROR: No JSON output from idris2-cov"
  exit 2
fi

LEAKS=$(echo "$JSON_OUTPUT" | \
  jq -r '.high_impact_targets[].funcName' 2>/dev/null | \
  grep -E '^(\{|_builtin\.|prim__|Prelude\.|Data\.|System\.|Control\.|Decidable\.|Language\.|Debug\.)' | \
  sort -u)

if [ -n "$LEAKS" ]; then
  echo "LEAKS DETECTED:"
  echo "$LEAKS" | while read func; do
    echo "  - $func"
  done
  echo ""
  LEAK_COUNT=$(echo "$LEAKS" | wc -l | tr -d ' ')
  echo "Found $LEAK_COUNT potential leaks. Update exclusion patterns in:"
  echo "  src/Coverage/DumpcasesParser.idr"
  exit 1
else
  echo "No leaks detected."
  exit 0
fi
