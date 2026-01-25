#!/bin/bash
# Test EVM version compatibility
# Verifies that contracts compile correctly for all supported hardforks
# and that version-specific opcodes are present/absent as expected

cd "$(dirname "$0")/.."

echo "=== EVM Version Compatibility Tests ==="
echo ""

# Build Counter contract first
echo "[Setup] Building Counter contract..."
./build/exec/idris2-evm examples/Counter.idr -o Counter.yul 2>/dev/null
YUL_FILE="build/exec/Counter.yul.yul"

if [ ! -f "$YUL_FILE" ]; then
    echo "ERROR: Failed to generate Yul file"
    exit 1
fi

# Test results
PASSED=0
FAILED=0

# Helper function to run a test
run_test() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    if [ "$expected" = "$actual" ]; then
        echo "[PASS] $name"
        PASSED=$((PASSED + 1))
    else
        echo "[FAIL] $name (expected: $expected, got: $actual)"
        FAILED=$((FAILED + 1))
    fi
}

# Test each EVM version compiles
echo ""
echo "--- Compilation Tests ---"

for version in london paris shanghai cancun prague osaka; do
    BYTECODE=$(solc --strict-assembly --evm-version $version --bin "$YUL_FILE" 2>/dev/null | tail -1)
    if [ -n "$BYTECODE" ] && [ ${#BYTECODE} -gt 100 ]; then
        run_test "$version compiles" "true" "true"
    else
        run_test "$version compiles" "true" "false"
    fi
done

# Test opcode presence
echo ""
echo "--- Opcode Tests ---"

# PUSH0 optimization - Shanghai+ bytecode should be smaller than London
# London uses PUSH1 0x00 (2 bytes), Shanghai+ uses PUSH0 (1 byte)
LONDON_BC=$(solc --strict-assembly --evm-version london --bin "$YUL_FILE" 2>/dev/null | tail -1)
SHANGHAI_BC=$(solc --strict-assembly --evm-version shanghai --bin "$YUL_FILE" 2>/dev/null | tail -1)
LONDON_LEN=${#LONDON_BC}
SHANGHAI_LEN=${#SHANGHAI_BC}

# Shanghai should produce smaller bytecode due to PUSH0
if [ $SHANGHAI_LEN -lt $LONDON_LEN ]; then
    run_test "Shanghai smaller than London (PUSH0)" "true" "true"
else
    run_test "Shanghai smaller than London (PUSH0)" "true" "false (london=$LONDON_LEN, shanghai=$SHANGHAI_LEN)"
fi

# Verify Shanghai bytecode contains 5f (PUSH0 opcode)
if echo "$SHANGHAI_BC" | grep -qi "5f"; then
    run_test "Shanghai has PUSH0 opcode" "true" "true"
else
    run_test "Shanghai has PUSH0 opcode" "true" "false"
fi

# Cancun should still have PUSH0
CANCUN_BC=$(solc --strict-assembly --evm-version cancun --bin "$YUL_FILE" 2>/dev/null | tail -1)
if echo "$CANCUN_BC" | grep -qi "5f"; then
    run_test "Cancun has PUSH0" "true" "true"
else
    run_test "Cancun has PUSH0" "true" "false"
fi

# Prague should have PUSH0
PRAGUE_BC=$(solc --strict-assembly --evm-version prague --bin "$YUL_FILE" 2>/dev/null | tail -1)
if echo "$PRAGUE_BC" | grep -qi "5f"; then
    run_test "Prague has PUSH0" "true" "true"
else
    run_test "Prague has PUSH0" "true" "false"
fi

# Osaka should have PUSH0
OSAKA_BC=$(solc --strict-assembly --evm-version osaka --bin "$YUL_FILE" 2>/dev/null | tail -1)
if echo "$OSAKA_BC" | grep -qi "5f"; then
    run_test "Osaka has PUSH0" "true" "true"
else
    run_test "Osaka has PUSH0" "true" "false"
fi

# Summary
echo ""
echo "=== Results ==="
TOTAL=$((PASSED + FAILED))
echo "Passed: $PASSED/$TOTAL"

if [ $FAILED -gt 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
else
    echo "ALL TESTS PASSED"
    exit 0
fi
