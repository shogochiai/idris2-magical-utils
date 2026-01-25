#!/bin/bash
# Test Counter contract using Foundry's cast

set -e

cd "$(dirname "$0")/.."

# Build the contract
echo "=== Building Counter contract ==="
./build/exec/idris2-evm examples/Counter.idr -o Counter.yul

# Get bytecode
BYTECODE=$(solc --strict-assembly --evm-version cancun --bin build/exec/Counter.yul.yul 2>/dev/null | tail -1)
echo "Bytecode length: ${#BYTECODE} chars"

# Kill any existing anvil
pkill -f "anvil" 2>/dev/null || true
sleep 1

# Start anvil in background
echo ""
echo "=== Starting anvil ==="
anvil --silent &
ANVIL_PID=$!
sleep 2

cleanup() {
    kill $ANVIL_PID 2>/dev/null || true
}
trap cleanup EXIT

# Deploy contract
echo ""
echo "=== Deploying contract ==="
DEPLOY_TX=$(cast send --rpc-url http://localhost:8545 --private-key 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80 --json --create "0x${BYTECODE}" 2>&1) || {
    echo "Deploy failed: $DEPLOY_TX"
    exit 1
}

CONTRACT=$(echo "$DEPLOY_TX" | jq -r '.contractAddress // empty')
if [ -z "$CONTRACT" ]; then
    echo "Failed to get contract address"
    echo "$DEPLOY_TX"
    exit 1
fi
echo "Contract deployed at: $CONTRACT"

# Check contract code
echo ""
echo "=== Checking deployed code ==="
CODE=$(cast code $CONTRACT --rpc-url http://localhost:8545)
echo "Deployed code length: ${#CODE} chars"

# Test getCount() - selector 0xa87d942c
echo ""
echo "=== Testing getCount() ==="
RESULT=$(cast call $CONTRACT "0xa87d942c" --rpc-url http://localhost:8545 2>&1) || {
    echo "getCount failed: $RESULT"
}
echo "getCount result: $RESULT"
echo "Decoded: $(cast --to-dec $RESULT 2>/dev/null || echo 'N/A')"

# Test increment() - selector 0xd09de08a
echo ""
echo "=== Testing increment() ==="
INC_TX=$(cast send --rpc-url http://localhost:8545 --private-key 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80 --json $CONTRACT "0xd09de08a" 2>&1) || {
    echo "increment failed: $INC_TX"
}
echo "increment status: $(echo "$INC_TX" | jq -r '.status')"

# Test getCount() again
echo ""
echo "=== Testing getCount() after increment ==="
RESULT=$(cast call $CONTRACT "0xa87d942c" --rpc-url http://localhost:8545 2>&1) || {
    echo "getCount failed: $RESULT"
}
echo "getCount result: $RESULT"
echo "Decoded: $(cast --to-dec $RESULT 2>/dev/null || echo 'N/A')"

# Test setCount(42) - selector 0xd14e62b8
echo ""
echo "=== Testing setCount(42) ==="
# ABI encode: selector + uint256(42)
SETCOUNT_DATA="0xd14e62b8000000000000000000000000000000000000000000000000000000000000002a"
SET_TX=$(cast send --rpc-url http://localhost:8545 --private-key 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80 --json $CONTRACT "$SETCOUNT_DATA" 2>&1) || {
    echo "setCount failed: $SET_TX"
}
echo "setCount status: $(echo "$SET_TX" | jq -r '.status')"

# Test getCount() after setCount
echo ""
echo "=== Testing getCount() after setCount(42) ==="
RESULT=$(cast call $CONTRACT "0xa87d942c" --rpc-url http://localhost:8545 2>&1)
echo "getCount result: $RESULT"
echo "Decoded: $(cast --to-dec $RESULT 2>/dev/null || echo 'N/A')"

# Test decrement() - selector 0x2baeceb7
echo ""
echo "=== Testing decrement() ==="
DEC_TX=$(cast send --rpc-url http://localhost:8545 --private-key 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80 --json $CONTRACT "0x2baeceb7" 2>&1) || {
    echo "decrement failed: $DEC_TX"
}
echo "decrement status: $(echo "$DEC_TX" | jq -r '.status')"

# Test getCount() after decrement
echo ""
echo "=== Testing getCount() after decrement ==="
RESULT=$(cast call $CONTRACT "0xa87d942c" --rpc-url http://localhost:8545 2>&1)
echo "getCount result: $RESULT"
echo "Decoded: $(cast --to-dec $RESULT 2>/dev/null || echo 'N/A')"

echo ""
echo "=== All tests complete ==="
