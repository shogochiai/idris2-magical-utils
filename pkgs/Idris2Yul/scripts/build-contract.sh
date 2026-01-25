#!/bin/bash
# Build Idris2 contract to EVM bytecode
# Usage: ./scripts/build-contract.sh <source.idr> [evm-version]
#
# EVM Versions (hardfork compatibility):
#   osaka     - Fusaka upgrade (December 2025) - PeerDAS, EOF
#   prague    - Pectra upgrade (May 2025) - EIP-7702 account abstraction
#   cancun    - Dencun upgrade (March 2024) - TLOAD/TSTORE, blobs
#   shanghai  - Shanghai upgrade (April 2023) - PUSH0
#   paris     - The Merge (September 2022)
#   london    - EIP-1559 (August 2021)

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <source.idr> [evm-version]"
    echo ""
    echo "EVM versions: cancun (default), shanghai, paris, london"
    exit 1
fi

SOURCE="$1"
EVM_VERSION="${2:-osaka}"  # Default to osaka (Fusaka - latest mainnet)
BASENAME=$(basename "$SOURCE" .idr)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

echo "=== Building $BASENAME ==="
echo "EVM Version: $EVM_VERSION"
echo ""

# Step 1: Idris2 -> Yul
echo "[1/3] Compiling Idris2 to Yul..."

# Build idris2-subcontract if not built
if [ ! -d "/Users/bob/code/idris2-subcontract/build/ttc" ]; then
  echo "Building idris2-subcontract..."
  (cd /Users/bob/code/idris2-subcontract && idris2 --build idris2-subcontract.ipkg)
fi

# Build idris2-yul if not built
if [ ! -f "./build/exec/idris2-yul" ]; then
  echo "Building idris2-yul..."
  idris2 --build idris2-yul.ipkg
fi

# Set package path
export IDRIS2_PACKAGE_PATH="/Users/bob/code/idris2-yul/depends:/Users/bob/code/idris2-subcontract/build/ttc:${IDRIS2_PACKAGE_PATH:-}"

# Check if source needs idris2-subcontract package
if grep -q "import.*Subcontract\|import.*MC\." "$SOURCE" 2>/dev/null; then
  ./build/exec/idris2-yul -p idris2-subcontract "$SOURCE" -o "$BASENAME.yul"
else
  ./build/exec/idris2-yul "$SOURCE" -o "$BASENAME.yul"
fi

YUL_FILE="build/exec/${BASENAME}.yul.yul"
if [ ! -f "$YUL_FILE" ]; then
    echo "Error: Yul file not generated"
    exit 1
fi

# Step 2: Yul -> EVM bytecode
echo "[2/3] Compiling Yul to EVM bytecode (--evm-version $EVM_VERSION)..."
BYTECODE=$(solc --strict-assembly --evm-version "$EVM_VERSION" --bin "$YUL_FILE" 2>&1 | tail -1)

if [ -z "$BYTECODE" ] || [ "$BYTECODE" = "=======" ]; then
    echo "Error: Failed to compile Yul"
    solc --strict-assembly --evm-version "$EVM_VERSION" "$YUL_FILE"
    exit 1
fi

# Step 3: Save outputs
echo "[3/3] Saving outputs..."
mkdir -p "build/output"
echo "$BYTECODE" > "build/output/${BASENAME}.bin"
cp "$YUL_FILE" "build/output/${BASENAME}.yul"

# Also generate runtime-only bytecode (for verification)
RUNTIME_BYTECODE=$(solc --strict-assembly --evm-version "$EVM_VERSION" --bin-runtime "$YUL_FILE" 2>&1 | tail -1)
echo "$RUNTIME_BYTECODE" > "build/output/${BASENAME}.bin-runtime"

echo ""
echo "=== Build Complete ==="
echo "Yul:      build/output/${BASENAME}.yul"
echo "Bytecode: build/output/${BASENAME}.bin (${#BYTECODE} chars)"
echo "Runtime:  build/output/${BASENAME}.bin-runtime"
echo ""
echo "Deploy with:"
echo "  cast send --create \$(cat build/output/${BASENAME}.bin) --private-key <KEY> --rpc-url <RPC>"
