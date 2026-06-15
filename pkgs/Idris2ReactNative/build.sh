#!/usr/bin/env bash
# Build the idris2-react-native PoC end to end.
#
# Produces poc-app/ : a React Native project whose UI + logic come entirely
# from Idris2 (compiled to JS via the `react-native` codegen), with the
# scaffold itself emitted by RN.Scaffold. No hand-written app JS/TS.
#
# Stages 1-4 are environment-light (node only). Stage 5 (iOS simulator)
# additionally needs CocoaPods + Xcode and is run separately.
set -euo pipefail

PKG="$(cd "$(dirname "$0")" && pwd)"
IDRIS2_RN="$PKG/build/exec/idris2-rn"
APP="${1:-$PKG/poc-app}"

echo "==> 1. Build the react-native backend (idris2-rn)"
pack build "$PKG/idris2-react-native.ipkg"

echo "==> 2. Install the RN library (RN.Node / RN.Runtime / RN.Scaffold)"
idris2 --install "$PKG/idris2-react-native-lib.ipkg"

echo "==> 3. Compile the MVU demo to a JS bundle with --cg react-native"
( cd "$PKG/demo" && "$IDRIS2_RN" -p idris2-react-native-lib --cg react-native \
    -o rn-demo.js src/Main.idr )

echo "==> 4. Generate the RN scaffold from Idris2 and assemble poc-app/"
( cd "$PKG/generator" && idris2 --build gen.ipkg && ./build/exec/gen "$APP" )
cp "$PKG/rn-runtime/idris-rn-shim.js"   "$APP/idris-rn-shim.js"
cp "$PKG/demo/build/exec/rn-demo.js"    "$APP/idris-bundle.js"

echo "==> Done. poc-app assembled at: $APP"
echo "    Stage 5 (needs CocoaPods + Xcode):"
echo "      cd '$APP' && npm install && (cd ios && pod install) && npx react-native run-ios"
