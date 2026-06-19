# Idris2AndroidCoverage

Device-driven UI coverage for `family=android` MVU apps — the root cure for the
**acceptance-hack** class of bugs, where a build ships a real `View.idr` + bundle but
hosts it in a way that never renders the spec screen (a plain Activity painting a
"proof" TextView, a placeholder bundle, hard-coded JSX), yet passes
`unzip|grep idris-bundle.js`, `adb install`, and `grep "extends ReactActivity"`.

- `Android.Coverage.DeviceProbe` (pure, total): a View is an `RN.Node` tree whose
  `testID`s are its observable surface; a `DeviceScenario` is the steps a device
  driver runs; `deviceGapZero` requires every declared testID to be driven.
- `scripts/run-device-e2e.sh`: the device half — `adb shell uiautomator dump`
  finds each testID as a resource-id/content-desc; a hollow host exposes none and
  fails with `Missing paths: N`, exit 1.

Wire `run-device-e2e.sh` into the android parity-ti surface so the screen the View
DECLARES must actually appear on a real device — making the hack unrepresentable.
