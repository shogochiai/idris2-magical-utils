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

## Real device PATH coverage (the actual root cure)

`DeviceProbe`'s testID set is an element-presence check. The REAL path coverage —
dumppaths denominator / on-device JS-trace numerator / exclusions applied — works
like this, now that the forked ES backend emits `__idris2_recordPathHit`:

1. Build the View with the forked compiler: `idris2-rn --cg react-native
   --dumppaths-json paths.json --dumppathshits hits.txt`. The JS bundle now contains
   `__idris2_recordPathHit('<App>.View.view#pN')` at every canonical CaseTree path,
   and `paths.json` is the denominator.
2. Prepend `scripts/device-pathhit-hook.js` to the RN bundle. It installs
   `globalThis.__idris2_recordPathHit` so each path hit while the View runs on the
   device logs `IDRIS_PATHHIT:<id>` (RN → logcat ReactNativeJS).
3. `scripts/run-device-pathcov.sh --package PKG --paths paths.json --module-prefix App`
   launches the app (optionally drives the UI), scrapes the hit ids from logcat as the
   numerator, applies exclusions to the denominator, and prints a parity-ti report:
   `total_paths / covered_paths / Missing paths`. Exit 1 unless Missing paths: 0.

A proof-TextView host never runs the View, so its numerator is empty → 0% → fail.
This is path coverage traced on the real device JS engine — the same shape as the
Chez/Scheme host path coverage, not a testID set.
