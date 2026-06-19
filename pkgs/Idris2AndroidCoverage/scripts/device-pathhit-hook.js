// device-pathhit-hook.js — the numerator collector for real device path coverage.
//
// PREPEND this to the RN/Hermes bundle (before idris-bundle.js loads). The forked
// ES backend, when the View was built with --dumppathshits, emits calls to
// globalThis.__idris2_recordPathHit('<path_id>') at every canonical CaseTree path.
// This hook installs that global so each path id hit WHILE THE VIEW RUNS ON THE
// DEVICE is logged to logcat (RN routes console.log to the ReactNativeJS tag), where
// run-device-pathcov.sh scrapes it as the coverage NUMERATOR. The path ids match the
// --dumppaths-json DENOMINATOR exactly, so device path coverage = |hits ∩ denom| /
// |denom \ exclusions| — the same path-coverage shape as the Chez/Scheme backend, but
// traced on the device JS engine instead of a host test run.
//
// Idempotent and production-safe: if a bundle without instrumentation loads this, the
// hook simply never fires. De-duplicates ids and prints each once to keep logcat light.
(function () {
  if (globalThis.__idris2_recordPathHit) return; // already installed
  var seen = Object.create(null);
  globalThis.__idris2_recordPathHit = function (id) {
    if (!seen[id]) {
      seen[id] = true;
      // single, greppable line per distinct path id
      try { console.log('IDRIS_PATHHIT:' + id); } catch (e) {}
    }
    return 0;
  };
  // marker so the scraper can confirm the hook was actually installed on-device
  try { console.log('IDRIS_PATHHIT_HOOK_INSTALLED'); } catch (e) {}
})();
