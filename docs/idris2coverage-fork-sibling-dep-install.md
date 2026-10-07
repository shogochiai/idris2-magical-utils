# idris2-coverage: fork local-dep install and sibling-ipkg discovery

`installNeededDepsIntoFork` (in `pkgs/Idris2Coverage/src/Coverage/UnifiedRunner.idr`)
installs a measured package's local deps into the FORKED compiler's package path,
because a direct `idris2 --build` (the only path that honours `--dumppaths-json`)
resolves deps from `~/.idris2` / `./depends` — not `pack.toml` / pack's store.

A package that depends on ANOTHER package of the same project could not be
path-measured unless that sibling was registered as a `type = "local"` entry in a
`pack.toml` (ledger failure type 94). The fix is sibling-ipkg discovery
(REQ_COV_FORK_DEPS_SIBLING_001):

- `ipkgPackageName : String -> Maybe String` reads a package name from an ipkg's
  `package <name>` line (pure; `Nothing` when there is no such line).
- `missingDepNames` drops wanted names already provided by a `pack.toml` local
  entry, so a name is never discovered twice.
- `siblingDepEntries : List String -> List (String, String, String) ->
  List (String, String, String)` matches `(dir, ipkgFile, content)` candidates to
  the wanted names, deduped by name.
- `discoverSiblingDepEntries` is the IO side: it lists and reads every `*.ipkg`
  under the measured package's git top-level (else its parents up to 3 levels),
  skipping `build/`, `.git/`, `.luci/` and `node_modules/`, and returns one
  `(name, absolute dir, ipkg file)` entry per wanted sibling — exactly like a
  `pack.toml` local entry.

Each install prints ONE stderr line per dependency, every run:

```
[dep-install] <name> source=<pack.toml|sibling:<dir>/<ipkg>> compiler=<idris2> libdir=<libdir> result=<installed|FAILED> log=<path>
```

where `libdir` is the output of `<idris2> --libdir`. Before the instrumented build
starts, every non-bundled wanted dependency is checked for visibility
(`<libdir>/<name>-*` must exist); a miss is reported as
`[dep-install] NOT VISIBLE to the instrumented build: <name>`, so a later
`Required <name> ... no matching version is installed` is never the first sign.
