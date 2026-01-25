# Frequently Asked Questions

## General

### What is idris2-coverage?

A test coverage tool for Idris2 that understands dependent types. Unlike traditional coverage tools, it excludes provably unreachable branches from your coverage denominator.

### Why can't I use a regular coverage tool?

Traditional coverage tools count all branches equally. In Idris2:

```idris
safeHead : (xs : List a) -> NonEmpty xs -> a
safeHead (x :: _) _ = x
-- The empty list case is provably impossible
```

A naive tool would report 50% coverage (1 of 2 cases). idris2-coverage correctly reports 100% because the empty case is unreachable by construction.

### Is this a compiler modification?

No. idris2-coverage is a **downstream tool** that analyzes existing Idris2 outputs (`--dumpcases` and Chez Scheme profiler). It requires no changes to the Idris2 compiler.

## Coverage Metrics

### Why is my coverage percentage low?

Common reasons:

1. **Untested code paths**: Functions that aren't exercised by tests
2. **Standard library included**: Check if stdlib functions appear in targets
3. **Generated code**: Compiler-generated functions may be counted

Solutions:
- Use `--uncovered` to focus on gaps
- Add exclusions in `.idris2-cov.toml`
- Write more tests for high-impact targets

### What does "bugs: N" mean in the output?

It means N functions have partial patterns (unhandled cases). Example:

```idris
process : Command -> IO ()
process Help = printHelp
process Version = printVersion
-- Missing: process (Unknown s) = ???
```

The compiler inserts `CRASH "Unhandled input for..."` for the missing case. This is a real bug you should fix.

### Why are some branches excluded?

Branches are excluded when they're provably unreachable:

| Exclusion | Reason |
|-----------|--------|
| `void`/`absurd` patterns | Type system proves impossibility |
| `Nat case not covered` | Optimizer artifact (Nat → Integer) |
| `No clauses in ...` | Empty type with no constructors |

### Can I get 100% coverage?

Yes! That's the point. By excluding impossible branches, 100% becomes achievable if you test all reachable code paths.

## Configuration

### How do I exclude internal modules?

Create `.idris2-cov.toml`:

```toml
[exclusions]
module_prefixes = ["MyProject.Internal.", "MyProject.Generated."]
```

### Why are standard library functions in my targets?

This shouldn't happen by default. If you see `Prelude.*` or `Data.*` functions:

1. Ensure you're using the latest version
2. Report the issue with your JSON output

### How do I exclude a specific package?

```toml
[exclusions]
packages = ["contrib", "network"]
```

Note: Packages from your ipkg's `depends` are auto-excluded.

## Technical

### How does it detect unreachable branches?

By parsing `--dumpcases` output and classifying CRASH nodes:

| CRASH Message | Classification |
|--------------|----------------|
| `"No clauses in..."` | Void/absurd - excluded |
| `"Unhandled input for..."` | Bug - counted |
| `"Nat case not covered"` | Optimizer - excluded |
| Other | Unknown - counted (conservative) |

### Why use Chez Scheme profiler?

It's already available with standard Idris2 installations and provides expression-level execution counts. We map these back to Idris functions.

### How accurate is the runtime hit detection?

Per-function accuracy is approximately 62% match rate. Unmatched functions fall back to proportional estimation. Standard library and compiler-generated functions are correctly excluded.

### What Idris2 versions are supported?

- **Minimum**: 0.7.0
- **Tested**: 0.7.0, 0.8.0

Different Idris2 versions may generate different compiler patterns. Report issues if you see unexpected "leaks" in your targets.

## Troubleshooting

### "No .ipkg file found"

The tool needs an ipkg to know which modules to analyze. Either:
- Run from a directory containing an `.ipkg` file
- Specify the path: `idris2-cov path/to/project.ipkg`

### Analysis takes too long

For large projects:
1. Use `--top 20` to limit output
2. Exclude test/example modules via config
3. Ensure you're not analyzing dependencies

### JSON output is malformed

Likely a bug. Please report with:
1. Your Idris2 version
2. The command you ran
3. The (partial) output

### Coverage differs between runs

This can happen if:
1. Tests have non-deterministic behavior
2. Profiler sampling is inconsistent (rare)

For CI, consider averaging multiple runs or using a threshold with margin.

## Contributing

### How do I report a bug?

Open an issue at https://github.com/yourname/idris2-coverage/issues with:
- Idris2 version (`idris2 --version`)
- idris2-cov version (`idris2-cov --version`)
- Minimal reproduction case

### How do I add new exclusion patterns?

See [Contributing](CONTRIBUTING.md) for the workflow using `./scripts/detect-leaks.sh`.

### Can I use this for my own language/compiler?

The architecture is specific to Idris2's `--dumpcases` output format, but the concepts (proof-aware coverage, CRASH classification) could be adapted to other dependently-typed languages.
