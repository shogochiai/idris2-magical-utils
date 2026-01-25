# Exclusion Vocabulary by Idris2 Version

This directory contains exclusion patterns organized by Idris2 semver patch version.

## Directory Structure

```
exclusions/
├── README.md           # This file
├── base.txt            # Patterns common to ALL versions
└── 0.8.0.txt           # Patterns specific to Idris2 0.8.0
```

## File Format

Each `.txt` file contains one pattern per line:

```
# Comments start with #
{csegen:*}              # Compiler-generated: common subexpression
{eta:*}                 # Compiler-generated: eta expansion
prim__*                 # Primitives
Prelude.*               # Standard library prefix
Data.*                  # Standard library prefix
```

Pattern types:
- `prefix*` - Matches any name starting with `prefix`
- Exact match otherwise

## Contributing

When you run `idris2-cov --report-leak`, the script will:
1. Detect your Idris2 version (e.g., `0.8.0`)
2. Create/update `exclusions/<version>.txt`
3. Open a PR with the new patterns

## Loading Order

1. `base.txt` is loaded first (always)
2. Version-specific file is loaded (e.g., `0.8.0.txt`)
3. Patterns are merged (union)
