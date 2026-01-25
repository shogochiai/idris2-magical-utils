# Getting Started

This guide walks you through installing and running idris2-coverage for the first time.

## Prerequisites

- **Idris2** 0.7.0 or later
- **Chez Scheme** (usually installed with Idris2)
- A project with an `.ipkg` file

## Installation

### From Source

```bash
git clone https://github.com/yourname/idris2-coverage.git
cd idris2-coverage
idris2 --build idris2-coverage.ipkg
```

The binary is created at `./build/exec/idris2-cov`.

### Optional: Install Globally

```bash
# Add to your PATH, or:
cp ./build/exec/idris2-cov ~/.local/bin/
```

## Your First Analysis

### 1. Navigate to Your Project

```bash
cd path/to/your/idris2/project
```

### 2. Run Coverage Analysis

```bash
# Analyze current directory (finds .ipkg automatically)
idris2-cov .

# Or specify the ipkg explicitly
idris2-cov myproject.ipkg
```

### 3. Understand the Output

```
=== Branch Classification ===
canonical:           2883    # Branches you should test
excluded_void:         45    # Provably unreachable (void/absurd)
bugs:                   3    # Partial functions (missing cases)
optimizer_artifacts:   12    # Compiler artifacts (ignore)
unknown:                0    # Unclassified (investigate)

=== High Impact Targets ===
1. MyModule.parseConfig        (15 branches, 0 tested)
2. MyModule.validateInput      (12 branches, 2 tested)
3. MyModule.processData        (8 branches, 3 tested)
```

## Common Options

```bash
# Show only uncovered functions
idris2-cov --uncovered .

# JSON output (for CI/scripts)
idris2-cov --json .

# Limit displayed targets
idris2-cov --top 10 .

# Version info
idris2-cov --version
```

## Project Configuration

Create `.idris2-cov.toml` in your project root:

```toml
[exclusions]
# Exclude internal modules from targets
module_prefixes = ["MyProject.Internal."]

# Exclude additional packages
packages = ["contrib"]
```

See [User Guide](USER_GUIDE.md) for all configuration options.

## Next Steps

- [User Guide](USER_GUIDE.md) - Full CLI reference
- [FAQ](FAQ.md) - Common questions
- [Contributing](CONTRIBUTING.md) - Help improve the tool

## Troubleshooting

### "No .ipkg file found"

Ensure you're in a directory with an `.ipkg` file, or specify the path explicitly:

```bash
idris2-cov path/to/project.ipkg
```

### "idris2 command not found"

Ensure Idris2 is installed and in your PATH:

```bash
idris2 --version
```

### Build Errors

Try rebuilding from clean:

```bash
rm -rf build/
idris2 --build idris2-coverage.ipkg
```
