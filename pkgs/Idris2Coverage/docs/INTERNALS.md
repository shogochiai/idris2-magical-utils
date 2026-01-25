# Internals

Deep technical details for contributors and curious users.

## CRASH Classification

### Background

Idris2's `--dumpcases` output contains CRASH nodes for various reasons. The key insight (from dunham's community discussion) is that **not all CRASHes are equal**:

> downstream consumers probably shouldn't treat all CRASH cases equivalently

### Classification Table

| CRASH Message | Type | Treatment | Rationale |
|---------------|------|-----------|-----------|
| `"No clauses in ..."` | `CrashNoClauses` | **Excluded** | Empty type, provably unreachable |
| `"Unhandled input for ..."` | `CrashUnhandledInput` | **Bug** | Partial function, missing case |
| `"Nat case not covered"` | `CrashOptimizerNat` | **Excluded** | Optimizer artifact from Nat→Integer |
| Other | `CrashUnknown` | **Counted** | Conservative: might be a real bug |

### Implementation

```idris
-- In DumpcasesParser.idr
classifyCrash : String -> CrashReason
classifyCrash msg =
  if isInfixOf "No clauses in" msg
     then CrashNoClauses
  else if isInfixOf "Unhandled input for" msg
     then CrashUnhandledInput
  else if isInfixOf "Nat case not covered" msg
     then CrashOptimizerNat
  else CrashUnknown msg
```

### Examples

**CrashNoClauses** (void pattern):
```idris
absurd : Void -> a
absurd v impossible
-- --dumpcases: (CRASH "No clauses in Coverage.Example.absurd")
```

**CrashUnhandledInput** (partial function):
```idris
unsafeHead : List a -> a
unsafeHead (x :: _) = x
-- --dumpcases: (CRASH "Unhandled input for Coverage.Example.unsafeHead")
```

**CrashOptimizerNat** (optimization artifact):
```idris
-- When pattern matching on Nat, the compiler optimizes to Integer
-- but leaves a CRASH for the "impossible" negative case
-- --dumpcases: (CRASH "Nat case not covered in ...")
```

## Compiler-Generated Patterns

### Machine-Generated Names (MN)

Idris2 generates internal names with format `{prefix:N}`:

| Prefix | Description | Source |
|--------|-------------|--------|
| `csegen` | Common Subexpression Elimination | `Compiler/Opts/CSE.idr` |
| `act` | Lambda lift actions | `Compiler/LambdaLift.idr` |
| `arg` | Builtin operation arguments | `Compiler/CompileExpr.idr` |
| `eta` | Eta expansion variables | `Compiler/CompileExpr.idr` |
| `eff` | Effect bindings (newtype) | `Compiler/CompileExpr.idr` |
| `ext` | External function inlining | `Compiler/Inline.idr` |
| `bind` | Bind expression variables | (internal) |
| `clam` | Case lambda variables | (internal) |
| `lamc` | Lambda case variables | (internal) |
| `e` | Generic expression variables | (internal) |
| `x` | Generic variable binding | `Compiler/CompileExpr.idr` |

### Special Names

| Pattern | Description |
|---------|-------------|
| `{__mainExpression:0}` | Program entry point |
| `{__leftTupleSection:N}` | Left tuple section syntax |
| `{__infixTupleSection:N}` | Infix tuple section syntax |

### Builtin Constructors

| Pattern | Description |
|---------|-------------|
| `_builtin.NIL` | Empty list `[]` |
| `_builtin.CONS` | List cons `(::)` |
| `_builtin.NOTHING` | `Nothing` constructor |
| `_builtin.JUST` | `Just` constructor |

### Type Constructors

Names ending with `.` are **data constructor case trees**:

```
Coverage.Types.MkBranchId.    -- Constructor for BranchId
Prelude.Types.List.           -- Constructor for List
```

These are not user-defined functions.

## Name Mangling (Chez Scheme)

### Algorithm

Idris2 mangles names for Chez Scheme compatibility:

```idris
chezEncodeChar : Char -> String
chezEncodeChar c =
  if isAlphaNum c || c == '_'
     then singleton c
     else "C-" ++ show (ord c)

chezMangle : String -> String
chezMangle name =
  let segments = split (== '.') name
  in fastConcat $ intersperse "-" $ map chezEncodeString segments
```

### Examples

| Idris Name | Scheme Name |
|------------|-------------|
| `Sample.add` | `Sample-add` |
| `Prelude.IO.putStrLn` | `PreludeC-45IO-putStrLn` |
| `Prelude.EqOrd.==` | `PreludeC-45EqOrd-C-61C-61` |
| `Data.List.filter` | `DataC-45List-filter` |

### Common Encodings

| Character | ASCII | Encoding |
|-----------|-------|----------|
| `-` (hyphen) | 45 | `C-45` |
| `.` (dot) | 46 | `C-46` |
| `=` | 61 | `C-61` |
| `>` | 62 | `C-62` |
| `<` | 60 | `C-60` |
| `+` | 43 | `C-43` |
| `*` | 42 | `C-42` |

### Matching Strategy

We use suffix matching with the mangled name:

```idris
findMatchingScheme : String -> List (String, Nat) -> Maybe (String, Nat)
findMatchingScheme expected defs =
  find (\(name, _) => name == expected || isSuffixOf expected name) defs
```

This handles both exact matches and cases where the Scheme name has additional prefixes.

## Profiler Output Format

### Chez Scheme Profiler

The `.ss.html` file contains spans with execution counts:

```html
<span class=pc4 title="line 747 char 77 count 6">(case expr ...)</span>
```

| Attribute | Meaning |
|-----------|---------|
| `class=pcN` | Heat level (0-12) for coloring |
| `line N` | Line number in .ss file |
| `char N` | Character position |
| `count N` | Execution count (0 = never executed) |

### Parsing

```idris
extractSpanInfo : String -> Maybe SpanInfo
-- Extracts line, char, count from title attribute

groupByFunction : List SpanInfo -> List (String, Nat) -> List FunctionCoverage
-- Groups spans by function based on line ranges from (define ...) parsing
```

## Coverage Formula

### Pragmatic Coverage

```
PragmaticCoverage = executed_canonical / total_canonical × 100%
```

Where:
- `total_canonical` = BCCanonical count from --dumpcases
- `executed_canonical` = Canonical branches with runtime hits

### Exclusions from Denominator

```
total_canonical = all_branches
                - BCExcludedNoClauses
                - BCOptimizerNat
```

**Not excluded** (conservative):
- `BCBugUnhandledInput` — real bugs, should be fixed
- `BCUnknownCrash` — might be bugs, investigate

## Exclusion File Format

Files in `exclusions/`:

```
# Comments start with #
{csegen:*}              # Wildcard: matches {csegen:0}, {csegen:42}, etc.
prim__cast_Integer_Int  # Exact match
Prelude.*               # Prefix wildcard
```

### Loading Order

1. `base.txt` — always loaded
2. `<version>.txt` — loaded if matches Idris2 version

Patterns are merged (union).

## Version Compatibility

### Tracking New Patterns

When Idris2 updates, new compiler-generated patterns may appear:

```bash
# Detect new patterns
./scripts/detect-leaks.sh path/to/project 1000

# Output:
# LEAKS DETECTED:
#   - {newpattern:42}
#   - SomeNew.Module.func
```

### Update Workflow

1. Identify pattern type (MN, stdlib, etc.)
2. Add to `exclusions/<version>.txt` or update code
3. Release new idris2-coverage version

### Compatibility Matrix

| idris2-coverage | Idris2 | Notes |
|-----------------|--------|-------|
| 0.1.x | 0.7.0+ | Initial release |

## Performance Considerations

### Large Projects

For projects with thousands of functions:

1. **Parsing**: --dumpcases output can be large; we stream-parse
2. **Profiling**: Chez profiler adds ~10-20% overhead
3. **Matching**: O(n×m) where n=Idris functions, m=Scheme definitions

### Optimizations

- Use `--top N` to limit target processing
- Exclusion checks are short-circuit evaluated
- Pattern matching uses prefix/suffix for efficiency

## Debugging

### Verbose Parsing

To debug --dumpcases parsing:

```bash
idris2 --dumpcases myproject.ipkg > dumpcases.txt
# Examine dumpcases.txt manually
```

### Profiler Issues

To debug profiler mapping:

```bash
# Keep intermediate files
ls build/exec/*_app/*.ss.html
# Examine profiler output manually
```

### Match Failures

If functions aren't matching:

1. Check mangled name: `chezMangle "Module.func"` → expected Scheme name
2. Search .ss file for the pattern
3. Verify function exists in --dumpcases output
