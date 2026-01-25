# Project Agent Instructions

## Quick Reference

```bash
# Analyze codebase and get recommended actions
lazy core ask <target_dir>

# Phase 1 (Vibe Bootstrap): Focus on test discovery
lazy core ask <target_dir> --steps=4

# Phase 2 (Spec Emergence): Bidirectional parity
lazy core ask <target_dir> --steps=1,2,3

# Phase 3 (TDVC Loop): Chase Zero Gap, find implicit bugs, and Vibe More
lazy core ask <target_dir> --steps=1,2,3,4
lazy core ask <target_dir> --steps=5
```

## Interpreting Output

- **URGENT** actions: Execute immediately
- **High** priority: Address in current session
- **Medium/Low**: Queue for later

## Policy Mapping

`lazy core ask` converts gaps → signals → recommendations.
Follow recommendations to maintain project health.

## Idris2 Compilation Memory Pitfalls

Idris2 compilation can explode from ~165MB to 16GB+ RAM when:

1. **Type ambiguity** - Same type name in multiple modules (e.g., `HttpResponse` in both packages) causes compiler to backtrack excessively during type inference
2. **Circular dependencies** - Modules in same package with complex interdependencies
3. **Unresolved imports** - Missing or conflicting module imports

**Fix**: Keep packages cleanly separated. Downstream packages (idris2-ouc) import types from here. Don't duplicate types across packages.
