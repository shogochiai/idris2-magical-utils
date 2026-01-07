# Compatibility & Execution Environments

## Requirements

- **Idris2**: v0.7.0 or later
- **pack**: Latest version (Idris2 package manager)
- **Chez Scheme**: Backend for Idris2 compilation

## Portability

idris2-coverage is designed to work across different development environments without hardcoded paths or local dependencies.

### How It Works

When analyzing a project, idris2-coverage generates a temporary `pack.toml` that references itself from GitHub:

```toml
[custom.all.idris2-coverage]
type   = "github"
url    = "https://github.com/aspect-and-syntax/idris2-coverage"
commit = "latest"
ipkg   = "idris2-coverage.ipkg"
```

This ensures the tool works on any machine without requiring local path configuration.

### Project Dependencies

If your project has a `pack.toml` with custom dependencies (e.g., non-public packages from GitHub), idris2-coverage will merge them with its own dependency. Your existing dependencies are preserved.

## CI/CD

GitHub Actions CI runs on every push/PR to main:

| Step | Description |
|------|-------------|
| Build | `pack build idris2-coverage.ipkg` on Ubuntu + macOS |
| Smoke test | Verify CLI responds to `--help` |
| Integration test | Clone and analyze [idris2-yul](https://github.com/shogochiai/idris2-yul) |

The integration test catches environment-dependent bugs (e.g., hardcoded paths) that only surface on clean machines.

## Local Development with Devcontainer

For developers who want to test in a clean environment locally, a devcontainer configuration is provided.

### Prerequisites

- Docker Desktop (4-5GB RAM allocation recommended)
- VSCode with Remote - Containers extension, or Docker CLI

### Using VSCode

1. Open the project in VSCode
2. `Cmd+Shift+P` → "Dev Containers: Reopen in Container"
3. Wait for the container to build (first time may take a few minutes)
4. Run commands in the integrated terminal:
   ```bash
   pack build idris2-coverage.ipkg
   ./build/exec/idris2-cov --help
   ```

### Using Docker CLI

```bash
docker run -it --rm \
  -v $(pwd):/workspace \
  -w /workspace \
  ghcr.io/joshuanianji/idris-2-docker/devcontainer:v0.7.0 \
  bash

# Inside container
pack build idris2-coverage.ipkg
./build/exec/idris2-cov .
```

### Resource Requirements

The devcontainer image includes Idris2, pack, and idris2-lsp. Minimum recommended:

- **RAM**: 8GB system (4GB allocated to Docker)
- **Disk**: 5GB free for the image
- **CPU**: Any (ARM64/AMD64 supported)

## Troubleshooting

### "Cannot connect to Docker daemon"

Start Docker Desktop before running devcontainer commands.

### Build fails in CI but works locally

This usually indicates environment-dependent code (hardcoded paths, local dependencies). Check `UnifiedRunner.idr` for any local path references.

### pack fails to resolve idris2-coverage

Ensure you're using a recent version of pack. The tool references itself via GitHub, which requires pack's custom package support.
