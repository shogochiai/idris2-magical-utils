#!/bin/bash
# Report exclusion pattern leaks as a GitHub Pull Request
# Usage: ./scripts/report-leak.sh [project_path] [top_n]
#
# Prerequisites:
#   - gh CLI installed and authenticated
#   - jq installed
#
# This script:
#   1. Clones/uses idris2-coverage repo
#   2. Detects leaks in high_impact_targets
#   3. Creates a branch with the leak report
#   4. Opens a PR to the main repo

set -e

UPSTREAM_REPO="shogochiai/idris2-coverage"
PROJECT=${1:-.}
TOP=${2:-1000}

echo "=== idris2-coverage Leak Reporter ==="
echo ""

# Check prerequisites
if ! command -v gh &> /dev/null; then
  echo "ERROR: 'gh' CLI not found. Install it: https://cli.github.com/"
  exit 1
fi

if ! command -v jq &> /dev/null; then
  echo "ERROR: 'jq' not found. Install it: brew install jq (macOS) or apt install jq (Linux)"
  exit 1
fi

if ! gh auth status &> /dev/null; then
  echo "ERROR: Not authenticated with GitHub. Run: gh auth login"
  exit 1
fi

# Determine repo location
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

# Check if we're in the idris2-coverage repo
if [ -f "$REPO_ROOT/idris2-coverage.ipkg" ]; then
  echo "Using local repo: $REPO_ROOT"
  WORK_DIR="$REPO_ROOT"
else
  # Not in repo - ask user
  echo "Not running from idris2-coverage repo."
  echo ""
  echo "Do you already have idris2-coverage cloned somewhere?"
  echo "  1) Yes, I have an existing clone"
  echo "  2) No, clone it for me"
  echo ""
  read -p "Choice [1/2]: " CLONE_CHOICE

  if [[ "$CLONE_CHOICE" == "1" ]]; then
    # User has existing clone
    echo ""
    read -p "Enter path to existing clone: " EXISTING_PATH
    EXISTING_PATH="${EXISTING_PATH/#\~/$HOME}"  # Expand ~

    if [ ! -f "$EXISTING_PATH/idris2-coverage.ipkg" ]; then
      echo "ERROR: Not a valid idris2-coverage repo: $EXISTING_PATH"
      echo "       (idris2-coverage.ipkg not found)"
      exit 1
    fi

    WORK_DIR="$EXISTING_PATH"
    echo "Using existing clone: $WORK_DIR"
  else
    # Need to clone - ask where first
    echo ""
    read -p "Where should I clone idris2-coverage? [~/src]: " CLONE_BASE
    CLONE_BASE="${CLONE_BASE:-$HOME/src}"
    CLONE_BASE="${CLONE_BASE/#\~/$HOME}"  # Expand ~

    WORK_DIR="$CLONE_BASE/idris2-coverage"

    echo ""
    echo "Will clone to: $WORK_DIR"
    read -p "Continue? [Y/n]: " CONTINUE_REPLY
    if [[ "$CONTINUE_REPLY" =~ ^[Nn] ]]; then
      echo "Aborted."
      exit 0
    fi

    if [ -d "$WORK_DIR" ]; then
      echo "Found existing clone at $WORK_DIR"
    else
      mkdir -p "$CLONE_BASE"
      cd "$CLONE_BASE"

      # Check if user owns the upstream repo (determines fork vs direct clone)
      GITHUB_USER=$(gh api user -q .login 2>/dev/null || echo "")
      UPSTREAM_OWNER=$(echo "$UPSTREAM_REPO" | cut -d'/' -f1)

      if [ "$GITHUB_USER" = "$UPSTREAM_OWNER" ]; then
        # Owner: clone directly
        echo "Cloning $UPSTREAM_REPO..."
        gh repo clone "$UPSTREAM_REPO" || {
          echo "ERROR: Could not clone repo"
          exit 1
        }
      else
        # Contributor: fork then clone
        echo "Forking and cloning $UPSTREAM_REPO..."
        gh repo fork "$UPSTREAM_REPO" --clone=true || {
          # If fork already exists, clone it
          echo "Fork may already exist, trying to clone..."
          gh repo clone "$GITHUB_USER/idris2-coverage" || {
            echo "ERROR: Could not fork/clone repo"
            exit 1
          }
        }
      fi
    fi

    echo "Working directory: $WORK_DIR"
  fi  # end of clone choice
fi  # end of not in repo

cd "$WORK_DIR"

# Make sure we're on main and up to date
git checkout main 2>/dev/null || git checkout master 2>/dev/null
git pull origin main 2>/dev/null || git pull origin master 2>/dev/null || true

# Build if needed
if [ ! -f "./build/exec/idris2-cov" ]; then
  echo "Building idris2-cov..."
  idris2 --build idris2-coverage.ipkg
fi

IDRIS2_VERSION_FULL=$(idris2 --version 2>/dev/null | head -1 || echo "unknown")
# Extract semver (e.g., "0.8.0" from "Idris 2, version 0.8.0-95333b3ad")
IDRIS2_SEMVER=$(echo "$IDRIS2_VERSION_FULL" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
if [ -z "$IDRIS2_SEMVER" ]; then
  IDRIS2_SEMVER="unknown"
fi
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BRANCH_NAME="exclusions-$IDRIS2_SEMVER-$TIMESTAMP"

echo ""
echo "Project: $PROJECT"
echo "Idris2: $IDRIS2_VERSION_FULL (semver: $IDRIS2_SEMVER)"
echo ""

# Convert PROJECT to absolute path if relative
if [[ "$PROJECT" != /* ]]; then
  PROJECT="$(cd "$OLDPWD" 2>/dev/null && cd "$PROJECT" && pwd)" || {
    echo "ERROR: Cannot find project: $PROJECT"
    exit 1
  }
fi

# Run analysis and capture only JSON
JSON_OUTPUT=$(./build/exec/idris2-cov --json --top $TOP "$PROJECT" 2>&1 | \
  sed -n '/^{/,$p')

if [ -z "$JSON_OUTPUT" ]; then
  echo "ERROR: No JSON output from idris2-cov"
  exit 2
fi

# Extract leaks
LEAKS=$(echo "$JSON_OUTPUT" | \
  jq -r '.high_impact_targets[].funcName' 2>/dev/null | \
  grep -E '^(\{|_builtin\.|prim__|Prelude\.|Data\.|System\.|Control\.|Decidable\.|Language\.|Debug\.)' | \
  sort -u)

if [ -z "$LEAKS" ]; then
  echo "No leaks detected. Nothing to report."
  exit 0
fi

LEAK_COUNT=$(echo "$LEAKS" | wc -l | tr -d ' ')
echo "Found $LEAK_COUNT potential leaks:"
echo "$LEAKS" | while read func; do
  echo "  - $func"
done
echo ""

# Generate LLM prompt for analysis
# Note: Using variable substitution in the prompt requires careful quoting
LLM_PROMPT="I'm using idris2-coverage to analyze my Idris2 project, and the following functions appeared in \`high_impact_targets\` but look like they should be excluded (stdlib, compiler-generated, or dependency code):

\`\`\`
$LEAKS
\`\`\`

**Idris2 version**: $IDRIS2_VERSION_FULL (semver: $IDRIS2_SEMVER)

Please help me categorize these:

1. **Standard Library** (Prelude.*, Data.*, System.*, Control.*, etc.)
   - These are from Idris2 base/contrib packages

2. **Compiler-Generated** ({csegen:N}, {eta:N}, _builtin.*, prim__*)
   - Machine names from optimization passes

3. **Type Constructors** (names ending with a period)
   - Auto-generated ADT case trees

4. **False Positives** (actually user code that looks like stdlib)
   - Should NOT be excluded

For each function, tell me:
- Category (1-4 above)
- If it is a new pattern, what exclusion rule should be added to idris2-coverage

Format your response as a checklist I can use for the PR."

echo "=========================================="
echo "  STEP 1: Verify these are actual leaks"
echo "=========================================="
echo ""
echo "Not sure if these are leaks? Copy this prompt to Claude/ChatGPT:"
echo ""
echo "--- COPY FROM HERE ---"
echo "$LLM_PROMPT"
echo "--- COPY TO HERE ---"
echo ""
echo "The LLM will help you categorize each function."
echo ""

# Confirm with user
read -p "Continue to create PR? (y=yes, n=no, c=copy prompt to clipboard) " -n 1 -r
echo
if [[ $REPLY =~ ^[Cc]$ ]]; then
  # Try to copy to clipboard
  if command -v pbcopy &> /dev/null; then
    echo "$LLM_PROMPT" | pbcopy
    echo "Copied to clipboard! Paste into Claude/ChatGPT."
  elif command -v xclip &> /dev/null; then
    echo "$LLM_PROMPT" | xclip -selection clipboard
    echo "Copied to clipboard! Paste into Claude/ChatGPT."
  else
    echo "Clipboard not available. Please copy manually."
  fi
  echo ""
  read -p "After reviewing, create PR? [y/N] " -n 1 -r
  echo
fi

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

# Create branch
echo "Creating branch: $BRANCH_NAME"
git checkout -b "$BRANCH_NAME"

# Update exclusions file for this Idris2 version
EXCLUSIONS_FILE="exclusions/$IDRIS2_SEMVER.txt"

# Create file if it doesn't exist
if [ ! -f "$EXCLUSIONS_FILE" ]; then
  cat > "$EXCLUSIONS_FILE" << INIT_EOF
# Exclusion patterns specific to Idris2 $IDRIS2_SEMVER
# Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
INIT_EOF
fi

# Add header for this contribution
echo "" >> "$EXCLUSIONS_FILE"
echo "# Added $(date -u +"%Y-%m-%d") from $(basename "$PROJECT")" >> "$EXCLUSIONS_FILE"

# Add new patterns (skip duplicates)
NEW_PATTERNS=0
echo "$LEAKS" | while read pattern; do
  if [ -n "$pattern" ]; then
    # Convert to wildcard pattern if appropriate
    # e.g., {csegen:123} -> {csegen:*}
    NORMALIZED=$(echo "$pattern" | sed 's/\({\w*:\)[0-9]*/\1*/g')

    # Check if already in base.txt or version file
    if ! grep -qF "$NORMALIZED" "exclusions/base.txt" 2>/dev/null && \
       ! grep -qF "$NORMALIZED" "$EXCLUSIONS_FILE" 2>/dev/null; then
      echo "$NORMALIZED" >> "$EXCLUSIONS_FILE"
      echo "  + $NORMALIZED"
      NEW_PATTERNS=$((NEW_PATTERNS + 1))
    fi
  fi
done

echo ""
echo "Updated: $EXCLUSIONS_FILE"

# Commit
git add "$EXCLUSIONS_FILE"
PROJECT_BASENAME=$(basename "$PROJECT")
git commit -m "exclusions: Add patterns for Idris2 $IDRIS2_SEMVER

Detected $LEAK_COUNT exclusion patterns that should be filtered from
high_impact_targets.

Idris2 version: $IDRIS2_VERSION_FULL
Project analyzed: $PROJECT_BASENAME"

# Push and create PR
echo "Pushing branch..."
git push -u origin "$BRANCH_NAME"

echo "Creating Pull Request..."
gh pr create \
  --repo "$UPSTREAM_REPO" \
  --title "exclusions($IDRIS2_SEMVER): Add $LEAK_COUNT patterns" \
  --body "## Summary

Added **$LEAK_COUNT exclusion patterns** to \`exclusions/$IDRIS2_SEMVER.txt\`.

**Idris2 version**: $IDRIS2_VERSION_FULL
**Project analyzed**: $PROJECT_BASENAME

## Patterns Added

\`\`\`
$LEAKS
\`\`\`

## LLM Analysis (Optional)

If you used Claude/ChatGPT to categorize these, paste the analysis here:

<!-- PASTE LLM ANALYSIS HERE -->

## File Changed

- \`exclusions/$IDRIS2_SEMVER.txt\` - Version-specific exclusion patterns

---
*Generated by \`scripts/report-leak.sh\`*"

echo ""
echo "Done! PR created."
echo ""
echo "Return to main branch:"
echo "  cd $WORK_DIR && git checkout main"
