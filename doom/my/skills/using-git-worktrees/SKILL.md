---
name: using-git-worktrees
description: Use when starting feature work that needs workspace isolation. Detects existing isolation, prefers native platform tools, or falls back to configured git worktrees.
---

# Using Git Worktrees

Detect and utilize workspace isolation to prevent branch pollution. Prefer native platform isolation tools first; fall back to git worktrees only if native tools are unavailable.

**Pre-computation Announcement:** "I'm using the using-git-worktrees skill to set up an isolated workspace."

## Step 0: Detect Existing Isolation

Determine if the workspace is already isolated. Verify you are not inside a submodule first.

```bash
# Check if inside a git submodule
if [ -n "$(git rev-parse --show-superproject-working-tree 2>/dev/null)" ]; then
    IS_SUBMODULE=true
else
    IS_SUBMODULE=false
fi

# Detect linked worktree
GIT_DIR=$(cd "$(git rev-parse --git-dir)" 2>/dev/null && pwd -P)
GIT_COMMON=$(cd "$(git rev-parse --git-common-dir)" 2>/dev/null && pwd -P)
```

- **If `GIT_DIR != GIT_COMMON` and `IS_SUBMODULE=false`:** Workspace is already isolated. Skip to **Step 2: Project Setup**. Report status:
  - On branch: `"Already in isolated workspace at <path> on branch <name>."`
  - Detached HEAD: `"Already in isolated workspace at <path> (detached HEAD)."`
- **Otherwise:** Request user consent to set up an isolated worktree (unless preference is pre-declared):
  > "Would you like me to set up an isolated worktree? It protects your current branch from changes."
  If denied, work in place and skip to **Step 2**.

## Step 1: Create Isolated Workspace

### 1a. Native Isolation Tools (Preferred)
Use available platform tools (e.g., `EnterWorktree`, `WorktreeCreate`, `/worktree`, `--worktree`). This avoids phantom git states. If successful, skip to **Step 2**.

### 1b. Git Worktree Fallback
Only use if no native tool is available.

#### Directory Selection Priority
1. User-declared path preference in instruction files.
2. Existing project-local directories: `.worktrees/` (preferred) or `worktrees/` at project root.
3. Legacy global path: `~/.config/superpowers/worktrees/<project-name>/`
4. Default fallback: `.worktrees/` at project root.

#### Ignore & Safety Verification (For local directories)
Verify the path is git-ignored before creation to prevent committing worktree contents:
```bash
git check-ignore -q .worktrees 2>/dev/null || git check-ignore -q worktrees 2>/dev/null
```
*If not ignored:* Add to `.gitignore`, commit the change, then proceed.

#### Creation Commands
```bash
PROJECT_NAME=$(basename "$(git rev-parse --show-toplevel)")
# Local path: .worktrees/$BRANCH_NAME
# Global path: ~/.config/superpowers/worktrees/$PROJECT_NAME/$BRANCH_NAME
git worktree add "$path" -b "$BRANCH_NAME"
cd "$path"
```
*Sandbox fallback:* If creation fails due to permissions, notify the user, work in place, and run baseline setup.

## Step 2: Project Setup
Detect project type and install dependencies:
- **Node.js:** `if [ -f package.json ]; then npm install; fi`
- **Rust:** `if [ -f Cargo.toml ]; then cargo build; fi`
- **Python:** `if [ -f requirements.txt ]; then pip install -r requirements.txt; fi; if [ -f pyproject.toml ]; then poetry install; fi`
- **Go:** `if [ -f go.mod ]; then go mod download; fi`

## Step 3: Verify Clean Baseline
Run tests to verify a clean starting state:
```bash
npm test / cargo test / pytest / go test ./...
```
- **If tests fail:** Report failure details, request instructions to proceed or investigate.
- **If tests pass:** Report readiness:
  ```
  Worktree ready at <full-path>
  Tests passing (<N> tests, 0 failures)
  Ready to implement <feature-name>
  ```

## Quick Reference & Rules

| Scenario | Action |
|---|---|
| Already isolated / inside submodule | Skip creation; submodules are treated as normal repos |
| Native tool available | Use native tool (Step 1a) |
| Fallback needed | Choose directory by priority, verify git-ignored, run `git worktree add` |
| Local directory not ignored | Add to `.gitignore` and commit first |
| Sandbox error | Work in place, log warning |
| Baseline tests fail | Report immediately and ask for user guidance |

### Red Flags & Mistakes to Avoid
- **Never** nested-create worktrees when already in an isolated workspace.
- **Never** run `git worktree add` if a platform-native isolation tool is available.
- **Never** skip `.gitignore` verification for project-local worktrees.
- **Never** skip baseline test execution or proceed with failing baseline tests without explicit user consent.
