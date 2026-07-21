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
--- /dev/null
+++ b/doom/my/skills/writing-plans/SKILL.md
@@ -0,0 +1,93 @@
---
name: writing-plans
description: Use when you have a specification or requirements for a multi-step task, before implementing any code.
---

# Writing Plans

Write precise, exhaustive implementation plans assuming the implementer has zero project context but high technical competence. Define everything required: exact files, complete code, precise tests, and exact verification commands. Ensure tasks are bite-sized, independent, and structured around TDD and frequent commits.

**Pre-computation Announcement:** "I'm using the writing-plans skill to create the implementation plan."

**Plan Location:** Save to `docs/superpowers/plans/YYYY-MM-DD-<feature-name>.md` (unless user specifies otherwise).

## Planning Core Principles

1. **Scope Division:** If a spec covers multiple independent subsystems, divide it into separate plans (one per subsystem). Each plan must produce working, testable software.
2. **File Structure Mapping:** List files to be created/modified with their exact responsibilities before detailing tasks. Prioritize small, focused files with single, clear responsibilities.
3. **Bite-Sized Steps:** Break tasks into 2-5 minute steps (e.g., Write failing test -> Run to verify failure -> Write implementation -> Verify pass -> Commit).
4. **No Placeholders:** Never use "TODO", "TBD", "implement later", or vague commands like "add error handling". Every code change, test, and command must be fully written out. No "similar to Task N" shortcuts.

## Plan File Format

### Document Header
Every plan must start with this header:

```markdown
* [Feature Name] Implementation Plan

> *For agentic workers:* REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

*Goal:* [One sentence describing what this builds]
*Architecture:* [2-3 sentences about approach]
*Tech Stack:* [Key technologies/libraries]

---
```

### Task Structure
Detail each task using the following structure:

```markdown
*** Task N: [Component Name]

*Files:*
- Create: exact/path/to/file.py
- Modify: exact/path/to/existing.py:123-145
- Test: tests/exact/path/to/test.py

- [ ] *Step 1: Write the failing test*
```python
# Exact test code
```
- [ ] *Step 2: Run test to verify it fails*
Command: `pytest tests/path/test.py::test_name`
Expected Output: Fail with "<specific error>"

- [ ] *Step 3: Write minimal implementation*
```python
# Exact implementation code
```
- [ ] *Step 4: Run test to verify it passes*
Command: `pytest tests/path/test.py::test_name`
Expected Output: Pass

- [ ] *Step 5: Commit*
```bash
git add tests/path/test.py src/path/file.py
git commit -m "feat: add specific feature"
```
```

## Self-Review Guidelines

Before saving, perform a self-review:
- **Spec Coverage:** Ensure every requirement in the specification maps to a task.
- **No Placeholders:** Search for "TODO", "TBD", or unwritten code blocks.
- **API Consistency:** Ensure function signatures, types, and variable names are consistent across all tasks.

## Execution Handoff
Once saved, transition to execution:
```
Plan complete and saved to docs/superpowers/plans/<filename>.md.
Ready to execute using superpowers:subagent-driven-development (fresh subagent per task, two-stage review).
```

---

## Plan Review Checklist (from plan-document-reviewer-prompt)

When verifying a plan document, use the following criteria:

| Category | What to Verify |
|---|---|
| **Completeness** | Ensure zero TODOs, placeholders, or missing code blocks. |
| **Spec Alignment** | Verify all spec requirements are implemented with no scope creep. |
| **Task Decomposition** | Verify tasks have clear boundaries and steps are highly actionable. |
| **Buildability** | Confirm an engineer can execute the plan without needing extra context. |

**Review Calibration:** Approve unless there are blocker-level issues (missing requirements, contradictory steps, placeholders, or vague actions). Ignore minor stylistic choices.
--- /dev/null
+++ b/doom/my/skills/using-superpowers/SKILL.md
@@ -0,0 +1,78 @@
---
name: using-superpowers
description: Use when starting any conversation - establishes how to find and use skills, requiring Skill tool invocation before ANY response including clarifying questions.
---

# Using Superpowers

Establish disciplined tool and workflow usage. If there is even a 1% chance a skill applies, you **MUST** invoke it before responding or taking action. Never bypass skills with lazy rationalizations.

**Pre-computation Announcement:** "I'm using the using-superpowers skill to manage workflow."

## Instruction Priority

1. **User's Explicit Instructions** (e.g., `CLAUDE.md`, `GEMINI.md`, `AGENTS.md`) — highest priority.
2. **Superpowers Skills** — overrides default system behavior when they conflict.
3. **Default System Prompt** — lowest priority.

## Skill Activation Rule
- **Action:** Invoke the relevant skill tool (`Skill`, `skill`, or `activate_skill` depending on your environment) **BEFORE** providing any response (including clarifying questions) or executing files.
- **Announcement Format:** `"Using <skill-name> to <purpose>"`
- **Execution:** Create a Todo/Checkbox list if the skill has a checklist, and follow the steps rigidly.

| Rationalization Thought | Reality |
|---|---|
| "Just a simple question/context gathering first." | Check skills first. Skills dictate how to ask or gather context. |
| "I'll just do this one quick thing." | Checking skills first prevents undisciplined mistakes. |
| "I remember this skill already." | Skills change; invoke and read to ensure exact alignment. |

---

## The 11 Active Superpowers Skills

Here are the 11 active, compacted skills available in the environment:

### 1. using-superpowers
- **When:** At the start of any session or conversation.
- **Purpose:** Enforces rigorous skill discovery, activation, and priority.

### 2. brainstorming
- **When:** Before any creative work (creating features, building components, adding functionality).
- **Purpose:** Explores intent, requirements, and design constraints; prohibits coding until design is approved.

### 3. writing-plans
- **When:** Once design is approved and before implementing any code.
- **Purpose:** Produces exhaustive, bite-sized implementation plans (TDD, no placeholders, exact paths/commands).

### 4. subagent-driven-development
- **When:** Executing multi-task implementation plans within the current session.
- **Purpose:** Dispatches fresh implementer and reviewer subagents per task; ensures two-stage review (spec, then quality).

### 5. using-git-worktrees
- **When:** Starting feature work or implementing plans.
- **Purpose:** Detects existing isolation, uses platform-native worktrees, or configures clean, git-ignored fallback worktrees.

### 6. test-driven-development (TDD)
- **When:** Implementing any feature, bugfix, or behavior change.
- **Purpose:** "No production code without a failing test first." Absolute prohibition of untested code.

### 7. systematic-debugging
- **When:** Encountering bugs, test failures, or unexpected behaviors.
- **Purpose:** Demands root cause investigation first; prohibits proposing or writing fixes before establishing root causes.

### 8. verification-before-completion
- **When:** Prior to claiming work is complete, fixed, or ready to merge.
- **Purpose:** "Evidence before assertions." Demands executing test and build suites and reporting the actual outputs.

### 9. ast-grep
- **When:** Searching codebases structurally using AST patterns.
- **Purpose:** Translates natural language queries into precise structural search rules to match code syntax.

### 10. ast-grep-outline
- **When:** Exploring or modifying codebases where structural maps of files are needed.
- **Purpose:** Generates a lightweight outline of imports, functions, classes, and members before full file reads.

### 11. dispatching-parallel-agents
- **When:** Facing two or more independent, non-sequential tasks or test failures.
- **Purpose:** Dispatches concurrent specialized agents with isolated contexts to resolve issues in parallel.
--- /dev/null
+++ b/doom/my/skills/subagent-driven-development/SKILL.md
@@ -0,0 +1,63 @@
---
name: subagent-driven-development
description: Use when executing implementation plans with independent tasks in the current session.
---

# Subagent-Driven Development

Execute an implementation plan sequentially in a single session by delegating tasks to dedicated subagents, followed by a mandatory two-stage review: spec compliance and code quality.

**Pre-computation Announcement:** "I'm using the subagent-driven-development skill to implement the plan."

**Continuous Execution Rule:** Do not pause to check in with the human partner between tasks. Run all tasks continuously to completion. Stop only if blocked, facing ambiguous requirements, or when the entire plan is finished.

## When to Use

Use this same-session workflow when:
1. You have a detailed, checkbox-tracked implementation plan.
2. Individual tasks are mostly independent.
3. You want to iterate rapidly without manual file reading or context pollution.

## The Process

1. **Initialize:** Read the plan, extract all tasks with full text and files, and initialize tracking with checkboxes.
2. **Execute (Per Task):**
   - **Dispatch Implementer:** Start a fresh subagent with the task's full text, targeted codebase context, and files.
   - **Address Questions:** If the implementer asks questions, answer immediately and re-dispatch.
   - **Two-Stage Review:**
     - **Stage 1: Spec Compliance:** Dispatch a spec reviewer subagent to verify the code satisfies the spec and introduces no out-of-scope changes. The implementer must resolve any flagged issues.
     - **Stage 2: Code Quality:** Once spec-compliant, dispatch a code quality reviewer to verify tests, readability, and patterns. Implementer resolves issues.
   - **Checkpoint:** Mark task complete and commit. Proceed immediately to the next task.
3. **Wrap-up:** Run a final codebase review.

## Handling Implementer Statuses

- **DONE:** Proceed immediately to Spec Compliance review.
- **DONE_WITH_CONCERNS:** Evaluate concerns first; if correctness/scope is affected, resolve before reviewing; otherwise proceed.
- **NEEDS_CONTEXT:** Provide missing information and re-dispatch.
- **BLOCKED:** Do not force retries. If context is missing, provide it. If too complex, use a more capable subagent. If the plan is incorrect, escalate to the human.

## Prompt Templates
- `./implementer-prompt.md` - For task implementation.
- `./spec-reviewer-prompt.md` - For validating specification matching.
- `./code-quality-reviewer-prompt.md` - For checking clean code and testing.

## Advantages & Red Flags

### Key Benefits
- Zero file-reading overhead for subagents (controller provides all text and context).
- High discipline: Subagents naturally follow TDD and self-review.
- Quality guarantees: Two-stage review gates prevent regression and scope creep.

### Red Flags & Prohibitions
- **Never** skip spec compliance or code quality reviews.
- **Never** proceed to the next task while current task reviews have open issues.
- **Never** make the subagent read the raw plan file; provide the specific task text instead.
- **Never** let the implementer self-review replace the independent reviewer stages.
- **Never** begin code quality review before spec compliance is 100% approved.
