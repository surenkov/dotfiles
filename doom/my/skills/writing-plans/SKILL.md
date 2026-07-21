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
