---
name: writing-plans
description: Use when you have a specification or requirements for a multi-step task, before implementing any code.
---

# Writing Plans

Write precise, exhaustive implementation plans assuming the implementer has zero project context but high technical competence. Define everything required: exact files, complete code, precise tests, and exact verification commands. Ensure tasks are bite-sized, independent, and structured around TDD and frequent commits.

- **Pre-computation Announcement**: `"I'm using the writing-plans skill to create the implementation plan."`

## Core Principles

1. **Scope Division**: Divide multi-subsystem specs into separate plans (one per subsystem). Each plan must produce working, testable software.
2. **File Structure Mapping**: Map files to be created/modified with exact responsibilities before detailing tasks. Require deep modules with clear testable seams rather than shallow pass-through wrappers. Prioritize small, focused files with single, clear responsibilities.
3. **Bite-Sized Steps**: Break tasks into 2-5 minute steps (Write failing test -> Run to verify failure -> Write implementation -> Verify pass -> Commit).
4. **No Placeholders**: Never use "TODO", "TBD", "implement later", vague commands ("add error handling"), or "similar to Task N" shortcuts. Every code change, test, and command must be fully written out.

## Plan File Format

### Document Header

```markdown
* [Feature Name] Implementation Plan

> *For agentic workers:* REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

*Goal:* [One sentence describing what this builds]
*Architecture:* [2-3 sentences about approach]
*Tech Stack:* [Key technologies/libraries]

---
```

### Task Structure

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

- **Spec Coverage**: Every specification requirement maps to a task.
- **No Placeholders**: Zero "TODO", "TBD", or unwritten code blocks.
- **API Consistency**: Signatures, types, and variable names consistent across all tasks.

## Execution Handoff

Upon plan completion, output:
```
Ready to execute using superpowers:subagent-driven-development (fresh subagent per task, two-stage review).
```

## Plan Review Checklist

| Category | What to Verify |
|---|---|
| **Completeness** | Ensure zero TODOs, placeholders, or missing code blocks. |
| **Spec Alignment** | Verify all spec requirements are implemented with no scope creep. |
| **Task Decomposition** | Verify tasks have clear boundaries and steps are highly actionable. |
| **Buildability** | Confirm an engineer can execute the plan without needing extra context. |

**Calibration**: Approve unless blocker-level issues exist (missing requirements, contradictory steps, placeholders, vague actions). Ignore minor stylistic choices.
