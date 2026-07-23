---
name: subagent-driven-development
description: Use when executing implementation plans with independent tasks in the current session.
---

# Subagent-Driven Development

Execute implementation plans sequentially in a single session by delegating independent tasks to subagents, enforced by a two-stage review: spec compliance followed by code quality.

**Pre-computation Announcement:** "I'm using the subagent-driven-development skill to implement the plan."

**Continuous Execution Rule:** Run continuously to completion without pausing for human check-ins between tasks. Stop only if blocked, encountering ambiguous requirements, or when all tasks are complete.

## When to Use

- Detailed, checkbox-tracked implementation plan exists.
- Tasks are largely independent.
- Rapid iteration without manual file-reading overhead or context pollution is desired.

## Execution Workflow
1. **Initialize:** Read plan, extract task descriptions and target files, initialize checkbox tracking.
2. **Execute Task:**
   - **Dispatch Implementer:** Start fresh subagent with full task text and necessary context/files.
   - **Answer Questions:** Respond immediately to implementer questions and re-dispatch.
   - **Stage 1: Spec Compliance Review:** Dispatch spec reviewer subagent to verify code satisfies spec without extra scope. Implementer fixes flagged issues.
   - **Stage 2: Code Quality Review:** Dispatch code quality reviewer to verify tests, readability, and design patterns. Implementer fixes flagged issues.
   - **Checkpoint:** Mark complete, commit changes, proceed immediately to next task.
3. **Wrap-up:** Perform final codebase review.

## Implementer Statuses & Handoff Protocol

### Implementer Statuses
- **DONE:** Proceed directly to Spec Compliance review.
- **DONE_WITH_CONCERNS:** Evaluate concerns; resolve scope/correctness issues before review, otherwise proceed to review.
- **NEEDS_CONTEXT:** Provide missing details and re-dispatch.
- **BLOCKED:** Do not force retries. Provide context, upgrade model capacity, or escalate plan defects to human.

### Subagent State Handoff & Compaction Protocol
When a subagent completes a task, encounters concerns, or passes review, it **MUST** compact its state into a standardized handoff block:
```markdown
### Subagent State Handoff
- **Current State:** [1-2 sentence snapshot of active execution state]
- **Completed Tasks:** [Concise list of completed checklist items & changes]
- **Next Steps:** [Actionable items for controller or next task]
- **Key References:** [Relative file paths and line ranges modified/created]
```

## Prompt Templates
- `./implementer-prompt.md` - Task implementation.
- `./spec-reviewer-prompt.md` - Specification alignment verification.
- `./code-quality-reviewer-prompt.md` - Code quality and test verification.

## Benefits & Critical Rules

### Key Benefits
- Zero subagent file-reading overhead (controller supplies all text and context).
- Strong TDD and self-review discipline.
- Two-stage review gates prevent regression and scope creep.

### Prohibitions
- **Never** skip spec compliance or code quality reviews.
- **Never** proceed to the next task while current task reviews have open issues.
- **Never** make subagents read raw plan files (pass task text directly).
- **Never** substitute implementer self-review for independent review stages.
- **Never** begin code quality review before spec compliance is 100% approved.
