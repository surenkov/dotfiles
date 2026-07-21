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
