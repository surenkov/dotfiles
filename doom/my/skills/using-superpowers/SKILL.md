---
name: using-superpowers
description: Use when starting any conversation - establishes how to find and use skills, requiring skill tool invocation before ANY response including clarifying questions.
---

# Using Superpowers

Establish disciplined tool and workflow usage. If there is even a 1% chance a skill applies, you **MUST** invoke it before responding or taking action. Never bypass skills with lazy rationalizations.

**Pre-computation Announcement:** "I'm using the using-superpowers skill to manage workflow."

## Instruction Priority

1. **User's Explicit Instructions** (e.g., `CLAUDE.md`, `GEMINI.md`, `AGENTS.md`) — highest priority.
2. **Superpowers Skills** — overrides default system behavior when they conflict.
3. **Default System Prompt** — lowest priority.

## Skill Activation Rule

- **Action:** Invoke the `skill` tool **BEFORE** providing any response (including clarifying questions) or executing files.
- **Announcement Format:** `"Using <skill-name> to <purpose>"`
- **Execution:** Create a Todo/Checkbox list if the skill has a checklist, and follow the steps rigidly.

| Rationalization Thought | Reality |
|---|---|
| "Just a simple question/context gathering first." | Check skills first. Skills dictate how to ask or gather context. |
| "I'll just do this one quick thing." | Checking skills first prevents undisciplined mistakes. |
| "I remember this skill already." | Skills change; invoke and read to ensure exact alignment. |

## The 14 Active Superpowers Skills

Here are the 14 active skills available in the environment:

### 1. using-superpowers
- **When:** At the start of any session or conversation.
- **Purpose:** Enforces rigorous skill discovery, activation, and priority.

### 2. brainstorming
- **When:** Before any creative work (creating features, building components, adding functionality, or modifying behavior).
- **Purpose:** Explores intent, requirements, and design constraints via environment fact-checking and decision-tree interrogation; prohibits coding until design is approved.

### 3. writing-plans
- **When:** Once design is approved and before implementing any code.
- **Purpose:** Produces exhaustive, bite-sized implementation plans using tracer-bullet vertical slicing, explicit dependency edges, Expand-Contract refactoring, and exact verification commands.

### 4. subagent-driven-development
- **When:** Executing multi-task implementation plans within the current session.
- **Purpose:** Dispatches fresh implementer and reviewer subagents per task with standardized state handoff compaction; ensures two-stage review (spec, then quality).

### 5. dispatching-parallel-agents
- **When:** Facing two or more independent, non-sequential tasks, test failures, or alternative design explorations (`DESIGN-IT-TWICE.md`).
- **Purpose:** Dispatches concurrent specialized agents with isolated contexts and state handoff blocks to resolve issues in parallel.

### 6. test-driven-development (TDD)
- **When:** Implementing any feature, bugfix, or behavior change.
- **Purpose:** "No production code without a failing test first." Absolute prohibition of untested code.

### 7. systematic-debugging
- **When:** Encountering bugs, test failures, or unexpected behaviors.
- **Purpose:** Demands root cause investigation first with a mandatory Red-Capable command gate, falsifiable hypotheses, minimization, and post-mortem prevention reviews.

### 8. verification-before-completion
- **When:** Prior to claiming work is complete, fixed, or ready to merge.
- **Purpose:** "Evidence before assertions." Demands executing test and build suites and reporting the actual outputs.

### 9. using-git-worktrees
- **When:** Starting feature work or implementing plans.
- **Purpose:** Detects existing isolation, uses platform-native worktrees, or configures clean, git-ignored fallback worktrees.

### 10. code-review
- **When:** Reviewing changes since a target ref (e.g., branches, PRs, or diffs).
- **Purpose:** Conducts parallel sub-agent reviews across two independent axes: Standards/smells and Spec compliance.

### 11. domain-modeling
- **When:** Pinning down domain terminology, context maps, or recording non-reversible architectural decisions.
- **Purpose:** Actively builds and sharpens the project's domain model, maintaining `CONTEXT.md` glossary and `docs/adr/`.

### 12. codebase-design
- **When:** Designing or improving a module's interface, finding deepening opportunities, or deciding seam placement.
- **Purpose:** Establishes deep module architecture theory (small interface + large behavior) and interface trade-off protocols (`DESIGN-IT-TWICE.md`).

### 13. improve-codebase-architecture
- **When:** Conducting codebase-wide structural audits or refactoring coupled/shallow modules.
- **Purpose:** Scans codebases for friction, generates visual HTML audit reports, and grills through refactoring proposals.

### 14. wayfinder
- **When:** Planning large epics wrapped in "fog" that span more than one agent session.
- **Purpose:** Charts multi-session efforts as a map of decision tickets on an issue tracker and resolves them one at a time.
