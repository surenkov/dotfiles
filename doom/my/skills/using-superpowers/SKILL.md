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

## The 12 Active Superpowers Skills

Here are the 12 active, compacted skills available in the environment:

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

### 12. code-review
- **When:** Reviewing changes since a target ref (e.g., branches, PRs, or diffs).
- **Purpose:** Conducts parallel sub-agent reviews across two independent axes: Standards/smells and Spec compliance.
