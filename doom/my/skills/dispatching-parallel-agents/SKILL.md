---
name: dispatching-parallel-agents
description: Use when facing 2+ independent tasks that can be worked on without shared state or sequential dependencies
---

# Dispatching Parallel Agents

## Overview

You delegate tasks to specialized agents with isolated context. By precisely crafting their instructions and context, you ensure they stay focused and succeed at their task. They should never inherit your session's context or history — you construct exactly what they need. This also preserves your own context for coordination work.

When you have multiple unrelated failures (different test files, different subsystems, different bugs), investigating them sequentially wastes time. Each investigation is independent and can happen in parallel.

**Core principle:** Dispatch one agent per independent problem domain. Let them work concurrently.

## When to Use

**Decision Logic:**
- Multiple failures -> Are they independent?
  - **No (related):** Use a single agent to investigate together.
  - **Yes:** Can they work without shared state?
    - **Yes:** Parallel dispatch (one agent per domain).
    - **No:** Sequential agents.

**Use when:**
- 3+ test files failing with different root causes
- Multiple subsystems broken independently
- Each problem can be understood without context from others
- No shared state between investigations

**Don't use when:**
- Failures are related (fix one might fix others)
- Need to understand full system state
- Agents would interfere with each other

## The Pattern

### 1. Identify Independent Domains

Group failures by what's broken:
- File A tests: Tool approval flow
- File B tests: Batch completion behavior
- File C tests: Abort functionality

Each domain is independent - fixing tool approval doesn't affect abort tests.

### 2. Create Focused Agent Tasks

Each agent gets:
- **Specific scope:** One test file or subsystem
- **Clear goal:** Make these tests pass
- **Constraints:** Don't change other code
- **Expected output:** Summary of what you found and fixed

### 3. Dispatch in Parallel

```typescript
// In Claude Code / AI environment
Task("Fix agent-tool-abort.test.ts failures")
Task("Fix batch-completion-behavior.test.ts failures")
Task("Fix tool-approval-race-conditions.test.ts failures")
// All three run concurrently
```

### 4. Review and Integrate

When agents return:
- Read each summary
- Verify fixes don't conflict
- Run full test suite
- Integrate all changes

## Agent Prompt Structure

Good agent prompts are:
1. **Focused** - One clear problem domain
2. **Self-contained** - All context needed to understand the problem
3. **Specific about output** - What should the agent return?

```markdown
Fix the 3 failing tests in src/agents/agent-tool-abort.test.ts:

1. "should abort tool with partial output capture" - expects 'interrupted at' in message
2. "should handle mixed completed and aborted tools" - fast tool aborted instead of completed
3. "should properly track pendingToolCount" - expects 3 results but gets 0

These are timing/race condition issues. Your task:

1. Read the test file and understand what each test verifies
2. Identify root cause - timing issues or actual bugs?
3. Fix by:
   - Replacing arbitrary timeouts with event-based waiting
   - Fixing bugs in abort implementation if found
   - Adjusting test expectations if testing changed behavior

Do NOT just increase timeouts - find the real issue.

Return: Summary of what you found and what you fixed.
```

## Background Research & Fact-Finding Archetype

When faced with unfamiliar APIs, third-party library specs, complex documentation, or deep codebase inquiries, delegate the research to an isolated sub-agent.

### Core Mandate
* **Primary-Source Focus:** Instruct the sub-agent to read primary sources (official docs, library type definitions, repository implementation files) rather than relying on training assumptions or secondary summaries.
* **Context Preservation:** Keeps your primary agent context clean and focused on coordination and synthesis.

### Prompt Template

```markdown
Conduct background research on: [Topic/API/Subsystem]

Target Primary Sources:
- Official documentation / specifications: [URLs / File Paths]
- Relevant code files: [Paths]

Your Task:
1. Thoroughly read and analyze the primary-source materials.
2. Investigate specific technical questions:
   - [Question 1]
   - [Question 2]

Report Requirements:
- Include direct citations and code snippets from primary sources for every finding.
- Provide clear, evidence-backed conclusions and recommendations for implementation.
- Do NOT modify production code; only produce the research report.

Return: Confirmation of the written report path and a high-level 3-bullet summary.
```

## Common Mistakes

**❌ Too broad:** "Fix all the tests" - agent gets lost
**✅ Specific:** "Fix agent-tool-abort.test.ts" - focused scope

**❌ No context:** "Fix the race condition" - agent doesn't know where
**✅ Context:** Paste the error messages and test names

**❌ No constraints:** Agent might refactor everything
**✅ Constraints:** "Do NOT change production code" or "Fix tests only"

**❌ Vague output:** "Fix it" - you don't know what changed
**✅ Specific:** "Return summary of root cause and changes"

## When NOT to Use

**Related failures:** Fixing one might fix others - investigate together first
**Need full context:** Understanding requires seeing entire system
**Exploratory debugging:** You don't know what's broken yet
**Shared state:** Agents would interfere (editing same files, using same resources)

## Real Example from Session

* 6 failures across 3 test files (`agent-tool-abort.test.ts`, `batch-completion-behavior.test.ts`, `tool-approval-race-conditions.test.ts`).
* Dispatched 3 parallel agents (1 per test file).
* All investigated concurrently, fixed root causes independently, and integrated without state conflicts.

## Key Benefits

* **Parallelization & Speed:** Concurrent investigations eliminate sequential bottlenecks.
* **Context Isolation:** Narrow scope prevents context contamination across problem domains.

## Verification

After agents return:
1. **Review each summary** - Understand what changed
2. **Check for conflicts** - Did agents edit same code?
3. **Run full suite** - Verify all fixes work together
4. **Spot check** - Agents can make systematic errors

