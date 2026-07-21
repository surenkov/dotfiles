---
name: systematic-debugging
description: Use when encountering any bug, test failure, or unexpected behavior, before proposing fixes
---

# Systematic Debugging (High-Density)

## Core Principles
* **Iron Law:** NO FIXES WITHOUT ROOT CAUSE INVESTIGATION FIRST. Proposing fixes before completing Phase 1 is prohibited.
* **Core Principle:** Always find the root cause. Symptom fixes are failures.
* **When to Use:** Use for all technical issues (test failures, production bugs, build/integration errors, etc.), especially under time pressure or after previous fixes failed.

---

## Process: The Four Phases

### Phase 1: Root Cause Investigation
1. **Read Errors Carefully:** Read complete stack traces, line numbers, file paths, and error codes.
2. **Reproduce Consistently:** Establish reliable trigger steps. If not reproducible, gather more data; do not guess.
3. **Check Recent Changes:** Analyze git diff, recent commits, dependencies, config, and env differences.
4. **Multi-Component Instrumentation:** For layered/distributed systems:
   * **Boundary Rule:** Before proposing fixes, log inputs/outputs at each component boundary, verify configuration propagation, and check state.
   * *Example:*
     ```bash
     # Workflow env check
     echo "IDENTITY: ${IDENTITY:+SET}${IDENTITY:-UNSET}"
     # Build script env check
     env | grep IDENTITY || echo "IDENTITY unset"
     # System credentials check
     security find-identity -v
     # Target command run
     codesign --sign "$IDENTITY" --verbose=4 "$APP"
     ```
5. **Trace Data Flow:** Trace backward from error site to data origin. Fix at source, not symptom. (See `root-cause-tracing.md`).

### Phase 2: Pattern Analysis
1. **Find Working Examples:** Locate similar, functioning code in the repository.
2. **Read References Fully:** Read reference implementations line-by-line; do not skim.
3. **Compare Differences:** List every discrepancy (configurations, environment, inputs) between working and broken states.
4. **Analyze Dependencies:** Identify environmental and configuration assumptions.

### Phase 3: Hypothesis and Testing
1. **Formulate Single Hypothesis:** Write down a specific "X causes Y because of Z" theory.
2. **Test Minimally:** Make the smallest isolated change to test the hypothesis. Change one variable at a time.
3. **Verify/Reset:** If it works, proceed to Phase 4. If it fails, revert the change completely, form a new hypothesis, and repeat. Do not pile up fixes.
4. **Acknowledge Ignorance:** Clearly state what you don't understand; research or ask for help rather than guessing.

### Phase 4: Implementation
1. **Failing Test Case:** Write an automated, minimal reproduction test (see `superpowers:test-driven-development`).
2. **Single Fix:** Resolve the identified root cause with one focused change. Avoid bundled refactoring or unrelated edits.
3. **Verify:** Confirm the fix passes and regressions are zero (see `superpowers:verification-before-completion`).
4. **Failure Threshold (3-Fix Limit):**
   * **< 3 fixes failed:** Revert, return to Phase 1, re-analyze.
   * **≥ 3 fixes failed:** **STOP immediately.** Do not attempt fix #4. Proceed to step 5.
5. **Architectural Analysis:** If 3+ fixes failed, the issue is likely architectural, not local:
   * *Indicators:* Fixes reveal coupling, require massive refactoring, or cause regressions elsewhere.
   * *Action:* Halt, question the soundness of the pattern, and discuss with your human partner before writing more code.

---

## Diagnostics & Red Flags

### Red Flags: STOP and Return to Phase 1
* Thinking: "Let's change X and see if it works"
* Proposing solutions before tracing data flow or collecting logs
* Bundling multiple experimental changes or skipping test validation
* Attempting a 4th fix without architectural discussion

### Human Partner Redirection Signals
* "Is that not happening?" (Assumed without verifying)
* "Will it show us...?" (Missing instrumentation/evidence)
* "Stop guessing" (Proposing fixes without tracing)
* "Ultrathink this" (Symptom-fixing instead of structural review)

### Common Rationalizations
| Excuse | Reality |
| :--- | :--- |
| "Issue is too simple/urgent for process" | Process is faster than guess-and-check thrashing. |
| "I'll write tests after fixing" | Untested fixes fail. Test-first prevents regression and confirms the fix. |
| "Multiple changes save time" | You lose variables isolation, introducing new bugs. |
| "One more fix attempt" (after 2 failures) | 3+ failures indicate architectural issues, not a missing tweak. |

---

## Environmental / Non-Reproducible Issues
If investigation proves the issue is purely external or timing-dependent:
1. Document the complete scope of the investigation.
2. Implement robust handling (retry policies, timeouts, fallback error states).
3. Inject monitoring/logging for future forensics.

---

## Reference Map
* **`root-cause-tracing.md`:** Backward tracing through execution stack.
* **`defense-in-depth.md`:** Multi-layer validation after root cause identification.
* **`condition-based-waiting.md`:** Eliminating arbitrary delays with condition polling.
--- /dev/null
+++ b/doom/my/skills/systematic-debugging/condition-based-waiting.md
@@ -0,0 +1,52 @@
+# Condition-Based Waiting (High-Density)
+
+## Overview & Rules
+* **Core Principle:** Wait for the actual condition of interest, not a guessed duration.
+* **When to Use:** Flaky tests, tests using `setTimeout`/`sleep`, parallel run timeouts, and asynchronous transitions.
+* **When NOT to Use:** Testing deliberate timing/throttling/debounce behavior. (If used, document the mathematical justification).
+
+---
+
+## Core Code Pattern
+
+```typescript
+// ❌ BEFORE (Guessing):
+await new Promise(r => setTimeout(r, 50));
+expect(getResult()).toBeDefined();
+
+// ✅ AFTER (Polling):
+await waitFor(() => getResult() !== undefined, "result population");
+expect(getResult()).toBeDefined();
+```
+
+---
+
+## Quick Reference Patterns
+
+| Target | Pattern |
+| :--- | :--- |
+| **Event** | `waitFor(() => events.find(e => e.type === 'DONE'), "DONE event")` |
+| **State** | `waitFor(() => machine.state === 'ready', "state ready")` |
+| **Count** | `waitFor(() => items.length >= 5, "items count >= 5")` |
+| **File** | `waitFor(() => fs.existsSync(path), "file creation")` |
+| **Complex** | `waitFor(() => obj.ready && obj.value > 10, "object initialization & threshold")` |
+
+---
+
+## Standard Polling Implementation
+
+```typescript
+async function waitFor<T>(
+  condition: () => T | undefined | null | false,
+  description: string,
+  timeoutMs = 5000,
+  pollIntervalMs = 10
+): Promise<T> {
+  const startTime = Date.now();
+  while (true) {
+    const result = condition();
+    if (result) return result;
+
+    if (Date.now() - startTime > timeoutMs) {
+      throw new Error(`Timeout waiting for ${description} after ${timeoutMs}ms`);
+    }
+    await new Promise(r => setTimeout(r, pollIntervalMs));
+  }
+}
+```
+
+---
+
+## Anti-Patterns & Corrective Actions
+* **Polling too fast:** Avoid intervals under 10ms (e.g., `setTimeout(check, 1)`) to prevent high CPU utilization.
+* **Infinite Loops:** Always define an explicit timeout parameter with an informative error message.
+* **Stale State:** Avoid passing pre-resolved variables to the condition callback. Ensure the callback executes fresh getters/queries each iteration.
+
+---
+
+## Justified Arbitrary Timeouts
+Arbitrary delays are valid *only* when testing timing behaviors (e.g. throttle, debounce) and must adhere to this sequence:
+1. Wait for the triggering condition using `waitFor`.
+2. Sleep for the defined behavior interval.
+3. Add a code comment mathematically justifying the duration.
+
+```typescript
+await waitForEvent(manager, 'TOOL_STARTED'); // 1. Condition
+await new Promise(r => setTimeout(r, 200));   // 2. 2 ticks at 100ms interval (Justified)
+```
--- /dev/null
+++ b/doom/my/skills/systematic-debugging/defense-in-depth.md
@@ -0,0 +1,52 @@
+# Defense-in-Depth Validation (High-Density)
+
+## Core Concept
+* **Principle:** Validate at **every** architectural layer data passes through. Make bugs structurally impossible.
+* **Rationale:** Single validation checks are routinely bypassed by alternative code paths, mocks in testing, or refactoring.
+
+---
+
+## The Four Validation Layers
+
+### Layer 1: Entry Point Validation
+* **Purpose:** Reject invalid input at the system/API boundary before processing.
+```typescript
+function createProject(name: string, workingDirectory: string) {
+  if (!workingDirectory || workingDirectory.trim() === '') throw new Error('Directory cannot be empty');
+  if (!existsSync(workingDirectory)) throw new Error(`Directory does not exist: ${workingDirectory}`);
+  if (!statSync(workingDirectory).isDirectory()) throw new Error(`Not a directory: ${workingDirectory}`);
+}
+```
+
+### Layer 2: Business Logic Validation
+* **Purpose:** Assert semantic state validity prior to execution.
+```typescript
+function initializeWorkspace(projectDir: string, sessionId: string) {
+  if (!projectDir) throw new Error('projectDir required for workspace initialization');
+}
+```
+
+### Layer 3: Environment Guards
+* **Purpose:** Enforce safety and prevent hazardous operations in restricted environments (e.g., tests).
+```typescript
+async function gitInit(directory: string) {
+  if (process.env.NODE_ENV === 'test') {
+    const normalized = normalize(resolve(directory));
+    const tmpDir = normalize(resolve(tmpdir()));
+    if (!normalized.startsWith(tmpDir)) {
+      throw new Error(`Refusing git init outside temp dir in test mode: ${directory}`);
+    }
+  }
+}
+```
+
+### Layer 4: Debug Instrumentation
+* **Purpose:** Log detailed context and stack traces immediately before dangerous operations.
+```typescript
+async function gitInit(directory: string) {
+  logger.debug('Executing git init', {
+    directory,
+    cwd: process.cwd(),
+    stack: new Error().stack,
+  });
+}
+```
+
+---
+
+## Implementation Sequence
+1. **Trace Data Flow:** Map the execution path from bad value origination to the point of failure.
+2. **Identify Checkpoints:** Pinpoint every transition boundary between components.
+3. **Inject Checks:** Implement appropriate validations (Layers 1-4) at each boundary.
+4. **Isolate and Test:** Verify that each layer is capable of catching the bad input independently (e.g., by mocking Layer 1 to confirm Layer 2 catches the invalid input).
--- /dev/null
+++ b/doom/my/skills/systematic-debugging/root-cause-tracing.md
@@ -0,0 +1,52 @@
+# Root Cause Tracing (High-Density)
+
+## Core Principle
+* **Rule:** Never patch where a symptom manifests. Trace backward through the call chain to the original trigger and resolve it at the source.
+* **When to Use:** Errors occurring deep within the stack, long call chains, unknown data origin, or test pollution.
+
+---
+
+## Backward Tracing Process
+
+```
+[1. Symptom]           Error: git init failed in packages/core (src folder)
+      ↑
+[2. Immediate Cause]   execFileAsync('git', ['init'], { cwd: projectDir }) 
+      ↑
+[3. Call Chain]        WorktreeManager.createSessionWorktree(projectDir)
+                       ↳ Session.initializeWorkspace() ↳ Session.create()
+      ↑
+[4. Data Inspection]   projectDir is passed as "" (empty string), defaulting to process.cwd()
+      ↑
+[5. Original Trigger]  setupCoreTest() returned { tempDir: "" } because it was 
+                       evaluated at module load, prior to beforeEach setup.
+```
+
+---
+
+## Diagnostic Instrumentation
+
+When call chains are obscured or dynamic, inject debug logging **before** the dangerous operation:
+
+```typescript
+async function gitInit(directory: string) {
+  const stack = new Error().stack;
+  console.error('DIAGNOSTIC STACK TRACE:', {
+    directory,
+    cwd: process.cwd(),
+    nodeEnv: process.env.NODE_ENV,
+    stack, // Captures complete call chain
+  });
+  await execFileAsync('git', ['init'], { cwd: directory });
+}
+```
+
+### Implementation Tips
+* **Stderr Routing:** Use `console.error()` rather than standard loggers, as test runners often suppress standard logs.
+* **Run & Filter:** Isolate logs by piping output: `npm test 2>&1 | grep 'DIAGNOSTIC'`
+* **Pre-empt Errors:** Always log state *before* a failure occurs to capture the environment successfully.
+* **Identify Polluters:** For test-leakage and state pollution issues, execute tests in isolation or use bisection scripts (e.g., `./find-polluter.sh '.git' 'src/**/*.test.ts'`).
+
+---
+
+## Root-Cause-Tracing Workflow
+
+1. **Detect Failure:** Identify immediate code block throwing/failing.
+2. **Trace Backwards:**
+   * Is this the ultimate source of truth?
+   * If **No**: Move one frame up the stack trace and repeat evaluation.
+   * If **Yes**: Formulate hypothesis and fix at this location.
+3. **Verify and Protect:** Apply **Defense-in-Depth Validation** at each boundary traversed during tracing.
