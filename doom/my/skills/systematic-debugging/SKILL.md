---
name: systematic-debugging
description: Use when encountering any bug, test failure, or unexpected behavior, before proposing fixes
---

# Systematic Debugging

## Core Principles
* **Iron Law:** NO FIXES WITHOUT ROOT CAUSE INVESTIGATION FIRST. Proposing fixes before completing Phase 1 is prohibited.
* **Core Principle:** Always find the root cause. Symptom fixes are failures.
* **When to Use:** Mandatory for all technical issues (test failures, production bugs, build/integration errors), especially under time pressure or after previous fix attempts failed.

## Process: The Four Phases

### Phase 1: Root Cause Investigation & Red-Capable Command Gate
1. **Read Errors Carefully:** Examine complete stack traces, line numbers, file paths, and error codes.
2. **Reproduce Consistently & Red-Capable Command Requirement:** Establish reliable trigger steps. You **MUST** construct a single, fast, deterministic, agent-runnable command that is *Red-Capable* (proven to fail on the bug right now, and will pass when fixed). Do not proceed to code analysis or hypothesis testing until this command exists and fails.
3. **Check Recent Changes & Minimization:** Shrink the reproduction to minimal load-bearing components (cutting inputs/callers one by one). Analyze `git diff`, recent commits, dependencies, configurations, and environment differences.
4. **Multi-Component Instrumentation:**
   * **Boundary Rule:** Log inputs/outputs at component boundaries, verify config propagation, and inspect state before proposing fixes.
   * **`[DEBUG-xxxx]` Tag Mandate:** Tag temporary logs with a unique session identifier (e.g., `[DEBUG-8f2a]`) to allow clean searching (`grep -rn "DEBUG-"`) and effortless removal before committing.
   * **Example:**
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
5. **Trace Data Flow:** Trace backward from error site to data origin. Fix at source, not symptom (see `root-cause-tracing.md`).

#### Feedback Loop Construction Strategies
Construct a deterministic, high-efficiency feedback loop using one of these 10 strategies:
1. **CLI Snapshot:** Capture and compare CLI outputs across runs for instant diffing.
2. **Playwright E2E:** Automate browser interactions and UI assertions to reproduce frontend states.
3. **Captured Trace Replay:** Replay recorded network traffic or execution traces to reproduce exact runtime contexts.
4. **Differential Loops:** Run failing and working implementations side-by-side to pinpoint divergence.
5. **Log Tailing:** Stream filtered log channels in real-time to observe dynamic event flows.
6. **Unit/Integration Isolation:** Extract the failing subsystem into a standalone, minimal test harness free of external dependencies.
7. **State Dumpers:** Serialize in-memory data structures to disk at key milestones for offline analysis.
8. **Mock Servers:** Replace flaky or slow third-party services with deterministic stubs.
9. **Benchmark Loops:** Execute high-frequency profiling loops to expose performance bottlenecks and memory leaks under load.
10. **Smoke Test Scripts:** Author lightweight, single-command shell scripts executing the precise reproduction path.

#### Loop Tightening Rules
* **Sub-2-Second Feedback Goal:** Optimize test harnesses to complete iterations in under 2 seconds (`<2s`).
* **Non-Deterministic Repro Stress Testing:** For flaky or intermittent bugs, run the reproduction script repeatedly (100+ executions) to force consistent reproduction and verify fix reliability.

### Phase 2: Pattern Analysis
1. **Find Working Examples:** Locate similar, functioning code in the repository.
2. **Read References Fully:** Read reference implementations line-by-line; do not skim.
3. **Compare Differences:** Document every discrepancy (configurations, environment, inputs) between working and broken states.
4. **Analyze Dependencies:** Identify environmental and configuration assumptions.

### Phase 3: Hypothesis and Testing
1. **Multi-Hypothesis Ranking Mandate:** Generate 3 to 5 distinct, falsifiable hypotheses before testing any changes.
2. **Falsifiable Format:** Write hypotheses explicitly as `"If X is the cause, changing Y will make bug disappear / changing Z will make it worse"`.
3. **Rank Hypotheses:** Order by probability and verification cost (effort/time). Document the list clearly.
4. **Test Minimally in Ranked Order:** Test the highest-ranked hypothesis using the smallest isolated change. Change one variable at a time.
5. **Verify/Reset:** If successful, proceed to Phase 4. If failed, revert changes completely, eliminate the hypothesis, move to the next ranked hypothesis, and repeat. Do not accumulate speculative fixes.
6. **Acknowledge Ignorance:** Explicitly state unknown factors; research or ask for help rather than guessing.

### Phase 4: Implementation & Verification
1. **Failing Test Case (Test Seam):** Write an automated, minimal reproduction test at the highest appropriate call-site seam (see `superpowers:test-driven-development`).
2. **Single Fix:** Resolve root cause with one focused change. Avoid bundled refactoring or unrelated edits.
3. **Verify with Red-Capable Command:** Confirm the Red-Capable command passes and regressions are zero (see `superpowers:verification-before-completion`).
4. **Failure Threshold (3-Fix Limit):**
   * **< 3 fixes failed:** Revert, return to Phase 1, and re-analyze.
   * **≥ 3 fixes failed:** **STOP immediately.** Do not attempt fix #4. Proceed to step 5.
5. **Architectural Analysis:** If 3+ fixes fail, the issue is architectural rather than local.
   * *Indicators:* Fixes reveal coupling, require massive refactoring, or cause regressions elsewhere.
   * *Action:* Halt, re-evaluate pattern soundness, and consult with your human partner before writing more code.

### Phase 5: Post-Mortem & Prevention Review
1. **Root Cause Retrospective:** Ask: *"What structural or testing gap allowed this bug to exist undetected?"*
2. **Prevention Handoff:** Log architectural debt or missing test seams discovered during debugging for follow-up via `domain-modeling` or `codebase-design`.

## Diagnostics & Red Flags

### Red Flags: STOP and Return to Phase 1
* Thinking: "Let's change X and see if it works."
* Proposing solutions before tracing data flow or collecting logs.
* Bundling multiple experimental changes or skipping test validation.
* Attempting a 4th fix without architectural discussion.

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
| "Multiple changes save time" | Loses variable isolation and introduces new bugs. |
| "One more fix attempt" (after 2 failures) | 3+ failures indicate architectural issues, not a missing tweak. |

## Environmental / Non-Reproducible Issues
If the issue is purely external or timing-dependent:
1. Document the complete scope of the investigation.
2. Implement robust handling (retry policies, timeouts, fallback error states).
3. Inject monitoring and logging for future forensics.

## Reference Map
* **`root-cause-tracing.md`:** Backward tracing through execution stack.
* **`defense-in-depth.md`:** Multi-layer validation after root cause identification.
* **`condition-based-waiting.md`:** Eliminating arbitrary delays with condition polling.
