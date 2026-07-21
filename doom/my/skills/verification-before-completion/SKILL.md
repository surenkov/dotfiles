---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing, before committing or creating PRs - requires running verification commands and confirming output before making any success claims; evidence before assertions always
---

# Verification Before Completion

## Overview

Claiming work is complete without verification is dishonesty, not efficiency. Core principle: Evidence before claims, always. Violating the letter of this rule violates its spirit.

## The Iron Law

```
NO COMPLETION CLAIMS WITHOUT FRESH VERIFICATION EVIDENCE
```
If you haven't run the verification command in this message, you cannot claim it passes.

## The Gate Function

BEFORE claiming any status or expressing satisfaction:
1. **IDENTIFY:** What command proves this claim?
2. **RUN:** Execute the FULL command (fresh, complete).
3. **READ:** Read full output, check exit code, count failures.
4. **VERIFY:** Does output confirm the claim?
   - If NO: State actual status with evidence.
   - If YES: State claim WITH evidence.
5. **ONLY THEN:** Make the claim.

*Skipping any step equals lying, not verifying.*

## Common Failures

| Claim | Requires | Not Sufficient |
| Tests pass | Test command output: 0 failures | Previous run, "should pass" |
| Linter clean | Linter output: 0 errors | Partial check, extrapolation |
| Build succeeds | Build command: exit 0 | Linter passing, logs look good |
| Bug fixed | Test original symptom: passes | Code changed, assumed fixed |
| Regression test works | Red-green cycle verified | Test passes once |
| Agent completed | VCS diff shows changes | Agent reports "success" |
| Requirements met | Line-by-line checklist | Tests passing |

## Red Flags - STOP

- Using "should", "probably", "seems to"
- Expressing satisfaction before verification ("Great!", "Perfect!", "Done!", etc.)
- About to commit/push/PR without verification
- Trusting agent success reports
- Relying on partial verification
- Thinking "just this once"
- Tired and wanting work over
- **ANY wording implying success without having run verification**

## Rationalization Prevention

| Excuse | Reality |
| "Should work now" | RUN the verification |
| "I'm confident" | Confidence ≠ evidence |
| "Just this once" | No exceptions |
| "Linter passed" | Linter ≠ compiler |
| "Agent said success" | Verify independently |
| "I'm tired" | Exhaustion ≠ excuse |
| "Partial check is enough" | Partial proves nothing |
| "Different words so rule doesn't apply" | Spirit over letter |

## Key Patterns

**Tests:**
```
✅ [Run test command] [See: 34/34 pass] "All tests pass"
❌ "Should pass now" / "Looks correct"
```

**Regression tests (TDD Red-Green):**
```
✅ Write → Run (pass) → Revert fix → Run (MUST FAIL) → Restore → Run (pass)
❌ "I've written a regression test" (without red-green verification)
```

**Build:**
```
✅ [Run build] [See: exit 0] "Build passes"
❌ "Linter passed" (linter doesn't check compilation)
```

**Requirements:**
```
✅ Re-read plan → Create checklist → Verify each → Report gaps or completion
❌ "Tests pass, phase complete"
```

**Agent delegation:**
```
✅ Agent reports success → Check VCS diff → Verify changes → Report actual state
❌ Trust agent report
```

## Code Review for Non-Trivial VCS Diffs

For all non-trivial changes (multi-file diffs, structural refactoring, new features, or core bug fixes), perform a rigorous diff inspection before claiming completion.

It is strongly recommended to invoke `code-review` skill to inspect and evaluate the VCS diff along two critical axes:

1. **Standards Compliance:**
   - Verify adherence to repository coding conventions, error handling standards, type safety, and architectural patterns.
   - Ensure temporary debug logs (e.g., `[DEBUG-xxxx]`), commented-out code, and extraneous edits have been completely purged.

2. **Specification & Requirements Compliance:**
   - Cross-check every requirement in the task prompt or feature plan line-by-line against actual diff changes.
   - Confirm no missing edge-case handling or unintended regressions were introduced in modified files.

## Failure Memories

- Human partner said "I don't believe you" — trust broken.
- Undefined functions shipped — would crash in production.
- Missing requirements shipped — incomplete features.
- Time wasted on false completion -> redirect -> rework.
- Violates core rule: Honesty is a core value. Lie and you will be replaced.

## When To Apply

**ALWAYS before:**
- ANY variation of success/completion claims
- ANY expression of satisfaction
- ANY positive statement about work state
- Committing, PR creation, task completion
- Moving to next task
- Delegating to agents

**Rule applies to:**
- Exact phrases
- Paraphrases and synonyms
- Implications of success
- ANY communication suggesting completion/correctness
