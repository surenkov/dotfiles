# Condition-Based Waiting (High-Density)

## Overview & Rules
* **Core Principle:** Wait for the actual condition of interest, not a guessed duration.
* **When to Use:** Flaky tests, tests using `setTimeout`/`sleep`, parallel run timeouts, and asynchronous transitions.
* **When NOT to Use:** Testing deliberate timing/throttling/debounce behavior. (If used, document the mathematical justification).

---

## Core Code Pattern

```typescript
// ❌ BEFORE (Guessing):
await new Promise(r => setTimeout(r, 50));
expect(getResult()).toBeDefined();

// ✅ AFTER (Polling):
await waitFor(() => getResult() !== undefined, "result population");
expect(getResult()).toBeDefined();
```

---

## Quick Reference Patterns

| Target | Pattern |
| :--- | :--- |
| **Event** | `waitFor(() => events.find(e => e.type === 'DONE'), "DONE event")` |
| **State** | `waitFor(() => machine.state === 'ready', "state ready")` |
| **Count** | `waitFor(() => items.length >= 5, "items count >= 5")` |
| **File** | `waitFor(() => fs.existsSync(path), "file creation")` |
| **Complex** | `waitFor(() => obj.ready && obj.value > 10, "object initialization & threshold")` |

---

## Standard Polling Implementation

```typescript
async function waitFor<T>(
  condition: () => T | undefined | null | false,
  description: string,
  timeoutMs = 5000,
  pollIntervalMs = 10
): Promise<T> {
  const startTime = Date.now();
  while (true) {
    const result = condition();
    if (result) return result;

    if (Date.now() - startTime > timeoutMs) {
      throw new Error(`Timeout waiting for ${description} after ${timeoutMs}ms`);
    }
    await new Promise(r => setTimeout(r, pollIntervalMs));
  }
}
```

---

## Anti-Patterns & Corrective Actions
* **Polling too fast:** Avoid intervals under 10ms (e.g., `setTimeout(check, 1)`) to prevent high CPU utilization.
* **Infinite Loops:** Always define an explicit timeout parameter with an informative error message.
* **Stale State:** Avoid passing pre-resolved variables to the condition callback. Ensure the callback executes fresh getters/queries each iteration.

---

## Justified Arbitrary Timeouts
Arbitrary delays are valid *only* when testing timing behaviors (e.g. throttle, debounce) and must adhere to this sequence:
1. Wait for the triggering condition using `waitFor`.
2. Sleep for the defined behavior interval.
3. Add a code comment mathematically justifying the duration.

```typescript
await waitForEvent(manager, 'TOOL_STARTED'); // 1. Condition
await new Promise(r => setTimeout(r, 200));   // 2. 2 ticks at 100ms interval (Justified)
```
