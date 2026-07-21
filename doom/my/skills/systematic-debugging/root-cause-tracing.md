# Root Cause Tracing

## Core Principle
* **Rule:** Never patch where a symptom manifests. Trace backward through the call chain to the original trigger and resolve it at the source.
* **When to Use:** Errors occurring deep within the stack, long call chains, unknown data origin, or test pollution.

## Backward Tracing Process

1. **Symptom:** Error: `git init` failed in `packages/core` (`src` folder).
2. **Immediate Cause:** `execFileAsync('git', ['init'], { cwd: projectDir })`.
3. **Call Chain:** `WorktreeManager.createSessionWorktree(projectDir)` → `Session.initializeWorkspace()` → `Session.create()`.
4. **Data Inspection:** `projectDir` passed as `""` (empty string), defaulting to `process.cwd()`.
5. **Original Trigger:** `setupCoreTest()` returned `{ tempDir: "" }` because evaluated at module load, prior to `beforeEach` setup.

## Diagnostic Instrumentation

When call chains are obscured or dynamic, inject debug logging **before** the dangerous operation:

```typescript
async function gitInit(directory: string) {
  const stack = new Error().stack;
  console.error('DIAGNOSTIC STACK TRACE:', {
    directory,
    cwd: process.cwd(),
    nodeEnv: process.env.NODE_ENV,
    stack, // Captures complete call chain
  });
  await execFileAsync('git', ['init'], { cwd: directory });
}
```

### Implementation Tips
* **Stderr Routing:** Use `console.error()` rather than standard loggers, as test runners often suppress standard logs.
* **Run & Filter:** Isolate logs by piping output: `npm test 2>&1 | grep 'DIAGNOSTIC'`
* **Pre-empt Errors:** Always log state *before* a failure occurs to capture the environment successfully.
* **Identify Polluters:** For test-leakage and state pollution issues, execute tests in isolation or use bisection scripts (e.g., `./find-polluter.sh '.git' 'src/**/*.test.ts'`).

## Root-Cause-Tracing Workflow

1. **Detect Failure:** Identify immediate code block throwing/failing.
2. **Trace Backwards:**
   * Is this the ultimate source of truth?
   * If **No**: Move one frame up the stack trace and repeat evaluation.
   * If **Yes**: Formulate hypothesis and fix at this location.
3. **Verify and Protect:** Apply **Defense-in-Depth Validation** at each boundary traversed during tracing.
