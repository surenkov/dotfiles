# Defense-in-Depth Validation (High-Density)

## Core Concept
* **Principle:** Validate at **every** architectural layer data passes through. Make bugs structurally impossible.
* **Rationale:** Single validation checks are routinely bypassed by alternative code paths, mocks in testing, or refactoring.

---

## The Four Validation Layers

### Layer 1: Entry Point Validation
* **Purpose:** Reject invalid input at the system/API boundary before processing.
```typescript
function createProject(name: string, workingDirectory: string) {
  if (!workingDirectory || workingDirectory.trim() === '') throw new Error('Directory cannot be empty');
  if (!existsSync(workingDirectory)) throw new Error(`Directory does not exist: ${workingDirectory}`);
  if (!statSync(workingDirectory).isDirectory()) throw new Error(`Not a directory: ${workingDirectory}`);
}
```

### Layer 2: Business Logic Validation
* **Purpose:** Assert semantic state validity prior to execution.
```typescript
function initializeWorkspace(projectDir: string, sessionId: string) {
  if (!projectDir) throw new Error('projectDir required for workspace initialization');
}
```

### Layer 3: Environment Guards
* **Purpose:** Enforce safety and prevent hazardous operations in restricted environments (e.g., tests).
```typescript
async function gitInit(directory: string) {
  if (process.env.NODE_ENV === 'test') {
    const normalized = normalize(resolve(directory));
    const tmpDir = normalize(resolve(tmpdir()));
    if (!normalized.startsWith(tmpDir)) {
      throw new Error(`Refusing git init outside temp dir in test mode: ${directory}`);
    }
  }
}
```

### Layer 4: Debug Instrumentation
* **Purpose:** Log detailed context and stack traces immediately before dangerous operations.
```typescript
async function gitInit(directory: string) {
  logger.debug('Executing git init', {
    directory,
    cwd: process.cwd(),
    stack: new Error().stack,
  });
}
```

---

## Implementation Sequence
1. **Trace Data Flow:** Map the execution path from bad value origination to the point of failure.
2. **Identify Checkpoints:** Pinpoint every transition boundary between components.
3. **Inject Checks:** Implement appropriate validations (Layers 1-4) at each boundary.
4. **Isolate and Test:** Verify that each layer is capable of catching the bad input independently (e.g., by mocking Layer 1 to confirm Layer 2 catches the invalid input).
