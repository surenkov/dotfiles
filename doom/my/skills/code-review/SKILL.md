---
name: code-review
description: Review changes since a fixed ref along two axes (Standards & Spec) using parallel sub-agents. Use when reviewing branches, PRs, or diffs.
---

# Code Review

Perform a two-axis review of the diff between `HEAD` and a specified target ref:
- **Axis 1: Standards** — Clean code, repo standards, and baseline code smells.
- **Axis 2: Spec Compliance** — Requirements, correctness, and scope boundaries.

Both axes run via **parallel sub-agents** using the `agent` tool to prevent context pollution, then findings are presented side-by-side without re-ranking.

## Process

### 1. Target Ref & Diff Resolution
Identify the target ref (`main`, `HEAD~1`, commit SHA, branch, tag).
Run diff checks:
```bash
git rev-parse <target-ref>
git diff <target-ref>...HEAD
git log <target-ref>..HEAD --oneline
```
Confirm the ref exists and the diff is non-empty before proceeding.

### 2. Locate Spec & Standards
- **Spec Sources:** Originating issue, spec file (`docs/`, `specs/`), or user description.
- **Standards Sources:** Project rules (`CODING_STANDARDS.md`, `CONTRIBUTING.md`, style guides) plus the **Fowler Smell Baseline**:
  - *Mysterious Name:* Names that obfuscate intent.
  - *Duplicated Code:* Repeated code shapes/logic across hunks.
  - *Feature Envy:* Methods accessing external data more than local state.
  - *Data Clumps:* Sets of primitives that should be bundled into a type.
  - *Primitive Obsession:* Overuse of basic primitives for domain concepts.
  - *Repeated Switches:* Recurrent conditional cascades on the same types.
  - *Shotgun Surgery:* Single logical changes scattered across many files.
  - *Divergent Change:* Single files edited for multiple unrelated reasons.
  - *Speculative Generality:* Unused abstractions or hooks.
  - *Message Chains:* Long method navigation paths.
  - *Middle Man:* Excessive delegation without value-add.
  - *Refused Bequest:* Subclasses ignoring inherited behavior.

*(Note: Repo-documented standards override baseline smells.)*

### 3. Dispatch Parallel Sub-Agents
Dispatch two sub-agents simultaneously using the `agent` tool:

1. **Standards Sub-Agent Prompt:**
   - Pass full diff command and commit list.
   - Pass standards files and Fowler smell baseline.
   - Instructions: Report violations of documented repo standards (cite file/rule) and baseline smells (cite hunk/line). Differentiate hard violations from judgement calls. Keep concise (<400 words).

2. **Spec Compliance Sub-Agent Prompt:**
   - Pass diff command, commit list, and spec source.
   - Instructions: Report (a) missing or partial requirements, (b) unrequested scope creep, and (c) incorrect implementations. Quote spec lines for each finding. Keep concise (<400 words).

### 4. Aggregate & Report
Present reports under `## Standards` and `## Spec Compliance` headers side-by-side. Do **not** merge or re-rank findings across axes.

End with a 1-line summary per axis noting finding counts and top concerns.
