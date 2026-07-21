---
name: brainstorming
description: You MUST use this before any creative work - creating features, building components, adding functionality, or modifying behavior. Explores user intent, requirements and design before implementation.
---

# Brainstorming Ideas Into Designs

Turn ideas into structured, high-quality designs and specifications through focused, collaborative alignment.

**CRITICAL REQUIREMENT:** Do NOT write code, scaffold files, or take implementation action until the design is presented and approved by the user. Applies to ALL tasks, regardless of complexity.

## Workflow

Execute steps sequentially:

1. **Context Exploration**: Inspect existing files, docs, and git history to understand constraints and structure.
2. **Targeted Clarification**: Ask **one targeted question at a time** to clarify goals, success criteria, and constraints. Use multiple-choice options where applicable to minimize user effort.
3. **Scope Decomposition**: If the request covers multiple independent subsystems, decompose them. Brainstorm and design the first sub-project first.
4. **Architectural Trade-offs / Design-It-Twice**: Propose 2-3 distinct interface options, contrasting module depth (small interface hiding complex implementation) versus caller simplicity. List trade-offs and explicitly recommend one option with reasoning.
5. **Iterative Specification**: Present proposed architecture and design in logical sections (scaled to complexity). Verify correctness and obtain user approval for each section.
6. **Spec Self-Review**: Validate document to eliminate TODOs/placeholders, resolve contradictions, and clarify ambiguities.
7. **User Final Review**: Ask user to review written spec. Refine if changes are requested.
8. **Transition**: Invoke `writing-plans` skill to generate implementation plan (the only allowed transition skill).

## Decision-Tree Protocol

- **One Question at a Time**: Never overwhelm the user with multiple questions in a single response.
- **Provide Concrete Options**: Always suggest 2-4 structured choices (A, B, C...) with default recommendations to reduce decision fatigue.
- **Decision-Tree Probing**: Use choices to narrow down problem space sequentially, following up based on selected branch.

## Architecture & Codebase Design Principles

- **Deep Modules**: Prefer small interfaces hiding large implementation capability and complexity. Avoid shallow modules where interface complexity approaches implementation complexity.
- **Seams**: Interface boundaries in architecture.
  - *1 Adapter = Hypothetical Seam*: Creating an abstraction interface for a single implementation/adapter is a hypothetical seam. Avoid premature abstraction.
  - *2 Adapters = Real Seam*: A seam becomes real when at least two concrete adapters share the interface.
- **Deletion Test**:
  - Deleting module causes complexity to re-spread across N caller sites -> **Deep Module**.
  - Deleting module causes complexity to vanish entirely without increasing caller burden -> **Pass-through / Shallow Module**.

## Domain Terminology & ADRs

- **Domain Terminology**: Maintain consistent domain terms. Whenever new domain concepts or terms emerge during brainstorming, update `CONTEXT.md` with clear definitions.

## Core Guidelines

- **Focus & Isolation**: Design decoupled systems with clear interfaces. Keep components small, modular, and testable.
- **Working in Existing Codebases**: Propose improvements to existing code only when they directly support the target goal. Maintain existing architectural style.
- **YAGNI**: Ruthlessly cut features that do not serve the immediate core goals.
- **Reviewer Prompt Integration**: Keep `spec-document-reviewer-prompt.md` as a separate file for dispatching dedicated spec-reviewer subagents.
