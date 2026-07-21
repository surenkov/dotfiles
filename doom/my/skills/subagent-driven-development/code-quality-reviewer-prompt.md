# Code Quality Reviewer Prompt Template

Verify implementation quality (clean, tested, maintainable). Dispatch ONLY after spec compliance review passes.

```
Task tool (general-purpose):
  Use template at requesting-code-review/code-reviewer.md

  DESCRIPTION: [task summary, from implementer's report]
  PLAN_OR_REQUIREMENTS: Task N from [plan-file]
  BASE_SHA: [commit before task]
  HEAD_SHA: [current commit]
```

**In addition to standard code quality concerns, the reviewer should check:**
- Does each file have one clear responsibility with a well-defined interface?
- Are units decomposed so they can be understood and tested independently?
- Is the implementation following the file structure from the plan?
- Did this implementation create new files that are already large, or significantly grow existing files? (Don't flag pre-existing file sizes — focus on what this change contributed.)

## Fowler Code Smells Taxonomy

Evaluate changes against Martin Fowler's code smells taxonomy:
- **Mysterious Name**: Functions, variables, or classes with unclear or misleading names.
- **Duplicated Code**: Identical or very similar code structures in multiple places.
- **Feature Envy**: A method that accesses data or methods of another object more than its own.
- **Data Clumps**: Groups of data items that frequently appear together across multiple places.
- **Primitive Obsession**: Overuse of primitive types instead of domain objects or value objects.
- **Repeated Switches**: Repeated switch/case or if/else conditional chains on the same type or status.
- **Shotgun Surgery**: Making a single change requires making many small changes across many different classes/files.
- **Divergent Change**: A single module or class is commonly changed in different ways for different reasons.
- **Speculative Generality**: Over-engineering or abstractions added for anticipated future needs that aren't currently required.
- **Message Chains**: A client coupled to the navigation structure (e.g., `a.getB().getC().getD()`).
- **Middle Man**: A class that mostly delegates work to another class without adding value (shallow wrapper).
- **Refused Bequest**: A subclass that uses only a small part of inherited methods/properties from its superclass.

**Code reviewer returns:** Strengths, Issues (Critical/Important/Minor), Assessment
