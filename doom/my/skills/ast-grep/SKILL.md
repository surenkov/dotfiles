---
name: ast-grep
description: Guide for writing ast-grep rules to perform structural code search and analysis. Use when searching codebases using AST patterns, finding specific language constructs, or executing complex structural code queries.
---

# ast-grep Code Search

## Workflow

1. **Target Identification**: Identify structural patterns, target language, edge cases, and scope.
2. **Test Sample**: Create a minimal code snippet representing target structures.
3. **Rule Construction**: Draft ast-grep rule YAML.
   - Always include `stopBy: end` on relational rules (`inside`, `has`).
   - Use `pattern` for simple matches, `kind` with `has`/`inside` for complex logic.
   - Combine sub-rules using `all`, `any`, or `not`.
4. **Validation**: Test rule against code sample using `--inline-rules` or `--rule`.
5. **Codebase Execution**: Run `ast-grep scan` or `ast-grep run` across target directories.

## CLI Commands

### Debug Query Structure (`--debug-query`)

Inspect CST/AST representation or pattern interpretation:

```bash
# Concrete Syntax Tree (includes punctuation/unnamed nodes)
ast-grep run --pattern 'async function example() { await fetch(); }' --lang javascript --debug-query=cst

# Abstract Syntax Tree (named nodes only)
ast-grep run --pattern 'class User { constructor() {} }' --lang javascript --debug-query=ast

# Pattern Interpretation
ast-grep run --pattern 'class $NAME { $$$BODY }' --lang javascript --debug-query=pattern
```

### Direct Pattern Search (`ast-grep run`)

```bash
# Basic pattern search across current directory
ast-grep run --pattern 'console.log($ARG)' --lang javascript .

# Target specific directory or path
ast-grep run --pattern 'class $NAME' --lang python /path/to/project

# Structured JSON output
ast-grep run --pattern 'function $NAME($$$)' --lang javascript --json .
```

### Rule-Based Search (`ast-grep scan`)

```bash
# Search using YAML rule file
ast-grep scan --rule my_rule.yml /path/to/project

# Search using inline YAML rule string
ast-grep scan --inline-rules "id: my-rule
language: javascript
rule:
  pattern: \$PATTERN" /path/to/project

# Test rule against snippet via stdin
echo "const x = await fetch();" | ast-grep scan --inline-rules "id: test
language: javascript
rule:
  pattern: await \$EXPR" --stdin

# Structured JSON output from scan
echo "const x = await fetch();" | ast-grep scan --inline-rules "..." --stdin --json
ast-grep scan --rule my_rule.yml --json /path/to/project
```

## Rule Writing Guidelines

- **Traversals**: Set `stopBy: end` in relational rules (`has`, `inside`) to force full subtree traversal.
- **Rule Hierarchy**:
  - `pattern`: Single AST node direct code match.
  - `kind` + Relational (`has`, `inside`): Node type matching with nested structural constraints.
  - Composite (`all`, `any`, `not`): Logical combination of multiple constraints.
- **Shell Escaping**: In `--inline-rules`, escape `$` as `\$VAR` inside double quotes or enclose pattern in single quotes `'... $VAR ...'`.

## Common Use Cases

### Find Async Functions Using Await
```bash
ast-grep scan --inline-rules "id: async-await
language: javascript
rule:
  all:
    - kind: function_declaration
    - has:
        pattern: await \$EXPR
        stopBy: end" /path/to/project
```

### Find Code Within Specific Context
```bash
ast-grep scan --inline-rules "id: console-in-class
language: javascript
rule:
  pattern: console.log(\$\$\$)
  inside:
    kind: method_definition
    stopBy: end" /path/to/project
```

### Find Async Functions Missing Try-Catch
```bash
ast-grep scan --inline-rules "id: async-no-trycatch
language: javascript
rule:
  all:
    - kind: function_declaration
    - has:
        pattern: await \$EXPR
        stopBy: end
    - not:
        has:
          pattern: try { \$\$\$ } catch (\$E) { \$\$\$ }
          stopBy: end" /path/to/project
```

## References

- `references/rule_reference.md`: Detailed specification for atomic, relational, and composite rules, plus metavariable syntax.
