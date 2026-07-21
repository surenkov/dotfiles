# ast-grep Rule Reference

Declarative YAML specifications for matching and filtering Abstract Syntax Tree (AST) nodes.

## Rule Object Overview

- At least one positive rule key (`kind`, `pattern`, etc.) is required.
- Multiple top-level fields within a rule object implicitly evaluate as logical AND.
- Explicit `all` composite rules guarantee evaluation order for dependent metavariables.

### Field Summary

| Property | Category | Type | Purpose | Example |
| :--- | :--- | :--- | :--- | :--- |
| `pattern` | Atomic | String \| Object | Matches AST node by code pattern | `pattern: console.log($ARG)` |
| `kind` | Atomic | String | Matches AST node by Tree-sitter node type | `kind: call_expression` |
| `regex` | Atomic | String | Matches node text via Rust regex | `regex: ^[a-z]+$` |
| `nthChild` | Atomic | Number \| String \| Object | Matches node by parent child index | `nthChild: 1` |
| `range` | Atomic | RangeObject | Matches node by start/end character position | `range: { start: { line: 0, column: 0 }, end: { line: 0, column: 10 } }` |
| `inside` | Relational | Object | Target must be inside node matching sub-rule | `inside: { pattern: class $C { $$$ }, stopBy: end }` |
| `has` | Relational | Object | Target must contain descendant matching sub-rule | `has: { pattern: await $EXPR, stopBy: end }` |
| `precedes` | Relational | Object | Target must appear before node matching sub-rule | `precedes: { pattern: return $VAL }` |
| `follows` | Relational | Object | Target must appear after node matching sub-rule | `follows: { pattern: import $M from '$P' }` |
| `all` | Composite | Array<Rule> | Matches if all sub-rules match (AND) | `all: [{ kind: call_expression }, { pattern: foo($A) }]` |
| `any` | Composite | Array<Rule> | Matches if any sub-rule matches (OR) | `any: [{ pattern: foo() }, { pattern: bar() }]` |
| `not` | Composite | Object | Matches if sub-rule does not match (NOT) | `not: { pattern: console.log($ARG) }` |
| `matches` | Composite | String | Matches referenced utility rule ID | `matches: my-utility-rule-id` |

## Atomic Rules

### `pattern`
Matches nodes by code structure.
- **String Form**: Direct code pattern with metavariables. `pattern: console.log($ARG)`
- **Object Form**:
  - `selector`: Pinpoints specific node type inside parsed pattern (e.g., `selector: field_definition`).
  - `context`: Provides surrounding code for proper parsing (e.g., `context: class { $F }`).
  - `strictness`: Matching strictness algorithm (`cst`, `smart`, `ast`, `relaxed`, `signature`).

### `kind`
Matches node by Tree-sitter grammar node kind string (e.g., `call_expression`, `function_declaration`).

### `regex`
Matches full text content of a node using Rust regular expression syntax. Note: Non-positive rule (requires structure context or matches any node with matching text).

### `nthChild`
Matches nodes by 1-based child position under parent (counts named nodes by default).
- `number`: Exact position (e.g., `nthChild: 1`).
- `string`: An+B position expression (e.g., `nthChild: "2n+1"`).
- `Object`:
  - `position`: Number or An+B expression.
  - `reverse`: Boolean (`true` counts backwards from end).
  - `ofRule`: Rule object to filter target siblings before indexing.

### `range`
Matches nodes by 0-based character line and column positions.
- Object spec: `{ start: { line: L1, column: C1 }, end: { line: L2, column: C2 } }`
- Bounds: `start` is inclusive; `end` is exclusive.

## Relational Rules

Relational rules filter target nodes based on structural position relative to other AST nodes.

### Rules
- `inside`: Target must be contained within a node matching sub-rule.
- `has`: Target must contain a descendant node matching sub-rule.
- `precedes`: Target must precede a node matching sub-rule.
- `follows`: Target must follow a node matching sub-rule.

### Search Options
- `stopBy`: Traversal termination control.
  - `"neighbor"` (default): Stops at immediate surrounding/child non-matching node.
  - `"end"`: Traverses to direction limit (root for `inside`, leaves for `has`).
  - `Rule object`: Stops when reaching a node matching the specified rule (inclusive).
- `field`: Filters specific named AST sub-node field on target (valid on `inside` and `has`).

*Best practice*: Explicitly specify `stopBy: end` for deep tree searches.

## Composite Rules

- `all`: Matches when all sub-rules match. Guarantees evaluation order (crucial for metavariable capture).
- `any`: Matches when at least one sub-rule matches.
- `not`: Inverts match result of a single sub-rule.
- `matches`: References utility rule by ID string. Enables modular rule reuse and recursion.

## Metavariables

Placeholders in patterns matching dynamic AST nodes.

### Types and Syntax
- `$VAR`: Captures a single named AST node.
  - Valid syntax: `$META`, `$META_VAR`, `$_`
  - Invalid syntax: `$invalid` (lowercase), `$123`, `$KEBAB-CASE`
  - Repeated variables enforce value identity: `$A == $A` matches identical operands.
- `$$VAR`: Captures a single unnamed AST node (operators, punctuation).
- `$$$VAR`: Multi-node capture matching zero or more AST nodes (non-greedy).
- `$_VAR`: Non-capturing metavariables (starts with `_`). Matches content without binding variables or enforcing value equality across occurrences.

### Constraints
- Metavariable token must span the entirety of an individual AST node.
- Invalid inline usages: `obj.on$EVENT`, `"Hello $WORLD"`, `a $OP b`, `$jq`.

## YAML Examples

### Functions Containing Await
```yaml
rule:
  kind: function_declaration
  has:
    pattern: await $EXPR
    stopBy: end
```

### Method Context Filtering
```yaml
rule:
  pattern: console.log($$$)
  inside:
    kind: method_definition
    stopBy: end
```

### Combined Conditions (Async Function without Try-Catch)
```yaml
rule:
  all:
    - kind: function_declaration
    - has:
        pattern: await $EXPR
        stopBy: end
    - not:
        has:
          pattern: try { $$$ } catch ($E) { $$$ }
          stopBy: end
```

### Multiple Alternatives
```yaml
rule:
  any:
    - pattern: console.log($$$)
    - pattern: console.warn($$$)
    - pattern: console.error($$$)
    - pattern: console.debug($$$)
```

## Troubleshooting

1. **No Match**: Use `--debug-query=cst` or `dump_syntax_tree` to verify AST node kinds and structure.
2. **Relational Failure**: Verify `stopBy: end` is included for deep traversal.
3. **Invalid Kind**: Verify node kind strings against language-specific Tree-sitter grammars.
4. **Unmatched Metavariables**: Ensure metavariables cover entire AST nodes rather than partial strings/tokens.
5. **Complex Rules**: Split nested logic into standalone sub-rules wrapped in `all`.

