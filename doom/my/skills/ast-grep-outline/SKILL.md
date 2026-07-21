---
name: ast-grep-outline
description: Use when exploring or modifying a codebase and you need a cheap structural map of files, directories, imports, exports, or direct members before reading full source.
---

# ast-grep outline

`ast-grep outline` generates a compact, syntax-only structural map with line numbers for top-level items (imports, functions, classes, structs, interfaces, modules, enums) and direct members (fields, methods, constructors, enum variants).

## Usage Examples

```shell
# Local structure of a file (default)
ast-grep outline <file>

# Filter by item category
ast-grep outline <file> --items imports
ast-grep outline <file> --items exports

# Map directory API surface or specific symbol types
ast-grep outline <dir> --items exports
ast-grep outline <dir> --type struct,enum,function

# Zoom into a specific symbol with full member details
ast-grep outline <file> --match <symbol> --type class --view expanded

# Dependency mapping across directory
ast-grep outline <dir> --items imports --view signatures

# Outline git-modified files
ast-grep outline $(git diff --name-only HEAD) --items exports
```

## Command Options

- `--items <KIND>`: Top-level item category.
  - `structure`: Local declarations (file default).
  - `exports`: Public API surface (directory default).
  - `imports`: Dependency declarations.
  - `all`: Combines imports, exports, and structure.
- `--view <VIEW>`: Output detail level.
  - `names`: Grouped symbol names (directory default).
  - `signatures`: One line per top-level item.
  - `digest`: Item signatures plus member names.
  - `expanded`: One line per member with explicit line numbers.
- `--match <REGEX>`: Rust regex (case-sensitive) matching top-level item names or signatures. Does not filter members directly.
- `--type <TYPES>`: Comma-separated top-level symbol types (`class`, `function`, `struct`, `enum`, etc.). Does not match member types (`method`, `field`).
- `--pub-members`: Hides private members when printing members.
- `--json=stream`: Streamed JSON output (one object per file) with precise byte/line ranges for programmatic parsing.

## Limitations

- Syntax-only parsing: Does not resolve type references, follow re-exports, or compute call graphs.
- Use `ast-grep run`, `rg`, or LSIF/LSP tools for semantic references, then run `ast-grep outline` on candidate files.

