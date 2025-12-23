# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains **evil-tex-ts** — a tree-sitter-based implementation of LaTeX text objects for Evil mode in Emacs. The `../reference-project/` directory contains the original `evil-tex` package as a reference implementation.

## Architecture

### evil-tex-ts (This Project)
- **Location**: current directory
- **Target**: Emacs 29.1+ with built-in tree-sitter
- **Dependencies**: `evil`, `treesit` (no AUCTeX required)
- **Main file**: `evil-tex-ts.el` (to be created)

Uses tree-sitter AST queries instead of regex for accurate parsing:
```elisp
(treesit-parent-until (treesit-node-at (point) 'latex)
  (lambda (n) (member (treesit-node-type n) '("generic_environment"))))
```

### Reference Implementation
- **Location**: `../reference-project/`
- **Source**: https://github.com/iyefrat/evil-tex
- **Approach**: Regex + AUCTeX parsing

## Key Text Objects

| Key | Name | Tree-sitter nodes |
|-----|------|-------------------|
| ie/ae | environment | `generic_environment`, `math_environment` |
| ic/ac | command | `generic_command` |
| im/am | math | `inline_formula`, `displayed_equation` |
| id/ad | delimiter | math delimiters |

## Evil Text Object Pattern

All text objects follow this pattern:
1. `evil-tex-ts--bounds-of-X` returns `(outer-beg outer-end inner-beg inner-end)`
2. `evil-define-text-object` for outer uses `(nbutlast result 2)`
3. `evil-define-text-object` for inner uses `(last result 2)`

## Toggles (mt* prefix)

- `mte` — environment asterisk: `equation` ↔ `equation*`
- `mtm` — math mode: `\(...\)` ↔ `\[...\]`
- `mtd` — delimiter sizing: `()` ↔ `\left(\right)`
- `mtc` — command asterisk: `\section` ↔ `\section*`

## Planning Documents

- `general_plan.md` — implementation roadmap with phases
- `examples.md` — usage examples for all features

## Testing

**IMPORTANT**: Always run `make test` after making changes to verify everything works.

- Test infrastructure: `tests/` directory
- Reference test file: `../reference-project/test/test.tex`
- Run all tests: `make test`
