# evil-tex-bora: General Plan

## Overview

Tree-sitter based LaTeX text objects for Evil mode in Emacs 29+.

## Requirements

- Emacs 29.1+
- evil 1.0+
- tree-sitter-latex grammar

## Architecture

### Core Components

1. **Tree-sitter Integration**
   - Parser initialization and validation
   - AST query functions
   - Node boundary extraction

2. **Text Objects**
   | Key | Name | Node Types |
   |-----|------|------------|
   | ie/ae | environment | `generic_environment`, `math_environment` |
   | ic/ac | command | `generic_command` |
   | im/am | math | `inline_formula`, `displayed_equation` |
   | id/ad | delimiter | math delimiters |

3. **Toggles** (configurable `mt*` or `ts*` prefix)
   | Key | Function |
   |-----|----------|
   | mtd | `()` <-> `\left(\right)` |
   | mte | `env` <-> `env*` |
   | mtm | `$`, `\(...\)` <-> `\[...\]` |
   | mtc | `\cmd` <-> `\cmd*` |

4. **Evil-surround Integration**
   - Surround pairs for m, M, c, e, d, ^, _
   - Prompt keymaps

### Key Tree-sitter Patterns

```elisp
;; Find parent node of specific type
(treesit-parent-until (treesit-node-at (point) 'latex)
  (lambda (n) (member (treesit-node-type n) '("generic_environment"))))

;; Get node boundaries
(treesit-node-start node)
(treesit-node-end node)

;; Get child by field name
(treesit-node-child-by-field-name env-node "begin")
```

## Implementation Phases

### Phase 1: Infrastructure
- [x] Create project structure
- [x] Create general_plan.md
- [x] Create examples.md
- [x] Create evil-tex-bora.el skeleton
- [x] Create test infrastructure (tests/, Makefile)
- [x] Create CLAUDE.md

### Phase 2: Text Objects
- [x] ie/ae (environment)
- [x] ic/ac (command)
- [x] im/am (math)
- [x] id/ad (delimiter)

### Phase 3: Toggles
- [x] mte (environment asterisk)
- [x] mtm (math mode)
- [x] mtd (delimiter sizing)
- [x] mtc (command asterisk)

### Phase 4: Surround
- [x] Evil-surround integration
- [x] Prompt keymaps

### Phase 5: Finalization
- [x] Minor mode (`evil-tex-bora-mode`)
- [x] Keymaps (text objects + toggle bindings)
- [x] Tests (113 tests passing)

## Reference

- Original evil-tex: `reference-project/evil-tex.el`
- Tree-sitter-latex grammar: https://github.com/latex-lsp/tree-sitter-latex
