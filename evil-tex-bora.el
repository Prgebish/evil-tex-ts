;;; evil-tex-bora.el --- Tree-sitter based LaTeX text objects for Evil -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author: chestnykh
;; Maintainer: chestnykh
;; Created: 2024
;; Version: 0.1.0
;; Keywords: tex, emulation, vi, evil, wp
;; Homepage: https://github.com/chestnykh/evil-tex-bora
;; Package-Requires: ((emacs "29.1") (evil "1.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Tree-sitter based LaTeX text objects for Evil mode.
;; Requires Emacs 29.1+ with built-in tree-sitter support.
;;
;; Text objects:
;;   ie/ae - LaTeX environment
;;   ic/ac - LaTeX command
;;   im/am - Math mode
;;   id/ad - Delimiters
;;
;; Toggles (mt prefix):
;;   mte - Toggle environment asterisk (equation <-> equation*)
;;   mtm - Toggle math mode (\(...\) <-> \[...\])
;;   mtd - Toggle delimiter sizing (() <-> \left(\right))
;;   mtc - Toggle command asterisk (\section <-> \section*)
;;
;;; Code:

(require 'evil)
(require 'treesit)

;;; Customization

(defgroup evil-tex-bora nil
  "Tree-sitter based LaTeX text objects for Evil."
  :version "29.1"
  :group 'evil
  :prefix "evil-tex-bora-")

(defcustom evil-tex-bora-select-newlines-with-envs t
  "Whether to select newlines with environment text objects.

When non-nil:
- Outer environment (ae) includes trailing newline for clean deletion
- Inner environment (ie) includes the newline before \\end{...} and is anchored
  to the \\end indentation (so `die' removes the whole inner block and doesn't
  land in column 0)
- With `cie' on multi-line environments, the inner region becomes linewise so
  Evil opens a blank line for insertion

This makes `dae' delete the entire environment including its line,
and `cie' place cursor on a clean line for replacement."
  :type 'boolean
  :group 'evil-tex-bora)

;;; Tree-sitter utilities

(defun evil-tex-bora--ensure-parser ()
  "Ensure LaTeX tree-sitter parser is available.
Returns non-nil if parser is ready, nil otherwise."
  (and (treesit-available-p)
       (treesit-language-available-p 'latex)))

(defun evil-tex-bora--get-node-at-point ()
  "Get tree-sitter node at point for LaTeX."
  (when (evil-tex-bora--ensure-parser)
    (treesit-node-at (point) 'latex)))

(defun evil-tex-bora--find-parent-by-type (node types)
  "Find parent of NODE that matches one of TYPES.
TYPES is a list of node type strings."
  (treesit-parent-until
   node
   (lambda (n) (member (treesit-node-type n) types))))

(defun evil-tex-bora--node-bounds (node)
  "Get bounds of NODE as (start . end)."
  (when node
    (cons (treesit-node-start node)
          (treesit-node-end node))))

;;; Text object helpers

(defun evil-tex-bora--bounds-of-environment ()
  "Return bounds of LaTeX environment at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

When `evil-tex-bora-select-newlines-with-envs' is non-nil:
- outer-end extends to include trailing newline (for clean `dae' deletion)
- inner selection includes the newline before \\end{...} (so `die' removes
  the whole inner block, like Vim's `di)` on multi-line parens)
- inner selection is anchored to the indentation level of the \\end line,
  so after `die' point doesn't jump to column 0."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (env-node (evil-tex-bora--find-parent-by-type
                         node '("generic_environment" "math_environment"))))
    (let* ((outer-beg (treesit-node-start env-node))
           (outer-end (treesit-node-end env-node))
           ;; Find \begin{...} and \end{...} to get inner bounds
           (begin-node (treesit-node-child-by-field-name env-node "begin"))
           (end-node (treesit-node-child-by-field-name env-node "end"))
           (inner-beg (if begin-node (treesit-node-end begin-node) outer-beg))
           (inner-end (if end-node (treesit-node-start end-node) outer-end)))
      ;; Adjust bounds for newlines if option is enabled
      (when evil-tex-bora-select-newlines-with-envs
        ;; Extend outer-beg to include leading whitespace on the line
        ;; This ensures 'dae' deletes the whole line including indentation
        (save-excursion
          (goto-char outer-beg)
          (let ((line-start (line-beginning-position)))
            (when (string-match-p "\\`[ \t]*\\'" (buffer-substring line-start outer-beg))
              (setq outer-beg line-start))))
        ;; Extend outer-end to include trailing newline
        (save-excursion
          (goto-char outer-end)
          (when (eq (char-after) ?\n)
            (setq outer-end (1+ outer-end))))
        (let ((end-command-pos inner-end)
              end-indent-col
              content-line-bol
              content-indent-col
              start-col)
          ;; Ensure END-COMMAND-POS points at the backslash of \end{...}
          ;; even if tree-sitter's `end' node starts at `end' (without the \).
          (save-excursion
            (goto-char end-command-pos)
            (when (and (> end-command-pos (point-min))
                       (eq (char-before) ?\\))
              (setq end-command-pos (1- end-command-pos))))
          (setq inner-end end-command-pos)
          (save-excursion
            (goto-char end-command-pos)
            (setq end-indent-col (current-column)))
          ;; If the environment is multi-line, anchor the inner range start/end
          ;; to the indentation of the \end line so `die' doesn't land in column 0.
          (save-excursion
            (goto-char inner-beg)
            (when (eq (char-after) ?\n)
              (setq content-line-bol (1+ (point)))))
          (when (and content-line-bol end-indent-col)
            (save-excursion
              (goto-char content-line-bol)
              (setq content-indent-col (progn (back-to-indentation) (current-column))))
            (setq start-col (min end-indent-col content-indent-col))
            ;; Start inside the indentation on the first content line.
            (save-excursion
              (goto-char content-line-bol)
              (move-to-column start-col)
              (setq inner-beg (point)))
            ;; End inside the indentation on the \end line (same column),
            ;; so the remaining indentation stays correct after deletion.
            (save-excursion
              (goto-char end-command-pos)
              (goto-char (line-beginning-position))
              (move-to-column start-col)
              (setq inner-end (point))))))
      (list outer-beg outer-end inner-beg inner-end))))

(defun evil-tex-bora--change-operator-p ()
  "Return non-nil when the current Evil operator is a change command."
  (when (boundp 'evil-this-operator)
    (let ((op evil-this-operator))
      (or (eq op 'evil-change)
          (and (boundp 'evil-change-commands)
               (memq op evil-change-commands))))))

(defun evil-tex-bora--bounds-of-environment-inner-lines ()
  "Return linewise inner bounds of environment at point as (BEG . END).

BEG is the beginning of the first line after \\begin{...}.
END is the beginning of the line containing \\end{...}.

Returns nil for single-line environments or if no environment is found."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (env-node (evil-tex-bora--find-parent-by-type
                         node '("generic_environment" "math_environment")))
              (begin-node (treesit-node-child-by-field-name env-node "begin"))
              (end-node (treesit-node-child-by-field-name env-node "end")))
    (let ((begin-end (treesit-node-end begin-node))
          (end-start (treesit-node-start end-node)))
      (save-excursion
        (goto-char begin-end)
        (when (eq (char-after) ?\n)
          (forward-char 1)
          (cons (point)
                (save-excursion
                  (goto-char end-start)
                  (line-beginning-position))))))))

(defconst evil-tex-bora--command-types
  '("generic_command"
    ;; Section commands
    "part" "chapter" "section" "subsection" "subsubsection"
    "paragraph" "subparagraph"
    ;; Reference commands
    "label_definition" "label_reference" "label_reference_range"
    "citation" "text_reference"
    ;; Include commands
    "package_include" "class_include" "latex_include"
    "biblatex_include" "bibtex_include" "graphics_include"
    "svg_include" "inkscape_include" "verbatim_include"
    "import_include" "input_include")
  "List of tree-sitter node types that represent LaTeX commands.")

(defun evil-tex-bora--bounds-of-command ()
  "Return bounds of LaTeX command at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

Inner command is defined as:
- If cursor is inside a {} group: the content of that {} group
- Otherwise: the content of the nearest {} group to the right
- If no {} groups exist: empty (inner-beg = inner-end = outer-end)

For example with cursor at |:
  \\frac{a|}{b}   -> inner is \"a\"
  \\frac{a}{b|}   -> inner is \"b\"
  \\fr|ac{a}{b}   -> inner is \"a\" (nearest to the right)
  \\sqrt[n]{x|}   -> inner is \"x\"
  \\alpha|        -> inner is empty"
  ;; Try fallback first - it handles cases like \sqrt[n]{x} where
  ;; tree-sitter doesn't recognize the curly group as part of the command
  (or (evil-tex-bora--bounds-of-command-fallback)
      (evil-tex-bora--bounds-of-command-tree-sitter)))

(defun evil-tex-bora--bounds-of-command-tree-sitter ()
  "Return bounds of LaTeX command using tree-sitter nodes.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

This function handles the case where tree-sitter doesn't include
optional arguments [...] or curly arguments {...} as part of the command node.
For example, \\sqrt[n]{x} is parsed as separate nodes:
- generic_command: \\sqrt
- text inside [...]: n
- curly_group: {x}

We detect this and extend the command bounds to include trailing [...] and {...}."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (cmd-node (evil-tex-bora--find-parent-by-type
                         node evil-tex-bora--command-types)))
    (let* ((outer-beg (treesit-node-start cmd-node))
           (outer-end (treesit-node-end cmd-node))
           (pt (point))
           ;; Collect curly arg nodes that are children of the command
           (curly-nodes (evil-tex-bora--collect-command-curly-args cmd-node)))
      ;; If the command has no curly children, check if there are trailing
      ;; [...] and {...} siblings that belong to this command
      (when (null curly-nodes)
        (let ((extended (evil-tex-bora--extend-command-bounds cmd-node)))
          (when extended
            (setq outer-end (nth 0 extended))
            (setq curly-nodes (nth 1 extended)))))
      (let* ((target-curly (evil-tex-bora--find-target-curly-group curly-nodes pt))
             (inner-beg (if target-curly
                            (1+ (treesit-node-start target-curly))
                          outer-end))
             (inner-end (if target-curly
                            (1- (treesit-node-end target-curly))
                          outer-end)))
        (list outer-beg outer-end inner-beg inner-end)))))

(defun evil-tex-bora--extend-command-bounds (cmd-node)
  "Check if CMD-NODE has trailing [...] and {...} siblings.
Returns (new-outer-end curly-nodes) if found, nil otherwise.
CURLY-NODES is a list of curly_group nodes for inner selection."
  (save-excursion
    (goto-char (treesit-node-end cmd-node))
    (let ((continue t)
          (new-end (treesit-node-end cmd-node))
          (curly-nodes nil))
      ;; Skip whitespace and look for [ or {
      (skip-chars-forward " \t\n")
      ;; Skip optional [...] argument
      (when (eq (char-after) ?\[)
        (forward-sexp)
        (setq new-end (point))
        (skip-chars-forward " \t\n"))
      ;; Collect all {...} arguments
      (while (eq (char-after) ?\{)
        (let ((grp-start (point)))
          (forward-sexp)
          (let ((grp-end (point)))
            ;; Find the curly_group node at this position
            (let ((node-at (treesit-node-at grp-start 'latex)))
              (when node-at
                (let ((curly-node (evil-tex-bora--find-parent-by-type
                                   node-at '("curly_group"))))
                  (when curly-node
                    (push curly-node curly-nodes)))))
            (setq new-end grp-end))
          (skip-chars-forward " \t\n")))
      ;; Only return if we found something new
      (when (and curly-nodes (> new-end (treesit-node-end cmd-node)))
        (list new-end (nreverse curly-nodes))))))

(defun evil-tex-bora--find-target-curly-group (curly-nodes pt)
  "Find the target curly group for inner selection from CURLY-NODES.
Returns the curly group containing PT, or the nearest one to the right of PT,
or the first curly group if PT is after all of them."
  (when curly-nodes
    (let ((containing nil)
          (nearest-right nil)
          (nearest-right-dist most-positive-fixnum))
      ;; First pass: find containing group or nearest to the right
      (dolist (curly curly-nodes)
        (let ((start (treesit-node-start curly))
              (end (treesit-node-end curly)))
          (cond
           ;; Point is inside this curly group
           ((and (>= pt start) (<= pt end))
            (setq containing curly))
           ;; This curly group is to the right of point
           ((> start pt)
            (let ((dist (- start pt)))
              (when (< dist nearest-right-dist)
                (setq nearest-right curly
                      nearest-right-dist dist)))))))
      ;; Return containing group, or nearest right, or first curly
      (or containing nearest-right (car curly-nodes)))))

(defconst evil-tex-bora--curly-arg-node-types
  '("curly_group" "curly_group_text" "curly_group_text_list"
    "curly_group_path" "curly_group_path_list" "curly_group_glob_pattern"
    "curly_group_label" "curly_group_key_value" "curly_group_author_list")
  "List of tree-sitter node types that represent curly brace command arguments.")

(defun evil-tex-bora--collect-command-curly-args (cmd-node)
  "Collect curly group argument nodes from CMD-NODE."
  (let ((args nil)
        (child-count (treesit-node-child-count cmd-node)))
    (dotimes (i child-count)
      (let* ((child (treesit-node-child cmd-node i))
             (type (treesit-node-type child)))
        (when (member type evil-tex-bora--curly-arg-node-types)
          (push child args))))
    (nreverse args)))

(defun evil-tex-bora--bounds-of-command-fallback ()
  "Return bounds of LaTeX command using fallback search.
This handles cases where tree-sitter doesn't recognize the command structure,
such as \\sqrt[n]{x} where the curly group is not a child of the command.
Also handles when cursor is inside optional [...] argument.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((node (evil-tex-bora--get-node-at-point)))
    (when node
      ;; Try to find orphan curly_group first
      (let ((curly-node (evil-tex-bora--find-parent-by-type
                         node '("curly_group"))))
        (if curly-node
            ;; Check if this curly_group is "orphan" (not part of a command)
            (let ((parent (treesit-node-parent curly-node)))
              (when (and parent
                         (member (treesit-node-type parent)
                                 '("source_file" "text" "curly_group"
                                   "inline_formula" "displayed_equation")))
                ;; This curly_group is not part of a recognized command
                ;; Look backward for a command pattern and find all curly groups
                (evil-tex-bora--find-command-with-curly-fallback curly-node)))
          ;; No curly_group found - maybe cursor is inside [...] argument
          ;; Check if we're inside brackets that follow a command
          (evil-tex-bora--find-command-from-bracket-position))))))

(defun evil-tex-bora--find-command-from-bracket-position ()
  "Find command bounds when cursor is inside optional [...] argument.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((pt (point))
          bracket-start bracket-end)
      ;; Check if we're inside brackets by looking for enclosing [ ... ]
      (save-excursion
        (when (re-search-backward "\\[" (line-beginning-position 0) t)
          (setq bracket-start (point))
          (condition-case nil
              (progn
                (forward-sexp)
                (when (> (point) pt)
                  ;; Point was inside this bracket pair
                  (setq bracket-end (point))))
            (error nil))))
      (when (and bracket-start bracket-end)
        ;; Now look backward for command
        (goto-char bracket-start)
        (skip-chars-backward " \t\n")
        (when (re-search-backward "\\\\[a-zA-Z@*]+" (line-beginning-position 0) t)
          (let ((cmd-start (match-beginning 0))
                (cmd-name-end (match-end 0)))
            ;; Verify the command is immediately before the bracket
            (goto-char cmd-name-end)
            (skip-chars-forward " \t\n")
            (when (= (point) bracket-start)
              ;; Found the command! Now collect the full extent
              (goto-char bracket-end)
              (skip-chars-forward " \t\n")
              ;; Collect all curly groups after the bracket
              (let ((cmd-end bracket-end)
                    (curly-groups nil))
                (while (eq (char-after) ?\{)
                  (let ((grp-start (point)))
                    (forward-sexp)
                    (push (cons grp-start (point)) curly-groups)
                    (setq cmd-end (point))
                    (skip-chars-forward " \t\n")))
                (setq curly-groups (nreverse curly-groups))
                ;; Find the target curly group
                (if curly-groups
                    (let ((target (evil-tex-bora--find-target-curly-group-from-positions
                                   curly-groups pt)))
                      (when target
                        (list cmd-start cmd-end
                              (1+ (car target)) (1- (cdr target)))))
                  ;; No curly groups - inner is empty
                  (list cmd-start cmd-end cmd-end cmd-end))))))))))

(defun evil-tex-bora--find-command-with-curly-fallback (curly-node)
  "Find command bounds when CURLY-NODE is not recognized as part of a command.
Looks backward from CURLY-NODE for a command pattern like \\cmd or \\cmd[...].
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((curly-start (treesit-node-start curly-node))
        (curly-end (treesit-node-end curly-node))
        (pt (point)))
    (save-excursion
      (goto-char curly-start)
      ;; Skip back over any preceding curly groups that belong to the same command
      (let ((first-curly-start curly-start)
            (continue t))
        (while continue
          (skip-chars-backward " \t\n")
          (if (eq (char-before) ?\})
            ;; There's a preceding curly group
            (progn
              (backward-sexp)
              (setq first-curly-start (point)))
            (setq continue nil)))
        ;; Now skip back over optional [...] argument
        (skip-chars-backward " \t\n")
        (when (eq (char-before) ?\])
          (backward-sexp))
        (skip-chars-backward " \t\n")
        ;; Look for \command pattern
        (when (re-search-backward "\\\\[a-zA-Z@*]+" (line-beginning-position 0) t)
          (let ((cmd-start (match-beginning 0))
                (cmd-name-end (match-end 0)))
            ;; Verify this command is immediately before our curly group
            (goto-char cmd-name-end)
            (skip-chars-forward " \t\n")
            (when (eq (char-after) ?\[)
              (forward-sexp)
              (skip-chars-forward " \t\n"))
            (when (= (point) first-curly-start)
              ;; Found the command - now find all curly groups and the command end
              (let ((cmd-end first-curly-start)
                    (curly-groups nil))
                ;; Collect all curly groups
                (while (eq (char-after) ?\{)
                  (let ((grp-start (point)))
                    (forward-sexp)
                    (push (cons grp-start (point)) curly-groups)
                    (setq cmd-end (point))
                    (skip-chars-forward " \t\n")))
                (setq curly-groups (nreverse curly-groups))
                ;; Find the target curly group (containing point or nearest right)
                (let ((target (evil-tex-bora--find-target-curly-group-from-positions
                               curly-groups pt)))
                  (when target
                    (list cmd-start cmd-end
                          (1+ (car target)) (1- (cdr target)))))))))))))

(defun evil-tex-bora--find-target-curly-group-from-positions (curly-positions pt)
  "Find target curly group from CURLY-POSITIONS (list of (start . end) cons).
Returns the group containing PT, or nearest to the right, or first."
  (when curly-positions
    (let ((containing nil)
          (nearest-right nil)
          (nearest-right-dist most-positive-fixnum))
      (dolist (grp curly-positions)
        (let ((start (car grp))
              (end (cdr grp)))
          (cond
           ((and (>= pt start) (<= pt end))
            (setq containing grp))
           ((> start pt)
            (let ((dist (- start pt)))
              (when (< dist nearest-right-dist)
                (setq nearest-right grp
                      nearest-right-dist dist)))))))
      (or containing nearest-right (car curly-positions)))))

(defconst evil-tex-bora--arg-node-types
  '("curly_group" "brack_group"
    ;; Text variants used by some commands
    "curly_group_text" "curly_group_text_list"
    ;; Path variants used by include commands
    "curly_group_path" "curly_group_path_list"
    "curly_group_glob_pattern"
    ;; Label variants
    "curly_group_label"
    ;; Key-value variants
    "brack_group_key_value" "curly_group_key_value"
    ;; Author/date variants
    "curly_group_author_list" "brack_group_date")
  "List of tree-sitter node types that represent command arguments.")

(defun evil-tex-bora--collect-command-args (cmd-node)
  "Collect all argument nodes from CMD-NODE."
  (let ((args nil)
        (child-count (treesit-node-child-count cmd-node)))
    (dotimes (i child-count)
      (let* ((child (treesit-node-child cmd-node i))
             (type (treesit-node-type child)))
        (when (member type evil-tex-bora--arg-node-types)
          (push child args))))
    (nreverse args)))

(defun evil-tex-bora--bounds-of-math ()
  "Return bounds of math environment at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (math-node (evil-tex-bora--find-parent-by-type
                          node '("inline_formula" "displayed_equation"
                                 "math_environment"))))
    (let* ((outer-beg (treesit-node-start math-node))
           (outer-end (treesit-node-end math-node))
           (node-type (treesit-node-type math-node)))
      (cond
       ;; For math_environment, use begin/end nodes like regular environments
       ((string= node-type "math_environment")
        (let* ((begin-node (treesit-node-child-by-field-name math-node "begin"))
               (end-node (treesit-node-child-by-field-name math-node "end"))
               (inner-beg (if begin-node (treesit-node-end begin-node) outer-beg))
               (inner-end (if end-node (treesit-node-start end-node) outer-end)))
          (list outer-beg outer-end inner-beg inner-end)))
       ;; For inline_formula and displayed_equation, find delimiter tokens
       (t
        (let* ((child-count (treesit-node-child-count math-node))
               (first-child (when (> child-count 0) (treesit-node-child math-node 0)))
               (last-child (when (> child-count 0) (treesit-node-child math-node (1- child-count))))
               ;; Inner starts after first delimiter, ends before last delimiter
               (inner-beg (if first-child (treesit-node-end first-child) outer-beg))
               (inner-end (if last-child (treesit-node-start last-child) outer-end)))
          (list outer-beg outer-end inner-beg inner-end)))))))

(defconst evil-tex-bora--delimiter-pairs
  '(("(" . ")")
    ("[" . "]")
    ("\\{" . "\\}")
    ("\\langle" . "\\rangle")
    ("\\lvert" . "\\rvert")
    ("\\lVert" . "\\rVert")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil"))
  "List of delimiter pairs (left . right) for matching.")

(defconst evil-tex-bora--delimiter-prefixes
  '("" "\\left" "\\right"
    "\\bigl" "\\bigr" "\\big"
    "\\Bigl" "\\Bigr" "\\Big"
    "\\biggl" "\\biggr" "\\bigg"
    "\\Biggl" "\\Biggr" "\\Bigg")
  "Prefixes that can appear before delimiters.")

(defun evil-tex-bora--bounds-of-delimiter ()
  "Return bounds of delimiter at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

Handles tree-sitter `math_delimiter' nodes (\\left/\\right, \\bigl/\\bigr)
and falls back to searching for matching delimiter pairs."
  (or (evil-tex-bora--bounds-of-math-delimiter)
      (evil-tex-bora--bounds-of-simple-delimiter)))

(defun evil-tex-bora--bounds-of-math-delimiter ()
  "Return bounds of math_delimiter node at point, or nil."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (delim-node (evil-tex-bora--find-parent-by-type
                           node '("math_delimiter"))))
    (let* ((outer-beg (treesit-node-start delim-node))
           (outer-end (treesit-node-end delim-node))
           ;; Find left and right delimiter children
           (left-delim (treesit-node-child-by-field-name delim-node "left_delimiter"))
           (right-command (treesit-node-child-by-field-name delim-node "right_command"))
           ;; Inner is between left delimiter and right command
           (inner-beg (if left-delim
                          (treesit-node-end left-delim)
                        outer-beg))
           (inner-end (if right-command
                          (treesit-node-start right-command)
                        outer-end)))
      (list outer-beg outer-end inner-beg inner-end))))

(defun evil-tex-bora--bounds-of-simple-delimiter ()
  "Return bounds of simple delimiter pair at point using search.
Finds closest enclosing (), [], or \\{\\}."
  (let ((best-bounds nil)
        (point-pos (point)))
    (save-excursion
      (dolist (pair evil-tex-bora--delimiter-pairs)
        (let* ((left (car pair))
               (right (cdr pair))
               (bounds (evil-tex-bora--find-delimiter-pair left right)))
          (when bounds
            ;; Check if this pair contains point and is closer than current best
            (let ((outer-beg (nth 0 bounds))
                  (outer-end (nth 1 bounds)))
              (when (and (>= point-pos outer-beg)
                         (<= point-pos outer-end)
                         (or (null best-bounds)
                             ;; Prefer smaller (more nested) delimiters
                             (> (nth 0 bounds) (nth 0 best-bounds))))
                (setq best-bounds bounds)))))))
    best-bounds))

(defun evil-tex-bora--find-delimiter-pair (left right)
  "Find matching delimiter pair LEFT and RIGHT around point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((orig-point (point))
        left-pos right-pos)
    (save-excursion
      ;; Search backward for left delimiter
      (when (evil-tex-bora--search-backward-delimiter left)
        (setq left-pos (point))
        ;; Search forward for matching right delimiter
        (goto-char orig-point)
        (when (evil-tex-bora--search-forward-delimiter right)
          (setq right-pos (point))
          ;; Verify this is a valid pair (left before right, both surround point)
          (when (and left-pos right-pos
                     (< left-pos orig-point)
                     (> right-pos orig-point))
            (list left-pos right-pos
                  (+ left-pos (length left))
                  (- right-pos (length right)))))))))

(defun evil-tex-bora--search-backward-delimiter (delim)
  "Search backward for DELIM, handling nesting.
Returns position if found, nil otherwise."
  (let ((depth 1)
        (regexp (regexp-quote delim)))
    (while (and (> depth 0)
                (re-search-backward regexp nil t))
      (setq depth (1- depth)))
    (when (= depth 0)
      (point))))

(defun evil-tex-bora--search-forward-delimiter (delim)
  "Search forward for DELIM.
Returns position after delim if found, nil otherwise."
  (let ((regexp (regexp-quote delim)))
    (when (re-search-forward regexp nil t)
      (point))))

;;; Evil text objects
;;
;; Text objects generally return a simple list (BEG END) to avoid visual-state
;; off-by-one issues with `inclusive'. Some operators (e.g. `cie' on multi-line
;; environments) need explicit linewise ranges to match Vim/Evil behavior.

;; Environment text objects (ie/ae)
(evil-define-text-object evil-tex-bora-inner-environment (count &optional beg end type)
  "Select inner LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-environment)))
    (if (and evil-tex-bora-select-newlines-with-envs
             (evil-tex-bora--change-operator-p))
        (if-let ((line-bounds (evil-tex-bora--bounds-of-environment-inner-lines)))
            (evil-range (car line-bounds) (cdr line-bounds) 'line)
          (list (nth 2 bounds) (nth 3 bounds)))
      (list (nth 2 bounds) (nth 3 bounds)))))

(evil-define-text-object evil-tex-bora-outer-environment (count &optional beg end type)
  "Select outer LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-environment)))
    (let ((outer-beg (nth 0 bounds))
          (outer-end (nth 1 bounds)))
      ;; Use linewise type when environment occupies whole lines
      ;; This ensures cursor lands on first non-whitespace char after 'dae'
      (if (and evil-tex-bora-select-newlines-with-envs
               (save-excursion
                 (goto-char outer-beg)
                 (bolp))
               (save-excursion
                 (goto-char outer-end)
                 (or (eobp) (bolp))))
          (evil-range outer-beg outer-end 'line)
        (list outer-beg outer-end)))))

;; Command text objects (ic/ac)
(evil-define-text-object evil-tex-bora-inner-command (count &optional beg end type)
  "Select inner LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-command)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-bora-outer-command (count &optional beg end type)
  "Select outer LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-command)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Math text objects (im/am)
(evil-define-text-object evil-tex-bora-inner-math (count &optional beg end type)
  "Select inner math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-math)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-bora-outer-math (count &optional beg end type)
  "Select outer math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-math)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Delimiter text objects (id/ad)
(evil-define-text-object evil-tex-bora-inner-delimiter (count &optional beg end type)
  "Select inner delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-delimiter)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-bora-outer-delimiter (count &optional beg end type)
  "Select outer delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-delimiter)))
    (list (nth 0 bounds) (nth 1 bounds))))

;;; Toggles

(defun evil-tex-bora-toggle-env-asterisk ()
  "Toggle asterisk on current environment (e.g., equation <-> equation*)."
  (interactive)
  ;; TODO: Implement
  (message "evil-tex-bora-toggle-env-asterisk: Not implemented yet"))

(defun evil-tex-bora-toggle-math-mode ()
  "Toggle math mode between inline and display (\\(...\\) <-> \\[...\\])."
  (interactive)
  ;; TODO: Implement
  (message "evil-tex-bora-toggle-math-mode: Not implemented yet"))

(defun evil-tex-bora-toggle-delim-size ()
  "Toggle delimiter sizing (() <-> \\left(\\right))."
  (interactive)
  ;; TODO: Implement
  (message "evil-tex-bora-toggle-delim-size: Not implemented yet"))

(defun evil-tex-bora-toggle-cmd-asterisk ()
  "Toggle asterisk on current command (\\section <-> \\section*)."
  (interactive)
  ;; TODO: Implement
  (message "evil-tex-bora-toggle-cmd-asterisk: Not implemented yet"))

;;; Keymap

(defvar evil-tex-bora-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Text objects are bound in evil's text object maps
    ;; Toggles use the mt prefix
    (evil-define-key 'normal map
      "mte" #'evil-tex-bora-toggle-env-asterisk
      "mtm" #'evil-tex-bora-toggle-math-mode
      "mtd" #'evil-tex-bora-toggle-delim-size
      "mtc" #'evil-tex-bora-toggle-cmd-asterisk)
    map)
  "Keymap for `evil-tex-bora-mode'.")

;;; Minor mode

(defvar evil-tex-bora--pending-first-non-blank nil
  "When non-nil, move to first non-blank after current command.")

(defun evil-tex-bora--post-command-first-non-blank ()
  "Move cursor to first non-blank character if pending."
  (when evil-tex-bora--pending-first-non-blank
    (setq evil-tex-bora--pending-first-non-blank nil)
    (back-to-indentation)))

(defun evil-tex-bora--delete-advice (orig-fn beg end &optional type &rest args)
  "Advice for `evil-delete' to ensure cursor moves to first non-blank.
After linewise deletion of environment, move cursor to indentation.
Only active in buffers where `evil-tex-bora-mode' is enabled."
  (let ((was-line-type (and evil-tex-bora-mode (eq type 'line))))
    (apply orig-fn beg end type args)
    (when was-line-type
      (setq evil-tex-bora--pending-first-non-blank t))))

(defun evil-tex-bora--setup-text-objects ()
  "Setup text objects for evil-tex-bora."
  ;; Environment
  (evil-define-key '(visual operator) evil-tex-bora-mode-map
    "ie" #'evil-tex-bora-inner-environment
    "ae" #'evil-tex-bora-outer-environment
    ;; Command
    "ic" #'evil-tex-bora-inner-command
    "ac" #'evil-tex-bora-outer-command
    ;; Math
    "im" #'evil-tex-bora-inner-math
    "am" #'evil-tex-bora-outer-math
    ;; Delimiter
    "id" #'evil-tex-bora-inner-delimiter
    "ad" #'evil-tex-bora-outer-delimiter))

;;;###autoload
(define-minor-mode evil-tex-bora-mode
  "Minor mode for LaTeX text objects using tree-sitter."
  :lighter " ETB"
  :keymap evil-tex-bora-mode-map
  (if evil-tex-bora-mode
      (progn
        (unless (evil-tex-bora--ensure-parser)
          (user-error "Tree-sitter LaTeX parser not available"))
        (evil-tex-bora--setup-text-objects)
        ;; Ensure treesit parser is created for this buffer
        (treesit-parser-create 'latex)
        ;; Add hook for cursor positioning after linewise delete
        (add-hook 'post-command-hook #'evil-tex-bora--post-command-first-non-blank nil t))
    ;; Cleanup when mode is disabled
    (remove-hook 'post-command-hook #'evil-tex-bora--post-command-first-non-blank t)))

;; Add global advice for evil-delete (only runs in evil-tex-bora-mode buffers)
(with-eval-after-load 'evil
  (advice-add 'evil-delete :around #'evil-tex-bora--delete-advice))

;;;###autoload
(defun evil-tex-bora-setup ()
  "Setup evil-tex-bora for LaTeX buffers."
  (interactive)
  (add-hook 'latex-mode-hook #'evil-tex-bora-mode)
  (add-hook 'LaTeX-mode-hook #'evil-tex-bora-mode))

(provide 'evil-tex-bora)
;;; evil-tex-bora.el ends here
