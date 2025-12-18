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

(defcustom evil-tex-bora-toggle-override-m t
  "Whether to bind toggle commands to `mt' prefix.
When non-nil, toggles are bound to mte, mtm, mtd, mtc.
This overrides the `m' key (set-marker) for the `t' character.
Set to nil to preserve the original `m' behavior."
  :type 'boolean
  :group 'evil-tex-bora)

(defcustom evil-tex-bora-toggle-override-t nil
  "Whether to bind toggle commands to `ts' prefix instead.
When non-nil, toggles are bound to tse, tsm, tsd, tsc.
This overrides the `t' motion (till char) for the `s' character.
Can be used together with `evil-tex-bora-toggle-override-m'."
  :type 'boolean
  :group 'evil-tex-bora)

(defcustom evil-tex-bora-preferred-inline-math 'dollar
  "Preferred inline math format for `evil-tex-bora-toggle-math-mode'.
When toggling from display math \\[...\\] back to inline, this
determines which format to use.

- `dollar': Use $...$ (default, shorter)
- `paren': Use \\(...\\) (LaTeX2e recommended)"
  :type '(choice (const :tag "$...$" dollar)
                 (const :tag "\\(...\\)" paren))
  :group 'evil-tex-bora)

(defcustom evil-tex-bora-include-newlines-in-envs t
  "Whether include newlines with env insertion via surround.

When non-nil, env insertions would force separate lines for
\\begin, inner text, and \\end."
  :type 'boolean
  :group 'evil-tex-bora)

;;; Evil-surround integration declarations

(defvar evil-surround-pairs-alist)
(defvar evil-surround-local-inner-text-object-map-list)
(defvar evil-surround-local-outer-text-object-map-list)
(defvar evil-embrace-evil-surround-keys)
(defvar which-key-idle-delay)
(defvar which-key-replacement-alist)
(declare-function which-key--hide-popup "ext:which-key")
(declare-function which-key--show-keymap "ext:which-key")

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

(defun evil-tex-bora--node-contains-point-p (node pt)
  "Return non-nil when NODE contains PT.

Tree-sitter node ranges are half-open: [start, end)."
  (and node (>= pt (treesit-node-start node)) (< pt (treesit-node-end node))))

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
  ;; Ensure we have a parser
  (unless (treesit-parser-list)
    (when (evil-tex-bora--ensure-parser)
      (treesit-parser-create 'latex)))
  ;; Try fallback first - it handles cases like \sqrt[n]{x} where
  ;; tree-sitter doesn't recognize the curly group as part of the command
  (or (evil-tex-bora--bounds-of-command-fallback)
      (evil-tex-bora--bounds-of-command-tree-sitter)))

(defun evil-tex-bora--command-curly-args (cmd-node)
  "Return command curly arguments info for CMD-NODE.

Returns (OUTER-END . CURLY-NODES), where OUTER-END may be extended to include
trailing optional [...] and curly {...} arguments that are not part of the
tree-sitter command node."
  (let ((outer-end (treesit-node-end cmd-node))
        (curly-nodes (evil-tex-bora--collect-command-curly-args cmd-node)))
    (when (null curly-nodes)
      (when-let ((extended (evil-tex-bora--extend-command-bounds cmd-node)))
        (setq outer-end (nth 0 extended))
        (setq curly-nodes (nth 1 extended))))
    (cons outer-end curly-nodes)))

(defun evil-tex-bora--nearest-ancestor-command-with-curly-arg (cmd-node pt)
  "Return nearest ancestor command of CMD-NODE whose curly arg contains PT.

This is used to prefer the surrounding command when point is on an inner
no-argument command (e.g. \\cdot) inside another command's {...} argument."
  (let ((parent (treesit-node-parent cmd-node))
        (found nil))
    (while (and parent (not found))
      (when (member (treesit-node-type parent) evil-tex-bora--command-types)
        (let* ((arg-info (evil-tex-bora--command-curly-args parent))
               (curly-nodes (cdr arg-info))
               (contains-point nil))
          (when curly-nodes
            (dolist (curly-node curly-nodes)
              (when (evil-tex-bora--node-contains-point-p curly-node pt)
                (setq contains-point t)))
            (when contains-point
              (setq found parent)))))
      (setq parent (treesit-node-parent parent)))
    found))

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
      ;; If this is an inner command without args (e.g. \\cdot) and point is
      ;; inside an ancestor command's curly argument, prefer that ancestor.
      ;; This matches the expected `vic` behavior for cases like:
      ;;   \\sqrt{a_1 \\cdot a_2|}  -> selects "a_1 \\cdot a_2"
      (when (null curly-nodes)
        (when-let ((ancestor (evil-tex-bora--nearest-ancestor-command-with-curly-arg
                              cmd-node pt)))
          (setq cmd-node ancestor)
          (setq outer-beg (treesit-node-start cmd-node))
          (let ((arg-info (evil-tex-bora--command-curly-args cmd-node)))
            (setq outer-end (car arg-info))
            (setq curly-nodes (cdr arg-info)))))
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
            ;; Check if this curly_group is "orphan" (not part of a recognized
            ;; command node). For example, tree-sitter-latex often parses
            ;; \sqrt[n]{x} as separate siblings (\sqrt, [...], {...}), so when
            ;; point is inside {...} there is no command node ancestor.
            (let ((enclosing-command
                   (evil-tex-bora--find-parent-by-type
                    curly-node evil-tex-bora--command-types)))
              (unless enclosing-command
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
      ;; Search backward for left delimiter (include current position)
      (when (evil-tex-bora--search-backward-delimiter left)
        (setq left-pos (point))
        ;; Search forward for matching right delimiter
        (goto-char orig-point)
        (when (evil-tex-bora--search-forward-delimiter right)
          (setq right-pos (point))
          ;; Verify this is a valid pair (left at or before point, right after point)
          (when (and left-pos right-pos
                     (<= left-pos orig-point)
                     (> right-pos orig-point))
            (list left-pos right-pos
                  (+ left-pos (length left))
                  (- right-pos (length right)))))))))

(defun evil-tex-bora--search-backward-delimiter (delim)
  "Search backward for DELIM, handling nesting.
Returns position if found, nil otherwise.
Skips delimiters that are part of \\( \\) \\[ \\] sequences.
Also checks if delimiter is at current position."
  (let ((depth 1)
        (regexp (regexp-quote delim))
        (delim-len (length delim)))
    ;; First check if delimiter is at current position
    (when (and (>= (point-max) (+ (point) delim-len))
               (string= (buffer-substring-no-properties (point) (+ (point) delim-len)) delim)
               (not (and (= delim-len 1)
                         (member delim '("(" ")" "[" "]"))
                         (eq (char-before) ?\\))))
      (setq depth 0))
    ;; Then search backward
    (while (and (> depth 0)
                (re-search-backward regexp nil t))
      ;; Skip if this is part of \( \) \[ \]
      (unless (and (= delim-len 1)
                   (member delim '("(" ")" "[" "]"))
                   (eq (char-before) ?\\))
        (setq depth (1- depth))))
    (when (= depth 0)
      (point))))

(defun evil-tex-bora--search-forward-delimiter (delim)
  "Search forward for DELIM.
Returns position after delim if found, nil otherwise.
Skips delimiters that are part of \\( \\) \\[ \\] sequences."
  (let ((regexp (regexp-quote delim))
        (found nil))
    (while (and (not found)
                (re-search-forward regexp nil t))
      ;; Skip if this is part of \( \) \[ \]
      (unless (and (= (length delim) 1)
                   (member delim '("(" ")" "[" "]"))
                   (eq (char-before (match-beginning 0)) ?\\))
        (setq found (point))))
    found))

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
  "Toggle asterisk on current environment (e.g., equation <-> equation*).
Toggles the asterisk in both \\begin{env} and \\end{env}."
  (interactive)
  (when-let* ((bounds (evil-tex-bora--bounds-of-environment))
              (outer-beg (nth 0 bounds))
              (outer-end (nth 1 bounds)))
    (save-excursion
      ;; Toggle asterisk in \end{...}
      (goto-char outer-beg)
      (when (re-search-forward "\\\\end{[^}*]+" outer-end t)
        (if (eq ?* (char-after))
            (delete-char 1)
          (insert "*")))
      ;; Toggle asterisk in \begin{...}
      (goto-char outer-beg)
      (when (re-search-forward "\\\\begin{[^}*]+" outer-end t)
        (if (eq ?* (char-after))
            (delete-char 1)
          (insert "*"))))))

(defun evil-tex-bora-toggle-math-mode ()
  "Toggle math mode between inline and display.
Supports $...$, \\(...\\), and \\[...\\].

Inline to display:
  $...$     -> \\[...\\]
  \\(...\\) -> \\[...\\]

Display to inline (based on `evil-tex-bora-preferred-inline-math'):
  \\[...\\] -> $...$ (if `dollar')
  \\[...\\] -> \\(...\\) (if `paren')

When converting display to inline, multiline content is normalized:
newlines are collapsed to single spaces."
  (interactive)
  (when-let* ((bounds (evil-tex-bora--bounds-of-math))
              (outer-beg (nth 0 bounds))
              (inner-beg (nth 2 bounds))
              (inner-end (nth 3 bounds))
              (outer-end (nth 1 bounds)))
    (let ((left-delim (buffer-substring-no-properties outer-beg inner-beg))
          (right-delim (buffer-substring-no-properties inner-end outer-end)))
      (save-excursion
        (cond
         ;; $...$ -> \[...\]
         ((and (string= left-delim "$") (string= right-delim "$"))
          (goto-char inner-end)
          (delete-char 1)
          (insert "\\]")
          (goto-char outer-beg)
          (delete-char 1)
          (insert "\\["))
         ;; \(...\) -> \[...\]
         ((and (string= left-delim "\\(") (string= right-delim "\\)"))
          (goto-char inner-end)
          (delete-char 2)
          (insert "\\]")
          (goto-char outer-beg)
          (delete-char 2)
          (insert "\\["))
         ;; \[...\] -> inline (based on preference)
         ((and (string= left-delim "\\[") (string= right-delim "\\]"))
          (let* ((use-dollar (eq evil-tex-bora-preferred-inline-math 'dollar))
                 (content (buffer-substring-no-properties inner-beg inner-end))
                 (had-newlines (string-match-p "\n" content))
                 ;; Normalize content: trim and collapse newlines to spaces
                 (normalized (if had-newlines
                                 (replace-regexp-in-string
                                  "[ \t]*\\(?:\n[ \t]*\\)+" " "
                                  (string-trim content))
                               content)))
            ;; Replace the entire math expression
            (delete-region outer-beg outer-end)
            (goto-char outer-beg)
            (insert (if use-dollar "$" "\\(")
                    normalized
                    (if use-dollar "$" "\\)"))
            (when had-newlines
              (message "Newlines collapsed to spaces")))))))))

(defconst evil-tex-bora--size-prefix-regexp
  "\\\\\\(?:left\\|right\\|bigl\\|bigr\\|Bigl\\|Bigr\\|biggl\\|biggr\\|Biggl\\|Biggr\\|bigg\\|Bigg\\|big\\|Big\\)"
  "Regexp matching sizing prefixes like \\left, \\bigl, etc.
Note: longer matches (bigl, bigr) must come before shorter ones (big).")

(defun evil-tex-bora-toggle-delim-size ()
  "Toggle delimiter sizing (() <-> \\left(\\right)).
Also converts \\bigl(\\bigr) etc. to plain () (one-way conversion)."
  (interactive)
  (when-let* ((bounds (evil-tex-bora--bounds-of-delimiter))
              (outer-beg (nth 0 bounds))
              (inner-beg (nth 2 bounds))
              (inner-end (nth 3 bounds))
              (outer-end (nth 1 bounds)))
    (let ((left-delim (buffer-substring-no-properties outer-beg inner-beg))
          (right-delim (buffer-substring-no-properties inner-end outer-end)))
      (save-excursion
        (cond
         ;; Has sizing prefix -> remove it
         ((string-match evil-tex-bora--size-prefix-regexp left-delim)
          (let ((new-left (replace-regexp-in-string
                           evil-tex-bora--size-prefix-regexp "" left-delim))
                (new-right (replace-regexp-in-string
                            evil-tex-bora--size-prefix-regexp "" right-delim)))
            (goto-char inner-end)
            (delete-region inner-end outer-end)
            (insert new-right)
            (goto-char outer-beg)
            (delete-region outer-beg inner-beg)
            (insert new-left)))
         ;; No prefix -> add \left \right
         (t
          (goto-char inner-end)
          (delete-region inner-end outer-end)
          (insert "\\right" right-delim)
          (goto-char outer-beg)
          (delete-region outer-beg inner-beg)
          (insert "\\left" left-delim)))))))

(defun evil-tex-bora-toggle-cmd-asterisk ()
  "Toggle asterisk on current command (\\section <-> \\section*).
The asterisk is placed between the command name and its arguments."
  (interactive)
  (when-let* ((bounds (evil-tex-bora--bounds-of-command))
              (outer-beg (nth 0 bounds))
              (inner-beg (nth 2 bounds)))
    (save-excursion
      (goto-char outer-beg)
      ;; Search for position right after the command name (before [] or {})
      (when (re-search-forward "\\\\[a-zA-Z@]+" inner-beg t)
        (cond
         ;; Already has asterisk -> remove it
         ((eq ?* (char-after))
          (delete-char 1))
         ;; No asterisk -> add it
         (t
          (insert "*")))))))

;;; Evil-surround integration

(defun evil-tex-bora--populate-surround-keymap (keymap generator-alist prefix
                                                       single-strings-fn &optional cons-fn)
  "Populate KEYMAP with keys and callbacks from GENERATOR-ALIST.

Each item in GENERATOR-ALIST is a cons (KEY . VALUE).  KEY is a string, VALUE can be:
- A string: call SINGLE-STRINGS-FN with it
- A cons of strings: call CONS-FN with it (or just return it if CONS-FN is nil)
- A function: bind directly

PREFIX is used to name generated functions.
Return KEYMAP."
  (dolist (pair generator-alist)
    (let ((key (car pair))
          (val (cdr pair))
          name)
      (cond
       ((stringp val)
        (setq name (intern (concat prefix val)))
        (fset name (lambda () (interactive) (funcall single-strings-fn val)))
        (define-key keymap key name))
       ((consp val)
        (setq name (intern (concat prefix (car val))))
        (if cons-fn
            (fset name (lambda () (interactive) (funcall cons-fn val)))
          (fset name (lambda () (interactive) val)))
        (define-key keymap key name))
       ((or (functionp val) (not val))
        (define-key keymap key val)))))
  keymap)

(defun evil-tex-bora--read-with-keymap (keymap)
  "Prompt the user to press a key from KEYMAP.
Return the result of the called function, or error if key not found."
  (let (key map-result)
    (when (require 'which-key nil t)
      (run-with-idle-timer
       which-key-idle-delay nil
       (lambda () (unless key
                    (which-key--show-keymap nil keymap nil nil t)))))
    (setq key (string (read-char)))
    (when (functionp #'which-key--hide-popup)
      (which-key--hide-popup))
    (setq map-result (lookup-key keymap key))
    (cond
     ((or (not map-result) (numberp map-result))
      (user-error "%s not found in keymap" key))
     ((functionp map-result)
      (call-interactively map-result))
     ((keymapp map-result)
      (evil-tex-bora--read-with-keymap map-result)))))

;; Format functions for surround

(defun evil-tex-bora-get-env-for-surrounding (env-name)
  "Format strings for the env named ENV-NAME for surrounding.
Return a cons of (\"\\begin{ENV-NAME}\" . \"\\end{ENV-NAME}\").
Respect the value of `evil-tex-bora-include-newlines-in-envs'."
  (interactive (list (read-from-minibuffer "env: " nil minibuffer-local-ns-map)))
  (cons (format "\\begin{%s}%s"
                env-name
                (if evil-tex-bora-include-newlines-in-envs "\n" ""))
        (format "%s\\end{%s}"
                (if evil-tex-bora-include-newlines-in-envs "\n" "")
                env-name)))

(defun evil-tex-bora--format-env-cons-for-surrounding (env-cons)
  "Format ENV-CONS for surrounding.
Add newlines if `evil-tex-bora-include-newlines-in-envs' is t."
  (if evil-tex-bora-include-newlines-in-envs
      (cons (concat (car env-cons) "\n") (concat "\n" (cdr env-cons)))
    env-cons))

(defun evil-tex-bora--format-accent-for-surrounding (accent)
  "Format ACCENT for surrounding: return a cons of (\\ACCENT{ . })."
  (cons (concat "\\" accent "{") "}"))

(defvar evil-tex-bora--last-command-empty nil
  "Non-nil if the last command text object used was empty.
For example, \\alpha is empty, \\frac{a}{b} is not.")

(defun evil-tex-bora--format-command-for-surrounding (command)
  "Format COMMAND for surrounding: return a cons of (\\COMMAND{ . })."
  (if evil-tex-bora--last-command-empty
      (cons (concat "\\" command "") "")
    (cons (concat "\\" command "{") "}")))

(defun evil-tex-bora--surround-inline-math ()
  "Return surround pair for inline math based on `evil-tex-bora-preferred-inline-math'."
  (if (eq evil-tex-bora-preferred-inline-math 'dollar)
      '("$" . "$")
    '("\\(" . "\\)")))

(defun evil-tex-bora--normalize-inline-math-region (beg end)
  "Normalize region BEG END for inline math: trim and collapse newlines to spaces.
Returns (NEW-END INDENT-STRING HAD-TRAILING-NEWLINE)."
  (save-excursion
    ;; Check if there's a trailing newline
    (goto-char end)
    (let ((had-trailing-newline (and (> end beg)
                                      (eq (char-before end) ?\n))))
      ;; First, trim trailing whitespace/newlines
      (skip-chars-backward " \t\n" beg)
      (let ((new-end (point)))
        (delete-region new-end end)
        (setq end new-end))
      ;; Capture indentation of the first non-blank line
      (goto-char beg)
      (skip-chars-forward " \t\n" end)
      (let* ((first-content-pos (point))
             (indent-string (if (> first-content-pos beg)
                                ;; Get indentation from the line where content starts
                                (save-excursion
                                  (goto-char first-content-pos)
                                  (let ((bol (line-beginning-position)))
                                    (buffer-substring-no-properties bol first-content-pos)))
                              "")))
        ;; Remove leading whitespace/newlines
        (delete-region beg first-content-pos)
        ;; Adjust end position after deletion
        (setq end (- end (- first-content-pos beg)))
        ;; Replace internal newlines (with surrounding whitespace) with single space
        (goto-char beg)
        (while (re-search-forward "[ \t]*\\(?:\n[ \t]*\\)+" end t)
          (let ((match-len (- (match-end 0) (match-beginning 0))))
            (replace-match " ")
            (setq end (- end (1- match-len)))))
        (list end indent-string had-trailing-newline)))))

(defun evil-tex-bora--surround-region-advice (orig-fn beg end type char &optional force-new-line)
  "Advice for `evil-surround-region' to normalize inline math regions.
Normalizes the region (trims and collapses newlines) when surrounding with inline math.
For linewise selections, changes type to `inclusive' to prevent evil-surround from
adding extra newlines."
  (let (indent-string had-trailing-newline content-length original-col original-line)
    (when (and evil-tex-bora-mode
               (eq char ?m))
      ;; Save cursor position (line and column)
      (setq original-line (line-number-at-pos))
      (setq original-col (current-column))
      (let ((result (evil-tex-bora--normalize-inline-math-region beg end)))
        (setq end (nth 0 result))
        (setq indent-string (nth 1 result))
        (setq had-trailing-newline (nth 2 result))
        ;; Calculate content length (from beg to new end)
        (setq content-length (- end beg)))
      ;; For inline math, force inclusive type to prevent evil-surround from
      ;; adding newlines around the content (linewise behavior)
      (when (eq type 'line)
        (setq type 'inclusive)))
    ;; Call original surround function
    (funcall orig-fn beg end type char force-new-line)
    ;; Post-process: add indent before and newline after the surround delimiters
    (when (and evil-tex-bora-mode
               (eq char ?m))
      (let ((delim-pair (evil-tex-bora--surround-inline-math)))
        (save-excursion
          ;; Add trailing newline after the closing delimiter FIRST
          ;; (before adding indent, so positions are simpler)
          (when had-trailing-newline
            ;; Position after closing delimiter:
            ;; beg + length(left-delim) + content-length + length(right-delim)
            (goto-char (+ beg
                          (length (car delim-pair))
                          content-length
                          (length (cdr delim-pair))))
            (insert "\n"))
          ;; Add indentation before the opening delimiter
          (when (and indent-string (> (length indent-string) 0))
            (goto-char beg)
            (insert indent-string))))
      ;; Restore cursor to original line and column
      (when (and original-line original-col)
        (goto-char (point-min))
        (forward-line (1- original-line))
        (move-to-column original-col)))))

;; Environment keymap for surround

(defvar evil-tex-bora-env-map (make-sparse-keymap)
  "Keymap for surrounding with environments.")

(defun evil-tex-bora-bind-to-env-map (key-generator-alist &optional keymap)
  "Bind envs from KEY-GENERATOR-ALIST to `evil-tex-bora-env-map' or KEYMAP.

Each item is a cons (KEY . VALUE):
- String VALUE: inserted env with that name
- Cons VALUE: text wrapped between car and cdr
- Function VALUE: called to get the cons"
  (evil-tex-bora--populate-surround-keymap
   (or keymap evil-tex-bora-env-map)
   key-generator-alist
   "evil-tex-bora-envs---"
   #'evil-tex-bora-get-env-for-surrounding
   #'evil-tex-bora--format-env-cons-for-surrounding))

(setq evil-tex-bora-env-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-bora-bind-to-env-map
         '(("x" . evil-tex-bora-get-env-for-surrounding)
           ("e" . "equation")
           ("E" . "equation*")
           ("f" . "figure")
           ("i" . "itemize")
           ("I" . "enumerate")
           ("b" . "frame")
           ("a" . "align")
           ("A" . "align*")
           ("y" . "array")
           ("n" . "alignat")
           ("N" . "alignat*")
           ("r" . "eqnarray")
           ("l" . "flalign")
           ("L" . "flalign*")
           ("g" . "gather")
           ("G" . "gather*")
           ("m" . "multline")
           ("M" . "multline*")
           ("c" . "cases")
           ("z" . "tikzpicture")
           ;; prefix t - theorems
           ("ta" . "axiom")
           ("tc" . "corollary")
           ("tC" . "claim")
           ("td" . "definition")
           ("te" . "examples")
           ("ts" . "exercise")
           ("tl" . "lemma")
           ("tp" . "proof")
           ("tq" . "question")
           ("tr" . "remark")
           ("tt" . "theorem"))
         keymap)
        keymap))

;; CDLaTeX accents keymap for surround

(defun evil-tex-bora--texmathp ()
  "Return non-nil if point is inside a math environment.
Uses tree-sitter to check for math context."
  (when-let* ((node (evil-tex-bora--get-node-at-point)))
    (evil-tex-bora--find-parent-by-type
     node '("inline_formula" "displayed_equation" "math_environment"))))

(defun evil-tex-bora-cdlatex-accents---rm ()
  "Return surround pair for rm style text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathrm{" . "}")
    '("\\textrm{" . "}")))

(defun evil-tex-bora-cdlatex-accents---it ()
  "Return surround pair for italic style text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathit{" . "}")
    '("\\textit{" . "}")))

(defun evil-tex-bora-cdlatex-accents---bf ()
  "Return surround pair for bold style text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathbf{" . "}")
    '("\\textbf{" . "}")))

(defun evil-tex-bora-cdlatex-accents---emph ()
  "Return surround pair for emphasized text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathem{" . "}")
    '("\\emph{" . "}")))

(defun evil-tex-bora-cdlatex-accents---tt ()
  "Return surround pair for typewriter style text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathtt{" . "}")
    '("\\texttt{" . "}")))

(defun evil-tex-bora-cdlatex-accents---sf ()
  "Return surround pair for sans-serif style text."
  (interactive)
  (if (evil-tex-bora--texmathp)
      '("\\mathsf{" . "}")
    '("\\textsf{" . "}")))

(defvar evil-tex-bora-cdlatex-accents-map (make-sparse-keymap)
  "Keymap for surrounding with cdlatex-style accents.")

(defun evil-tex-bora-bind-to-cdlatex-accents-map (key-generator-alist &optional keymap)
  "Bind accent macros from KEY-GENERATOR-ALIST to `evil-tex-bora-cdlatex-accents-map'."
  (evil-tex-bora--populate-surround-keymap
   (or keymap evil-tex-bora-cdlatex-accents-map)
   key-generator-alist
   "evil-tex-bora-cdlatex-accents---"
   #'evil-tex-bora--format-accent-for-surrounding))

(setq evil-tex-bora-cdlatex-accents-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-bora-bind-to-cdlatex-accents-map
         '(("." . "dot")
           (":" . "ddot")
           ("~" . "tilde")
           ("N" . "widetilde")
           ("^" . "hat")
           ("H" . "widehat")
           ("-" . "bar")
           ("T" . "overline")
           ("_" . "underline")
           ("{" . "overbrace")
           ("}" . "underbrace")
           (">" . "vec")
           ("/" . "grave")
           ("\"" . "acute")
           ("v" . "check")
           ("u" . "breve")
           ("m" . "mbox")
           ("c" . "mathcal")
           ("q" . "sqrt")
           ("r" . evil-tex-bora-cdlatex-accents---rm)
           ("i" . evil-tex-bora-cdlatex-accents---it)
           ("l" . "textsl")
           ("b" . evil-tex-bora-cdlatex-accents---bf)
           ("e" . evil-tex-bora-cdlatex-accents---emph)
           ("y" . evil-tex-bora-cdlatex-accents---tt)
           ("f" . evil-tex-bora-cdlatex-accents---sf)
           ("0" . ("{\\textstyle " . "}"))
           ("1" . ("{\\displaystyle " . "}"))
           ("2" . ("{\\scriptstyle " . "}"))
           ("3" . ("{\\scriptscriptstyle " . "}")))
         keymap)
        keymap))

;; Delimiter keymap for surround

(defvar evil-tex-bora-delim-map (make-sparse-keymap)
  "Keymap for surrounding with delimiters.")

(defun evil-tex-bora-bind-to-delim-map (key-generator-alist &optional keymap)
  "Bind delimiters from KEY-GENERATOR-ALIST to `evil-tex-bora-delim-map'."
  (evil-tex-bora--populate-surround-keymap
   (or keymap evil-tex-bora-delim-map)
   key-generator-alist
   "evil-tex-bora-delims---"
   #'identity))

(setq evil-tex-bora-delim-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-bora-bind-to-delim-map
         '(("P" "(" . ")")
           ("p" "\\left(" . "\\right)")
           ("S" "[" . "]")
           ("s" "\\left[" . "\\right]")
           ("C" "\\{" . "\\}")
           ("c" "\\left\\{" . "\\right\\}")
           ("R" "\\langle " . "\\rangle")
           ("r" "\\left\\langle " . "\\right\\rangle")
           ("v" "\\left\\lvert" . "\\right\\rvert")
           ("V" "\\lvert" . "\\rvert")
           ("n" "\\left\\lVert" . "\\right\\rVert")
           ("N" "\\lVert" . "\\rVert"))
         keymap)
        keymap))

;; Prompt functions for surround

(defun evil-tex-bora-surround-env-prompt ()
  "Prompt user for an env to surround with using `evil-tex-bora-env-map'."
  (evil-tex-bora--read-with-keymap evil-tex-bora-env-map))

(defun evil-tex-bora-surround-cdlatex-accents-prompt ()
  "Prompt user for an accent to surround with using `evil-tex-bora-cdlatex-accents-map'."
  (evil-tex-bora--read-with-keymap evil-tex-bora-cdlatex-accents-map))

(defun evil-tex-bora-surround-delim-prompt ()
  "Prompt user for a delimiter to surround with using `evil-tex-bora-delim-map'."
  (evil-tex-bora--read-with-keymap evil-tex-bora-delim-map))

(defun evil-tex-bora-surround-command-prompt ()
  "Ask the user for the command they'd like to surround with."
  (evil-tex-bora--format-command-for-surrounding
   (read-from-minibuffer "command: \\" nil minibuffer-local-ns-map)))

;; Surround delimiters alist

(defvar evil-tex-bora-surround-delimiters
  `((?m . ,#'evil-tex-bora--surround-inline-math)
    (?M "\\[" . "\\]")
    (?c . ,#'evil-tex-bora-surround-command-prompt)
    (?e . ,#'evil-tex-bora-surround-env-prompt)
    (?d . ,#'evil-tex-bora-surround-delim-prompt)
    (?\; . ,#'evil-tex-bora-surround-cdlatex-accents-prompt)
    (?q "`" . "'")
    (?Q "``" . "''")
    (?^ "^{" . "}")
    (?_ "_{" . "}")
    (?T "&" . "&"))
  "Delimiter pairs for `evil-surround'.

Each element is (CHAR LEFT-DELIM . RIGHT-DELIM) or (CHAR . FUNCTION).
- m: inline math (uses `evil-tex-bora-preferred-inline-math': $...$ or \\(...\\))
- M: display math \\[...\\]
- c: command (prompts for name)
- e: environment (prompts with keymap)
- d: delimiter (prompts with keymap)
- ;: cdlatex accent (prompts with keymap)
- q: single LaTeX quote `...'
- Q: double LaTeX quote ``...''
- ^: superscript ^{...}
- _: subscript _{...}
- T: table cell &...&")

;; Text object keymaps for surround

(defvar evil-tex-bora-inner-text-objects-map (make-sparse-keymap)
  "Inner text object keymap for `evil-tex-bora'.")

(defvar evil-tex-bora-outer-text-objects-map (make-sparse-keymap)
  "Outer text object keymap for `evil-tex-bora'.")

;; Setup functions

(defun evil-tex-bora-set-up-surround ()
  "Configure evil-surround for LaTeX editing.
Things like 'csm' (change surrounding math) will work after this."
  (setq-local evil-surround-pairs-alist
              (append evil-tex-bora-surround-delimiters evil-surround-pairs-alist))
  ;; Use local text object maps if evil-surround supports it
  (when (and (boundp 'evil-surround-local-inner-text-object-map-list)
             (boundp 'evil-surround-local-outer-text-object-map-list))
    (add-to-list 'evil-surround-local-inner-text-object-map-list
                 evil-tex-bora-inner-text-objects-map)
    (add-to-list 'evil-surround-local-outer-text-object-map-list
                 evil-tex-bora-outer-text-objects-map))
  ;; Add advice for normalizing inline math regions
  (advice-add 'evil-surround-region :around #'evil-tex-bora--surround-region-advice))

(defun evil-tex-bora-set-up-embrace ()
  "Configure evil-embrace not to steal our evil-surround keybinds."
  (setq-local evil-embrace-evil-surround-keys
              (append
               (mapcar #'car evil-tex-bora-surround-delimiters)
               evil-embrace-evil-surround-keys)))

;; Populate text object keymaps
(with-eval-after-load 'evil
  ;; Inner text objects
  (define-key evil-tex-bora-inner-text-objects-map "e" #'evil-tex-bora-inner-environment)
  (define-key evil-tex-bora-inner-text-objects-map "c" #'evil-tex-bora-inner-command)
  (define-key evil-tex-bora-inner-text-objects-map "m" #'evil-tex-bora-inner-math)
  (define-key evil-tex-bora-inner-text-objects-map "d" #'evil-tex-bora-inner-delimiter)
  ;; Outer text objects
  (define-key evil-tex-bora-outer-text-objects-map "e" #'evil-tex-bora-outer-environment)
  (define-key evil-tex-bora-outer-text-objects-map "c" #'evil-tex-bora-outer-command)
  (define-key evil-tex-bora-outer-text-objects-map "m" #'evil-tex-bora-outer-math)
  (define-key evil-tex-bora-outer-text-objects-map "d" #'evil-tex-bora-outer-delimiter))

;; Shorten which-key descriptions
(with-eval-after-load 'which-key
  (push
   '(("\\`." . "evil-tex-bora-.*---\\(.*\\)") . (nil . "\\1"))
   which-key-replacement-alist))

;;; Keymap

(defvar evil-tex-bora-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'evil-tex-bora-toggle-env-asterisk)
    (define-key map "m" #'evil-tex-bora-toggle-math-mode)
    (define-key map "d" #'evil-tex-bora-toggle-delim-size)
    (define-key map "c" #'evil-tex-bora-toggle-cmd-asterisk)
    map)
  "Keymap for evil-tex-bora toggle commands.
Bound to `mt' or `ts' prefix depending on configuration.")

(defvar evil-tex-bora-mode-map
  (make-sparse-keymap)
  "Keymap for `evil-tex-bora-mode'.")

(defun evil-tex-bora--setup-toggle-keybindings ()
  "Setup toggle keybindings based on customization options.
Call this after changing `evil-tex-bora-toggle-override-m' or
`evil-tex-bora-toggle-override-t'."
  (when (boundp 'evil-normal-state-local-map)
    ;; Bind to mt* if enabled
    (when evil-tex-bora-toggle-override-m
      (evil-define-key 'normal evil-tex-bora-mode-map
        "mt" evil-tex-bora-toggle-map))
    ;; Bind to ts* if enabled
    (when evil-tex-bora-toggle-override-t
      (evil-define-key 'normal evil-tex-bora-mode-map
        "ts" evil-tex-bora-toggle-map))))

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
  "Setup text objects for evil-tex-bora.
Binds text objects to Evil's inner/outer text object maps.
These bindings are global but the functions check if evil-tex-bora-mode is active."
  (when (and (boundp 'evil-inner-text-objects-map)
             (boundp 'evil-outer-text-objects-map))
    ;; Inner text objects (used with 'i' prefix, e.g., vic, vie)
    (define-key evil-inner-text-objects-map "e" #'evil-tex-bora-inner-environment)
    (define-key evil-inner-text-objects-map "c" #'evil-tex-bora-inner-command)
    (define-key evil-inner-text-objects-map "m" #'evil-tex-bora-inner-math)
    (define-key evil-inner-text-objects-map "d" #'evil-tex-bora-inner-delimiter)
    ;; Outer text objects (used with 'a' prefix, e.g., vac, vae)
    (define-key evil-outer-text-objects-map "e" #'evil-tex-bora-outer-environment)
    (define-key evil-outer-text-objects-map "c" #'evil-tex-bora-outer-command)
    (define-key evil-outer-text-objects-map "m" #'evil-tex-bora-outer-math)
    (define-key evil-outer-text-objects-map "d" #'evil-tex-bora-outer-delimiter)))

;; Setup text objects and toggle keybindings when package is loaded (after evil)
(with-eval-after-load 'evil
  (evil-tex-bora--setup-text-objects)
  (evil-tex-bora--setup-toggle-keybindings))

;;;###autoload
(define-minor-mode evil-tex-bora-mode
  "Minor mode for LaTeX text objects using tree-sitter."
  :lighter " ETB"
  :keymap evil-tex-bora-mode-map
  (if evil-tex-bora-mode
      (progn
        (unless (evil-tex-bora--ensure-parser)
          (user-error "Tree-sitter LaTeX parser not available"))
        ;; Ensure treesit parser is created for this buffer
        (treesit-parser-create 'latex)
        ;; Add hook for cursor positioning after linewise delete
        (add-hook 'post-command-hook #'evil-tex-bora--post-command-first-non-blank nil t)
        ;; Setup evil-surround integration
        (when (require 'evil-surround nil t)
          (evil-tex-bora-set-up-surround))
        ;; Setup evil-embrace integration
        (when (require 'evil-embrace nil t)
          (evil-tex-bora-set-up-embrace)))
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
