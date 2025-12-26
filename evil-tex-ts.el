;;; evil-tex-ts.el --- Tree-sitter based LaTeX text objects for Evil -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author: chestnykh
;; Maintainer: chestnykh
;; Created: 2024
;; Version: 0.1.0
;; Keywords: tex, emulation, vi, evil, wp
;; Homepage: https://github.com/Prgebish/evil-tex-ts
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

;;; Declaring optional external variables
(defvar evil-surround-pairs-alist)
(defvar evil-surround-local-inner-text-object-map-list)
(defvar evil-surround-local-outer-text-object-map-list)
(defvar evil-embrace-evil-surround-keys)

;;; Customization

(defgroup evil-tex-ts nil
  "Tree-sitter based LaTeX text objects for Evil."
  :version "29.1"
  :group 'evil
  :prefix "evil-tex-ts-")

(defcustom evil-tex-ts-select-newlines-with-envs t
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
  :group 'evil-tex-ts)

(defcustom evil-tex-ts-toggle-override-m t
  "Whether to bind toggle commands to `mt' prefix.
When non-nil, toggles are bound to mte, mtm, mtd, mtc.
This overrides the `m' key (set-marker) for the `t' character.
Set to nil to preserve the original `m' behavior."
  :type 'boolean
  :group 'evil-tex-ts)

(defcustom evil-tex-ts-toggle-override-t nil
  "Whether to bind toggle commands to `ts' prefix instead.
When non-nil, toggles are bound to tse, tsm, tsd, tsc.
This overrides the `t' motion (till char) for the `s' character.
Can be used together with `evil-tex-ts-toggle-override-m'."
  :type 'boolean
  :group 'evil-tex-ts)

(defcustom evil-tex-ts-preferred-inline-math 'dollar
  "Preferred inline math format for `evil-tex-ts-toggle-math-mode'.
When toggling from display math \\[...\\] back to inline, this
determines which format to use.

- `dollar': Use $...$ (default, shorter)
- `paren': Use \\(...\\) (LaTeX2e recommended)"
  :type '(choice (const :tag "$...$" dollar)
                 (const :tag "\\(...\\)" paren))
  :group 'evil-tex-ts)

(defcustom evil-tex-ts-include-newlines-in-envs t
  "Whether include newlines with env insertion via surround.

When non-nil, env insertions would force separate lines for
\\begin, inner text, and \\end."
  :type 'boolean
  :group 'evil-tex-ts)

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

(defun evil-tex-ts--ensure-parser ()
  "Ensure LaTeX tree-sitter parser is available.
Returns non-nil if parser is ready, nil otherwise."
  (and (treesit-available-p)
       (treesit-language-available-p 'latex)))

(defun evil-tex-ts--get-node-at-point ()
  "Get tree-sitter node at point for LaTeX."
  (when (evil-tex-ts--ensure-parser)
    (treesit-node-at (point) 'latex)))

(defun evil-tex-ts--find-parent-by-type (node types)
  "Find parent of NODE that matches one of TYPES.
TYPES is a list of node type strings."
  (treesit-parent-until
   node
   (lambda (n) (member (treesit-node-type n) types))))

(defun evil-tex-ts--node-bounds (node)
  "Get bounds of NODE as (start . end)."
  (when node
    (cons (treesit-node-start node)
          (treesit-node-end node))))

(defun evil-tex-ts--node-contains-point-p (node pt)
  "Return non-nil when NODE contains PT.

Tree-sitter node ranges are half-open: [start, end)."
  (and node (>= pt (treesit-node-start node)) (< pt (treesit-node-end node))))

;;; Text object helpers

(defun evil-tex-ts--bounds-of-environment-at-node (env-node)
  "Return bounds for ENV-NODE.
Helper function that extracts bounds from a known environment node.

When `evil-tex-ts-select-newlines-with-envs' is non-nil:
- outer-end extends to include trailing newline (for clean `dae' deletion)
- inner selection includes the newline before \\end{...}
- inner selection is anchored to the indentation level of the \\end line"
  (let* ((outer-beg (treesit-node-start env-node))
         (outer-end (treesit-node-end env-node))
         ;; Find \begin{...} and \end{...} to get inner bounds
         (begin-node (treesit-node-child-by-field-name env-node "begin"))
         (end-node (treesit-node-child-by-field-name env-node "end"))
         (inner-beg (if begin-node (treesit-node-end begin-node) outer-beg))
         (inner-end (if end-node (treesit-node-start end-node) outer-end)))
    ;; Adjust bounds for newlines if option is enabled
    (when evil-tex-ts-select-newlines-with-envs
      ;; Extend outer-beg to include leading whitespace on the line
      ;; This ensures 'dae' deletes the whole line including indentation
      (save-excursion
        (goto-char outer-beg)
        (let ((line-start (line-beginning-position)))
          (when (string-match-p "\\`[ \t]*\\'" (buffer-substring line-start outer-beg))
            (setq outer-beg line-start))))
      ;; Extend outer-end to include trailing newline, but NOT if the next
      ;; line starts with \end{...} (which would be parent environment's end).
      ;; This prevents capturing the parent's \end{...} when dealing with
      ;; nested environments.
      (save-excursion
        (goto-char outer-end)
        (when (and (eq (char-after) ?\n)
                   (or (>= (1+ outer-end) (point-max))
                       (save-excursion
                         (forward-char 1)
                         (not (looking-at "[ \t]*\\\\end{")))))
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
    (list outer-beg outer-end inner-beg inner-end)))

(defun evil-tex-ts--find-environment-forward ()
  "Search forward on current line for nearest environment and return its bounds.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((line-end (line-end-position))
          (found nil))
      ;; Search for \begin{ on the current line
      (while (and (not found)
                  (re-search-forward "\\\\begin{" line-end t))
        (goto-char (match-beginning 0))
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (env-node (evil-tex-ts--find-parent-by-type
                               node '("generic_environment" "math_environment"))))
          (setq found (evil-tex-ts--bounds-of-environment-at-node env-node)))
        (unless found
          (goto-char (match-end 0))))
      found)))

(defun evil-tex-ts--bounds-of-environment ()
  "Return bounds of LaTeX environment at or after point on the same line.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

When `evil-tex-ts-select-newlines-with-envs' is non-nil:
- outer-end extends to include trailing newline (for clean `dae' deletion)
- inner selection includes the newline before \\end{...} (so `die' removes
  the whole inner block, like Vim's `di)` on multi-line parens)
- inner selection is anchored to the indentation level of the \\end line,
  so after `die' point doesn't jump to column 0.

First tries to find environment containing point.
If not found, searches forward on the same line for the nearest environment."
  (or
   ;; First try: find environment containing point
   (when-let* ((node (evil-tex-ts--get-node-at-point))
               (env-node (evil-tex-ts--find-parent-by-type
                          node '("generic_environment" "math_environment"))))
     (evil-tex-ts--bounds-of-environment-at-node env-node))
   ;; Fallback: search forward on the same line
   (evil-tex-ts--find-environment-forward)))

(defun evil-tex-ts--change-operator-p ()
  "Return non-nil when the current Evil operator is a change command."
  (when (boundp 'evil-this-operator)
    (let ((op evil-this-operator))
      (or (eq op 'evil-change)
          (and (boundp 'evil-change-commands)
               (memq op evil-change-commands))))))

(defun evil-tex-ts--bounds-of-environment-inner-lines ()
  "Return linewise inner bounds of environment at point as (BEG . END).

BEG is the beginning of the first line after \\begin{...}.
END is the beginning of the line containing \\end{...}.

Returns nil for single-line environments or if no environment is found."
  (when-let* ((node (evil-tex-ts--get-node-at-point))
              (env-node (evil-tex-ts--find-parent-by-type
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

(defconst evil-tex-ts--command-types
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
    "import_include" "input_include"
    ;; Text mode commands (like \text, \textbf, etc. in math)
    "text_mode")
  "List of tree-sitter node types that represent LaTeX commands.")

(defun evil-tex-ts--find-command-forward ()
  "Search forward on current line for nearest command and return its bounds.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((line-end (line-end-position))
          (found nil))
      ;; Search for backslash followed by letters (command pattern)
      (while (and (not found)
                  (re-search-forward "\\\\[a-zA-Z@]+" line-end t))
        (goto-char (match-beginning 0))
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (cmd-node (evil-tex-ts--find-parent-by-type
                               node evil-tex-ts--command-types)))
          (let* ((outer-beg (treesit-node-start cmd-node))
                 (arg-info (evil-tex-ts--command-curly-args cmd-node))
                 (outer-end (car arg-info))
                 (curly-nodes (cdr arg-info))
                 (target-curly (evil-tex-ts--find-target-curly-group curly-nodes (point)))
                 (inner-beg (if target-curly
                                (1+ (treesit-node-start target-curly))
                              outer-end))
                 (inner-end (if target-curly
                                (1- (treesit-node-end target-curly))
                              outer-end)))
            (setq found (list outer-beg outer-end inner-beg inner-end))))
        (unless found
          (goto-char (match-end 0))))
      found)))

(defun evil-tex-ts--bounds-of-command ()
  "Return bounds of LaTeX command at or after point on the same line.
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
  \\alpha|        -> inner is empty

First tries to find command containing point.
If not found, searches forward on the same line for the nearest command."
  ;; Ensure we have a parser
  (unless (treesit-parser-list)
    (when (evil-tex-ts--ensure-parser)
      (treesit-parser-create 'latex)))
  ;; Try fallback first - it handles cases like \sqrt[n]{x} where
  ;; tree-sitter doesn't recognize the curly group as part of the command
  (or (evil-tex-ts--bounds-of-command-fallback)
      (evil-tex-ts--bounds-of-command-tree-sitter)
      ;; Fallback: search forward on the same line
      (evil-tex-ts--find-command-forward)))

(defun evil-tex-ts--command-curly-args (cmd-node)
  "Return command curly arguments info for CMD-NODE.

Returns (OUTER-END . CURLY-NODES), where OUTER-END may be extended to include
trailing optional [...] and curly {...} arguments that are not part of the
tree-sitter command node."
  (let ((outer-end (treesit-node-end cmd-node))
        (curly-nodes (evil-tex-ts--collect-command-curly-args cmd-node)))
    (when (null curly-nodes)
      (when-let ((extended (evil-tex-ts--extend-command-bounds cmd-node)))
        (setq outer-end (nth 0 extended))
        (setq curly-nodes (nth 1 extended))))
    (cons outer-end curly-nodes)))

(defun evil-tex-ts--nearest-ancestor-command-with-curly-arg (cmd-node pt)
  "Return nearest ancestor command of CMD-NODE whose curly arg contains PT.

This is used to prefer the surrounding command when point is on an inner
no-argument command (e.g. \\cdot) inside another command's {...} argument."
  (let ((parent (treesit-node-parent cmd-node))
        (found nil))
    (while (and parent (not found))
      (when (member (treesit-node-type parent) evil-tex-ts--command-types)
        (let* ((arg-info (evil-tex-ts--command-curly-args parent))
               (curly-nodes (cdr arg-info))
               (contains-point nil))
          (when curly-nodes
            (dolist (curly-node curly-nodes)
              (when (evil-tex-ts--node-contains-point-p curly-node pt)
                (setq contains-point t)))
            (when contains-point
              (setq found parent)))))
      (setq parent (treesit-node-parent parent)))
    found))

(defun evil-tex-ts--bounds-of-command-tree-sitter ()
  "Return bounds of LaTeX command using tree-sitter nodes.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

This function handles the case where tree-sitter doesn't include
optional arguments [...] or curly arguments {...} as part of the command node.
For example, \\sqrt[n]{x} is parsed as separate nodes:
- generic_command: \\sqrt
- text inside [...]: n
- curly_group: {x}

We detect this and extend the command bounds to include trailing [...] and {...}."
  (when-let* ((node (evil-tex-ts--get-node-at-point))
              (cmd-node (evil-tex-ts--find-parent-by-type
                         node evil-tex-ts--command-types)))
    (let* ((outer-beg (treesit-node-start cmd-node))
           (outer-end (treesit-node-end cmd-node))
           (pt (point))
           ;; Collect curly arg nodes that are children of the command
           (curly-nodes (evil-tex-ts--collect-command-curly-args cmd-node)))
      ;; If the command has no curly children, check if there are trailing
      ;; [...] and {...} siblings that belong to this command
      (when (null curly-nodes)
        (let ((extended (evil-tex-ts--extend-command-bounds cmd-node)))
          (when extended
            (setq outer-end (nth 0 extended))
            (setq curly-nodes (nth 1 extended)))))
      ;; If this is an inner command without args (e.g. \\cdot) and point is
      ;; inside an ancestor command's curly argument, prefer that ancestor.
      ;; This matches the expected `vic` behavior for cases like:
      ;;   \\sqrt{a_1 \\cdot a_2|}  -> selects "a_1 \\cdot a_2"
      (when (null curly-nodes)
        (when-let ((ancestor (evil-tex-ts--nearest-ancestor-command-with-curly-arg
                              cmd-node pt)))
          (setq cmd-node ancestor)
          (setq outer-beg (treesit-node-start cmd-node))
          (let ((arg-info (evil-tex-ts--command-curly-args cmd-node)))
            (setq outer-end (car arg-info))
            (setq curly-nodes (cdr arg-info)))))
      (let* ((target-curly (evil-tex-ts--find-target-curly-group curly-nodes pt))
             (inner-beg (if target-curly
                            (1+ (treesit-node-start target-curly))
                          outer-end))
             (inner-end (if target-curly
                            (1- (treesit-node-end target-curly))
                          outer-end)))
        (list outer-beg outer-end inner-beg inner-end)))))

(defun evil-tex-ts--extend-command-bounds (cmd-node)
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
                (let ((curly-node (evil-tex-ts--find-parent-by-type
                                   node-at '("curly_group"))))
                  (when curly-node
                    (push curly-node curly-nodes)))))
            (setq new-end grp-end))
          (skip-chars-forward " \t\n")))
      ;; Only return if we found something new
      (when (and curly-nodes (> new-end (treesit-node-end cmd-node)))
        (list new-end (nreverse curly-nodes))))))

(defun evil-tex-ts--find-target-curly-group (curly-nodes pt)
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

(defconst evil-tex-ts--curly-arg-node-types
  '("curly_group" "curly_group_text" "curly_group_text_list"
    "curly_group_path" "curly_group_path_list" "curly_group_glob_pattern"
    "curly_group_label" "curly_group_key_value" "curly_group_author_list")
  "List of tree-sitter node types that represent curly brace command arguments.")

(defun evil-tex-ts--collect-command-curly-args (cmd-node)
  "Collect curly group argument nodes from CMD-NODE."
  (let ((args nil)
        (child-count (treesit-node-child-count cmd-node)))
    (dotimes (i child-count)
      (let* ((child (treesit-node-child cmd-node i))
             (type (treesit-node-type child)))
        (when (member type evil-tex-ts--curly-arg-node-types)
          (push child args))))
    (nreverse args)))

(defun evil-tex-ts--bounds-of-command-fallback ()
  "Return bounds of LaTeX command using fallback search.
This handles cases where tree-sitter doesn't recognize the command structure,
such as \\sqrt[n]{x} where the curly group is not a child of the command.
Also handles when cursor is inside optional [...] argument.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((node (evil-tex-ts--get-node-at-point)))
    (when node
      ;; Try to find orphan curly_group first
      (let ((curly-node (evil-tex-ts--find-parent-by-type
                         node '("curly_group"))))
        (if curly-node
            ;; Check if this curly_group is "orphan" (not part of a recognized
            ;; command node). For example, tree-sitter-latex often parses
            ;; \sqrt[n]{x} as separate siblings (\sqrt, [...], {...}), so when
            ;; point is inside {...} there is no command node ancestor.
            (let ((enclosing-command
                   (evil-tex-ts--find-parent-by-type
                    curly-node evil-tex-ts--command-types)))
              (unless enclosing-command
                ;; Look backward for a command pattern and find all curly groups
                (evil-tex-ts--find-command-with-curly-fallback curly-node)))
          ;; No curly_group found - maybe cursor is inside [...] argument
          ;; Check if we're inside brackets that follow a command
          (evil-tex-ts--find-command-from-bracket-position))))))

(defun evil-tex-ts--find-command-from-bracket-position ()
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
                    (let ((target (evil-tex-ts--find-target-curly-group-from-positions
                                   curly-groups pt)))
                      (when target
                        (list cmd-start cmd-end
                              (1+ (car target)) (1- (cdr target)))))
                  ;; No curly groups - inner is empty
                  (list cmd-start cmd-end cmd-end cmd-end))))))))))

(defun evil-tex-ts--find-command-with-curly-fallback (curly-node)
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
                (let ((target (evil-tex-ts--find-target-curly-group-from-positions
                               curly-groups pt)))
                  (when target
                    (list cmd-start cmd-end
                          (1+ (car target)) (1- (cdr target)))))))))))))

(defun evil-tex-ts--find-target-curly-group-from-positions (curly-positions pt)
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

(defconst evil-tex-ts--arg-node-types
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

(defun evil-tex-ts--collect-command-args (cmd-node)
  "Collect all argument nodes from CMD-NODE."
  (let ((args nil)
        (child-count (treesit-node-child-count cmd-node)))
    (dotimes (i child-count)
      (let* ((child (treesit-node-child cmd-node i))
             (type (treesit-node-type child)))
        (when (member type evil-tex-ts--arg-node-types)
          (push child args))))
    (nreverse args)))

(defun evil-tex-ts--bounds-of-math-at-node (math-node)
  "Return bounds for MATH-NODE.
Helper function that extracts bounds from a known math node."
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
        (list outer-beg outer-end inner-beg inner-end))))))

(defun evil-tex-ts--find-math-forward ()
  "Search forward on current line for nearest math and return its bounds.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((line-end (line-end-position))
          (found nil))
      ;; Search for common math delimiters on the current line
      (while (and (not found)
                  (re-search-forward "\\$\\|\\\\(\\|\\\\\\[\\|\\\\begin{" line-end t))
        (backward-char (length (match-string 0)))
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (math-node (evil-tex-ts--find-parent-by-type
                                node '("inline_formula" "displayed_equation"
                                       "math_environment"))))
          (setq found (evil-tex-ts--bounds-of-math-at-node math-node)))
        (unless found
          (forward-char (length (match-string 0)))))
      found)))

(defun evil-tex-ts--bounds-of-math ()
  "Return bounds of math environment at or after point on the same line.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

First tries to find math containing point.
If not found, searches forward on the same line for the nearest math."
  (or
   ;; First try: find math containing point
   (when-let* ((node (evil-tex-ts--get-node-at-point))
               (math-node (evil-tex-ts--find-parent-by-type
                           node '("inline_formula" "displayed_equation"
                                  "math_environment"))))
     (evil-tex-ts--bounds-of-math-at-node math-node))
   ;; Fallback: search forward on the same line
   (evil-tex-ts--find-math-forward)))

(defconst evil-tex-ts--delimiter-pairs
  '(("(" . ")")
    ("[" . "]")
    ("\\{" . "\\}")
    ("\\langle" . "\\rangle")
    ("\\lvert" . "\\rvert")
    ("\\lVert" . "\\rVert")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil"))
  "List of delimiter pairs (left . right) for matching.")

(defconst evil-tex-ts--delimiter-prefixes
  '("" "\\left" "\\right"
    "\\bigl" "\\bigr" "\\big"
    "\\Bigl" "\\Bigr" "\\Big"
    "\\biggl" "\\biggr" "\\bigg"
    "\\Biggl" "\\Biggr" "\\Bigg")
  "Prefixes that can appear before delimiters.")

(defun evil-tex-ts--find-delimiter-forward ()
  "Search forward on current line for nearest delimiter and return its bounds.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((line-end (line-end-position))
          (found nil))
      ;; Search for opening delimiters on the current line
      (while (and (not found)
                  (re-search-forward "\\\\left\\|\\\\bigl\\|\\\\Bigl\\|\\\\biggl\\|\\\\Biggl\\|\\\\big\\|\\\\Big\\|\\\\bigg\\|\\\\Bigg\\|[([{]\\|\\\\{\\|\\\\langle\\|\\\\lvert\\|\\\\lVert\\|\\\\lfloor\\|\\\\lceil" line-end t))
        (goto-char (match-beginning 0))
        ;; Try math_delimiter first
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (delim-node (evil-tex-ts--find-parent-by-type
                                 node '("math_delimiter"))))
          (let* ((outer-beg (treesit-node-start delim-node))
                 (outer-end (treesit-node-end delim-node))
                 (left-delim (treesit-node-child-by-field-name delim-node "left_delimiter"))
                 (right-command (treesit-node-child-by-field-name delim-node "right_command"))
                 (inner-beg (if left-delim (treesit-node-end left-delim) outer-beg))
                 (inner-end (if right-command (treesit-node-start right-command) outer-end)))
            (setq found (list outer-beg outer-end inner-beg inner-end))))
        ;; If not math_delimiter, try simple delimiter pair
        (unless found
          (dolist (pair evil-tex-ts--delimiter-pairs)
            (when (and (not found)
                       (looking-at (regexp-quote (car pair))))
              (let ((left (car pair))
                    (right (cdr pair)))
                (condition-case nil
                    (let ((start (point)))
                      (forward-sexp)
                      (let ((end (point)))
                        (setq found (list start end
                                          (+ start (length left))
                                          (- end (length right))))))
                  (error nil))))))
        (unless found
          (goto-char (1+ (match-beginning 0)))))
      found)))

(defun evil-tex-ts--bounds-of-delimiter ()
  "Return bounds of delimiter at or after point on the same line.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

Handles tree-sitter `math_delimiter' nodes (\\left/\\right, \\bigl/\\bigr)
and falls back to searching for matching delimiter pairs.

First tries to find delimiter containing point.
If not found, searches forward on the same line for the nearest delimiter."
  (or (evil-tex-ts--bounds-of-math-delimiter)
      (evil-tex-ts--bounds-of-simple-delimiter)
      ;; Fallback: search forward on the same line
      (evil-tex-ts--find-delimiter-forward)))

(defun evil-tex-ts--bounds-of-math-delimiter ()
  "Return bounds of math_delimiter node at point, or nil."
  (when-let* ((node (evil-tex-ts--get-node-at-point))
              (delim-node (evil-tex-ts--find-parent-by-type
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

(defun evil-tex-ts--bounds-of-simple-delimiter ()
  "Return bounds of simple delimiter pair at point using search.
Finds closest enclosing (), [], or \\{\\}."
  (let ((best-bounds nil)
        (point-pos (point)))
    (save-excursion
      (dolist (pair evil-tex-ts--delimiter-pairs)
        (let* ((left (car pair))
               (right (cdr pair))
               (bounds (evil-tex-ts--find-delimiter-pair left right)))
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

(defun evil-tex-ts--find-delimiter-pair (left right)
  "Find matching delimiter pair LEFT and RIGHT around point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((orig-point (point))
        left-pos right-pos)
    (save-excursion
      ;; Search backward for left delimiter (include current position)
      (when (evil-tex-ts--search-backward-delimiter left)
        (setq left-pos (point))
        ;; Search forward for matching right delimiter
        (goto-char orig-point)
        (when (evil-tex-ts--search-forward-delimiter right)
          (setq right-pos (point))
          ;; Verify this is a valid pair (left at or before point, right after point)
          (when (and left-pos right-pos
                     (<= left-pos orig-point)
                     (> right-pos orig-point))
            (list left-pos right-pos
                  (+ left-pos (length left))
                  (- right-pos (length right)))))))))

(defun evil-tex-ts--search-backward-delimiter (delim)
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

(defun evil-tex-ts--search-forward-delimiter (delim)
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

;;; Superscript/Subscript text objects

(defun evil-tex-ts--bounds-of-script-at-node (script-node script-type)
  "Return bounds for SCRIPT-NODE of SCRIPT-TYPE.
Helper function that extracts bounds from a known script node."
  (let* ((outer-beg (treesit-node-start script-node))
         (outer-end (treesit-node-end script-node))
         ;; Get the argument (field name is same as script type)
         (arg-node (treesit-node-child-by-field-name script-node script-type))
         inner-beg inner-end)
    (if arg-node
        (let ((arg-type (treesit-node-type arg-node)))
          (if (string= arg-type "curly_group")
              ;; For curly_group: inner is content inside braces
              (progn
                (setq inner-beg (1+ (treesit-node-start arg-node)))
                (setq inner-end (1- (treesit-node-end arg-node))))
            ;; For letter, command_name, etc.: inner is the whole arg
            (setq inner-beg (treesit-node-start arg-node))
            (setq inner-end (treesit-node-end arg-node))))
      ;; No arg node found, inner equals outer (shouldn't happen)
      (setq inner-beg outer-beg)
      (setq inner-end outer-end))
    (list outer-beg outer-end inner-beg inner-end)))

(defun evil-tex-ts--find-script-forward (script-type)
  "Search forward for nearest SCRIPT-TYPE and return its bounds.
SCRIPT-TYPE is either \"superscript\" or \"subscript\".
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (let ((search-char (if (string= script-type "superscript") ?^ ?_)))
    (save-excursion
      (when (search-forward (char-to-string search-char) nil t)
        (backward-char)  ; Move back onto the ^ or _
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (script-node (evil-tex-ts--find-parent-by-type
                                  node (list script-type))))
          (evil-tex-ts--bounds-of-script-at-node script-node script-type))))))

(defun evil-tex-ts--bounds-of-script (script-type)
  "Return bounds of superscript or subscript at or after point.
SCRIPT-TYPE is either \"superscript\" or \"subscript\".
Returns (outer-beg outer-end inner-beg inner-end) or nil.

First tries to find a script containing point.
If not found, searches forward for the nearest script.

Outer bounds cover the entire script (e.g., ^{foo} or ^b or ^\\bar).
Inner bounds cover the argument:
- For curly groups {foo}: the content inside braces (foo)
- For single letters (b): the letter itself
- For commands (\\bar): the command itself"
  (or
   ;; First try: find script containing point
   (when-let* ((node (evil-tex-ts--get-node-at-point))
               (script-node (evil-tex-ts--find-parent-by-type
                             node (list script-type))))
     (evil-tex-ts--bounds-of-script-at-node script-node script-type))
   ;; Fallback: search forward for nearest script
   (evil-tex-ts--find-script-forward script-type)))

(defun evil-tex-ts--bounds-of-superscript ()
  "Return bounds of superscript at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (evil-tex-ts--bounds-of-script "superscript"))

(defun evil-tex-ts--bounds-of-subscript ()
  "Return bounds of subscript at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (evil-tex-ts--bounds-of-script "subscript"))

;;; Section text objects

(defconst evil-tex-ts--section-types
  '("part" "chapter" "section" "subsection" "subsubsection"
    "paragraph" "subparagraph")
  "List of LaTeX section node types in hierarchical order (highest first).")

(defun evil-tex-ts--find-next-section-start (pos)
  "Find the start position of the next section after POS.
Returns the position or nil if no next section found."
  (save-excursion
    (goto-char pos)
    (when (re-search-forward
           "\\\\\\(?:part\\|chapter\\|section\\|subsection\\|subsubsection\\|paragraph\\|subparagraph\\)\\*?{"
           nil t)
      (match-beginning 0))))

(defun evil-tex-ts--bounds-of-section-at-node (section-node)
  "Return bounds for SECTION-NODE.
Helper function that extracts bounds from a known section node.
Section extends until the next section command or end of buffer."
  (let* ((outer-beg (treesit-node-start section-node))
         (tree-sitter-end (treesit-node-end section-node))
         ;; Find the title curly_group to determine inner-beg
         (title-node (treesit-node-child-by-field-name section-node "text"))
         (inner-beg (if title-node
                        (treesit-node-end title-node)
                      outer-beg))
         ;; Extend outer-end to the next section or end of buffer
         (next-section-start (evil-tex-ts--find-next-section-start tree-sitter-end))
         (outer-end (or next-section-start (point-max)))
         (inner-end outer-end))
    ;; Adjust inner-beg to skip newline after title if present
    (when (and evil-tex-ts-select-newlines-with-envs
               (< inner-beg (point-max))
               (eq (char-after inner-beg) ?\n))
      (setq inner-beg (1+ inner-beg)))
    (list outer-beg outer-end inner-beg inner-end)))

(defun evil-tex-ts--find-section-forward ()
  "Search forward on current line for nearest section and return its bounds.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (save-excursion
    (let ((line-end (line-end-position))
          (found nil))
      ;; Search for section commands on the current line
      ;; Use simple alternation pattern for better reliability
      (while (and (not found)
                  (re-search-forward "\\\\\\(?:part\\|chapter\\|section\\|subsection\\|subsubsection\\|paragraph\\|subparagraph\\)" line-end t))
        (goto-char (match-beginning 0))
        (when-let* ((node (treesit-node-at (point) 'latex))
                    (section-node (evil-tex-ts--find-parent-by-type
                                   node evil-tex-ts--section-types)))
          (setq found (evil-tex-ts--bounds-of-section-at-node section-node)))
        (unless found
          (goto-char (match-end 0))))
      found)))

(defun evil-tex-ts--bounds-of-section ()
  "Return bounds of LaTeX section at or after point on the same line.
Returns (outer-beg outer-end inner-beg inner-end) or nil.

Sections include: part, chapter, section, subsection, subsubsection,
paragraph, and subparagraph.

Outer bounds cover the entire section from \\section{...} to the end.
Inner bounds start after the section title's closing brace.

First tries to find section containing point.
If not found, searches forward on the same line for the nearest section."
  (or
   ;; First try: find section containing point
   (when-let* ((node (evil-tex-ts--get-node-at-point))
               (section-node (evil-tex-ts--find-parent-by-type
                              node evil-tex-ts--section-types)))
     (evil-tex-ts--bounds-of-section-at-node section-node))
   ;; Fallback: search forward on the same line
   (evil-tex-ts--find-section-forward)))

;;; Section navigation

(defun evil-tex-ts-go-back-section (&optional count)
  "Go back to the closest section heading.
If COUNT is given, go COUNT sections back."
  (interactive "p")
  (setq count (or count 1))
  (let ((section-regexp "\\\\\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)\\*?{"))
    (dotimes (_ count)
      ;; If on a section command, move back first to not find the same one
      (when (looking-at section-regexp)
        (backward-char))
      (re-search-backward section-regexp nil t))))

(defun evil-tex-ts-go-forward-section (&optional count)
  "Go forward to the closest section heading.
If COUNT is given, go COUNT sections forward."
  (interactive "p")
  (setq count (or count 1))
  (let ((section-regexp "\\\\\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)\\*?{"))
    (dotimes (_ count)
      ;; If on a section command, skip past it first
      (when (looking-at section-regexp)
        (forward-char))
      (re-search-forward section-regexp nil t)
      ;; Move to the beginning of the match
      (goto-char (match-beginning 0)))))

;;; Section toggle

(defvar evil-tex-ts-section-name-history nil
  "History for section name changes with `evil-tex-ts-toggle-section'.")

(defun evil-tex-ts-toggle-section ()
  "Change the name of the surrounding section.
Prompts for a new section type (e.g., section -> subsection)."
  (interactive)
  (when-let* ((bounds (evil-tex-ts--bounds-of-section))
              (outer-beg (nth 0 bounds)))
    (save-excursion
      (goto-char outer-beg)
      (when (looking-at "\\\\\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)\\(\\*?\\)")
        (let* ((old-type (match-string 1))
               (has-star (match-string 2))
               (new-type (completing-read
                          (format "Change \\%s to: \\" old-type)
                          evil-tex-ts--section-types
                          nil t nil
                          'evil-tex-ts-section-name-history
                          old-type)))
          (replace-match (concat "\\" new-type has-star) t t))))))

;;; Evil text objects
;;
;; Text objects generally return a simple list (BEG END) to avoid visual-state
;; off-by-one issues with `inclusive'. Some operators (e.g. `cie' on multi-line
;; environments) need explicit linewise ranges to match Vim/Evil behavior.

;; Environment text objects (ie/ae)
(evil-define-text-object evil-tex-ts-inner-environment (count &optional beg end type)
  "Select inner LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-environment)))
    (if (and evil-tex-ts-select-newlines-with-envs
             (evil-tex-ts--change-operator-p))
        (if-let ((line-bounds (evil-tex-ts--bounds-of-environment-inner-lines)))
            (evil-range (car line-bounds) (cdr line-bounds) 'line)
          (list (nth 2 bounds) (nth 3 bounds)))
      (list (nth 2 bounds) (nth 3 bounds)))))

(evil-define-text-object evil-tex-ts-outer-environment (count &optional beg end type)
  "Select outer LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-environment)))
    (let ((outer-beg (nth 0 bounds))
          (outer-end (nth 1 bounds)))
      ;; Use linewise type when environment occupies whole lines
      ;; This ensures cursor lands on first non-whitespace char after 'dae'
      (if (and evil-tex-ts-select-newlines-with-envs
               (save-excursion
                 (goto-char outer-beg)
                 (bolp))
               (save-excursion
                 (goto-char outer-end)
                 (or (eobp) (bolp))))
          (evil-range outer-beg outer-end 'line)
        (list outer-beg outer-end)))))

;; Command text objects (ic/ac)
(evil-define-text-object evil-tex-ts-inner-command (count &optional beg end type)
  "Select inner LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-command)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-command (count &optional beg end type)
  "Select outer LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-command)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Math text objects (im/am)
(evil-define-text-object evil-tex-ts-inner-math (count &optional beg end type)
  "Select inner math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-math)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-math (count &optional beg end type)
  "Select outer math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-math)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Delimiter text objects (id/ad)
(evil-define-text-object evil-tex-ts-inner-delimiter (count &optional beg end type)
  "Select inner delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-delimiter)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-delimiter (count &optional beg end type)
  "Select outer delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-delimiter)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Superscript text objects (i^/a^)
(evil-define-text-object evil-tex-ts-inner-superscript (count &optional beg end type)
  "Select inner superscript.
For ^{foo}: selects foo
For ^b: selects b
For ^\\bar: selects \\bar"
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-superscript)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-superscript (count &optional beg end type)
  "Select outer superscript.
For ^{foo}: selects ^{foo}
For ^b: selects ^b
For ^\\bar: selects ^\\bar"
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-superscript)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Subscript text objects (i_/a_)
(evil-define-text-object evil-tex-ts-inner-subscript (count &optional beg end type)
  "Select inner subscript.
For _{foo}: selects foo
For _b: selects b
For _\\bar: selects \\bar"
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-subscript)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-subscript (count &optional beg end type)
  "Select outer subscript.
For _{foo}: selects _{foo}
For _b: selects _b
For _\\bar: selects _\\bar"
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-subscript)))
    (list (nth 0 bounds) (nth 1 bounds))))

;; Section text objects (iS/aS)
(evil-define-text-object evil-tex-ts-inner-section (count &optional beg end type)
  "Select inner LaTeX section.
Selects content after the section title to the end of the section."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-section)))
    (list (nth 2 bounds) (nth 3 bounds))))

(evil-define-text-object evil-tex-ts-outer-section (count &optional beg end type)
  "Select outer LaTeX section.
Selects from \\section{...} to the end of the section."
  :extend-selection nil
  (when-let ((bounds (evil-tex-ts--bounds-of-section)))
    (list (nth 0 bounds) (nth 1 bounds))))

;;; Toggles

(defun evil-tex-ts-toggle-env-asterisk ()
  "Toggle asterisk on current environment (e.g., equation <-> equation*).
Toggles the asterisk in both \\begin{env} and \\end{env}."
  (interactive)
  (when-let* ((bounds (evil-tex-ts--bounds-of-environment))
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

(defun evil-tex-ts-toggle-math-mode ()
  "Toggle math mode between inline and display.
Supports $...$, \\(...\\), and \\[...\\].

Inline to display:
  $...$     -> \\[...\\]
  \\(...\\) -> \\[...\\]

Display to inline (based on `evil-tex-ts-preferred-inline-math'):
  \\[...\\] -> $...$ (if `dollar')
  \\[...\\] -> \\(...\\) (if `paren')

When converting display to inline, multiline content is normalized:
newlines are collapsed to single spaces."
  (interactive)
  (when-let* ((bounds (evil-tex-ts--bounds-of-math))
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
          (let* ((use-dollar (eq evil-tex-ts-preferred-inline-math 'dollar))
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

(defconst evil-tex-ts--size-prefix-regexp
  "\\\\\\(?:left\\|right\\|bigl\\|bigr\\|Bigl\\|Bigr\\|biggl\\|biggr\\|Biggl\\|Biggr\\|bigg\\|Bigg\\|big\\|Big\\)"
  "Regexp matching sizing prefixes like \\left, \\bigl, etc.
Note: longer matches (bigl, bigr) must come before shorter ones (big).")

(defun evil-tex-ts-toggle-delim-size ()
  "Toggle delimiter sizing (() <-> \\left(\\right)).
Also converts \\bigl(\\bigr) etc. to plain () (one-way conversion)."
  (interactive)
  (when-let* ((bounds (evil-tex-ts--bounds-of-delimiter))
              (outer-beg (nth 0 bounds))
              (inner-beg (nth 2 bounds))
              (inner-end (nth 3 bounds))
              (outer-end (nth 1 bounds)))
    (let ((left-delim (buffer-substring-no-properties outer-beg inner-beg))
          (right-delim (buffer-substring-no-properties inner-end outer-end)))
      (save-excursion
        (cond
         ;; Has sizing prefix -> remove it
         ((string-match evil-tex-ts--size-prefix-regexp left-delim)
          (let ((new-left (replace-regexp-in-string
                           evil-tex-ts--size-prefix-regexp "" left-delim))
                (new-right (replace-regexp-in-string
                            evil-tex-ts--size-prefix-regexp "" right-delim)))
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

(defun evil-tex-ts-toggle-cmd-asterisk ()
  "Toggle asterisk on current command (\\section <-> \\section*).
The asterisk is placed between the command name and its arguments."
  (interactive)
  (when-let* ((bounds (evil-tex-ts--bounds-of-command))
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

(defun evil-tex-ts-toggle-math-align ()
  "Toggle between display math and align* environment.

\\(...\\), \\=\\[...\\] -> \\begin{align*}...\\end{align*}
\\begin{align*}...\\end{align*} -> \\=\\[...\\]

Also works with inline math ($...$) converting to align*."
  (interactive)
  (when-let* ((node (evil-tex-ts--get-node-at-point))
              (math-node (evil-tex-ts--find-parent-by-type
                          node '("inline_formula" "displayed_equation"
                                 "math_environment"))))
    (let* ((node-type (treesit-node-type math-node))
           (outer-beg (treesit-node-start math-node))
           (outer-end (treesit-node-end math-node)))
      (cond
       ;; Inside math_environment (e.g., align*) -> convert to display math
       ((string= node-type "math_environment")
        (let* ((begin-node (treesit-node-child-by-field-name math-node "begin"))
               (end-node (treesit-node-child-by-field-name math-node "end"))
               (inner-beg (if begin-node (treesit-node-end begin-node) outer-beg))
               (inner-end (if end-node (treesit-node-start end-node) outer-end))
               (content (string-trim (buffer-substring-no-properties inner-beg inner-end))))
          ;; Delete environment and insert display math (multiline format)
          (save-excursion
            (delete-region outer-beg outer-end)
            (goto-char outer-beg)
            (insert "\\[\n")
            (insert content)
            (insert "\n\\]")
            (indent-region outer-beg (point)))))
       ;; Inside inline_formula or displayed_equation -> convert to align*
       (t
        (let* ((child-count (treesit-node-child-count math-node))
               (first-child (treesit-node-child math-node 0))
               (last-child (treesit-node-child math-node (1- child-count)))
               (inner-beg (if first-child (treesit-node-end first-child) outer-beg))
               (inner-end (if last-child (treesit-node-start last-child) outer-end))
               ;; Trim whitespace from content to avoid duplicate newlines
               (content (string-trim (buffer-substring-no-properties inner-beg inner-end)))
               ;; Check if there's non-whitespace before on its line
               (needs-newline-before
                (save-excursion
                  (goto-char outer-beg)
                  (let ((line-start (line-beginning-position)))
                    (and (> outer-beg line-start)
                         (string-match-p "[^ \t]"
                                         (buffer-substring-no-properties
                                          line-start outer-beg)))))))
          ;; Delete old math delimiters and insert align*
          (save-excursion
            (delete-region outer-beg outer-end)
            (goto-char outer-beg)
            (when needs-newline-before
              (insert "\n"))
            (insert "\\begin{align*}\n")
            (insert content)
            (insert "\n\\end{align*}")
            ;; Let Emacs indent the environment
            (indent-region outer-beg (point)))))))))

;;; Evil-surround integration

(defun evil-tex-ts--populate-surround-keymap (keymap generator-alist prefix
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

(defun evil-tex-ts--read-with-keymap (keymap)
  "Prompt the user to press a key from KEYMAP.
Return the result of the called function, or error if key not found."
  (let (key map-result)
    (when (and (require 'which-key nil t)
               (fboundp 'which-key--show-keymap))
      (run-with-idle-timer
       which-key-idle-delay nil
       (lambda () (unless key
                    (which-key--show-keymap nil keymap nil nil t)))))
    (setq key (string (read-char)))
    (when (fboundp 'which-key--hide-popup)
      (which-key--hide-popup))
    (setq map-result (lookup-key keymap key))
    (cond
     ((or (not map-result) (numberp map-result))
      (user-error "%s not found in keymap" key))
     ((functionp map-result)
      (call-interactively map-result))
     ((keymapp map-result)
      (evil-tex-ts--read-with-keymap map-result)))))

;; Format functions for surround

(defun evil-tex-ts-get-env-for-surrounding (env-name)
  "Format strings for the env named ENV-NAME for surrounding.
Return a cons of (\"\\begin{ENV-NAME}\" . \"\\end{ENV-NAME}\").
Respect the value of `evil-tex-ts-include-newlines-in-envs'."
  (interactive (list (read-from-minibuffer "env: " nil minibuffer-local-ns-map)))
  (cons (format "\\begin{%s}%s"
                env-name
                (if evil-tex-ts-include-newlines-in-envs "\n" ""))
        (format "%s\\end{%s}"
                (if evil-tex-ts-include-newlines-in-envs "\n" "")
                env-name)))

(defun evil-tex-ts--format-env-cons-for-surrounding (env-cons)
  "Format ENV-CONS for surrounding.
Add newlines if `evil-tex-ts-include-newlines-in-envs' is t."
  (if evil-tex-ts-include-newlines-in-envs
      (cons (concat (car env-cons) "\n") (concat "\n" (cdr env-cons)))
    env-cons))

(defun evil-tex-ts--format-accent-for-surrounding (accent)
  "Format ACCENT for surrounding: return a cons of (\\ACCENT{ . })."
  (cons (concat "\\" accent "{") "}"))

(defvar evil-tex-ts--last-command-empty nil
  "Non-nil if the last command text object used was empty.
For example, \\alpha is empty, \\frac{a}{b} is not.")

(defun evil-tex-ts--format-command-for-surrounding (command)
  "Format COMMAND for surrounding: return a cons of (\\COMMAND{ . })."
  (if evil-tex-ts--last-command-empty
      (cons (concat "\\" command "") "")
    (cons (concat "\\" command "{") "}")))

(defun evil-tex-ts--surround-inline-math ()
  "Return surround pair for inline math based on `evil-tex-ts-preferred-inline-math'."
  (if (eq evil-tex-ts-preferred-inline-math 'dollar)
      '("$" . "$")
    '("\\(" . "\\)")))

(defun evil-tex-ts--normalize-inline-math-region (beg end)
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

(defun evil-tex-ts--surround-region-advice (orig-fn beg end type char &optional force-new-line)
  "Advice for `evil-surround-region' to normalize regions for inline math, commands, and accents.
Normalizes the region (trims and collapses newlines) when surrounding with inline math,
commands, CDLaTeX accents, superscript, or subscript. For linewise selections, changes
type to `inclusive' to prevent evil-surround from adding extra newlines.

For inline math (?m): normalization happens BEFORE surrounding.
For commands (?c): normalization happens AFTER surrounding (after prompt).
For CDLaTeX accents (?\\;): normalization happens AFTER surrounding (after prompt).
For superscript (?^) and subscript (?_): normalization happens AFTER surrounding.
For environments (?e): line breaks are added when surrounding partial lines."
  (let (indent-string had-trailing-newline content-length original-col original-line
        is-inline-math is-command is-env is-cdlatex-accent is-super-or-subscript
        ;; For environment: remember if there's text before/after on the same line
        env-has-text-before env-has-text-after env-indent env-is-linewise)
    (setq is-inline-math (and evil-tex-ts-mode (eq char ?m)))
    (setq is-command (and evil-tex-ts-mode (eq char ?c)))
    (setq is-env (and evil-tex-ts-mode (eq char ?e)))
    (setq is-cdlatex-accent (and evil-tex-ts-mode (eq char ?\;)))
    (setq is-super-or-subscript (and evil-tex-ts-mode (memq char '(?^ ?_))))
    ;; For environment: check if surrounding partial line (not linewise)
    (when (and is-env (not (eq type 'line)))
      (save-excursion
        ;; Check for non-whitespace text before beg on the same line
        (goto-char beg)
        (let ((line-start (line-beginning-position)))
          (when (> beg line-start)
            (let ((text-before (buffer-substring-no-properties line-start beg)))
              (setq env-has-text-before (string-match-p "[^ \t]" text-before))
              ;; Capture indentation (leading whitespace)
              (when (string-match "^\\([ \t]*\\)" text-before)
                (setq env-indent (match-string 1 text-before))))))
        ;; Check for non-whitespace text after end on the same line
        (goto-char end)
        (let ((line-end (line-end-position)))
          (when (< end line-end)
            (let ((text-after (buffer-substring-no-properties end line-end)))
              (setq env-has-text-after (string-match-p "[^ \t]" text-after)))))))
    ;; For environment linewise selection: handle trailing newline and indentation
    (when (and is-env (eq type 'line))
      (setq env-is-linewise t)
      ;; Linewise selection includes trailing newline as line marker.
      ;; Remove only ONE trailing newline (the linewise marker), preserving
      ;; any blank lines that user intentionally selected.
      (when (and (> end beg) (eq (char-before end) ?\n))
        (setq end (1- end)))
      ;; Use inclusive type to prevent evil-surround from adding extra newlines
      (setq type 'inclusive)
      ;; Capture indentation from the first content line for later use
      (save-excursion
        (goto-char beg)
        (setq env-indent (buffer-substring-no-properties
                          (line-beginning-position)
                          (progn (back-to-indentation) (point))))))
    ;; For inline math: normalize BEFORE calling orig-fn
    (when is-inline-math
      ;; Save cursor position (line and column)
      (setq original-line (line-number-at-pos))
      (setq original-col (current-column))
      (let ((result (evil-tex-ts--normalize-inline-math-region beg end)))
        (setq end (nth 0 result))
        (setq indent-string (nth 1 result))
        (setq had-trailing-newline (nth 2 result))
        ;; Calculate content length (from beg to new end)
        (setq content-length (- end beg)))
      ;; Force inclusive type to prevent evil-surround from
      ;; adding newlines around the content (linewise behavior)
      (when (eq type 'line)
        (setq type 'inclusive)))
    ;; Call original surround function
    (funcall orig-fn beg end type char force-new-line)
    ;; For environments: add line breaks for partial lines and handle indentation
    (when is-env
      (save-excursion
        (let ((indent (or env-indent ""))
              env-start env-end)
          (goto-char beg)
          ;; Find \begin{...}
          (when (search-forward "\\begin{" nil t)
            (let ((begin-start (- (point) 7)))  ; position of backslash
              (setq env-start begin-start)
              ;; Find end of \begin{...} line (after the closing })
              (search-forward "}" nil t)
              ;; For partial line with text-before: add newline before \begin
              (when env-has-text-before
                (goto-char begin-start)
                (insert "\n"))))
          ;; Find \end{...} - search from env-start since beg may be stale after insert
          (when env-start
            (goto-char env-start))
          (when (search-forward "\\end{" nil t)
            (search-forward "}" nil t)
            (setq env-end (point))
            ;; For partial line with text-after: add newline after \end{...}
            (when env-has-text-after
              (insert "\n")
              ;; Indent the line with text-after using the original line's indent
              (when (and indent (> (length indent) 0))
                (insert indent))))
          ;; Let Emacs handle indentation for the entire environment
          ;; This applies to ALL environment surrounds (linewise, partial line, or whole line)
          (when (and env-start env-end)
            (indent-region env-start env-end)))))
    ;; For commands, CDLaTeX accents, and super/subscript: normalize AFTER calling orig-fn
    (when (or is-command is-cdlatex-accent is-super-or-subscript)
      (save-excursion
        (goto-char beg)
        ;; Find the opening { of the command/accent/script
        (when (search-forward "{" nil t)
          (let ((inner-beg (point))
                inner-end)
            ;; Find the matching closing }
            (backward-char)
            (forward-sexp)
            (setq inner-end (1- (point)))
            ;; Normalize content if it has newlines
            (let* ((content (buffer-substring-no-properties inner-beg inner-end))
                   (has-newlines (string-match-p "\n" content)))
              (when has-newlines
                (let ((normalized (replace-regexp-in-string
                                   "[ \t]*\\(?:\n[ \t]*\\)+" " "
                                   (string-trim content))))
                  (delete-region inner-beg inner-end)
                  (goto-char inner-beg)
                  (insert normalized))))))))
    ;; Post-process for inline math: add indent before and newline after
    (when is-inline-math
      (let ((delim-pair (evil-tex-ts--surround-inline-math)))
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

(defvar evil-tex-ts-env-map (make-sparse-keymap)
  "Keymap for surrounding with environments.")

(defun evil-tex-ts-bind-to-env-map (key-generator-alist &optional keymap)
  "Bind envs from KEY-GENERATOR-ALIST to `evil-tex-ts-env-map' or KEYMAP.

Each item is a cons (KEY . VALUE):
- String VALUE: inserted env with that name
- Cons VALUE: text wrapped between car and cdr
- Function VALUE: called to get the cons"
  (evil-tex-ts--populate-surround-keymap
   (or keymap evil-tex-ts-env-map)
   key-generator-alist
   "evil-tex-ts-envs---"
   #'evil-tex-ts-get-env-for-surrounding
   #'evil-tex-ts--format-env-cons-for-surrounding))

(defalias 'evil-tex-ts-envs---+prompt #'evil-tex-ts-get-env-for-surrounding
  "Prompt for environment name.")

(setq evil-tex-ts-env-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-ts-bind-to-env-map
         '(("x" . evil-tex-ts-envs---+prompt)
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

(defun evil-tex-ts--texmathp ()
  "Return non-nil if point is inside a math environment.
Uses tree-sitter to check for math context."
  (when-let* ((node (evil-tex-ts--get-node-at-point)))
    (evil-tex-ts--find-parent-by-type
     node '("inline_formula" "displayed_equation" "math_environment"))))

(defun evil-tex-ts-cdlatex-accents---rm ()
  "Return surround pair for rm style text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathrm{" . "}")
    '("\\textrm{" . "}")))

(defun evil-tex-ts-cdlatex-accents---it ()
  "Return surround pair for italic style text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathit{" . "}")
    '("\\textit{" . "}")))

(defun evil-tex-ts-cdlatex-accents---bf ()
  "Return surround pair for bold style text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathbf{" . "}")
    '("\\textbf{" . "}")))

(defun evil-tex-ts-cdlatex-accents---emph ()
  "Return surround pair for emphasized text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathem{" . "}")
    '("\\emph{" . "}")))

(defun evil-tex-ts-cdlatex-accents---tt ()
  "Return surround pair for typewriter style text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathtt{" . "}")
    '("\\texttt{" . "}")))

(defun evil-tex-ts-cdlatex-accents---sf ()
  "Return surround pair for sans-serif style text."
  (interactive)
  (if (evil-tex-ts--texmathp)
      '("\\mathsf{" . "}")
    '("\\textsf{" . "}")))

(defvar evil-tex-ts-cdlatex-accents-map (make-sparse-keymap)
  "Keymap for surrounding with cdlatex-style accents.")

(defun evil-tex-ts-bind-to-cdlatex-accents-map (key-generator-alist &optional keymap)
  "Bind accent macros from KEY-GENERATOR-ALIST to `evil-tex-ts-cdlatex-accents-map'."
  (evil-tex-ts--populate-surround-keymap
   (or keymap evil-tex-ts-cdlatex-accents-map)
   key-generator-alist
   "evil-tex-ts-cdlatex-accents---"
   #'evil-tex-ts--format-accent-for-surrounding))

(setq evil-tex-ts-cdlatex-accents-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-ts-bind-to-cdlatex-accents-map
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
           ("r" . evil-tex-ts-cdlatex-accents---rm)
           ("i" . evil-tex-ts-cdlatex-accents---it)
           ("l" . "textsl")
           ("b" . evil-tex-ts-cdlatex-accents---bf)
           ("e" . evil-tex-ts-cdlatex-accents---emph)
           ("y" . evil-tex-ts-cdlatex-accents---tt)
           ("f" . evil-tex-ts-cdlatex-accents---sf)
           ("0" . ("{\\textstyle " . "}"))
           ("1" . ("{\\displaystyle " . "}"))
           ("2" . ("{\\scriptstyle " . "}"))
           ("3" . ("{\\scriptscriptstyle " . "}")))
         keymap)
        keymap))

;; Delimiter keymap for surround

(defvar evil-tex-ts-delim-map (make-sparse-keymap)
  "Keymap for surrounding with delimiters.")

(defun evil-tex-ts-bind-to-delim-map (key-generator-alist &optional keymap)
  "Bind delimiters from KEY-GENERATOR-ALIST to `evil-tex-ts-delim-map'."
  (evil-tex-ts--populate-surround-keymap
   (or keymap evil-tex-ts-delim-map)
   key-generator-alist
   "evil-tex-ts-delims---"
   #'identity))

(setq evil-tex-ts-delim-map
      (let ((keymap (make-sparse-keymap)))
        (evil-tex-ts-bind-to-delim-map
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

(defun evil-tex-ts-surround-env-prompt ()
  "Prompt user for an env to surround with using `evil-tex-ts-env-map'."
  (evil-tex-ts--read-with-keymap evil-tex-ts-env-map))

(defun evil-tex-ts-surround-cdlatex-accents-prompt ()
  "Prompt user for an accent to surround with using `evil-tex-ts-cdlatex-accents-map'."
  (evil-tex-ts--read-with-keymap evil-tex-ts-cdlatex-accents-map))

(defun evil-tex-ts-surround-delim-prompt ()
  "Prompt user for a delimiter to surround with using `evil-tex-ts-delim-map'."
  (evil-tex-ts--read-with-keymap evil-tex-ts-delim-map))

(defun evil-tex-ts-surround-command-prompt ()
  "Ask the user for the command they'd like to surround with."
  (evil-tex-ts--format-command-for-surrounding
   (read-from-minibuffer "command: \\" nil minibuffer-local-ns-map)))

;; Surround delimiters alist

(defvar evil-tex-ts-surround-delimiters
  `((?m . ,#'evil-tex-ts--surround-inline-math)
    (?M "\\[" . "\\]")
    (?c . ,#'evil-tex-ts-surround-command-prompt)
    (?e . ,#'evil-tex-ts-surround-env-prompt)
    (?d . ,#'evil-tex-ts-surround-delim-prompt)
    (?\; . ,#'evil-tex-ts-surround-cdlatex-accents-prompt)
    (?q "`" . "'")
    (?Q "``" . "''")
    (?^ "^{" . "}")
    (?_ "_{" . "}")
    (?T "&" . "&"))
  "Delimiter pairs for `evil-surround'.

Each element is (CHAR LEFT-DELIM . RIGHT-DELIM) or (CHAR . FUNCTION).
- m: inline math (uses `evil-tex-ts-preferred-inline-math': $...$ or \\(...\\))
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

(defvar evil-tex-ts-inner-text-objects-map (make-sparse-keymap)
  "Inner text object keymap for `evil-tex-ts'.")

(defvar evil-tex-ts-outer-text-objects-map (make-sparse-keymap)
  "Outer text object keymap for `evil-tex-ts'.")

;; Setup functions

(defun evil-tex-ts-set-up-surround ()
  "Configure evil-surround for LaTeX editing.
Things like 'csm' (change surrounding math) will work after this."
  (setq-local evil-surround-pairs-alist
              (append evil-tex-ts-surround-delimiters evil-surround-pairs-alist))
  ;; Use local text object maps if evil-surround supports it
  (when (and (boundp 'evil-surround-local-inner-text-object-map-list)
             (boundp 'evil-surround-local-outer-text-object-map-list))
    (add-to-list 'evil-surround-local-inner-text-object-map-list
                 evil-tex-ts-inner-text-objects-map)
    (add-to-list 'evil-surround-local-outer-text-object-map-list
                 evil-tex-ts-outer-text-objects-map))
  ;; Add advice for normalizing inline math regions
  (advice-add 'evil-surround-region :around #'evil-tex-ts--surround-region-advice))

(defun evil-tex-ts-set-up-embrace ()
  "Configure evil-embrace not to steal our evil-surround keybinds."
  (setq-local evil-embrace-evil-surround-keys
              (append
               (mapcar #'car evil-tex-ts-surround-delimiters)
               evil-embrace-evil-surround-keys)))

;; Populate text object keymaps
(with-eval-after-load 'evil
  ;; Inner text objects
  (define-key evil-tex-ts-inner-text-objects-map "e" #'evil-tex-ts-inner-environment)
  (define-key evil-tex-ts-inner-text-objects-map "c" #'evil-tex-ts-inner-command)
  (define-key evil-tex-ts-inner-text-objects-map "m" #'evil-tex-ts-inner-math)
  (define-key evil-tex-ts-inner-text-objects-map "d" #'evil-tex-ts-inner-delimiter)
  (define-key evil-tex-ts-inner-text-objects-map "^" #'evil-tex-ts-inner-superscript)
  (define-key evil-tex-ts-inner-text-objects-map "_" #'evil-tex-ts-inner-subscript)
  (define-key evil-tex-ts-inner-text-objects-map "S" #'evil-tex-ts-inner-section)
  ;; Outer text objects
  (define-key evil-tex-ts-outer-text-objects-map "e" #'evil-tex-ts-outer-environment)
  (define-key evil-tex-ts-outer-text-objects-map "c" #'evil-tex-ts-outer-command)
  (define-key evil-tex-ts-outer-text-objects-map "m" #'evil-tex-ts-outer-math)
  (define-key evil-tex-ts-outer-text-objects-map "d" #'evil-tex-ts-outer-delimiter)
  (define-key evil-tex-ts-outer-text-objects-map "^" #'evil-tex-ts-outer-superscript)
  (define-key evil-tex-ts-outer-text-objects-map "_" #'evil-tex-ts-outer-subscript)
  (define-key evil-tex-ts-outer-text-objects-map "S" #'evil-tex-ts-outer-section))

;; Shorten which-key descriptions
(with-eval-after-load 'which-key
  (push
   '(("\\`." . "evil-tex-ts-.*---\\(.*\\)") . (nil . "\\1"))
   which-key-replacement-alist))

;;; evil-surround integration

(defvar evil-tex-ts-section-name-history-surround nil
  "History for section name selection in surround operations.")

(defun evil-tex-ts--format-section-for-surround (section-type)
  "Format SECTION-TYPE for evil-surround.
Returns a cons of (opening . closing) strings."
  (cons (concat "\\" section-type "{") "}"))

(defun evil-tex-ts-surround-section-prompt ()
  "Prompt user for a section type to surround with.
Returns a cons of (opening . closing) strings for evil-surround."
  (let ((section-type (completing-read
                       "Section type: \\"
                       evil-tex-ts--section-types
                       nil t nil
                       'evil-tex-ts-section-name-history-surround
                       "section")))
    (evil-tex-ts--format-section-for-surround section-type)))


(defun evil-tex-ts-set-up-surround ()
  "Configure evil-surround for LaTeX editing.
Adds LaTeX-specific surround pairs like environments, commands, and sections."
  (when (boundp 'evil-surround-pairs-alist)
    (setq-local evil-surround-pairs-alist
                (append evil-tex-ts-surround-delimiters evil-surround-pairs-alist)))
  ;; Register our text object maps with evil-surround
  (when (and (boundp 'evil-surround-local-inner-text-object-map-list)
             (boundp 'evil-surround-local-outer-text-object-map-list))
    (add-to-list 'evil-surround-local-inner-text-object-map-list
                 evil-tex-ts-inner-text-objects-map)
    (add-to-list 'evil-surround-local-outer-text-object-map-list
                 evil-tex-ts-outer-text-objects-map))
  ;; Add advice for environment surround to call indent-region
  (advice-add 'evil-surround-region :around #'evil-tex-ts--surround-region-advice))

(defun evil-tex-ts-set-up-embrace ()
  "Configure evil-embrace not to steal our evil-surround keybinds."
  (when (boundp 'evil-embrace-evil-surround-keys)
    (setq-local evil-embrace-evil-surround-keys
                (append
                 (delq nil (mapcar (lambda (x) (when (characterp (car x)) (car x)))
                                   evil-tex-ts-surround-delimiters))
                 evil-embrace-evil-surround-keys))))

;;; Keymap

(defvar evil-tex-ts-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'evil-tex-ts-toggle-env-asterisk)
    (define-key map "m" #'evil-tex-ts-toggle-math-mode)
    (define-key map "M" #'evil-tex-ts-toggle-math-align)
    (define-key map "d" #'evil-tex-ts-toggle-delim-size)
    (define-key map "c" #'evil-tex-ts-toggle-cmd-asterisk)
    (define-key map "S" #'evil-tex-ts-toggle-section)
    map)
  "Keymap for evil-tex-ts toggle commands.
Bound to `mt' or `ts' prefix depending on configuration.")

(defvar evil-tex-ts-mode-map
  (make-sparse-keymap)
  "Keymap for `evil-tex-ts-mode'.")

(defun evil-tex-ts--setup-toggle-keybindings ()
  "Setup toggle keybindings based on customization options.
Call this after changing `evil-tex-ts-toggle-override-m' or
`evil-tex-ts-toggle-override-t'."
  (when (boundp 'evil-normal-state-local-map)
    ;; Bind to mt* if enabled
    (when evil-tex-ts-toggle-override-m
      (evil-define-key 'normal evil-tex-ts-mode-map
        "mt" evil-tex-ts-toggle-map))
    ;; Bind to ts* if enabled
    (when evil-tex-ts-toggle-override-t
      (evil-define-key 'normal evil-tex-ts-mode-map
        "ts" evil-tex-ts-toggle-map))))

;;; Minor mode

(defvar evil-tex-ts--pending-first-non-blank nil
  "When non-nil, move to first non-blank after current command.")

(defun evil-tex-ts--post-command-first-non-blank ()
  "Move cursor to first non-blank character if pending."
  (when evil-tex-ts--pending-first-non-blank
    (setq evil-tex-ts--pending-first-non-blank nil)
    (back-to-indentation)))

(defun evil-tex-ts--delete-advice (orig-fn beg end &optional type &rest args)
  "Advice for `evil-delete' to ensure cursor moves to first non-blank.
After linewise deletion of environment, move cursor to indentation.
Only active in buffers where `evil-tex-ts-mode' is enabled."
  (let ((was-line-type (and evil-tex-ts-mode (eq type 'line))))
    (apply orig-fn beg end type args)
    (when was-line-type
      (setq evil-tex-ts--pending-first-non-blank t))))

(defun evil-tex-ts--setup-text-objects ()
  "Setup text objects for evil-tex-ts.
Binds text objects to Evil's inner/outer text object maps.
These bindings are global but the functions check if evil-tex-ts-mode is active."
  (when (and (boundp 'evil-inner-text-objects-map)
             (boundp 'evil-outer-text-objects-map))
    ;; Inner text objects (used with 'i' prefix, e.g., vic, vie)
    (define-key evil-inner-text-objects-map "e" #'evil-tex-ts-inner-environment)
    (define-key evil-inner-text-objects-map "c" #'evil-tex-ts-inner-command)
    (define-key evil-inner-text-objects-map "m" #'evil-tex-ts-inner-math)
    (define-key evil-inner-text-objects-map "d" #'evil-tex-ts-inner-delimiter)
    (define-key evil-inner-text-objects-map "^" #'evil-tex-ts-inner-superscript)
    (define-key evil-inner-text-objects-map "_" #'evil-tex-ts-inner-subscript)
    (define-key evil-inner-text-objects-map "S" #'evil-tex-ts-inner-section)
    ;; Outer text objects (used with 'a' prefix, e.g., vac, vae)
    (define-key evil-outer-text-objects-map "e" #'evil-tex-ts-outer-environment)
    (define-key evil-outer-text-objects-map "c" #'evil-tex-ts-outer-command)
    (define-key evil-outer-text-objects-map "m" #'evil-tex-ts-outer-math)
    (define-key evil-outer-text-objects-map "d" #'evil-tex-ts-outer-delimiter)
    (define-key evil-outer-text-objects-map "^" #'evil-tex-ts-outer-superscript)
    (define-key evil-outer-text-objects-map "_" #'evil-tex-ts-outer-subscript)
    (define-key evil-outer-text-objects-map "S" #'evil-tex-ts-outer-section)))

(defun evil-tex-ts--setup-section-navigation ()
  "Setup section navigation keybindings.
Binds [[ and ]] for section navigation in normal and visual states."
  (evil-define-key 'normal evil-tex-ts-mode-map
    "[[" #'evil-tex-ts-go-back-section
    "]]" #'evil-tex-ts-go-forward-section)
  (evil-define-key 'visual evil-tex-ts-mode-map
    "[[" #'evil-tex-ts-go-back-section
    "]]" #'evil-tex-ts-go-forward-section))

;; Setup text objects and toggle keybindings when package is loaded (after evil)
(with-eval-after-load 'evil
  (evil-tex-ts--setup-text-objects)
  (evil-tex-ts--setup-toggle-keybindings)
  (evil-tex-ts--setup-section-navigation))

;;;###autoload
(define-minor-mode evil-tex-ts-mode
  "Minor mode for LaTeX text objects using tree-sitter."
  :lighter " ETB"
  :keymap evil-tex-ts-mode-map
  (if evil-tex-ts-mode
      (progn
        (unless (evil-tex-ts--ensure-parser)
          (user-error "Tree-sitter LaTeX parser not available"))
        ;; Ensure treesit parser is created for this buffer
        (treesit-parser-create 'latex)
        ;; Add hook for cursor positioning after linewise delete
        (add-hook 'post-command-hook #'evil-tex-ts--post-command-first-non-blank nil t)
        ;; Setup evil-surround integration
        (when (featurep 'evil-surround)
          (evil-tex-ts-set-up-surround))
        ;; Setup evil-embrace integration
        (when (require 'evil-embrace nil t)
          (evil-tex-ts-set-up-embrace)))
    ;; Cleanup when mode is disabled
    (remove-hook 'post-command-hook #'evil-tex-ts--post-command-first-non-blank t)))

;; Add global advice for evil-delete (only runs in evil-tex-ts-mode buffers)
(with-eval-after-load 'evil
  (advice-add 'evil-delete :around #'evil-tex-ts--delete-advice))

;; Setup evil-surround integration when it loads (for buffers where evil-tex-ts-mode is already active)
(with-eval-after-load 'evil-surround
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when evil-tex-ts-mode
        (evil-tex-ts-set-up-surround)))))

;;;###autoload
(defun evil-tex-ts-setup ()
  "Setup evil-tex-ts for LaTeX buffers."
  (interactive)
  (add-hook 'latex-mode-hook #'evil-tex-ts-mode)
  (add-hook 'LaTeX-mode-hook #'evil-tex-ts-mode))

(provide 'evil-tex-ts)
;;; evil-tex-ts.el ends here
