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
Returns (outer-beg outer-end inner-beg inner-end) or nil."
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
      (list outer-beg outer-end inner-beg inner-end))))

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

Inner command is defined as the content inside all {}'s and []'s,
or empty (inner-beg = inner-end = outer-end) if none exist.
For example:
  \\textbf{hello} -> inner is \"hello\"
  \\frac{a}{b}    -> inner is \"a}{b\"
  \\alpha         -> inner is empty"
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (cmd-node (evil-tex-bora--find-parent-by-type
                         node evil-tex-bora--command-types)))
    (let* ((outer-beg (treesit-node-start cmd-node))
           (outer-end (treesit-node-end cmd-node))
           ;; Collect all arg nodes (curly_group, brack_group)
           (arg-nodes (evil-tex-bora--collect-command-args cmd-node))
           (inner-beg (if arg-nodes
                          (1+ (treesit-node-start (car arg-nodes)))
                        outer-end))
           (inner-end (if arg-nodes
                          (1- (treesit-node-end (car (last arg-nodes))))
                        outer-end)))
      (list outer-beg outer-end inner-beg inner-end))))

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

;; Environment text objects (ie/ae)
(evil-define-text-object evil-tex-bora-inner-environment (count &optional beg end type)
  "Select inner LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-environment)))
    (evil-range (nth 2 bounds) (nth 3 bounds) type)))

(evil-define-text-object evil-tex-bora-outer-environment (count &optional beg end type)
  "Select outer LaTeX environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-environment)))
    (evil-range (nth 0 bounds) (nth 1 bounds) type)))

;; Command text objects (ic/ac)
(evil-define-text-object evil-tex-bora-inner-command (count &optional beg end type)
  "Select inner LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-command)))
    (evil-range (nth 2 bounds) (nth 3 bounds) type)))

(evil-define-text-object evil-tex-bora-outer-command (count &optional beg end type)
  "Select outer LaTeX command."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-command)))
    (evil-range (nth 0 bounds) (nth 1 bounds) type)))

;; Math text objects (im/am)
(evil-define-text-object evil-tex-bora-inner-math (count &optional beg end type)
  "Select inner math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-math)))
    (evil-range (nth 2 bounds) (nth 3 bounds) type)))

(evil-define-text-object evil-tex-bora-outer-math (count &optional beg end type)
  "Select outer math environment."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-math)))
    (evil-range (nth 0 bounds) (nth 1 bounds) type)))

;; Delimiter text objects (id/ad)
(evil-define-text-object evil-tex-bora-inner-delimiter (count &optional beg end type)
  "Select inner delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-delimiter)))
    (evil-range (nth 2 bounds) (nth 3 bounds) type)))

(evil-define-text-object evil-tex-bora-outer-delimiter (count &optional beg end type)
  "Select outer delimiter."
  :extend-selection nil
  (when-let ((bounds (evil-tex-bora--bounds-of-delimiter)))
    (evil-range (nth 0 bounds) (nth 1 bounds) type)))

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
        (treesit-parser-create 'latex))
    ;; Cleanup when mode is disabled
    nil))

;;;###autoload
(defun evil-tex-bora-setup ()
  "Setup evil-tex-bora for LaTeX buffers."
  (interactive)
  (add-hook 'latex-mode-hook #'evil-tex-bora-mode)
  (add-hook 'LaTeX-mode-hook #'evil-tex-bora-mode))

(provide 'evil-tex-bora)
;;; evil-tex-bora.el ends here
