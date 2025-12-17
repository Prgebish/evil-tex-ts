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

(defun evil-tex-bora--bounds-of-command ()
  "Return bounds of LaTeX command at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (cmd-node (evil-tex-bora--find-parent-by-type
                         node '("generic_command"))))
    (let* ((outer-beg (treesit-node-start cmd-node))
           (outer-end (treesit-node-end cmd-node))
           ;; Inner is the argument content (inside braces)
           (arg-node (treesit-node-child-by-field-name cmd-node "arg"))
           (inner-beg (if arg-node
                          (1+ (treesit-node-start arg-node))
                        outer-beg))
           (inner-end (if arg-node
                          (1- (treesit-node-end arg-node))
                        outer-end)))
      (list outer-beg outer-end inner-beg inner-end))))

(defun evil-tex-bora--bounds-of-math ()
  "Return bounds of math environment at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  (when-let* ((node (evil-tex-bora--get-node-at-point))
              (math-node (evil-tex-bora--find-parent-by-type
                          node '("inline_formula" "displayed_equation"
                                 "math_environment"))))
    (let* ((outer-beg (treesit-node-start math-node))
           (outer-end (treesit-node-end math-node))
           (node-type (treesit-node-type math-node))
           ;; Inner bounds depend on delimiter type
           (inner-beg (cond
                       ((string= node-type "inline_formula")
                        (+ outer-beg 2))  ; Skip \( or $
                       ((string= node-type "displayed_equation")
                        (+ outer-beg 2))  ; Skip \[ or $$
                       (t outer-beg)))
           (inner-end (cond
                       ((string= node-type "inline_formula")
                        (- outer-end 2))  ; Skip \) or $
                       ((string= node-type "displayed_equation")
                        (- outer-end 2))  ; Skip \] or $$
                       (t outer-end))))
      (list outer-beg outer-end inner-beg inner-end))))

(defun evil-tex-bora--bounds-of-delimiter ()
  "Return bounds of delimiter at point.
Returns (outer-beg outer-end inner-beg inner-end) or nil."
  ;; TODO: Implement delimiter detection
  ;; This requires finding matching pairs like (), [], {}, \left(\right), etc.
  nil)

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
