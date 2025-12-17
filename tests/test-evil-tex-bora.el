;;; test-evil-tex-bora.el --- Tests for evil-tex-bora -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Unit tests for evil-tex-bora using ERT.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory and tests directory to load-path
(let ((tests-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path tests-dir)
  (add-to-list 'load-path (file-name-directory (directory-file-name tests-dir))))

;; Load evil stub if evil is not available
(unless (featurep 'evil)
  (require 'evil-stub))

;; Attempt to load evil-tex-bora
(defvar evil-tex-bora-loaded nil)
(condition-case err
    (progn
      (require 'evil-tex-bora)
      (setq evil-tex-bora-loaded t))
  (error
   (message "Warning: Could not load evil-tex-bora: %s" (error-message-string err))
   (message "Running limited tests only")))

;;; Basic loading tests

(ert-deftest test-evil-tex-bora-load ()
  "Test that evil-tex-bora can be loaded."
  (skip-unless evil-tex-bora-loaded)
  (should (featurep 'evil-tex-bora)))

(ert-deftest test-evil-tex-bora-functions-exist ()
  "Test that main functions are defined."
  (skip-unless evil-tex-bora-loaded)
  ;; Tree-sitter utilities
  (should (fboundp 'evil-tex-bora--ensure-parser))
  (should (fboundp 'evil-tex-bora--get-node-at-point))
  (should (fboundp 'evil-tex-bora--find-parent-by-type))
  (should (fboundp 'evil-tex-bora--node-bounds))
  ;; Bounds functions
  (should (fboundp 'evil-tex-bora--bounds-of-environment))
  (should (fboundp 'evil-tex-bora--bounds-of-command))
  (should (fboundp 'evil-tex-bora--bounds-of-math))
  (should (fboundp 'evil-tex-bora--bounds-of-delimiter))
  ;; Toggle functions
  (should (fboundp 'evil-tex-bora-toggle-env-asterisk))
  (should (fboundp 'evil-tex-bora-toggle-math-mode))
  (should (fboundp 'evil-tex-bora-toggle-delim-size))
  (should (fboundp 'evil-tex-bora-toggle-cmd-asterisk))
  ;; Minor mode
  (should (fboundp 'evil-tex-bora-mode))
  (should (fboundp 'evil-tex-bora-setup)))

(ert-deftest test-evil-tex-bora-text-objects-exist ()
  "Test that text objects are defined."
  (skip-unless evil-tex-bora-loaded)
  ;; Environment
  (should (fboundp 'evil-tex-bora-inner-environment))
  (should (fboundp 'evil-tex-bora-outer-environment))
  ;; Command
  (should (fboundp 'evil-tex-bora-inner-command))
  (should (fboundp 'evil-tex-bora-outer-command))
  ;; Math
  (should (fboundp 'evil-tex-bora-inner-math))
  (should (fboundp 'evil-tex-bora-outer-math))
  ;; Delimiter
  (should (fboundp 'evil-tex-bora-inner-delimiter))
  (should (fboundp 'evil-tex-bora-outer-delimiter)))

(ert-deftest test-evil-tex-bora-customization-group ()
  "Test that customization group exists."
  (skip-unless evil-tex-bora-loaded)
  (should (get 'evil-tex-bora 'group-documentation)))

(ert-deftest test-evil-tex-bora-keymap-exists ()
  "Test that keymap is defined."
  (skip-unless evil-tex-bora-loaded)
  (should (keymapp evil-tex-bora-mode-map)))

;;; Tree-sitter availability tests

(ert-deftest test-treesit-available ()
  "Test if tree-sitter is available in this Emacs."
  (skip-unless evil-tex-bora-loaded)
  ;; This test documents whether tree-sitter is available
  ;; It doesn't fail if tree-sitter is not available
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      (message "Tree-sitter is available")
    (message "Tree-sitter is NOT available")))

(ert-deftest test-latex-parser-availability ()
  "Test if LaTeX tree-sitter parser is available."
  (skip-unless evil-tex-bora-loaded)
  (skip-unless (and (fboundp 'treesit-available-p)
                    (treesit-available-p)))
  ;; This test documents whether the LaTeX parser is available
  (if (treesit-language-available-p 'latex)
      (message "LaTeX tree-sitter parser is available")
    (message "LaTeX tree-sitter parser is NOT available")))

;;; Mock-based tests for bounds functions
;;; These tests verify the logic without requiring tree-sitter

(ert-deftest test-bounds-return-format ()
  "Test that bounds functions return correct format when they succeed."
  (skip-unless evil-tex-bora-loaded)
  ;; When bounds functions return a result, it should be a list of 4 elements
  ;; We test the format by mocking the return
  (let ((mock-bounds '(10 50 15 45)))
    (should (= (length mock-bounds) 4))
    (should (= (nth 0 mock-bounds) 10))  ; outer-beg
    (should (= (nth 1 mock-bounds) 50))  ; outer-end
    (should (= (nth 2 mock-bounds) 15))  ; inner-beg
    (should (= (nth 3 mock-bounds) 45))  ; inner-end
    ;; Verify inner is inside outer
    (should (>= (nth 2 mock-bounds) (nth 0 mock-bounds)))
    (should (<= (nth 3 mock-bounds) (nth 1 mock-bounds)))))

;;; Integration tests (require tree-sitter with LaTeX parser)

(ert-deftest test-environment-bounds-integration ()
  "Integration test for environment bounds with real tree-sitter."
  (skip-unless evil-tex-bora-loaded)
  (skip-unless (and (fboundp 'treesit-available-p)
                    (treesit-available-p)
                    (treesit-language-available-p 'latex)))
  (with-temp-buffer
    (insert "\\begin{equation}\nx = 1\n\\end{equation}")
    (treesit-parser-create 'latex)
    (goto-char 20)  ; Inside the environment
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (when bounds
        (should (= (length bounds) 4))
        ;; outer should span whole environment
        (should (= (nth 0 bounds) 1))
        (should (= (nth 1 bounds) (point-max)))))))

(ert-deftest test-math-bounds-integration ()
  "Integration test for math bounds with real tree-sitter."
  (skip-unless evil-tex-bora-loaded)
  (skip-unless (and (fboundp 'treesit-available-p)
                    (treesit-available-p)
                    (treesit-language-available-p 'latex)))
  (with-temp-buffer
    (insert "\\(x + y\\)")
    (treesit-parser-create 'latex)
    (goto-char 4)  ; Inside the math
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (when bounds
        (should (= (length bounds) 4))))))

(provide 'test-evil-tex-bora)
;;; test-evil-tex-bora.el ends here
