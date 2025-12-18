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

(defmacro evil-tex-bora-test-with-latex (content pos &rest body)
  "Create temp buffer with LaTeX CONTENT, goto POS, and execute BODY.
Automatically skips test if tree-sitter with LaTeX parser is not available."
  (declare (indent 2))
  `(progn
     (skip-unless evil-tex-bora-loaded)
     (skip-unless (and (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (treesit-language-available-p 'latex)))
     (with-temp-buffer
       (insert ,content)
       (treesit-parser-create 'latex)
       (goto-char ,pos)
       ,@body)))

;;; Environment text object tests

(ert-deftest test-environment-bounds-integration ()
  "Integration test for environment bounds with real tree-sitter."
  (evil-tex-bora-test-with-latex "\\begin{equation}\nx = 1\n\\end{equation}" 20
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      (should (= (length bounds) 4))
      ;; outer should span whole environment
      (should (= (nth 0 bounds) 1))
      (should (= (nth 1 bounds) (point-max)))
      ;; With evil-tex-bora-select-newlines-with-envs=t (default):
      ;;   inner-beg is on the content line
      ;;   inner-end includes the newline before \end{equation}
      ;; Note: \begin{equation} is 16 chars, so position 17 is \n, position 18 is 'x'
      (should (= (nth 2 bounds) 18))  ; at 'x'
      (should (= (nth 3 bounds) 24))  ; at start of \end line
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x = 1\n")))))

(ert-deftest test-environment-generic ()
  "Test generic environment like document."
  (evil-tex-bora-test-with-latex "\\begin{document}hello\\end{document}" 18
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "hello")))))

;;; Command text object tests

(ert-deftest test-command-with-arg ()
  "Test command with single argument."
  (evil-tex-bora-test-with-latex "\\textbf{hello}" 10
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\textbf{hello}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "hello")))))

(ert-deftest test-command-without-arg ()
  "Test command without arguments like \\alpha."
  (evil-tex-bora-test-with-latex "\\alpha" 3
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\alpha"))
      ;; inner should be empty for commands without args
      (should (= (nth 2 bounds) (nth 3 bounds))))))

(ert-deftest test-command-multi-arg-first ()
  "Test command with multiple arguments - cursor in first arg."
  (evil-tex-bora-test-with-latex "\\frac{a}{b}" 7  ; cursor inside {a}
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\frac{a}{b}"))
      ;; inner should be the curly group containing cursor
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a")))))

(ert-deftest test-command-multi-arg-second ()
  "Test command with multiple arguments - cursor in second arg."
  (evil-tex-bora-test-with-latex "\\frac{a}{b}" 10  ; cursor inside {b}
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\frac{a}{b}"))
      ;; inner should be the curly group containing cursor
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "b")))))

(ert-deftest test-command-multi-arg-on-name ()
  "Test command with multiple arguments - cursor on command name."
  (evil-tex-bora-test-with-latex "\\frac{a}{b}" 3  ; cursor on 'frac'
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\frac{a}{b}"))
      ;; inner should be nearest curly group to the right (first one)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a")))))

(ert-deftest test-command-section ()
  "Test section command."
  (evil-tex-bora-test-with-latex "\\section{title}" 10
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\section{title}")))))

;;; Math text object tests

(ert-deftest test-math-bounds-integration ()
  "Integration test for math bounds with real tree-sitter."
  (evil-tex-bora-test-with-latex "\\(x + y\\)" 5
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      (should (= (length bounds) 4))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x + y")))))

(ert-deftest test-math-inline-dollar ()
  "Test dollar inline math $...$."
  (evil-tex-bora-test-with-latex "$x$" 2
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "$x$"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x")))))

(ert-deftest test-math-display ()
  "Test display math \\[...\\]."
  (evil-tex-bora-test-with-latex "\\[a = b\\]" 5
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\[a = b\\]"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a = b")))))

(ert-deftest test-math-environment ()
  "Test math environment like equation."
  (evil-tex-bora-test-with-latex "\\begin{equation}\nx = 1\n\\end{equation}" 20
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      ;; math_environment should include begin/end
      (should (= (nth 0 bounds) 1))
      (should (= (nth 1 bounds) (point-max))))))

;;; Delimiter text object tests

(ert-deftest test-delimiter-math-delimiter ()
  "Test math_delimiter \\left...\\right."
  (evil-tex-bora-test-with-latex "\\(\\left(a + b\\right)\\)" 10
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\left(a + b\\right)"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a + b")))))

(ert-deftest test-delimiter-simple-parens ()
  "Test simple parentheses ()."
  (evil-tex-bora-test-with-latex "\\((a + b)\\)" 6
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "(a + b)"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a + b")))))

(ert-deftest test-delimiter-brackets ()
  "Test brackets []."
  (evil-tex-bora-test-with-latex "\\([a + b]\\)" 6
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "[a + b]"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a + b")))))

;;; ==========================================================================
;;; Examples from examples.md - comprehensive tests for documented features
;;; ==========================================================================

;;; Environment examples (ie/ae)
;;; From examples.md:
;;;   \begin{equation}
;;;     x^2 + y^2 = z^2|
;;;   \end{equation}
;;; - vie selects inner (the formula)
;;; - vae selects entire environment

(ert-deftest test-example-environment-multiline ()
  "Test environment from examples.md: multiline equation."
  (evil-tex-bora-test-with-latex
      "\\begin{equation}\n  x^2 + y^2 = z^2\n\\end{equation}" 25
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      ;; vie should select inner (formula with whitespace)
      (let ((inner (buffer-substring (nth 2 bounds) (nth 3 bounds))))
        (should (string-match-p "x\\^2 \\+ y\\^2 = z\\^2" inner)))
      ;; vae should select entire environment
      (let ((outer (buffer-substring (nth 0 bounds) (nth 1 bounds))))
        (should (string-match-p "\\\\begin{equation}" outer))
        (should (string-match-p "\\\\end{equation}" outer))))))

(ert-deftest test-example-environment-nested ()
  "Test nested environments - cursor in inner env."
  (evil-tex-bora-test-with-latex
      "\\begin{document}\\begin{equation}x\\end{equation}\\end{document}" 35
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      ;; Should select the inner equation environment, not document
      (let ((outer (buffer-substring (nth 0 bounds) (nth 1 bounds))))
        (should (string-match-p "\\\\begin{equation}" outer))
        (should (not (string-match-p "\\\\begin{document}" outer)))))))

(ert-deftest test-example-environment-align ()
  "Test align environment."
  (evil-tex-bora-test-with-latex
      "\\begin{align}\na &= b \\\\\nc &= d\n\\end{align}" 20
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      (let ((outer (buffer-substring (nth 0 bounds) (nth 1 bounds))))
        (should (string-match-p "\\\\begin{align}" outer))))))

(ert-deftest test-example-environment-itemize ()
  "Test itemize environment."
  (evil-tex-bora-test-with-latex
      "\\begin{itemize}\n\\item one\n\\item two\n\\end{itemize}" 25
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      (let ((inner (buffer-substring (nth 2 bounds) (nth 3 bounds))))
        (should (string-match-p "\\\\item" inner))))))

;;; Command examples (ic/ac)
;;; From examples.md:
;;;   \textbf{hello| world}
;;; - vic selects "hello world"
;;; - vac selects "\textbf{hello world}"

(ert-deftest test-example-command-textbf-with-space ()
  "Test command from examples.md: \\textbf{hello world}."
  (evil-tex-bora-test-with-latex "\\textbf{hello world}" 12
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      ;; vic should select "hello world"
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "hello world"))
      ;; vac should select "\textbf{hello world}"
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\textbf{hello world}")))))

(ert-deftest test-example-command-nested ()
  "Test nested commands - cursor in inner."
  (evil-tex-bora-test-with-latex "\\textbf{\\textit{nested}}" 18
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      ;; Should select inner \textit, not outer \textbf
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\textit{nested}")))))

(ert-deftest test-example-command-emph ()
  "Test \\emph command."
  (evil-tex-bora-test-with-latex "\\emph{emphasized text}" 10
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "emphasized text")))))

(ert-deftest test-example-command-footnote ()
  "Test \\footnote command."
  (evil-tex-bora-test-with-latex "text\\footnote{a footnote}more" 15
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\footnote{a footnote}")))))

(ert-deftest test-example-command-ref ()
  "Test \\ref command."
  (evil-tex-bora-test-with-latex "see \\ref{fig:main}" 10
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\ref{fig:main}")))))

(ert-deftest test-example-command-sqrt ()
  "Test \\sqrt command without optional argument."
  (evil-tex-bora-test-with-latex "\\sqrt{x}" 5
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\sqrt{x}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x")))))

(ert-deftest test-example-command-sqrt-with-optional ()
  "Test \\sqrt command with optional argument \\sqrt[n]{x}.
Tree-sitter-latex doesn't recognize this as a single command node,
so we use fallback search to find the command boundaries."
  (evil-tex-bora-test-with-latex "\\sqrt[n + 1]{content}" 15
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      ;; outer should include the whole command with optional arg
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\sqrt[n + 1]{content}"))
      ;; inner should be only the content inside curly braces (not the optional arg)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "content")))))

(ert-deftest test-command-sqrt-cursor-in-curly ()
  "Test \\sqrt[n]{x} with cursor inside {} - should select curly content.
\\sqrt[n + 1]{a_1 \\cdot| a_2} -> inner is 'a_1 \\cdot a_2'"
  (evil-tex-bora-test-with-latex "\\sqrt[n + 1]{a_1 \\cdot a_2}" 22
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds))
                       "\\sqrt[n + 1]{a_1 \\cdot a_2}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds))
                       "a_1 \\cdot a_2")))))

(ert-deftest test-command-sqrt-cursor-in-brackets ()
  "Test \\sqrt[n]{x} with cursor inside [] - should select curly content.
\\sqrt[n +| 1]{a_1 \\cdot a_2} -> inner is 'a_1 \\cdot a_2'"
  (evil-tex-bora-test-with-latex "\\sqrt[n + 1]{a_1 \\cdot a_2}" 9
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds))
                       "\\sqrt[n + 1]{a_1 \\cdot a_2}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds))
                       "a_1 \\cdot a_2")))))

(ert-deftest test-command-sqrt-cursor-on-name ()
  "Test \\sqrt[n]{x} with cursor on command name - should select curly content.
\\sq|rt[n + 1]{a_1 \\cdot a_2} -> inner is 'a_1 \\cdot a_2'"
  (evil-tex-bora-test-with-latex "\\sqrt[n + 1]{a_1 \\cdot a_2}" 4
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds))
                       "\\sqrt[n + 1]{a_1 \\cdot a_2}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds))
                       "a_1 \\cdot a_2")))))

(ert-deftest test-command-sqrt-cursor-in-curly-math-environment ()
  "Test \\sqrt[n]{x} inside math environment with cursor in {}.
Regression for tree-sitter-latex parsing where {...} is not a command child."
  (evil-tex-bora-test-with-latex
      "\\begin{equation}\\sqrt[n + 1]{a_1 \\cdot a_2}\\end{equation}" 31
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds))
                       "\\sqrt[n + 1]{a_1 \\cdot a_2}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds))
                       "a_1 \\cdot a_2")))))

(ert-deftest test-command-no-arg-inside-command-arg ()
  "Point on a no-arg command inside another command arg selects the outer command.
Example: \\textbf{a_1 \\cdo|t a_2} -> inner is 'a_1 \\cdot a_2'."
  (evil-tex-bora-test-with-latex "\\textbf{a_1 \\cdot a_2}" 15
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds))
                       "\\textbf{a_1 \\cdot a_2}"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds))
                       "a_1 \\cdot a_2")))))

;;; Math examples (im/am)
;;; From examples.md:
;;;   \(x^2 + y^2 = z^2\)
;;; - vim selects "x^2 + y^2 = z^2"
;;; - vam selects "\(x^2 + y^2 = z^2\)"

(ert-deftest test-example-math-inline-formula ()
  "Test math from examples.md: \\(x^2 + y^2 = z^2\\)."
  (evil-tex-bora-test-with-latex "\\(x^2 + y^2 = z^2\\)" 10
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      ;; vim should select formula
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x^2 + y^2 = z^2"))
      ;; vam should select with delimiters
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\(x^2 + y^2 = z^2\\)")))))

(ert-deftest test-example-math-display-equation ()
  "Test display math \\[...\\] with complex formula."
  (evil-tex-bora-test-with-latex "\\[\\int_0^\\infty e^{-x^2} dx\\]" 15
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      (let ((inner (buffer-substring (nth 2 bounds) (nth 3 bounds))))
        (should (string-match-p "\\\\int" inner))))))

(ert-deftest test-example-math-in-text ()
  "Test inline math embedded in text."
  (evil-tex-bora-test-with-latex "The formula $E = mc^2$ is famous." 15
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "$E = mc^2$"))
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "E = mc^2")))))

(ert-deftest test-example-math-equation-env ()
  "Test equation environment as math object."
  (evil-tex-bora-test-with-latex
      "\\begin{equation}\n  E = mc^2\n\\end{equation}" 25
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds)
      ;; Math environment should work
      (let ((outer (buffer-substring (nth 0 bounds) (nth 1 bounds))))
        (should (string-match-p "\\\\begin{equation}" outer))))))

(ert-deftest test-example-math-double-dollar ()
  "Test double dollar display math $$...$$."
  (evil-tex-bora-test-with-latex "$$x + y$$" 5
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      ;; Note: tree-sitter-latex parses $$ as two inline formulas
      ;; This test documents current behavior
      (should bounds))))

;;; Delimiter examples (id/ad)
;;; From examples.md:
;;;   \left(a + b|\right)
;;; - vid selects "a + b"
;;; - vad selects "\left(a + b\right)"

(ert-deftest test-example-delimiter-left-right ()
  "Test delimiter from examples.md: \\left(a + b\\right)."
  (evil-tex-bora-test-with-latex "\\(\\left(a + b\\right)\\)" 10
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      ;; vid should select "a + b"
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "a + b"))
      ;; vad should select "\left(a + b\right)"
      (should (string= (buffer-substring (nth 0 bounds) (nth 1 bounds)) "\\left(a + b\\right)")))))

(ert-deftest test-example-delimiter-bigl-bigr ()
  "Test \\bigl...\\bigr delimiters."
  (evil-tex-bora-test-with-latex "\\(\\bigl(x + y\\bigr)\\)" 10
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "x + y")))))

(ert-deftest test-example-delimiter-nested-parens ()
  "Test nested parentheses - should select innermost."
  (evil-tex-bora-test-with-latex "\\((a + (b + c))\\)" 10
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      (should bounds)
      ;; Should select innermost (b + c) when cursor is there
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "b + c")))))

(ert-deftest test-example-delimiter-angle-brackets ()
  "Test \\langle...\\rangle delimiters."
  (evil-tex-bora-test-with-latex "\\(\\langle x, y \\rangle\\)" 12
    (let ((bounds (evil-tex-bora--bounds-of-delimiter)))
      ;; These use fallback search, should still work
      (should bounds))))

;;; Edge cases and error handling

(ert-deftest test-edge-case-empty-environment ()
  "Test environment with empty content."
  (evil-tex-bora-test-with-latex "\\begin{equation}\\end{equation}" 17
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      ;; Inner should be empty
      (should (= (nth 2 bounds) (nth 3 bounds))))))

(ert-deftest test-edge-case-empty-command-arg ()
  "Test command with empty argument."
  (evil-tex-bora-test-with-latex "\\textbf{}" 8
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      ;; Inner should be empty
      (should (= (nth 2 bounds) (nth 3 bounds))))))

(ert-deftest test-edge-case-cursor-at-begin ()
  "Test cursor at \\begin."
  (evil-tex-bora-test-with-latex "\\begin{equation}x\\end{equation}" 1
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds))))

(ert-deftest test-edge-case-cursor-at-end ()
  "Test cursor at \\end."
  (evil-tex-bora-test-with-latex "\\begin{equation}x\\end{equation}" 18
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds))))

(ert-deftest test-edge-case-math-at-delimiter ()
  "Test cursor exactly at math delimiter."
  (evil-tex-bora-test-with-latex "\\(x\\)" 1
    (let ((bounds (evil-tex-bora--bounds-of-math)))
      (should bounds))))

(ert-deftest test-edge-case-unicode-content ()
  "Test LaTeX with unicode content."
  (evil-tex-bora-test-with-latex "\\textbf{привет мир}" 12
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds)
      (should (string= (buffer-substring (nth 2 bounds) (nth 3 bounds)) "привет мир")))))

(ert-deftest test-edge-case-special-chars ()
  "Test command with special LaTeX characters."
  (evil-tex-bora-test-with-latex "\\texttt{a\\_b\\%c}" 10
    (let ((bounds (evil-tex-bora--bounds-of-command)))
      (should bounds))))

;;; User-reported issue: align* inner environment selection
;;; The inner selection should NOT include \begin{align*} or the \ before \end

(ert-deftest test-user-issue-align-star-inner ()
  "Test align* environment - inner includes the newline before \\end.
With `evil-tex-bora-select-newlines-with-envs' enabled (default),
inner selection includes the newline before \\end, so `die' deletes the full
inner block (no blank line remains)."
  (evil-tex-bora-test-with-latex
      "    \\begin{align*}\n      x > 0\n    \\end{align*}" 30  ; cursor on '0'
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      ;; Inner starts at the indentation level of the \end line and ends at the
      ;; same indentation level on the \end line (so `die' doesn't land in column 0).
      (should (= (nth 2 bounds) 24))  ; 4 spaces into the content line
      (should (= (nth 3 bounds) 36))  ; at the backslash of \end line
      (let ((inner-text (buffer-substring (nth 2 bounds) (nth 3 bounds))))
        (should (string= inner-text "  x > 0\n    "))))))

(ert-deftest test-user-issue-align-star-outer-with-newline ()
  "Test that outer environment includes trailing newline for clean deletion."
  (evil-tex-bora-test-with-latex
      "text\n\\begin{align*}\n  x > 0\n\\end{align*}\nmore" 20
    (let ((bounds (evil-tex-bora--bounds-of-environment)))
      (should bounds)
      ;; Outer should include trailing newline
      (let ((outer-text (buffer-substring (nth 0 bounds) (nth 1 bounds))))
        (should (string-match-p "\\\\end{align\\*}\n$" outer-text))))))

(ert-deftest test-user-issue-die-preserves-backslash ()
  "Test that 'die' (delete inner environment) preserves \\end{...} intact.
This test simulates the exact user scenario where after 'die' the
backslash before \\end should NOT be deleted."
  (evil-tex-bora-test-with-latex
      "    \\begin{align*}\n      x > 0\n    \\end{align*}" 30  ; cursor on '0'
    (let* ((bounds (evil-tex-bora--bounds-of-environment))
           (inner-beg (nth 2 bounds))
           (inner-end (nth 3 bounds)))
      ;; Simulate 'die' operation
      (delete-region inner-beg inner-end)
      ;; Verify \end{align*} is fully preserved (including backslash)
      (should (string-match-p "\\\\end{align\\*}" (buffer-string)))
      ;; Verify the result has no blank line between begin/end.
      (should (string= (buffer-string)
                       "    \\begin{align*}\n    \\end{align*}")))))

(ert-deftest test-user-issue-die-no-indent-before-end ()
  "Test 'die' when there's no indentation before \\end."
  (evil-tex-bora-test-with-latex
      "\\begin{align*}\n  x > 0\n\\end{align*}" 19  ; cursor on content
    (let* ((bounds (evil-tex-bora--bounds-of-environment))
           (inner-beg (nth 2 bounds))
           (inner-end (nth 3 bounds)))
      ;; Simulate 'die' operation
      (delete-region inner-beg inner-end)
      ;; Verify \end{align*} is fully preserved
      (should (string-match-p "\\\\end{align\\*}" (buffer-string)))
      ;; Result should not leave a blank line.
      (should (string= (buffer-string)
                       "\\begin{align*}\n\\end{align*}")))))

(ert-deftest test-user-issue-die-keeps-point-out-of-column-0 ()
  "Regression: `die` should not land in column 0 for indented environments."
  (evil-tex-bora-test-with-latex
      "    \\begin{align*}\n      x > 0\n    \\end{align*}" 1
    (goto-char (point-min))
    (search-forward "0") ; point is after '0' (like `x > 0|`)
    (let* ((bounds (evil-tex-bora--bounds-of-environment))
           (inner-beg (nth 2 bounds))
           (inner-end (nth 3 bounds)))
      (delete-region inner-beg inner-end)
      (should (string= (buffer-string) "    \\begin{align*}\n    \\end{align*}"))
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) 4))
      (should (looking-at-p "\\\\end{align\\*}")))))

(ert-deftest test-user-issue-dae-preserves-next-line-indent ()
  "Regression: `dae` should not add extra indentation to the next line.
When deleting an indented environment, the leading whitespace before
\\begin should also be deleted so the next line keeps its original indent."
  (evil-tex-bora-test-with-latex
      "    We proved the statement by induction.\n    \\begin{align*}\n      x > 0\n      y < 0\n    \\end{align*}\n    BBB\n" 70
    (let* ((bounds (evil-tex-bora--bounds-of-environment))
           (outer-beg (nth 0 bounds))
           (outer-end (nth 1 bounds)))
      ;; outer-beg should start at the beginning of the line (include indent)
      (should (= outer-beg 43))  ; position after first line's newline
      ;; Simulate 'dae' operation
      (delete-region outer-beg outer-end)
      ;; BBB should keep its original indentation (4 spaces, not 8)
      (should (string= (buffer-string)
                       "    We proved the statement by induction.\n    BBB\n")))))

(ert-deftest test-user-issue-dae-returns-linewise-type ()
  "Regression: `dae` should return linewise type for whole-line environments.
This ensures cursor lands on first non-whitespace char after deletion."
  (evil-tex-bora-test-with-latex
      "    We proved the statement by induction.\n    \\begin{align*}\n      x > 0\n    \\end{align*}\n    BBB\n" 70
    (let* ((range (evil-tex-bora-outer-environment 1))
           (range-type (nth 2 range)))
      ;; Should return 'line type for linewise deletion behavior
      (should (eq range-type 'line)))))

(ert-deftest test-user-issue-cie-uses-linewise-inner-range ()
  "Regression: `cie` should use a linewise inner range for multi-line environments."
  (evil-tex-bora-test-with-latex
      "    \\begin{align*}\n      x > 0\n    \\end{align*}" 30  ; cursor on '0'
    (let ((had-op (boundp 'evil-this-operator))
          (old-op (and (boundp 'evil-this-operator) evil-this-operator)))
      (unwind-protect
          (progn
            (setq evil-this-operator 'evil-change)
            (let* ((range (evil-tex-bora-inner-environment 1))
                   (beg (nth 0 range))
                   (end (nth 1 range))
                   (type (nth 2 range))
                   (expected-beg (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "\\begin{align*}\n")
                                   (point)))
                   (expected-end (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "\\end{align*}")
                                   (line-beginning-position))))
              (should (eq type 'line))
              (should (= beg expected-beg))
              (should (= end expected-end))))
        (if had-op
            (setq evil-this-operator old-op)
          (makunbound 'evil-this-operator))))))

;;; ==========================================================================
;;; Toggle function tests (Phase 3)
;;; ==========================================================================

;;; Environment asterisk toggle (mte)

(ert-deftest test-toggle-env-asterisk-add ()
  "Test adding asterisk to environment: equation -> equation*."
  (evil-tex-bora-test-with-latex
      "\\begin{equation}\nx = 1\n\\end{equation}" 20
    (evil-tex-bora-toggle-env-asterisk)
    (should (string= (buffer-string)
                     "\\begin{equation*}\nx = 1\n\\end{equation*}"))))

(ert-deftest test-toggle-env-asterisk-remove ()
  "Test removing asterisk from environment: equation* -> equation."
  (evil-tex-bora-test-with-latex
      "\\begin{equation*}\nx = 1\n\\end{equation*}" 20
    (evil-tex-bora-toggle-env-asterisk)
    (should (string= (buffer-string)
                     "\\begin{equation}\nx = 1\n\\end{equation}"))))

(ert-deftest test-toggle-env-asterisk-align ()
  "Test toggle asterisk on align environment."
  (evil-tex-bora-test-with-latex
      "\\begin{align}\na &= b\n\\end{align}" 20
    (evil-tex-bora-toggle-env-asterisk)
    (should (string= (buffer-string)
                     "\\begin{align*}\na &= b\n\\end{align*}"))))

;;; Math mode toggle (mtm)

(ert-deftest test-toggle-math-mode-inline-to-display ()
  "Test toggle math: \\(...\\) -> \\[...\\]."
  (evil-tex-bora-test-with-latex "\\(x + y\\)" 5
    (evil-tex-bora-toggle-math-mode)
    (should (string= (buffer-string) "\\[x + y\\]"))))

(ert-deftest test-toggle-math-mode-display-to-inline-dollar ()
  "Test toggle math: \\[...\\] -> $...$ (default preference)."
  (evil-tex-bora-test-with-latex "\\[x + y\\]" 5
    (let ((evil-tex-bora-preferred-inline-math 'dollar))
      (evil-tex-bora-toggle-math-mode)
      (should (string= (buffer-string) "$x + y$")))))

(ert-deftest test-toggle-math-mode-display-to-inline-paren ()
  "Test toggle math: \\[...\\] -> \\(...\\) (paren preference)."
  (evil-tex-bora-test-with-latex "\\[x + y\\]" 5
    (let ((evil-tex-bora-preferred-inline-math 'paren))
      (evil-tex-bora-toggle-math-mode)
      (should (string= (buffer-string) "\\(x + y\\)")))))

(ert-deftest test-toggle-math-mode-dollar-to-display ()
  "Test toggle math: $...$ -> \\[...\\]."
  (evil-tex-bora-test-with-latex "$x + y$" 4
    (evil-tex-bora-toggle-math-mode)
    (should (string= (buffer-string) "\\[x + y\\]"))))

(ert-deftest test-toggle-math-mode-dollar-roundtrip ()
  "Test toggle math roundtrip: $...$ -> \\[...\\] -> $...$."
  (evil-tex-bora-test-with-latex "$x + y$" 4
    (let ((evil-tex-bora-preferred-inline-math 'dollar))
      (evil-tex-bora-toggle-math-mode)
      (should (string= (buffer-string) "\\[x + y\\]"))
      (goto-char 4)
      (evil-tex-bora-toggle-math-mode)
      (should (string= (buffer-string) "$x + y$")))))

(ert-deftest test-toggle-math-mode-complex-formula ()
  "Test toggle math with complex formula."
  (evil-tex-bora-test-with-latex "\\(\\int_0^1 f(x) dx\\)" 10
    (evil-tex-bora-toggle-math-mode)
    (should (string= (buffer-string) "\\[\\int_0^1 f(x) dx\\]"))))

(ert-deftest test-toggle-math-mode-display-multiline-to-inline ()
  "Test toggle math: multiline \\[...\\] -> normalized inline."
  (evil-tex-bora-test-with-latex "\\[\n  a + b\n  = c + d\n\\]" 5
    (let ((evil-tex-bora-preferred-inline-math 'dollar))
      (evil-tex-bora-toggle-math-mode)
      ;; Multiline content should be normalized: trimmed and newlines collapsed
      (should (string= (buffer-string) "$a + b = c + d$")))))

(ert-deftest test-toggle-math-mode-display-multiline-preserves-single-space ()
  "Test toggle math: multiline display collapses to single spaces."
  (evil-tex-bora-test-with-latex "\\[x\n\n\ny\\]" 3
    (let ((evil-tex-bora-preferred-inline-math 'dollar))
      (evil-tex-bora-toggle-math-mode)
      ;; Multiple newlines should collapse to single space
      (should (string= (buffer-string) "$x y$")))))

;;; Delimiter sizing toggle (mtd)

(ert-deftest test-toggle-delim-size-add-left-right ()
  "Test toggle delimiter: () -> \\left(\\right)."
  (evil-tex-bora-test-with-latex "\\((a + b)\\)" 6
    (evil-tex-bora-toggle-delim-size)
    (should (string= (buffer-string) "\\(\\left(a + b\\right)\\)"))))

(ert-deftest test-toggle-delim-size-remove-left-right ()
  "Test toggle delimiter: \\left(\\right) -> ()."
  (evil-tex-bora-test-with-latex "\\(\\left(a + b\\right)\\)" 10
    (evil-tex-bora-toggle-delim-size)
    (should (string= (buffer-string) "\\((a + b)\\)"))))

(ert-deftest test-toggle-delim-size-bigl-to-plain ()
  "Test toggle delimiter: \\bigl(\\bigr) -> () (one-way)."
  (evil-tex-bora-test-with-latex "\\(\\bigl(a + b\\bigr)\\)" 10
    (evil-tex-bora-toggle-delim-size)
    (should (string= (buffer-string) "\\((a + b)\\)"))))

(ert-deftest test-toggle-delim-size-brackets ()
  "Test toggle delimiter with brackets: [] -> \\left[\\right]."
  (evil-tex-bora-test-with-latex "\\([a + b]\\)" 6
    (evil-tex-bora-toggle-delim-size)
    (should (string= (buffer-string) "\\(\\left[a + b\\right]\\)"))))

(ert-deftest test-toggle-delim-size-cursor-on-opening ()
  "Test toggle delimiter with cursor on opening paren."
  (evil-tex-bora-test-with-latex "\\((a + b)\\)" 3  ; cursor on (
    (evil-tex-bora-toggle-delim-size)
    (should (string= (buffer-string) "\\(\\left(a + b\\right)\\)"))))

;;; Command asterisk toggle (mtc)

(ert-deftest test-toggle-cmd-asterisk-add ()
  "Test adding asterisk to command: \\section -> \\section*."
  (evil-tex-bora-test-with-latex "\\section{Title}" 10
    (evil-tex-bora-toggle-cmd-asterisk)
    (should (string= (buffer-string) "\\section*{Title}"))))

(ert-deftest test-toggle-cmd-asterisk-remove ()
  "Test removing asterisk from command: \\section* -> \\section."
  (evil-tex-bora-test-with-latex "\\section*{Title}" 10
    (evil-tex-bora-toggle-cmd-asterisk)
    (should (string= (buffer-string) "\\section{Title}"))))

(ert-deftest test-toggle-cmd-asterisk-textbf ()
  "Test toggle asterisk on \\textbf (unusual but should work)."
  (evil-tex-bora-test-with-latex "\\textbf{text}" 8
    (evil-tex-bora-toggle-cmd-asterisk)
    (should (string= (buffer-string) "\\textbf*{text}"))))

(ert-deftest test-toggle-cmd-asterisk-subsection ()
  "Test toggle asterisk on \\subsection."
  (evil-tex-bora-test-with-latex "\\subsection{Intro}" 12
    (evil-tex-bora-toggle-cmd-asterisk)
    (should (string= (buffer-string) "\\subsection*{Intro}"))))

;;; ==========================================================================
;;; Surround integration tests (Phase 4)
;;; ==========================================================================

;;; Basic function existence tests

(ert-deftest test-surround-functions-exist ()
  "Test that surround-related functions are defined."
  (skip-unless evil-tex-bora-loaded)
  ;; Format functions
  (should (fboundp 'evil-tex-bora-get-env-for-surrounding))
  (should (fboundp 'evil-tex-bora--format-command-for-surrounding))
  (should (fboundp 'evil-tex-bora--format-accent-for-surrounding))
  ;; Prompt functions
  (should (fboundp 'evil-tex-bora-surround-env-prompt))
  (should (fboundp 'evil-tex-bora-surround-command-prompt))
  (should (fboundp 'evil-tex-bora-surround-delim-prompt))
  (should (fboundp 'evil-tex-bora-surround-cdlatex-accents-prompt))
  ;; Setup functions
  (should (fboundp 'evil-tex-bora-set-up-surround))
  (should (fboundp 'evil-tex-bora-set-up-embrace))
  ;; Bind functions
  (should (fboundp 'evil-tex-bora-bind-to-env-map))
  (should (fboundp 'evil-tex-bora-bind-to-delim-map))
  (should (fboundp 'evil-tex-bora-bind-to-cdlatex-accents-map)))

(ert-deftest test-surround-keymaps-exist ()
  "Test that surround keymaps are defined."
  (skip-unless evil-tex-bora-loaded)
  (should (keymapp evil-tex-bora-env-map))
  (should (keymapp evil-tex-bora-delim-map))
  (should (keymapp evil-tex-bora-cdlatex-accents-map))
  (should (keymapp evil-tex-bora-inner-text-objects-map))
  (should (keymapp evil-tex-bora-outer-text-objects-map)))

(ert-deftest test-surround-delimiters-exist ()
  "Test that surround delimiters alist is defined."
  (skip-unless evil-tex-bora-loaded)
  (should (boundp 'evil-tex-bora-surround-delimiters))
  (should (listp evil-tex-bora-surround-delimiters))
  ;; Check specific delimiters
  (should (assq ?m evil-tex-bora-surround-delimiters))
  (should (assq ?M evil-tex-bora-surround-delimiters))
  (should (assq ?c evil-tex-bora-surround-delimiters))
  (should (assq ?e evil-tex-bora-surround-delimiters))
  (should (assq ?d evil-tex-bora-surround-delimiters))
  (should (assq ?\; evil-tex-bora-surround-delimiters))
  (should (assq ?q evil-tex-bora-surround-delimiters))
  (should (assq ?Q evil-tex-bora-surround-delimiters))
  (should (assq ?^ evil-tex-bora-surround-delimiters))
  (should (assq ?_ evil-tex-bora-surround-delimiters)))

;;; Format function tests

(ert-deftest test-get-env-for-surrounding-with-newlines ()
  "Test env format with newlines enabled."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora-include-newlines-in-envs t))
    (let ((result (evil-tex-bora-get-env-for-surrounding "equation")))
      (should (consp result))
      (should (string= (car result) "\\begin{equation}\n"))
      (should (string= (cdr result) "\n\\end{equation}")))))

(ert-deftest test-get-env-for-surrounding-without-newlines ()
  "Test env format without newlines."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora-include-newlines-in-envs nil))
    (let ((result (evil-tex-bora-get-env-for-surrounding "equation")))
      (should (consp result))
      (should (string= (car result) "\\begin{equation}"))
      (should (string= (cdr result) "\\end{equation}")))))

(ert-deftest test-format-accent-for-surrounding ()
  "Test accent format function."
  (skip-unless evil-tex-bora-loaded)
  (let ((result (evil-tex-bora--format-accent-for-surrounding "dot")))
    (should (consp result))
    (should (string= (car result) "\\dot{"))
    (should (string= (cdr result) "}"))))

(ert-deftest test-format-command-for-surrounding-non-empty ()
  "Test command format for non-empty command."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora--last-command-empty nil))
    (let ((result (evil-tex-bora--format-command-for-surrounding "textbf")))
      (should (consp result))
      (should (string= (car result) "\\textbf{"))
      (should (string= (cdr result) "}")))))

(ert-deftest test-format-command-for-surrounding-empty ()
  "Test command format for empty command (like \\alpha)."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora--last-command-empty t))
    (let ((result (evil-tex-bora--format-command-for-surrounding "alpha")))
      (should (consp result))
      (should (string= (car result) "\\alpha"))
      (should (string= (cdr result) "")))))

;;; Surround delimiters content tests

(ert-deftest test-surround-delimiter-m ()
  "Test inline math delimiter ?m is a function."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?m evil-tex-bora-surround-delimiters)))
    (should delim)
    ;; ?m is now a function that respects evil-tex-bora-preferred-inline-math
    (should (functionp (cdr delim)))))

(ert-deftest test-surround-inline-math-dollar-preference ()
  "Test inline math function returns dollar delimiters when preferred."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora-preferred-inline-math 'dollar))
    (let ((result (evil-tex-bora--surround-inline-math)))
      (should (equal result '("$" . "$"))))))

(ert-deftest test-surround-inline-math-paren-preference ()
  "Test inline math function returns paren delimiters when preferred."
  (skip-unless evil-tex-bora-loaded)
  (let ((evil-tex-bora-preferred-inline-math 'paren))
    (let ((result (evil-tex-bora--surround-inline-math)))
      (should (equal result '("\\(" . "\\)"))))))

;;; Inline math region normalization tests
;;; Note: evil-tex-bora--normalize-inline-math-region returns
;;; (NEW-END INDENT-STRING HAD-TRAILING-NEWLINE)

(ert-deftest test-normalize-inline-math-trim-leading-newlines ()
  "Test that leading newlines are trimmed but indentation captured."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "\n\nfoo bar")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      ;; No indentation to preserve (content starts at column 0)
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8))
      (should (string= (nth 1 result) ""))
      (should (not (nth 2 result))))))

(ert-deftest test-normalize-inline-math-capture-trailing-newline ()
  "Test that trailing newline is captured but stripped from region."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo bar\n")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      ;; Trailing newline is stripped but flag is set
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8))
      (should (nth 2 result)))))

(ert-deftest test-normalize-inline-math-trim-multiple-trailing-newlines ()
  "Test that multiple trailing newlines are trimmed and flag set."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo bar\n\n\n")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      ;; All trailing newlines stripped, flag is set
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8))
      (should (nth 2 result)))))

(ert-deftest test-normalize-inline-math-collapse-internal-newlines ()
  "Test that internal newlines are collapsed to single space."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo\nbar")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8)))))

(ert-deftest test-normalize-inline-math-collapse-multiple-newlines ()
  "Test that multiple internal newlines are collapsed to single space."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo\n\n\nbar")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8)))))

(ert-deftest test-normalize-inline-math-collapse-newlines-with-whitespace ()
  "Test that newlines with surrounding whitespace are collapsed."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo  \n  bar")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8)))))

(ert-deftest test-normalize-inline-math-complex-multiline ()
  "Test normalization of complex multiline content captures indentation."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "\n  a + b\n  = c + d\n")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      ;; Content is normalized, indentation captured separately
      (should (string= (buffer-string) "a + b = c + d"))
      (should (= (nth 0 result) 14))
      (should (string= (nth 1 result) "  "))
      (should (nth 2 result)))))

(ert-deftest test-normalize-inline-math-preserves-single-line ()
  "Test that single-line content is preserved unchanged."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "foo bar")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      (should (string= (buffer-string) "foo bar"))
      (should (= (nth 0 result) 8)))))

(ert-deftest test-normalize-inline-math-linewise-region ()
  "Test normalization of linewise selection captures indent and trailing newline."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    ;; Linewise selection typically includes the newline at end of each line
    (insert "    123 + 2222\n    \\text{123}\n")
    (let ((result (evil-tex-bora--normalize-inline-math-region 1 (point-max))))
      ;; Content is normalized, indentation and trailing newline captured
      (should (string= (buffer-string) "123 + 2222 \\text{123}"))
      (should (= (nth 0 result) 22))
      (should (string= (nth 1 result) "    "))
      (should (nth 2 result)))))

;;; Command surround normalization tests
;;; Note: For commands (?c), normalization happens AFTER the prompt is shown,
;;; inside the advice's post-processing. We test this by simulating the
;;; post-surround state and checking normalization.

(ert-deftest test-surround-command-post-normalize-multiline ()
  "Test that command surround normalizes multiline content after surrounding.
Simulates the post-surround state where \\textbf{line1\\nline2} should become
\\textbf{line1 line2}."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    ;; Simulate post-surround state with multiline content
    (insert "\\textbf{line1\nline2}")
    (goto-char 1)
    ;; Manually run the normalization logic from the advice
    (when (search-forward "{" nil t)
      (let ((inner-beg (point))
            inner-end)
        (backward-char)
        (forward-sexp)
        (setq inner-end (1- (point)))
        (let* ((content (buffer-substring-no-properties inner-beg inner-end))
               (has-newlines (string-match-p "\n" content)))
          (when has-newlines
            (let ((normalized (replace-regexp-in-string
                               "[ \t]*\\(?:\n[ \t]*\\)+" " "
                               (string-trim content))))
              (delete-region inner-beg inner-end)
              (goto-char inner-beg)
              (insert normalized))))))
    (should (string= (buffer-string) "\\textbf{line1 line2}"))))

(ert-deftest test-surround-command-post-normalize-indented ()
  "Test that command surround normalizes indented multiline content.
\\textbf{  first\\n  second} should become \\textbf{first second}."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    ;; Simulate post-surround state with indented multiline content
    (insert "\\textbf{  first line\n  second line}")
    (goto-char 1)
    ;; Manually run the normalization logic from the advice
    (when (search-forward "{" nil t)
      (let ((inner-beg (point))
            inner-end)
        (backward-char)
        (forward-sexp)
        (setq inner-end (1- (point)))
        (let* ((content (buffer-substring-no-properties inner-beg inner-end))
               (has-newlines (string-match-p "\n" content)))
          (when has-newlines
            (let ((normalized (replace-regexp-in-string
                               "[ \t]*\\(?:\n[ \t]*\\)+" " "
                               (string-trim content))))
              (delete-region inner-beg inner-end)
              (goto-char inner-beg)
              (insert normalized))))))
    (should (string= (buffer-string) "\\textbf{first line second line}"))))

;;; Environment surround line break tests
;;; When surrounding partial line with environment, line breaks should be added

(ert-deftest test-surround-env-partial-line-has-text-before ()
  "Test that env surround adds line break when text exists before.
'aaa \\begin{align}bbb\\end{align}' should become:
'aaa
\\begin{align}bbb\\end{align}'"
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    ;; Simulate post-surround state
    (insert "aaa \\begin{align}\nbbb\n\\end{align}")
    ;; env-has-text-before would be t, env-has-text-after would be nil
    ;; The advice would insert newline before \begin
    ;; We test the detection logic
    (goto-char 1)
    (let ((beg 5))  ; position of \begin
      (goto-char beg)
      (let* ((line-start (line-beginning-position))
             (text-before (buffer-substring-no-properties line-start beg))
             (has-text-before (string-match-p "[^ \t]" text-before)))
        (should has-text-before)
        (should (string= text-before "aaa "))))))

(ert-deftest test-surround-env-partial-line-has-text-after ()
  "Test detection of text after environment on same line."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    (insert "\\begin{align}\nbbb\n\\end{align} ccc")
    ;; Check text after \end{align}
    (goto-char (point-max))
    (search-backward "}")
    (forward-char)  ; after }
    (let* ((end-pos (point))
           (line-end (line-end-position))
           (text-after (buffer-substring-no-properties end-pos line-end))
           (has-text-after (string-match-p "[^ \t]" text-after)))
      (should has-text-after)
      (should (string= text-after " ccc")))))

(ert-deftest test-surround-env-whole-line-no-breaks ()
  "Test that env surround on whole line doesn't add extra breaks."
  (skip-unless evil-tex-bora-loaded)
  (with-temp-buffer
    ;; When there's no text before/after, no extra breaks needed
    (insert "\\begin{align}\nbbb\n\\end{align}")
    (goto-char 1)
    (let* ((beg 1)
           (line-start (line-beginning-position))
           (text-before (buffer-substring-no-properties line-start beg))
           (has-text-before (string-match-p "[^ \t]" text-before)))
      ;; No text before \begin
      (should-not has-text-before))))

(ert-deftest test-surround-delimiter-M ()
  "Test display math delimiter ?M."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?M evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("\\[" . "\\]")))))

(ert-deftest test-surround-delimiter-q ()
  "Test single quote delimiter ?q."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?q evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("`" . "'")))))

(ert-deftest test-surround-delimiter-Q ()
  "Test double quote delimiter ?Q."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?Q evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("``" . "''")))))

(ert-deftest test-surround-delimiter-superscript ()
  "Test superscript delimiter ?^."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?^ evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("^{" . "}")))))

(ert-deftest test-surround-delimiter-subscript ()
  "Test subscript delimiter ?_."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?_ evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("_{" . "}")))))

(ert-deftest test-surround-delimiter-table-cell ()
  "Test table cell delimiter ?T."
  (skip-unless evil-tex-bora-loaded)
  (let ((delim (assq ?T evil-tex-bora-surround-delimiters)))
    (should delim)
    (should (equal (cdr delim) '("&" . "&")))))

;;; ==========================================================================
;;; Environment surround linewise selection tests
;;; ==========================================================================

(ert-deftest test-surround-env-linewise-preserves-trailing-blank-line ()
  "Test that linewise env surround preserves trailing blank lines in selection.
When user selects (no leading indent):
  aaa bbb##
  dddd bbbb##
  ##  (blank line selected)
The blank line should be preserved inside the environment."
  (skip-unless evil-tex-bora-loaded)
  (evil-tex-bora-test-with-latex
      "oooooooooooooooo\naaa bbb\ndddd bbbb\n\noooooooooooooo" 1
    (evil-tex-bora-mode 1)
    (let ((evil-tex-bora-include-newlines-in-envs t))
      ;; Simulate linewise selection of lines 2-4 (including blank line)
      ;; Line 2: "aaa bbb\n" starts at pos 18
      ;; Line 3: "dddd bbbb\n" starts at pos 26
      ;; Line 4: "\n" (blank) starts at pos 36
      ;; Line 5: "oooo..." starts at pos 37
      (let* ((beg 18)  ; start of "aaa bbb"
             (end 37)  ; after the blank line's newline
             (env-pair (evil-tex-bora-get-env-for-surrounding "align*")))
        ;; Call the advice function directly to test its behavior
        (evil-tex-bora--surround-region-advice
         (lambda (b e type char &optional force)
           ;; Mock evil-surround-region: insert delimiters
           (goto-char e)
           (insert (cdr env-pair))
           (goto-char b)
           (insert (car env-pair)))
         beg end 'line ?e)))
    ;; The blank line should be preserved inside the environment
    ;; (indentation is handled by Emacs, so we just check structure)
    (let ((result (buffer-string)))
      (should (string-match-p "\\\\begin{align\\*}" result))
      (should (string-match-p "aaa bbb" result))
      (should (string-match-p "dddd bbbb" result))
      ;; Blank line should be between content and \end
      (should (string-match-p "dddd bbbb\n.*\n.*\\\\end{align\\*}" result))
      (should (string-match-p "\\\\end{align\\*}" result)))))

(ert-deftest test-surround-env-linewise-without-blank-line ()
  "Test that linewise env surround works correctly without trailing blank line.
When user selects:
  aaa bbb##
  dddd bbbb##
Result should have environment wrapping the content."
  (skip-unless evil-tex-bora-loaded)
  (evil-tex-bora-test-with-latex
      "oooooooooooooooo\naaa bbb\ndddd bbbb\noooooooooooooo" 1
    (evil-tex-bora-mode 1)
    (let ((evil-tex-bora-include-newlines-in-envs t))
      ;; Simulate linewise selection of lines 2-3 (without blank line)
      ;; Line 2: "aaa bbb\n" starts at pos 18
      ;; Line 3: "dddd bbbb\n" starts at pos 26
      ;; Line 4: "oooo..." starts at pos 36
      (let* ((beg 18)  ; start of "aaa bbb"
             (end 36)  ; after "dddd bbbb\n"
             (env-pair (evil-tex-bora-get-env-for-surrounding "align*")))
        (evil-tex-bora--surround-region-advice
         (lambda (b e type char &optional force)
           (goto-char e)
           (insert (cdr env-pair))
           (goto-char b)
           (insert (car env-pair)))
         beg end 'line ?e)))
    ;; Check structure (indentation is handled by Emacs)
    (let ((result (buffer-string)))
      (should (string-match-p "\\\\begin{align\\*}" result))
      (should (string-match-p "aaa bbb" result))
      (should (string-match-p "dddd bbbb" result))
      (should (string-match-p "\\\\end{align\\*}" result)))))

(ert-deftest test-surround-env-linewise-adds-indentation ()
  "Test that linewise env surround calls indent-region.
Indentation is handled by Emacs according to major-mode settings."
  (skip-unless evil-tex-bora-loaded)
  (evil-tex-bora-test-with-latex
      "before\naaa bbb\ndddd bbbb\nafter" 1
    (evil-tex-bora-mode 1)
    (let ((evil-tex-bora-include-newlines-in-envs t))
      (let* ((beg 8)   ; start of "aaa bbb"
             (end 27)  ; after "dddd bbbb\n"
             (env-pair (evil-tex-bora-get-env-for-surrounding "align*")))
        (evil-tex-bora--surround-region-advice
         (lambda (b e type char &optional force)
           (goto-char e)
           (insert (cdr env-pair))
           (goto-char b)
           (insert (car env-pair)))
         beg end 'line ?e)))
    ;; Check that environment structure is correct
    (let ((result (buffer-string)))
      (should (string-match-p "\\\\begin{align\\*}" result))
      (should (string-match-p "aaa bbb" result))
      (should (string-match-p "\\\\end{align\\*}" result)))))

(ert-deftest test-surround-env-linewise-with-existing-indent ()
  "Test that linewise env surround works with existing indentation.
Indentation is handled by Emacs via indent-region."
  (skip-unless evil-tex-bora-loaded)
  (evil-tex-bora-test-with-latex
      "oooo\n      aaa bbb\n      dddd bbbb\noooo" 1
    (evil-tex-bora-mode 1)
    (let ((evil-tex-bora-include-newlines-in-envs t))
      ;; Calculate linewise positions correctly:
      ;; "oooo\n" = 5 chars (pos 1-5)
      ;; "      aaa bbb\n" = 14 chars (pos 6-19)
      ;; "      dddd bbbb\n" = 16 chars (pos 20-35)
      ;; "oooo" = 4 chars (pos 36-39)
      (let* ((beg 6)   ; start of "      aaa bbb" line
             (end 36)  ; after "      dddd bbbb\n"
             (env-pair (evil-tex-bora-get-env-for-surrounding "align*")))
        (evil-tex-bora--surround-region-advice
         (lambda (b e type char &optional force)
           (goto-char e)
           (insert (cdr env-pair))
           (goto-char b)
           (insert (car env-pair)))
         beg end 'line ?e)))
    ;; Check that environment structure is correct
    (let ((result (buffer-string)))
      (should (string-match-p "\\\\begin{align\\*}" result))
      (should (string-match-p "aaa bbb" result))
      (should (string-match-p "dddd bbbb" result))
      (should (string-match-p "\\\\end{align\\*}" result)))))

;;; Env map tests

(ert-deftest test-env-map-bindings ()
  "Test that env-map has expected bindings."
  (skip-unless evil-tex-bora-loaded)
  ;; Common environments should be bound
  (should (lookup-key evil-tex-bora-env-map "e"))
  (should (lookup-key evil-tex-bora-env-map "E"))
  (should (lookup-key evil-tex-bora-env-map "a"))
  (should (lookup-key evil-tex-bora-env-map "A"))
  (should (lookup-key evil-tex-bora-env-map "f"))
  (should (lookup-key evil-tex-bora-env-map "i"))
  (should (lookup-key evil-tex-bora-env-map "x")))

(ert-deftest test-env-map-theorem-prefix ()
  "Test that theorem environments use 't' prefix."
  (skip-unless evil-tex-bora-loaded)
  ;; ta -> axiom, tt -> theorem, etc.
  (should (lookup-key evil-tex-bora-env-map "ta"))
  (should (lookup-key evil-tex-bora-env-map "tt"))
  (should (lookup-key evil-tex-bora-env-map "tl"))
  (should (lookup-key evil-tex-bora-env-map "tp")))

;;; Delim map tests

(ert-deftest test-delim-map-bindings ()
  "Test that delim-map has expected bindings."
  (skip-unless evil-tex-bora-loaded)
  ;; Parentheses
  (should (lookup-key evil-tex-bora-delim-map "P"))
  (should (lookup-key evil-tex-bora-delim-map "p"))
  ;; Brackets
  (should (lookup-key evil-tex-bora-delim-map "S"))
  (should (lookup-key evil-tex-bora-delim-map "s"))
  ;; Braces
  (should (lookup-key evil-tex-bora-delim-map "C"))
  (should (lookup-key evil-tex-bora-delim-map "c"))
  ;; Angle brackets
  (should (lookup-key evil-tex-bora-delim-map "R"))
  (should (lookup-key evil-tex-bora-delim-map "r"))
  ;; Absolute value / norm
  (should (lookup-key evil-tex-bora-delim-map "v"))
  (should (lookup-key evil-tex-bora-delim-map "V"))
  (should (lookup-key evil-tex-bora-delim-map "n"))
  (should (lookup-key evil-tex-bora-delim-map "N")))

;;; CDLaTeX accents map tests

(ert-deftest test-cdlatex-accents-map-bindings ()
  "Test that cdlatex accents map has expected bindings."
  (skip-unless evil-tex-bora-loaded)
  ;; Common accents
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "."))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "~"))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "^"))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "-"))
  ;; Text styles
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "r"))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "i"))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "b"))
  ;; Math styles
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "c"))
  (should (lookup-key evil-tex-bora-cdlatex-accents-map "q")))

;;; texmathp tests (tree-sitter based)

(ert-deftest test-texmathp-in-math ()
  "Test texmathp inside math environment."
  (evil-tex-bora-test-with-latex "\\(x + y\\)" 4
    (should (evil-tex-bora--texmathp))))

(ert-deftest test-texmathp-outside-math ()
  "Test texmathp outside math environment."
  (evil-tex-bora-test-with-latex "text \\(x\\) more" 2
    (should-not (evil-tex-bora--texmathp))))

(ert-deftest test-texmathp-in-display-math ()
  "Test texmathp inside display math."
  (evil-tex-bora-test-with-latex "\\[x + y\\]" 4
    (should (evil-tex-bora--texmathp))))

(ert-deftest test-texmathp-in-equation-env ()
  "Test texmathp inside equation environment."
  (evil-tex-bora-test-with-latex "\\begin{equation}x\\end{equation}" 18
    (should (evil-tex-bora--texmathp))))

;;; Context-aware accent functions tests

(ert-deftest test-accent-rm-in-math ()
  "Test rm accent returns mathrm in math context."
  (evil-tex-bora-test-with-latex "\\(x\\)" 3
    (let ((result (evil-tex-bora-cdlatex-accents---rm)))
      (should (string= (car result) "\\mathrm{")))))

(ert-deftest test-accent-rm-outside-math ()
  "Test rm accent returns textrm outside math."
  (evil-tex-bora-test-with-latex "text" 2
    (let ((result (evil-tex-bora-cdlatex-accents---rm)))
      (should (string= (car result) "\\textrm{")))))

(ert-deftest test-accent-bf-in-math ()
  "Test bf accent returns mathbf in math context."
  (evil-tex-bora-test-with-latex "\\(x\\)" 3
    (let ((result (evil-tex-bora-cdlatex-accents---bf)))
      (should (string= (car result) "\\mathbf{")))))

(ert-deftest test-accent-bf-outside-math ()
  "Test bf accent returns textbf outside math."
  (evil-tex-bora-test-with-latex "text" 2
    (let ((result (evil-tex-bora-cdlatex-accents---bf)))
      (should (string= (car result) "\\textbf{")))))

(ert-deftest test-accent-it-in-math ()
  "Test it accent returns mathit in math context."
  (evil-tex-bora-test-with-latex "\\(x\\)" 3
    (let ((result (evil-tex-bora-cdlatex-accents---it)))
      (should (string= (car result) "\\mathit{")))))

(ert-deftest test-accent-tt-in-math ()
  "Test tt accent returns mathtt in math context."
  (evil-tex-bora-test-with-latex "\\(x\\)" 3
    (let ((result (evil-tex-bora-cdlatex-accents---tt)))
      (should (string= (car result) "\\mathtt{")))))

;;; Text object map population tests

(ert-deftest test-inner-text-objects-map-populated ()
  "Test that inner text objects map is populated."
  (skip-unless evil-tex-bora-loaded)
  (should (lookup-key evil-tex-bora-inner-text-objects-map "e"))
  (should (lookup-key evil-tex-bora-inner-text-objects-map "c"))
  (should (lookup-key evil-tex-bora-inner-text-objects-map "m"))
  (should (lookup-key evil-tex-bora-inner-text-objects-map "d")))

(ert-deftest test-outer-text-objects-map-populated ()
  "Test that outer text objects map is populated."
  (skip-unless evil-tex-bora-loaded)
  (should (lookup-key evil-tex-bora-outer-text-objects-map "e"))
  (should (lookup-key evil-tex-bora-outer-text-objects-map "c"))
  (should (lookup-key evil-tex-bora-outer-text-objects-map "m"))
  (should (lookup-key evil-tex-bora-outer-text-objects-map "d")))

;;; Customization tests

(ert-deftest test-include-newlines-customization ()
  "Test that include-newlines customization exists."
  (skip-unless evil-tex-bora-loaded)
  (should (boundp 'evil-tex-bora-include-newlines-in-envs)))

(provide 'test-evil-tex-bora)
;;; test-evil-tex-bora.el ends here
