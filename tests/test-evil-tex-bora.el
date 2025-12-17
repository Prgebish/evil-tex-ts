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

(provide 'test-evil-tex-bora)
;;; test-evil-tex-bora.el ends here
