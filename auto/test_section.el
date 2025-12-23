;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "test_section"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10")
   (LaTeX-add-environments
    '("Solution" LaTeX-env-args ["argument"] 0)
    '("Task" LaTeX-env-args ["argument"] 0)
    '("Condition" LaTeX-env-args ["argument"] 0)))
 :latex)

