;;; evil-stub.el --- Minimal Evil stub for testing -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Minimal stub of Evil functions needed for testing evil-tex-bora
;; without requiring the full Evil package.
;;
;;; Code:

(unless (featurep 'evil)
  ;; Provide minimal evil macros and functions for testing

  (defmacro evil-define-text-object (name args &rest body)
    "Stub for `evil-define-text-object'."
    (declare (indent defun))
    `(defun ,name ,args
       ,@body))

  (defun evil-range (beg end &optional type)
    "Stub for `evil-range'."
    (list beg end type))

  (defmacro evil-define-key (state keymap &rest bindings)
    "Stub for `evil-define-key'."
    (declare (indent defun))
    `(progn
       ,@(cl-loop for (key def) on bindings by #'cddr
                  collect `(define-key ,keymap ,key ,def))))

  (provide 'evil))

(provide 'evil-stub)
;;; evil-stub.el ends here
