(in-package :info.read-eval-print.editor)

(define-mode common-lisp-mode () ())

(add-auto-mode-alist 'common-lisp-mode "\\.lisp$")


(define-command indent :in common-lisp-mode ()
  (funcall (read-from-string "INFO.READ-EVAL-PRINT.EDITOR.CL-INDENT:COMMON-LISP-INDENT-FUNCTION")
           (info.read-eval-print.editor.command::point) nil))


(loop for (mode keyseq command)
      in `((:normal (:super #\e) info.read-eval-print.editor.command::eval-last-sexp)
           (:insert (:super #\e) info.read-eval-print.editor.command::eval-last-sexp))
      do (set-key *common-lisp-mode-map*  mode keyseq command))
