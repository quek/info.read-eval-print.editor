(in-package :info.read-eval-print.editor)

(define-mode common-lisp-mode () ())

(add-auto-mode-alist 'common-lisp-mode "\\.lisp$")


(define-command indent :in common-lisp-mode ()
  (info.read-eval-print.editor.command::insert "common lisp indent"))


(loop for (mode keyseq command)
      in `((:normal (:super #\e) info.read-eval-print.editor.command::eval-last-sexp)
           (:insert (:super #\e) info.read-eval-print.editor.command::eval-last-sexp))
      do (set-key *common-lisp-mode-map*  mode keyseq command))
