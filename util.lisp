(in-package :info.read-eval-print.editor)

(defparameter *whitespace* '(#\Space #\Newline #\Return #\Tab #\Vt #\Nul))

(defun whitespace-p (char)
  (member char *whitespace*))

(defun sexp-end-p (char)
  (or (whitespace-p char)
      (char= char #\))))
