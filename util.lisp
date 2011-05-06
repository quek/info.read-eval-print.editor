(in-package :info.read-eval-print.editor)

(defparameter *whitespace* '(#\Space #\Newline #\Return #\Tab #\Vt #\Nul))

(defun whitespace-p (char)
  (member char *whitespace*))
