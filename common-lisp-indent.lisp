(cl:defpackage :info.read-eval-print.editor.common-lisp.indent
  (:use :cl :info.read-eval-print.editor.command :quek)
  (:export #:indent-line)
  (:shadowing-import-from :cl #:close)
  (:shadowing-import-from :quek #:split)
  (:import-from :info.read-eval-print.editor #:save-excursion))

(cl:in-package :info.read-eval-print.editor.common-lisp.indent)

(series::install :pkg :info.read-eval-print.editor.common-lisp.indent
                 :implicit-map t)


(defvar *indent* (make-hash-table :test #'equal))

(defun find-left-paren (point)
  (let ((level 0))
    (collect-first
     (choose-if (lambda (pos &aux (c (char-after pos)))
                  (cond ((char= #\( c)
                         (if (zerop level)
                             pos
                             (progn (decf level)
                                    nil)))
                        ((char= #\) c)
                         (incf level)
                         nil)
                        (t nil)))
                (scan-range :from (1- point) :downto 0 :by -1)))))

(defun compute-line-indent (point)
  (save-excursion
    (let ((left-paren (or (find-left-paren point)
                          0)))
      (setf (point) left-paren)
      (current-column))))

(defun indent-line (&optional (point (point)))
  (let ((beginning-of-line (save-excursion (beginning-of-line) (point)))
        (indent (compute-line-indent point)))
    (delete-indentation)
    (insert* (make-string indent :initial-element #\Space)
             beginning-of-line)))
