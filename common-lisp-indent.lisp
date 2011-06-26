(quek:sdefpackage :info.read-eval-print.editor.common-lisp.indent
                  (:use :cl :info.read-eval-print.editor.command :quek)
                  (:export #:indent-line)
                  (:shadowing-import-from :cl #:close)
                  (:shadowing-import-from :quek #:split)
                  (:import-from :info.read-eval-print.editor #:save-excursion))

(cl:in-package :info.read-eval-print.editor.common-lisp.indent)

(defvar *indent* (make-hash-table :test #'equal))


(defun parse-sexp (point)
  (awhen (find-left-paren point)
    (let ((column (progn
                    (setf (point) it)
                    (current-column))))
      (end-of-sexp)
      (let ((sexp (buffer-substring-no-properties (1+ it) (1+ (point)))))
        (print sexp)
        (values column sexp)))))

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

(defun find-symbol* (name package)
  (let ((name (string-upcase name)))
    (or (ppcre:register-groups-bind (p n) ("(.*?)::?(.*)" name)
          (find-symbol n (find-package p)))
        (find-symbol name package))))

(defun compute-line-indent (point)
  (save-excursion
    (let ((package (current-package)))
      (multiple-value-bind (column sexp) (parse-sexp point)
        (unless column
          (return-from compute-line-indent 0))
        (let ((v (gethash sexp *indent*)))
          (if v
              (+ column v)
              (if (alexandria:starts-with #\( sexp)
                  (+ column 1)
                  (let ((sym (find-symbol* sexp package)))
                    (if (macro-function sym)
                        (+ column 2)
                        (+ column (+ 2 (length sexp))))))))))))

(defun indent-line (&optional (point (point)))
  (let* ((beginning-of-line (save-excursion
                              (setf (point) point)
                              (beginning-of-line)
                              (point)))
         (indent (compute-line-indent beginning-of-line)))
    (delete-indentation)
    (insert* (make-string indent :initial-element #\Space)
             beginning-of-line)))



(setf (gethash "let" *indent*) 2)
(setf (gethash "defun" *indent*) 2)
