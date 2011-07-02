(in-package :info.read-eval-print.editor)

(define-mode common-lisp-mode () ())

(add-auto-mode-alist 'common-lisp-mode "\\.lisp$")


(define-command indent :in common-lisp-mode ()
  (funcall (read-from-string "INFO.READ-EVAL-PRINT.EDITOR.COMMON-LISP.INDENT:INDENT-LINE")
           (point)))


(defun iter-forward-sexp (iter &optional (count 1))
  (iter-forward-skip-whitespace iter)
  (dotimes (i count)
    (let ((char (text-iter-char iter)))
      (cond ((char= #\( char)
             (loop with level = 1
                   for i = (text-iter-move iter)
                   for c = (text-iter-char iter)
                   until (or (zerop level) (text-iter-is-end iter))
                   if (char= #\( c)
                     do (incf level)
                   else if (char= #\) c)
                          do (decf level)))
            ((char= #\) char)
             (text-iter-move iter))
            (t (loop until (or (text-iter-is-end iter)
                               (whitespace-p (text-iter-char iter)))
                     do (text-iter-move iter)))))
    (iter-forward-skip-whitespace iter)))

(defun iter-end-of-sexp (iter &optional (count 1))
  (dotimes (i count)
    (text-iter-move iter)
    (iter-forward-skip-whitespace iter)
    (let ((char (text-iter-char iter)))
      (cond ((char= #\( char)
             (loop with level = 1
                   for i = (text-iter-move iter)
                   for c = (text-iter-char iter)
                   until (or (zerop level) (text-iter-is-end iter))
                   if (char= #\( c)
                     do (incf level)
                   else if (char= #\) c)
                          do (decf level)))
            ((char= #\) char))
            (t (loop until (text-iter-is-end iter)
                     until (and (whitespace-p (text-iter-char iter))
                                (progn
                                  (text-iter-move iter :direction :backward)
                                  t))
                     do (text-iter-move iter)))))))

(defun iter-backward-sexp (iter &optional (count 1))
  (dotimes (i count)
    (text-iter-move iter :direction :backward)
    (iter-backward-skip-whitespace iter)
    (let ((char (text-iter-char iter)))
      (cond ((char= #\) char)
             (loop with level = 1
                   for i = (text-iter-move iter :direction :backward)
                   for c = (text-iter-char iter)
                   if (char= #\) c)
                     do (incf level)
                   else if (char= #\( c)
                          do (decf level)
                   until (or (zerop level) (text-iter-is-start iter))))
            ((char= #\( char))
            (t (loop for c = (text-iter-char iter)
                     until (text-iter-is-start iter)
                     until (and (or (whitespace-p c)
                                    (char= #\( c)
                                    (char= #\) c))
                                (progn (text-iter-move iter)
                                       t))
                     do (text-iter-move iter :direction :backward)))))))

(defun last-sexp (buffer)
  (let ((start (iter-at-mark buffer))
        (end (iter-at-mark buffer)))
    (iter-backward-sexp start)
    (iter-forward-sexp end)
    (buffer-insert buffer (prin1-to-string (eval (read-from-string (slice buffer start end)))))))

(defun iter-up-list (iter)
  (unless (text-iter-is-start iter)
    (text-iter-move iter :direction :backward)
    (unless (char= #\( (text-iter-char iter))
      (iter-backward-sexp iter)
      (iter-up-list iter))))

(defun iter-down-list (iter)
  (unless (text-iter-is-end iter)
    (text-iter-move iter)
    (unless (char= #\( (text-iter-char iter))
      (iter-down-list iter))))


(define-command eval-last-sexp ()
  (last-sexp *buffer*))

(define-command forward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (iter-forward-sexp iter count )
    (update-cursor *buffer* iter)))

(define-command backward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (iter-backward-sexp iter count)
    (update-cursor *buffer* iter)))

(define-command end-of-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (iter-end-of-sexp iter count)
    (update-cursor *buffer* iter)))

(define-command up-list (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do (iter-up-list iter))
    (update-cursor *buffer* iter)))

(define-command-alias up-list backward-up-list)

(define-command down-list (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (dotimes (i count)
      (iter-down-list iter))
    (update-cursor *buffer* iter)))

(define-command beginning-of-defun ()
  (let ((iter (iter-at-mark *buffer*)))
    (multiple-value-bind (p s e) (text-iter-search iter "(defun" :direction :backward)
      (declare (ignore e))
      (when p
        (update-cursor *buffer* s)))))

(defun search-buffer-package ()
  (let ((re "^\\((cl:\\|common-lisp:)?in-package\\b[ \\t']*([^\\)]+)[ \\t]*\\)"))
    (when (or (re-search-backward re)
              (re-search-forward re))
      (match-string-no-properties  2))))

(define-command current-package ()
  (or (ignore-errors (find-package (string-upcase (search-buffer-package))))
      (find-package :cl-user)))

(define-command defun-at-point ()
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (forward-sexp)
      (let ((end (point)))
        (quek:p start end)
        (info.read-eval-print.editor.command:buffer-substring-no-properties start end)))))

(define-command eval-defun ()
  (let ((defun-form (defun-at-point))
        (*package* (current-package)))
    (handler-case (message (eval (read-from-string defun-form)))
      (error (e)
        (open-info-frame (format nil "(in-package :~a)~%~%~a~%~%~a"
                                 (string-downcase (package-name *package*))
                                 defun-form
                                 e))))))

(define-command compile-and-load-file ()
  (open-info-frame
   (with-output-to-string (*standard-output*)
     (let ((*error-output* *standard-output*))
       (load (compile-file (file-of *buffer*)))))))

(defun symbol-at-point ()
  (save-excursion
    (let ((start (iter-at-mark *buffer*))
          (end (iter-at-mark *buffer*)))
      (iter-backward-sexp start)
      (text-buffer-slice *buffer* start end))))

(define-command complete-symbol ()
  (let* ((symbol (symbol-at-point))
         (symbols (car (swank:simple-completions symbol (search-buffer-package)))))
    (if (len=1 symbols)
        (progn
          (let ((end (iter-at-mark *buffer*))
                (start (iter-at-mark *buffer*)))
            (iter-backward-sexp start)
            (text-buffer-delete *buffer* start end)
            (buffer-insert *buffer* (car symbols))))
        (open-info-frame (with-output-to-string (out)
                                 (iterate ((x (scan symbols)))
                                   (format out "~a~%" x)))))))

(define-command indent-and-complete-symbol ()
  (indent)
  (complete-symbol))

(loop for (mode keyseq command)
      in `((:normal (:meta #\h) backward-sexp)
           (:insert (:meta #\h) backward-sexp)
           (:normal (:meta #\j) down-list)
           (:insert (:meta #\j) down-list)
           (:normal (:meta #\k) up-list)
           (:insert (:meta #\k) up-list)
           (:normal (:meta #\l) forward-sexp)
           (:insert (:meta #\l) forward-sexp)
           (:normal (:meta #\e) eval-last-sexp)
           (:insert (:meta #\e) eval-last-sexp)
           (:normal (:meta #\x) eval-defun)
           (:insert (:meta #\x) eval-defun)
           (:normal (:meta #\k) compile-and-load-file)
           (:insert (:control #\i) indent-and-complete-symbol)
           (:insert (#\Tab) indent)
           (:insert (:control #\m) newline-and-indent)
           (:insert (#\Return) newline-and-indent))
      do (set-key *common-lisp-mode-map*  mode keyseq command))
