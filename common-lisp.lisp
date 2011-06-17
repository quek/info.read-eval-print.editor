(in-package :info.read-eval-print.editor)

(define-mode common-lisp-mode () ())

(add-auto-mode-alist 'common-lisp-mode "\\.lisp$")


(define-command indent :in common-lisp-mode ()
  (funcall (read-from-string "INFO.READ-EVAL-PRINT.EDITOR.COMMON-LISP.INDENT:INDENT-LINE")
           (info.read-eval-print.editor.command::point)))


(defun forward-sexp (iter &optional (count 1))
  (dotimes (i count)
    (forward-skip-whitespace iter)
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
            (t (loop until (whitespace-p (text-iter-char iter))
                     do (text-iter-move iter)))))))

(defun backward-sexp (iter &optional (count 1))
  (dotimes (i count)
    (text-iter-move iter :direction :backward)
    (backward-skip-whitespace iter)
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
                     until (or (whitespace-p c)
                               (char= #\( c)
                               (char= #\) c)
                               (text-iter-is-start iter))
                     do (text-iter-move iter :direction :backward)
                     finally (when (or (whitespace-p c)
                                       (char= #\( c)
                                       (char= #\) c))
                               (text-iter-move iter))))))))

(defun last-sexp (buffer)
  (let ((start (iter-at-mark buffer))
        (end (iter-at-mark buffer)))
    (backward-sexp start)
    (forward-sexp end)
    (insert buffer (prin1-to-string (eval (read-from-string (slice buffer start end)))))))

(defun up-list (iter)
  (unless (text-iter-is-start iter)
    (text-iter-move iter :direction :backwoard)
    (unless (char= #\( (text-iter-char iter))
      (backward-sexp iter)
      (up-list iter))))

(defun down-list (iter)
  (unless (text-iter-is-end iter)
    (text-iter-move iter)
    (unless (char= #\( (text-iter-char iter))
      (down-list iter))))


(define-command eval-last-sexp ()
  (last-sexp *buffer*))

(define-command forward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (forward-sexp iter count )
    (update-cursor *buffer* iter)))

(define-command backward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (backward-sexp iter count)
    (update-cursor *buffer* iter)))

(define-command up-list (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do (up-list iter))
    (update-cursor *buffer* iter)))

(define-command-alias up-list backward-up-list)

(define-command down-list (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (dotimes (i count)
      (down-list iter))
    (update-cursor *buffer* iter)))

(define-command beginning-of-defun ()
  (let ((iter (iter-at-mark *buffer*)))
    (when (text-iter-search iter "defun" :direction :backward)
      (text-iter-move iter :direction :backward)
      (update-cursor *buffer* iter))))



(loop for (mode keyseq command)
      in `((:normal (:super #\e) info.read-eval-print.editor.command::eval-last-sexp)
           (:insert (:super #\e) info.read-eval-print.editor.command::eval-last-sexp)
           (:insert (:control #\i) info.read-eval-print.editor.command::indent)
           (:insert (#\Tab) info.read-eval-print.editor.command::indent)
           (:insert (:control #\m) info.read-eval-print.editor.command::newline-and-indent)
           (:insert (#\Newline) info.read-eval-print.editor.command::newline-and-indent))
      do (set-key *common-lisp-mode-map*  mode keyseq command))
