(in-package :info.read-eval-print.editor)

(defclass* buffer ()
  ((object)
   (view)
   (name nil)
   (file nil)
   (external-format :utf-8)))

(defmethod slice ((buffer buffer) start end)
  (text-buffer-slice (object-of buffer) start end))

(defmethod insert ((buffer buffer) (text string))
  (text-buffer-insert (object-of buffer) text))

(defun text-of (buffer)
  (text-buffer-text (object-of buffer)))

(defun (setf text-of) (text buffer)
  (setf (text-buffer-text (object-of buffer)) text))

(defun iter-at-mark (buffer)
  (text-buffer-get-iter-at-mark (object-of buffer)
                                (text-buffer-insertion-mark (object-of buffer))))

(defun place-cursor (buffer iter)
  (text-buffer-place-cursor (object-of buffer) iter))

(defun start-iter (buffer)
  (text-buffer-get-start-iter (object-of buffer)))

(defun end-iter (buffer)
  (text-buffer-get-end-iter (object-of buffer)))

(defmethod find-file (buffer file)
  (if (probe-file file)
      (progn
        (setf (text-of buffer)
              (collect 'string (scan-file file #'read-char))))
      (progn
        (setf (text-of buffer) "")))
  (setf (file-of buffer) file)
  (setf (name-of buffer) (file-namestring file))
  (let ((*buffer* buffer))
    (info.read-eval-print.editor.command::beginning-of-buffer)))

(defun save-buffer (buffer)
  (with-open-file (out (file-of buffer)
                       :direction :output
                       :if-exists :supersede
                       :external-format (external-format-of buffer))
    (write-sequence (text-of buffer) out)))

(defun forward-skip-whitespace (iter)
  (loop while (and (whitespace-p (text-iter-char iter))
                   (not (text-iter-is-end iter)))
        do (text-iter-move iter)))

(defun backward-skip-whitespace (iter)
  (loop while (and (whitespace-p (text-iter-char iter))
                   (not (text-iter-is-start iter)))
        do (text-iter-move iter :direction :backward)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info.read-eval-print.editor.command::backward-char (&optional (n 1))
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-move iter :count n :direction :backward)
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::forward-char (&optional (n 1))
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-move iter :count n :direction :forward)
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::next-line (&optional (n 1))
  (let* ((iter (iter-at-mark *buffer*))
         (line-offset (text-iter-line-offset iter)))
    (loop repeat n do (text-view-forward-display-line (view-of *buffer*) iter))
    (setf (text-iter-line-offset iter) line-offset)
    (when (/= line-offset (text-iter-line-offset iter))
      (text-view-forward-display-line-end *view* iter))
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::previous-line (&optional (n 1))
  (let* ((iter (iter-at-mark *buffer*))
         (line-offset (text-iter-line-offset iter)))
    (loop repeat n do (text-view-backward-display-line (view-of *buffer*) iter))
    (setf (text-iter-line-offset iter) line-offset)
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::beginning-of-buffer ()
  (let ((iter (start-iter *buffer*)))
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::end-of-buffer ()
  (let ((iter (end-iter *buffer*)))
    (place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::w ()
  (let ((*buffer* (current-buffer-of *editor*)))
   (save-buffer *buffer*)))

(defun info.read-eval-print.editor.command::eval-last-sexp ()
  (last-sexp *buffer*))

(defun info.read-eval-print.editor.command::forward-sexp (&optional (count 1))
  (let ((iter (iter-at-mark *buffer*)))
    (forward-sexp iter count )
    (place-cursor *buffer* iter)))


(defun info.read-eval-print.editor.command::backward-sexp (&optional (count 1))
  (let ((iter (iter-at-mark *buffer*)))
    (backward-sexp iter count)
    (place-cursor *buffer* iter)))
