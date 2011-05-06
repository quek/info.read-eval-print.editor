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

(defun beginning-of-sexp (buffer)
  )

(defun end-of-sexp (buffer)
  )

(defun last-sexp (buffer)
  (let ((end (iter-at-mark buffer)))
    (if (whitespace-p (text-iter-char end))
        (loop do (text-iter-move end :direction :backward)
              if (zerop (text-iter-offset end))
                do (return)
              unless (whitespace-p (text-iter-char end))
                do (progn (text-iter-move end)
                          (return)))
        (loop until (whitespace-p (text-iter-char end)) ; stop by #\Nul at end of buffer
              do (text-iter-move end :direction :forward)))
    (let ((start (iter-at-mark buffer)))
      (setf (text-iter-offset start) (text-iter-offset end))
      (text-iter-move start :direction :backward)
      (let ((sexp (if (char= #\) (text-iter-char start))
                      (loop with level = 1
                            for c = (progn (text-iter-move start :direction :backward)
                                           (text-iter-char start))
                            if (zerop (text-iter-offset start))
                              do (return (slice buffer start end))
                            if (char= #\) (text-iter-char start))
                              do (incf level)
                            else if (char= #\( (text-iter-char start))
                                   do (decf level)
                            if (zerop level)
                              do (return (slice buffer start end)))
                      (loop do (text-iter-move start :direction :backward)
                            if (whitespace-p (text-iter-char start))
                              do (progn (text-iter-move start :direction :forward)
                                        (return (slice buffer start end)))
                            if (zerop (text-iter-offset start))
                              do (return (slice buffer start end))))))
        (let ((result (princ-to-string (eval (read-from-string sexp)))))
          (insert buffer result))))))




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
  (step (last-sexp *buffer*)))
