(in-package :info.read-eval-print.editor)


(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (setf (style-scheme buffer) *default-buffer-style-scheme*))

(defmethod update-cursor ((buffer buffer) iter)
  (text-buffer-place-cursor buffer iter)
  (text-view-scroll-mark-onscreen (view-of (frame-of buffer))
                                  (text-buffer-insertion-mark buffer)))

(defmethod digit-argument-of ((buffer buffer))
  (with-slots (digit-argument) buffer
    (prog1 digit-argument
      (setf digit-argument nil))))

(defmethod (setf digit-argument-of) ((number number) (buffer buffer))
  (with-slots (digit-argument) buffer
    (if digit-argument
        (setf digit-argument (+ (* digit-argument 10) number))
        (setf digit-argument number))))

(defmethod (setf digit-argument-of) ((char character) (buffer buffer))
  (setf (digit-argument-of buffer) (- (char-code char) (char-code #\0))))

(defmethod slice ((buffer buffer) start end)
  (text-buffer-slice buffer start end))

(defmethod insert ((buffer buffer) (text string) &key (position :cursor))
  (text-buffer-insert buffer text :position position))

(defun text-of (buffer)
  (text-buffer-text buffer))

(defun (setf text-of) (text buffer)
  (setf (text-buffer-text buffer) text))

(defun iter-at-mark (buffer)
  (text-buffer-get-iter-at-mark buffer
                                (text-buffer-insertion-mark buffer)))

(defun point (buffer)
  (text-iter-offset (iter-at-mark buffer)))

(defun (setf point) (offset buffer)
  (let ((iter (iter-at-mark buffer)))
    (setf (text-iter-offset iter) offset)
    (update-cursor buffer iter)))

(defun start-iter (buffer)
  (text-buffer-get-start-iter buffer))

(defun end-iter (buffer)
  (text-buffer-get-end-iter buffer))


(defun auto-mode (file)
  (collect-first
   (#Mcdr
    (choose-if (lambda (x)
                 (when (ppcre:scan (car x) (string file))
                   (cdr x)))
               (scan *auto-mode-alist*)))))

(defmethod find-file (buffer file)
  (setf (file-of buffer) file)
  (setf (name-of buffer) (file-namestring file))
  (guess-language buffer)
  (awhen (auto-mode file)
    (enable-mode (mode-of buffer) it))
  (if (probe-file file)
      (setf (values (text-of buffer)
                    (external-format-of buffer))
            (read-file file))
      (progn
        (setf (text-of buffer) "")))
  (let ((*buffer* buffer))
    (info.read-eval-print.editor.command::beginning-of-buffer)))

(defmethod guess-language ((buffer buffer))
  (awhen (gtk-source-language-manager-guess-language
          (gtk-source-language-manager-get-default)
          (string (file-of buffer))
          (cffi-sys:null-pointer))
    (setf (source-buffer-language buffer) it)))

(defmethod (setf style-scheme) (style-scheme-id (buffer buffer))
  (setf (source-buffer-style-scheme buffer)
        (gtk-source-style-scheme-manager-get-scheme
         (gtk-source-style-scheme-manager-get-default)
         style-scheme-id)))

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

(define-command point ()
  (point *buffer*))

(define-command (setf point) (offset)
  (setf (point *buffer*) offset))

(define-command-alias (setf point) goto-char)

(define-command backward-char (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-move iter :count count :direction :backward)
    (update-cursor *buffer* iter)))

(define-command forward-char (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-move iter :count count :direction :forward)
    (update-cursor *buffer* iter)))

(define-command next-line (&optional (count *digit-argument*))
  (let* ((iter (iter-at-mark *buffer*))
         (line-offset (text-iter-line-offset iter)))
    ;; (loop repeat count do (text-view-forward-display-line (frame-of *buffer*) iter))
    (text-iter-move iter :count count :by :line)
    (setf (text-iter-line-offset iter) line-offset)
    (when (/= line-offset (text-iter-line-offset iter))
      (text-view-forward-display-line-end *frame* iter))
    (update-cursor *buffer* iter)))


(define-command previous-line (&optional (count *digit-argument*))
  (let* ((iter (iter-at-mark *buffer*))
         (line-offset (text-iter-line-offset iter)))
    ;; (loop repeat count do (text-view-backward-display-line (frame-of *buffer*) iter))
    (text-iter-move iter :count count :by :line :direction :backward)
    (setf (text-iter-line-offset iter) line-offset)
    (update-cursor *buffer* iter)))


(define-command beginning-of-buffer ()
  (let ((iter (start-iter *buffer*)))
    (update-cursor *buffer* iter)))

(define-command end-of-buffer ()
  (let ((iter (end-iter *buffer*)))
    (update-cursor *buffer* iter)))

(define-command end-of-line ()
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-forward-to-line-end iter)
    (update-cursor *buffer* iter)))

(define-command forward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (forward-sexp iter count )
    (update-cursor *buffer* iter)))


(define-command backward-sexp (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (backward-sexp iter count)
    (update-cursor *buffer* iter)))


(define-command delete-char (&optional (count *digit-argument*))
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (text-iter-move end :count count)
    (text-buffer-delete *buffer* start end)))

(define-command backward-delete-char (&optional (count *digit-argument*))
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (text-iter-move start :count count :direction :backward)
    (text-buffer-delete *buffer* start end)))

(define-command yank-current-line (&optional (count *digit-argument*))
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset start) 0)
    (dotimes (i (1- count))
      (text-iter-move end :count count :by :line))
    (text-iter-forward-to-line-end end)
    (setf (yank-of *buffer*) (slice *buffer* start end))))

(define-command paste-below-cursor (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (dotimes (i count)
      (insert *buffer* (format nil "~a~%" (yank-of *buffer*)) :position iter))))

(define-command w ()
  (let ((*buffer* (current-buffer-of *editor*)))
   (save-buffer *buffer*)))

(define-command undo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-undo *buffer*)))

(define-command redo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-redo *buffer*)))

(define-command eval-last-sexp ()
  (last-sexp *buffer*))
