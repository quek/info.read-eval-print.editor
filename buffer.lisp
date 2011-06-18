(in-package :info.read-eval-print.editor)


(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (setf (style-scheme buffer) *default-buffer-style-scheme*)
  (setf (source-buffer-language buffer)
        (gtk-source-language-manager-guess-language
         (gtk-source-language-manager-get-default)
         "a.lisp"
         (cffi-sys:null-pointer))))

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


(defmacro save-excursion (&body body)
  `(let ((pos (point *buffer*)))
     (unwind-protect (progn ,@body)
       (setf (point *buffer*) pos))))


(defun scan-char-forward ()
  (declare (optimizable-series-function))
  (producing (z) ((iter (iter-at-mark *buffer*)) c)
             (loop
               (tagbody
                  (if (text-iter-is-end iter)
                      (terminate-producing))
                  (setq c (text-iter-char iter))
                  (text-iter-move iter)
                  (next-out z c)))))

(defun scan-char-backward ()
  (declare (optimizable-series-function))
  (producing (z) ((iter (iter-at-mark *buffer*)) (finish nil) c)
             (loop
               (tagbody
                  (if (text-iter-is-start iter)
                      (terminate-producing))
                  (text-iter-move iter :direction :backward)
                  (next-out z (text-iter-char iter))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command point ()
  (point *buffer*))

(define-command (setf point) (offset)
  (setf (point *buffer*) offset))

(define-command insert (text)
  (insert *buffer* text))

(define-command insert* (text pos)
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-offset iter) pos)
    (insert *buffer* text :position iter)))

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

(define-command-alias next-line forward-line)


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

(define-command beginning-of-line ()
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (update-cursor *buffer* iter)))

(define-command end-of-line ()
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-forward-to-line-end iter)
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
    (text-iter-move iter :by :line)
    (dotimes (i count)
      (insert *buffer* (format nil "~a~%" (yank-of *buffer*)) :position iter))
    (text-iter-move iter :by :line :direction :backward)
    (update-cursor *buffer* iter)))

(define-command w ()
  (let ((*buffer* (current-buffer-of *editor*)))
   (save-buffer *buffer*)))

(define-command undo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-undo *buffer*)))

(define-command redo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-redo *buffer*)))

(define-command looking-at (regexp)
  (ppcre:scan (str "^" regexp) (text-of *buffer*) :start (point *buffer*)))

(define-command line-number-at-pos ()
  (text-iter-line (iter-at-mark *buffer*)))

(define-command current-column ()
  (text-iter-line-offset (iter-at-mark *buffer*)))

(define-command back-to-indentation ()
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (awhen (ppcre:scan (ppcre:create-scanner "\\S|$" :multi-line-mode t)
                       (text-of *buffer*)
                       :start (text-iter-offset iter))
      (setf (text-iter-line-offset iter) it)
      (update-cursor *buffer* iter))))

(define-command buffer-substring-no-properties (start end)
  (when (< end start)
    (rotatef start end))
  (let ((a (iter-at-mark *buffer*))
        (b (iter-at-mark *buffer*)))
    (setf (text-iter-offset a) start
          (text-iter-offset b) end)
    (text-buffer-slice *buffer* a b)))


(define-command char-after (&optional (pos (point *buffer*)))
  (ignore-errors (char (text-of *buffer*) pos)))

(define-command skip-chars-forward (regexp &optional end)
  (let ((text (text-of *buffer*))
        (end (or end (text-iter-offset (aprog1 (iter-at-mark *buffer*)
                                         (text-iter-forward-to-end it))))))
    (collect-first
     (choose-if (lambda (x)
                  (not (ppcre:scan regexp text :start x :end (1+ x))))
                (scan-range :from (point *buffer*) :below end)))))

(define-command eolp ()
  (text-iter-ends-line (iter-at-mark *buffer*)))

(define-command forward-word (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-move iter :by :word :count count)))

(define-command delete-indentation ()
  (let ((iter (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (loop while (find (text-iter-char iter) #(#\Space #\Tab))
          do (setf (text-iter-offset end)
                   (1+ (text-iter-offset iter)))
             (text-buffer-delete *buffer* iter end))))

(let (m)

  (define-command re-search-forward (regexp)
    (setf m nil)
    (let ((scanner (ppcre:create-scanner regexp :multi-line-mode t)))
      (setf m (multiple-value-list (ppcre:scan scanner (text-of *buffer*) :start (point *buffer*))))))

  (define-command re-search-backward (regexp)
    (setf m nil)
    (let ((scanner (ppcre:create-scanner (str "(?:.*)" regexp) :multi-line-mode t)))
      (setf m (multiple-value-list (ppcre:scan scanner (text-of *buffer*) :end (point *buffer*))))))

  (define-command match-string-no-properties (n)
    (ignore-errors
      (let ((g (nth (1+ n) m)))
        (info.read-eval-print.editor.command::buffer-substring-no-properties
         (aref g 0) (aref g 1))))))

(define-command delete-line ()
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset start) 0)
    (text-iter-forward-to-line-end end)
    (text-iter-move end)
    (setf (yank-of *buffer*)
          (string-right-trim '(#\Newline #\Return) (text-buffer-slice *buffer* start end)))
    (text-buffer-delete *buffer* start end)))
