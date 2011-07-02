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

(defmethod buffer-insert ((buffer buffer) (text string) &key (position :cursor))
  (text-buffer-insert buffer text :position position))

(defun text-of (buffer)
  (text-buffer-text buffer))

(defun (setf text-of) (text buffer)
  (setf (text-buffer-text buffer) text))

(defun iter-at-mark (buffer)
  (text-buffer-get-iter-at-mark buffer
                                (text-buffer-insertion-mark buffer)))

(defun buffer-point (buffer)
  (text-iter-offset (iter-at-mark buffer)))

(defun (setf buffer-point) (offset buffer)
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

(defmethod (setf file-of) :after (file (buffer buffer))
  (setf (name-of buffer) (file-namestring file)))

(defmethod find-file (buffer file)
  (setf (file-of buffer) file)
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
    (beginning-of-buffer)))

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

(defun save-buffer (buffer &optional (file (file-of buffer)))
  (let ((file (or file (file-of buffer))))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :external-format (external-format-of buffer))
      (write-sequence (text-of buffer) out))
    (setf (file-of buffer) file)
    (update-status (frame-of buffer))))

(defun iter-forward-skip-whitespace (iter)
  (loop while (and (whitespace-p (text-iter-char iter))
                   (not (text-iter-is-end iter)))
        do (text-iter-move iter)))

(defun iter-backward-skip-whitespace (iter)
  (loop while (and (whitespace-p (text-iter-char iter))
                   (not (text-iter-is-start iter)))
        do (text-iter-move iter :direction :backward)))


(defmacro save-excursion (&body body)
  `(let ((pos (buffer-point *buffer*)))
     (unwind-protect (progn ,@body)
       (setf (buffer-point *buffer*) pos))))


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
  (buffer-point *buffer*))

(define-command (setf point) (offset)
  (setf (buffer-point *buffer*) offset))

(define-command insert (text)
  (buffer-insert *buffer* text))

(define-command insert* (text pos)
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-offset iter) pos)
    (buffer-insert *buffer* text :position iter)))

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
    (text-iter-move iter :count count :by :line)
    (setf (text-iter-line-offset iter) line-offset)
    (when (/= line-offset (text-iter-line-offset iter))
      (text-view-forward-display-line-end *frame* iter))
    (update-cursor *buffer* iter)))

(define-command-alias next-line forward-line)


(define-command previous-line (&optional (count *digit-argument*))
  (let* ((iter (iter-at-mark *buffer*))
         (line-offset (text-iter-line-offset iter)))
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

(define-command beginning-of-line-or-digit-0 ()
  (with-slots (digit-argument) *buffer*
    (if digit-argument
        (setf (digit-argument-of *buffer*) 0)
        (beginning-of-line))))

(define-command end-of-line ()
  (let ((iter (iter-at-mark *buffer*)))
    (text-iter-forward-to-line-end iter)
    (update-cursor *buffer* iter)))

(define-command back-to-indentation ()
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (loop while (member (text-iter-char iter) '(#\Space #\Tab) :test #'char=)
          do (text-iter-move iter))
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
    (setf (register-value (register-of *editor*))
          (slice *buffer* start end))))

(define-command paste-below-cursor (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset iter) 0)
    (unless (text-iter-move iter :by :line)
      (buffer-insert *buffer* (string #\Newline) :position iter))
    (dotimes (i count)
      (buffer-insert *buffer*
              (format nil "~a~%" (register-value (register-of *editor*)))
              :position iter))
    (text-iter-move iter :by :line :direction :backward)
    (update-cursor *buffer* iter)))

(define-command w (&optional file-name)
  (let ((*buffer* (current-buffer-of *editor*)))
   (save-buffer *buffer* file-name)))

(define-command undo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-undo *buffer*)))

(define-command redo (&optional (count *digit-argument*))
  (dotimes (i count)
    (source-buffer-redo *buffer*)))

(define-command looking-at (regexp)
  (ppcre:scan (str "^" regexp) (text-of *buffer*) :start (point)))

(define-command line-number-at-pos ()
  (text-iter-line (iter-at-mark *buffer*)))

(define-command current-column ()
  (text-iter-line-offset (iter-at-mark *buffer*)))

(define-command buffer-substring-no-properties (start end)
  (when (< end start)
    (rotatef start end))
  (let ((a (iter-at-mark *buffer*))
        (b (iter-at-mark *buffer*)))
    (setf (text-iter-offset a) start
          (text-iter-offset b) end)
    (text-buffer-slice *buffer* a b)))

(define-command char-after (&optional pos)
  (let ((iter (iter-at-mark *buffer*)))
    (when pos
      (setf (text-iter-offset iter) pos))
    (let ((c (text-iter-char iter)))
      (if (char= c #\Nul)
          nil
          c))))

(define-command skip-chars-forward (regexp &optional end)
  (let ((text (text-of *buffer*))
        (end (or end (text-iter-offset (aprog1 (iter-at-mark *buffer*)
                                         (text-iter-forward-to-end it))))))
    (collect-first
     (choose-if (lambda (x)
                  (not (ppcre:scan regexp text :start x :end (1+ x))))
                (scan-range :from (point) :below end)))))

(define-command eolp ()
  (text-iter-ends-line (iter-at-mark *buffer*)))

(define-command same-category-char (a b)
  (cond ((whitespace-p a)
         (whitespace-p b))
        (t
         (equal (cl-unicode:script a)
                (cl-unicode:script b)))))

(define-command forward-word (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (iter-forward-skip-whitespace iter)
    (loop repeat count do
      (loop until (text-iter-is-end iter)
            with first-char = (text-iter-char iter)
            do (text-iter-move iter)
            while (same-category-char first-char (text-iter-char iter)))
      (iter-forward-skip-whitespace iter))
    (update-cursor *buffer* iter)))

(define-command forward-word* (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (iter-forward-skip-whitespace iter)
    (dotimes (i count)
      (loop until (text-iter-is-end iter)
            do (text-iter-move iter)
            until (whitespace-p (text-iter-char iter)))
      (iter-forward-skip-whitespace iter))
    (update-cursor *buffer* iter)))

(define-command end-of-word (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do
      (text-iter-move iter)
      (iter-forward-skip-whitespace iter)
      (loop until (text-iter-is-end iter)
            with first-char = (text-iter-char iter)
            do (text-iter-move iter)
            while (or (same-category-char first-char (text-iter-char iter))
                      (progn
                        (text-iter-move iter :direction :backward)
                        nil))))
    (update-cursor *buffer* iter)))

(define-command end-of-word* (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do
      (text-iter-move iter)
      (iter-forward-skip-whitespace iter)
      (loop until (text-iter-is-end iter)
            do (text-iter-move iter)
            until (and (whitespace-p (text-iter-char iter))
                       (progn
                         (text-iter-move iter :direction :backward)
                         t))))
    (update-cursor *buffer* iter)))

(define-command backward-word (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do
      (text-iter-move iter :direction :backward)
      (iter-backward-skip-whitespace iter)
      (loop until (text-iter-is-start iter)
            with first-char = (text-iter-char iter)
            do (text-iter-move iter :direction :backward)
            while (or (same-category-char first-char (text-iter-char iter))
                      (progn
                        (text-iter-move iter)
                        nil))))
    (update-cursor *buffer* iter)))

(define-command backward-word* (&optional (count *digit-argument*))
  (let ((iter (iter-at-mark *buffer*)))
    (loop repeat count do
      (text-iter-move iter :direction :backward)
      (iter-backward-skip-whitespace iter)
      (loop until (text-iter-is-start iter)
            do (text-iter-move iter :direction :backward)
            until (and (whitespace-p (text-iter-char iter))
                       (progn
                         (text-iter-move iter)
                         t))))
    (update-cursor *buffer* iter)))

(define-command forward-skip-whitespace ()
  (let ((iter (iter-at-mark *buffer*)))
    (iter-forward-skip-whitespace iter)
    (update-cursor *buffer* iter)))

(define-command backward-skip-whitespace ()
  (let ((iter (iter-at-mark *buffer*)))
    (iter-backward-skip-whitespace iter)
    (update-cursor *buffer* iter)))

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
      (setf m (multiple-value-list (ppcre:scan scanner (text-of *buffer*) :start (point))))))

  (define-command re-search-backward (regexp)
    (setf m nil)
    (let ((scanner (ppcre:create-scanner (str "(?:.*)" regexp) :multi-line-mode t)))
      (setf m (multiple-value-list (ppcre:scan scanner (text-of *buffer*) :end (point))))))

  (define-command match-string-no-properties (n)
    (ignore-errors
      (let ((g (nth (1+ n) m)))
        (buffer-substring-no-properties
         (aref g 0) (aref g 1))))))

(define-command delete-line ()
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (setf (text-iter-line-offset start) 0)
    (text-iter-forward-to-line-end end)
    (text-iter-move end)
    (setf (register-value (register-of *editor*))
          (string-right-trim '(#\Newline #\Return) (text-buffer-slice *buffer* start end)))
    (text-buffer-delete *buffer* start end)))

(define-command delete-word (&optional (count *digit-argument*))
  (let ((start (iter-at-mark *buffer*))
        (end (iter-at-mark *buffer*)))
    (text-iter-move end :count count :by :word)
    (setf (register-value (register-of *editor*))
          (text-buffer-slice *buffer* start end))
    (text-buffer-delete *buffer* start end)))