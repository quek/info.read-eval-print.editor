(in-package :info.read-eval-print.editor)


(defgeneric get-key (key-map editor-mode keyseq))

(defgeneric set-key (key-map editor-mode keyseq command &optional restore-temp-key-map))

(defgeneric push-temp-key-map (key-map temp-key-map))

(defgeneric pop-temp-key-map (key-map))

(defgeneric key-map-table (key-map editor-mode))


(defgeneric enable-mode (mode &rest mode-to-enable))

(defgeneric disable-mode (mode &rest mode-to-disable))

(define-layered-function key-binding (mode keyseq editor-mode)
  (:method-combination or))



(defun anonymous-class-p (class)
  (null (class-name class)))


(defclass* key-map ()
  ((normal (make-hash-table :test #'equal))
   (insert (make-hash-table :test #'equal))
   (inherits nil)
   (temp-key-map nil)))

(defmethod key-map-table (key-map (editor-mode (eql :normal)))
  (normal-of key-map))
(defmethod key-map-table (key-map (editor-mode (eql :insert)))
  (insert-of key-map))

(defmethod get-key (key-map editor-mode keyseq)
  (aif (temp-key-map-of key-map)
       (awhen (gethash keyseq (key-map-table (car it) editor-mode))
         (prog1 (car it)
           (when (cdr it)
             (pop-temp-key-map key-map))))
       (aif (gethash keyseq (key-map-table key-map editor-mode))
            (car it)
            (collect-first (get-key (scan (inherits-of key-map)) editor-mode keyseq)))))

(defmethod set-key (key-map editor-mode keyseq command &optional (restore-temp-key-map t))
  (setf (gethash keyseq (key-map-table key-map editor-mode))
        (cons command restore-temp-key-map)))

(defmethod set-key :around (key-map editor-mode keyseq command &optional (restore-temp-key-map t))
  (loop for x in keyseq
        if (and (keywordp x)
                (not (member x '(:control :alt :shif :super :hyper))))
          do (error "invalid keyseq ~a." keyseq))
  (call-next-method key-map editor-mode keyseq command restore-temp-key-map))

(defmethod push-temp-key-map (key-map temp-key-map)
  (push temp-key-map (temp-key-map-of key-map)))

(defmethod pop-temp-key-map (key-map)
  (pop (temp-key-map-of key-map)))



(define-layered-class mode ()
  ((enabled-modes :initarg :enabled-modes
                  :initform `(fundamental-mode)
                  :accessor enabled-modes-of)))

(defun mode-layer-context (mode)
  (collect-fn t #'current-layer-context
              (lambda (context layer)
                (adjoin-layer layer context))
              (scan (enabled-modes-of mode))))

(defun get-key-binding (mode keyseq &optional (editor-mode (mode-of *editor*)))
  (funcall-with-layer-context (mode-layer-context mode)
                              #'key-binding
                              mode
                              keyseq
                              editor-mode))

(defun funcall-with-mode (mode function &rest args)
  (apply-with-layer-context (mode-layer-context mode)
                            function
                            args))

(define-layered-method key-binding or (mode keyseq editor-mode)
  (declare (ignore editor-mode))
  nil)

(defmethod enable-mode (mode &rest mode-to-enable)
  (let ((xs (append mode-to-enable (enabled-modes-of mode))))
    (setf (enabled-modes-of mode)
          (delete-duplicates xs :from-end t))))

(defmethod disable-mode (mode &rest mode-to-disable)
  (iterate ((x (scan mode-to-disable)))
    (setf (enabled-modes-of mode)
          (delete x (enabled-modes-of mode)))))



(defmacro define-mode (mode (&rest super-modes)
                       (&rest slots)
                       &rest class-options)
  (let ((key-map (sym "*" mode "-MAP*")))
    `(progn
       (defvar ,key-map (make-instance 'key-map))
       (deflayer ,mode ,super-modes)
       (define-layered-class mode :in ,mode
         ,slots
         ,@class-options)
       (define-layered-method key-binding :in ,mode or (mode keyseq editor-mode)
                              (get-key ,key-map editor-mode keyseq)))))

(define-mode fundamental-mode () ())

(define-mode lisp-mode () ())

(define-mode common-lisp-mode (lisp-mode) ())

(pushnew '("\\.lisp$" . common-lisp-mode) *auto-mode-alist* :test #'equal)

(define-mode show-paren-mode () ())

(defmethod print-object ((x mode) stream)
  (print-unreadable-object (x stream)
    (format stream "~a ~(~{~a~^ ~}~)" (name-of x) (enabled-modes-of x))))



(defvar *digit-argument-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\0) info.read-eval-print.editor.command::digit-argument-0)
           ((#\1) info.read-eval-print.editor.command::digit-argument-1)
           ((#\2) info.read-eval-print.editor.command::digit-argument-2)
           ((#\3) info.read-eval-print.editor.command::digit-argument-3)
           ((#\4) info.read-eval-print.editor.command::digit-argument-4)
           ((#\5) info.read-eval-print.editor.command::digit-argument-5)
           ((#\6) info.read-eval-print.editor.command::digit-argument-6)
           ((#\7) info.read-eval-print.editor.command::digit-argument-7)
           ((#\8) info.read-eval-print.editor.command::digit-argument-8)
           ((#\9) info.read-eval-print.editor.command::digit-argument-9))
      do (set-key *fundamental-mode-map* :normal keyseq command nil))

(pushnew *digit-argument-map* (inherits-of *fundamental-mode-map*))

(loop for (keyseq command)
      in `(((#\:) info.read-eval-print.editor.command::command-mode)
           ((#\i) info.read-eval-print.editor.command::i)
           ((#\a) info.read-eval-print.editor.command::a)
           ((#\o) info.read-eval-print.editor.command::o)
           ((#\h) info.read-eval-print.editor.command::backward-char)
           ((#\j) info.read-eval-print.editor.command::next-line)
           ((#\k) info.read-eval-print.editor.command::previous-line)
           ((#\l) info.read-eval-print.editor.command::forward-char)
           ((#\w) info.read-eval-print.editor.command::forward-sexp)
           ((#\b) info.read-eval-print.editor.command::backward-sexp)
           ((#\G) info.read-eval-print.editor.command::end-of-buffer)
           ((#\$) info.read-eval-print.editor.command::end-of-line)
           ((#\x) info.read-eval-print.editor.command::delete-char)
           ((#\X) info.read-eval-print.editor.command::backward-delete-char)
           ((#\u) info.read-eval-print.editor.command::undo)
           ((:control #\r) info.read-eval-print.editor.command::redo)
           ((#\p) info.read-eval-print.editor.command::paste-below-cursor)
           ((#\g) ,(lambda () (push-temp-key-map *fundamental-mode-map* *g-key-map*)))
           ((#\y) ,(lambda () (push-temp-key-map *fundamental-mode-map* *y-key-map*)))
           ((:control #\w) ,(lambda () (push-temp-key-map *fundamental-mode-map* *ctl-w-key-map*))))
      do (set-key *fundamental-mode-map* :normal keyseq command))

(loop for (keyseq command)
      in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
           ((:control #\[) info.read-eval-print.editor.command::normal-mode)
           ((#\Esc) info.read-eval-print.editor.command::normal-mode))
      do (set-key *fundamental-mode-map* :insert keyseq command))


(defvar *g-key-map* (make-instance 'key-map :inherits (list *digit-argument-map*)))

(loop for (keyseq command)
      in `(((#\g) info.read-eval-print.editor.command::beginning-of-buffer))
      do (set-key *g-key-map* :normal keyseq command))

(defvar *y-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\y) info.read-eval-print.editor.command::yank-current-line))
      do (set-key *y-key-map* :normal keyseq command))

(defvar *ctl-w-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\h) ,(lambda () (window-h *editor*)))
           ((#\j) ,(lambda () (window-j *editor*)))
           ((#\k) ,(lambda () (window-k *editor*)))
           ((#\l) ,(lambda () (window-l *editor*))))
      do (set-key *ctl-w-key-map* :normal keyseq command))


(loop for (mode keyseq command)
      in `((:normal (:super #\e) info.read-eval-print.editor.command::eval-last-sexp)
           (:insert (:super #\e) info.read-eval-print.editor.command::eval-last-sexp))
      do (set-key *common-lisp-mode-map*  mode keyseq command))

