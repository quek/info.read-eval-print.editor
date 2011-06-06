(in-package :info.read-eval-print.editor)


(defun anonymous-class-p (class)
  (null (class-name class)))

(defgeneric enable-mode (mode mode-to-enable &rest initargs)
  (:method (mode mode-to-enable &rest initargs)
    (let* ((current-class (class-of mode))
           (superclasses (cons(find-class mode-to-enable)
                              (if (anonymous-class-p current-class)
                                  (c2mop:class-direct-superclasses current-class)
                                  (list current-class))))
           (new-class (make-instance 'c2mop:standard-class
                                     :direct-superclasses superclasses)))
      (apply #'change-class mode new-class initargs))))

(defgeneric disable-mode (mode mode-to-disable &rest initargs)
  (:method (mode mode-to-disable &rest initargs)
    (let* ((current-class (class-of mode))
           (superclasses (remove (find-class mode-to-disable)
                                 (c2mop:class-direct-superclasses current-class)))
           (new-class (make-instance 'c2mop:standard-class
                                     :direct-superclasses superclasses)))
      (apply #'change-class mode new-class initargs))))

(defgeneric enabled-mode (mode)
  (:method (mode)
    (let ((class (class-of mode)))
      (if (anonymous-class-p class)
          (collect (class-name (scan (c2mop:class-direct-superclasses class))))
          (list (class-name class))))))

(defgeneric key-binding (mode-or-key-map keyseq &optional editor-mode)
  (:method-combination or))


(defgeneric key-map-table (key-map editor-mode)
  (:method (key-map (editor-mode (eql :normal)))
    (normal-of key-map))
  (:method (key-map (editor-mode (eql :insert)))
    (insert-of key-map)))

(defgeneric get-key (key-map editor-mode keyseq))

(defgeneric set-key (key-map editor-mode keyseq command &optional restore-temp-key-map))

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

(defgeneric push-temp-key-map (key-map temp-key-map)
  (:method (key-map temp-key-map)
    (push temp-key-map (temp-key-map-of key-map))))

(defgeneric pop-temp-key-map (key-map)
  (:method (key-map)
    (pop (temp-key-map-of key-map))))

(defclass* key-map ()
  ((normal (make-hash-table :test #'equal))
   (insert (make-hash-table :test #'equal))
   (inherits nil)
   (temp-key-map nil)))

(defmacro define-mode (mode (&rest super-modes)
                       (&rest slots)
                       &rest class-options)
  (let ((key-map (sym "*" mode "-MAP*")))
    `(progn
       (defvar ,key-map (make-instance 'key-map))
       (defclass* ,mode ,(or super-modes (list 'mode))
         ,slots
         (:default-initargs :key-map ,key-map)
         ,@class-options))))


(defclass* mode ()
  ((name nil)
   (key-map (make-instance 'key-map))))

(defmethod key-binding or ((mode mode) keyseq &optional (editor-mode (mode-of *editor*)))
  (get-key (key-map-of mode) editor-mode keyseq))


(defmethod key-map ((mode mode) editor-mode)
  (key-map (key-map-of mode) editor-mode))

(define-mode fundamental-mode ()
  ())

(define-mode lisp-mode ()
  ())

(define-mode common-lisp-mode (lisp-mode)
  ())

(pushnew '("\\.lisp$" . common-lisp-mode) *auto-mode-alist* :test #'equal)

(define-mode show-paren-mode ()
  ())

(defmethod print-object ((x mode) stream)
  (print-unreadable-object (x stream)
    (format stream "~a ~(~{~a~^ ~}~)" (name-of x) (enabled-mode x))))



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
           ((#\e) info.read-eval-print.editor.command::eval-last-sexp)
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




(let ((x (make-instance 'fundamental-mode :name "*scratch*")))
  (print x)
  (enable-mode x 'common-lisp-mode)
  (print x)
  (enable-mode x 'show-paren-mode)
  (print x)
  (disable-mode x 'common-lisp-mode)
  (print x)
  (disable-mode x 'show-paren-mode)
  (print x))
;;-> 
;;   #<*scratch* fundamental-mode> 
;;   #<*scratch* common-lisp-mode fundamental-mode> 
;;   #<*scratch* show-paren-mode common-lisp-mode fundamental-mode> 
;;   #<*scratch* show-paren-mode fundamental-mode> 
;;   #<*scratch* fundamental-mode> 
;;=> #<*scratch* fundamental-mode>

(let ((x (make-instance 'c2cl:standard-class
                        :direct-superclasses (list (find-class 'common-lisp-mode)
                                                   (find-class 'show-paren-mode)))))
  (print (c2mop:class-direct-superclasses x))
  (print (make-instance x :name "ま"))
  x)
;;-> 
;;   (#<STANDARD-CLASS COMMON-LISP-MODE> #<STANDARD-CLASS SHOW-PAREN-MODE>) 
;;   #<ま common-lisp-mode show-paren-mode> 
;;=> #<STANDARD-CLASS NIL {1008B5C691}>
