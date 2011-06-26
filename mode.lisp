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

(defun get-key-from-inherits (key-map editor-mode keyseq)
  (collect-first (get-key (scan (inherits-of key-map)) editor-mode keyseq)))

(defmethod get-key (key-map editor-mode keyseq)
  (aif (temp-key-map-of key-map)
       (let ((temp-key-map (car it)))
         (aif (gethash keyseq (key-map-table temp-key-map editor-mode))
              (progn (when (cdr it)
                       (pop-temp-key-map key-map))
                     (car it))
              (aif (get-key-from-inherits temp-key-map editor-mode keyseq)
                   it
                   (progn
                     (pop-temp-key-map key-map)
                     nil))))
       (aif (gethash keyseq (key-map-table key-map editor-mode))
            (car it)
            (get-key-from-inherits key-map editor-mode keyseq))))

(defmethod set-key (key-map editor-mode keyseq command &optional (restore-temp-key-map t))
  (setf (gethash keyseq (key-map-table key-map editor-mode))
        (cons command restore-temp-key-map)))

(defmethod set-key :around (key-map editor-mode keyseq command &optional (restore-temp-key-map t))
  (loop for x in keyseq
        if (and (keywordp x)
                (not (member x '(:control :meta :shif :super :hyper))))
          do (error "invalid keyseq ~a." keyseq))
  (call-next-method key-map editor-mode keyseq command restore-temp-key-map))

(defmethod push-temp-key-map (key-map temp-key-map)
  (push temp-key-map (temp-key-map-of key-map)))

(defmethod pop-temp-key-map (key-map)
  (pop (temp-key-map-of key-map)))



(define-layered-class mode ()
  ((enabled-modes :initarg :enabled-modes
                  :initform `(common-lisp-mode fundamental-mode)
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

(defmethod print-object ((x mode) stream)
  (print-unreadable-object (x stream)
    (format stream "~a ~(~{~a~^ ~}~)" (name-of x) (enabled-modes-of x))))


(defun add-auto-mode-alist (mode &rest regexp)
  (iterate ((x (scan regexp)))
    (pushnew `(,x . ,mode) *auto-mode-alist* :test #'equal))
  *auto-mode-alist*)

(defun remove-auto-mode-alist (regexp)
  (setf *auto-mode-alist*
        (delete regexp *auto-mode-alist* :key #'car :test #'equal)))



