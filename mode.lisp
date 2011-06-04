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

(defgeneric key-binding (mode keyseq)
  (:method-combination or))

(defclass* key-map ()
  ((map (make-hash-table :test #'equal))))

(defclass* mode ()
  ((name nil)
   (key-map (make-instance 'key-map))))

(defclass* fundamental-mode (mode)
  ())

(defclass* lisp-mode (mode)
  ())

(defclass* common-lisp-mode (lisp-mode)
  ())

(defclass* show-paren-mode (mode)
  ())

(defmethod print-object ((x mode) stream)
  (print-unreadable-object (x stream)
    (format stream "~a ~(~{~a~^ ~}~)" (name-of x) (enabled-mode x))))


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
;;   #<ま (COMMON-LISP-MODE SHOW-PAREN-MODE)> 
;;=> #<STANDARD-CLASS NIL {10048B11E1}>
