(in-package :info.read-eval-print.editor)

(defparameter *whitespace* '(#\Space #\Newline #\Return #\Tab #\Vt #\Nul))

(defun whitespace-p (char)
  (member char *whitespace*))

(defun sexp-end-p (char)
  (or (whitespace-p char)
      (char= char #\))))

(defun guess-file-encoding (path)
  (let ((buffer (make-array 8192 :initial-element 0
                                 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
      (read-sequence buffer s)
      (jp:make-encoding (jp:guess buffer :jp)))))

(defun read-file (path)
  (let ((external-format (or (guess-file-encoding path) :default))
        (buffer (make-array 8192 :element-type 'character)))
    (values
     (with-output-to-string (out)
       (with-open-file (in path :external-format external-format)
         (loop for len = (read-sequence buffer in)
               while (plusp len)
               do (write-sequence buffer out :end len))))
     external-format)))


(defun len=1 (list)
  (and (consp list)
       (null (cdr list))))

(defun len>1 (list)
  (consp (cdr list)))

(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))
