(in-package :info.read-eval-print.editor)

(defmethod register-value (register &optional (char #\"))
  (gethash char (places-of register)))

(defmethod (setf register-value) (value register &optional (char #\"))
  (setf (gethash char (places-of register)) value))

(defmethod register-push (register value)
  (with-slots (place) register
   (let ((nums '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1)))
     (iterate ((n (scan nums))
               (m (previous (scan nums))))
       (when m
         (setf (gethash m place) (gethash n place)))
     (setf (gethash #\1 place) value)))))