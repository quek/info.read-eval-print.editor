(in-package :info.read-eval-print.editor)

(defclass* editor ()
  ((window)
   (info-frame)
   (current-frame nil)
   (top-frame nil)
   (buffers nil)
   (buffer-key-bindings)
   (current-buffer)
   (command-buffer)
   (command-view)
   (dispatch-tables `((:command . ,*command-dispatch-table*)))
   (command-key-bindings)
   (mode :normal :type (member :normal :insert :command))
   (register (make-instance 'register))))

(defclass* frame (v-box)
  ((view :reader t)
   (status-view :reader t))
  (:metaclass gobject-class))

(defclass* buffer (source-buffer)
  ((frame)
   (name nil)
   (file nil)
   (digit-argument nil :accessor nil)
   (external-format :utf-8)
   (mode (make-instance 'mode)))
  (:metaclass gobject-class))

(defclass* register ()
  ((places (make-hash-table :test #'eql))))
