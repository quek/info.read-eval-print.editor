;;;; info.read-eval-print.editor.lisp

(in-package :info.read-eval-print.editor)

;;; "info.read-eval-print.editor" goes here. Hacks and glory await!

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :info.read-eval-print.editor)))

(defvar *editor*)
(defvar *view*)
(defvar *buffer*)

(defgeneric dispatch-event (dispatch-table sender event))
(defgeneric restore-dispatch-table (temporary-dispatch-table))

(defclass* dispatch-table ()
  ((table (make-hash-table :test #'equal))
   (default nil)))

(defclass* temporary-dispatch-table (dispatch-table)
  ((dispatch-table-to-restore)
   (mode)))

(defmethod restore-dispatch-table ((self temporary-dispatch-table))
  (setf (dispatch-table *editor* (mode-of self))
        (dispatch-table-to-restore-of self)))


(defvar *normal-dispatch-table*  (make-instance 'dispatch-table :default (constantly t)))
(defvar *insert-dispatch-table*  (make-instance 'dispatch-table))
(defvar *command-dispatch-table* (make-instance 'dispatch-table))

(defvar *normal-g-dispatch-table*
  (make-instance 'temporary-dispatch-table
                 :mode :normal
                 :dispatch-table-to-restore *normal-dispatch-table*
                 :default (constantly t)))



(defclass* editor ()
  ((window)
   (buffer-text-view)
   (buffer-key-bindings)
   (command-text-view)
   (dispatch-tables `((:normal . ,*normal-dispatch-table*)
                      (:insert . ,*insert-dispatch-table*)
                      (:command . ,*command-dispatch-table*)))
   (command-key-bindings)
   (mode :normal)))

(defmethod (setf dispatch-table) (dispatch-table editor mode)
  (setf (cdr (assoc mode (dispatch-tables-of editor)))
        dispatch-table))

(defmethod dispatch-table (editor &optional (mode (mode-of editor)))
  (cdr (assoc mode (dispatch-tables-of editor))))


(defmethod dispatch-event ((dispatch-table dispatch-table) sender event-key)
  (awhen (gethash (sort-keyseq (event-key-to-keyseq event-key))
                  (table-of dispatch-table)
                  (default-of dispatch-table))
    (funcall it)
    t))

(defmethod dispatch-event :around ((dispatch-table temporary-dispatch-table) sender event-key)
  (aprog1 (call-next-method)
    (when it
      (restore-dispatch-table dispatch-table))))

(defun sort-keyseq (keyseq)
  (sort (copy-seq keyseq)
        (lambda (a b)
          (string< (princ-to-string a)
                   (princ-to-string b)))))

(defmethod set-command (dispatch-table keyseq command)
  (loop for x in keyseq
        if (and (keywordp x)
                (not (member x '(:control :alt :shif :super :hyper))))
          do (error "invalid keyseq ~a." keyseq))
  (setf (gethash (sort-keyseq keyseq) (table-of dispatch-table)) command))

(defun main ()
  (with-main-loop
    (let ((builder (make-instance 'builder)))
      (builder-add-from-file builder (namestring (merge-pathnames "gtk.ui" *src-location*)))
      (let* ((window (builder-get-object builder "main_window"))
             (buffer-text-view (builder-get-object builder "buffer_text_view"))
             (command-text-view (builder-get-object builder "command_text_view")))
        (setf *editor* (make-instance 'editor
                                      :window window
                                      :buffer-text-view buffer-text-view
                                      :command-text-view command-text-view))
        (builder-connect-signals-simple
         builder
         `(("buffer_text_view_key_press_event_cb"
            buffer-text-view-key-press-event-cb)
           ("buffer_text_view_key_release_event_cb"
            buffer-text-view-key-release-event-cb)
           ("command_text_view_key_press_event_cb"
            command-text-view-key-press-event-cb)
           ("command_text_view_key_release_event_cb"
            command-text-view-key-release-event-cb)))
        (connect-signal window
                        "destroy"
                        (lambda (w) (declare (ignore w)) (leave-gtk-main)))
        (widget-show window)))))

(defun event-key-to-keyseq (event-key)
  (loop for x in (list* (event-key-keyval event-key)
                        (event-key-state event-key))
        if (eq :control-mask x)
          collect :control
        else if (numberp x)
               collect (cond ((= #.(gdk:keyval-from-name "Return") x)
                              #\Return)
                             (t (gdk:keyval-to-char x)))))

(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*view* buffer-text-view)
        (*buffer* (text-view-buffer buffer-text-view)))
    (dispatch-event dispatch-table buffer-text-view event-key)))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*view* command-text-view)
        (*buffer* (text-view-buffer command-text-view)))
    (dispatch-event dispatch-table command-text-view event-key)))


(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)
