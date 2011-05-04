;;;; info.read-eval-print.editor.lisp

(in-package :info.read-eval-print.editor)

;;; "info.read-eval-print.editor" goes here. Hacks and glory await!

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :info.read-eval-print.editor)))

(defvar *editor*)

(defclass* editor ()
  ((window)
   (buffer-text-view)
   (buffer-key-bindings)
   (command-text-view)
   (dispatch-tables `((:normal . ,(make-instance 'dispatch-table))
                      (:insert . ,(make-instance 'dispatch-table))
                      (:command . ,(make-instance 'dispatch-table))))
   (command-key-bindings)
   (mode :normal)))

(defmethod initialize-instance :after ((editor editor) &rest initargs)
  (declare (ignore initargs))
  (let ((normal (dispatch-table editor :normal)))
    (set-command normal '(#\;) 'info.read-eval-print.editor.command::command-mode)))


(defmethod dispatch-table (editor &optional (mode (mode-of editor)))
  (cdr (assoc mode (dispatch-tables-of editor))))

(defgeneric dispatch-event (dispatch-table sender event))

(defclass* dispatch-table ()
  ((table :initform (make-hash-table :test #'equal))))


(defmethod dispatch-event (dispatch-table sender event-key)
  (awhen (gethash (sort-keyseq (event-key-to-keyseq event-key))
                  (table-of dispatch-table))
    (funcall it)))

(defun sort-keyseq (keyseq)
  (sort (copy-seq keyseq)
        (lambda (a b)
          (string< (princ-to-string a)
                   (princ-to-string b)))))

(defmethod set-command (dispatch-table keyseq command)
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
  (list* (gdk:keyval-to-char (event-key-keyval event-key))
         (event-key-state event-key)))

(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*)))
    (dispatch-event dispatch-table buffer-text-view event-key)))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (cond ((= #.(keyval-from-name "Return") (event-key-keyval event-key))
         (run-command (subseq (text-buffer-text (text-view-buffer command-text-view))
                              1)))
        ((= #.(keyval-from-name "Escape") (event-key-keyval event-key))
         (setf (text-buffer-text (text-view-buffer (command-text-view-of *editor*)))
               "")
         (widget-grab-focus (buffer-text-view-of *editor*))
         t)
        (t nil)))

(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)

(defun run-command (line)
  (multiple-value-bind (command args) (parse-command line)
    (apply command args)
    (setf (text-buffer-text (text-view-buffer (command-text-view-of *editor*)))
          "")))

(defun parse-command (line)
  (let ((splited (ppcre:split "\\s" line)))
    (values (intern (string-upcase (car splited))
                    :info.read-eval-print.editor.command)
            (cdr splited))))


