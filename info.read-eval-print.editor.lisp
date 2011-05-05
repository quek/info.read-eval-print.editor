;;;; info.read-eval-print.editor.lisp

(in-package :info.read-eval-print.editor)

;;; "info.read-eval-print.editor" goes here. Hacks and glory await!

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :info.read-eval-print.editor)))

(defvar *editor*)
(defvar *view*)
(defvar *buffer*)

(defclass* editor ()
  ((window)
   (buffer-text-view)
   (buffer-key-bindings)
   (command-text-view)
   (dispatch-tables `((:normal . ,(make-instance 'dispatch-table :default (constantly t)))
                      (:insert . ,(make-instance 'dispatch-table))
                      (:command . ,(make-instance 'dispatch-table))))
   (command-key-bindings)
   (mode :normal)))

(defmethod initialize-instance :after ((editor editor) &rest initargs)
  (declare (ignore initargs))
  (let ((table (dispatch-table editor :normal)))
    (set-command table '(#\;) 'info.read-eval-print.editor.command::command-mode)
    (set-command table '(#\i) 'info.read-eval-print.editor.command::insert-mode)
    (set-command table '(#\d) 'info.read-eval-print.editor.command::backward-char)
    (set-command table '(#\h) 'info.read-eval-print.editor.command::next-line)
    (set-command table '(#\t) 'info.read-eval-print.editor.command::previous-line)
    (set-command table '(#\n) 'info.read-eval-print.editor.command::forward-char))
  (let ((table (dispatch-table editor :insert)))
    (set-command table '(:control #\c) 'info.read-eval-print.editor.command::normal-mode))
  (let ((table (dispatch-table editor :command)))
    (set-command table '(:control #\c) 'info.read-eval-print.editor.command::normal-mode)
    (set-command table '(#\Esc) 'info.read-eval-print.editor.command::normal-mode)
    (set-command table '(:control #\m) 'info.read-eval-print.editor.command::run-command)
    (set-command table '(#\Return) 'info.read-eval-print.editor.command::run-command)))



(defmethod dispatch-table (editor &optional (mode (mode-of editor)))
  (cdr (assoc mode (dispatch-tables-of editor))))

(defgeneric dispatch-event (dispatch-table sender event))

(defclass* dispatch-table ()
  ((table (make-hash-table :test #'equal))
   (default nil)))


(defmethod dispatch-event (dispatch-table sender event-key)
  (let ((*view* sender)
        (*buffer* (text-view-buffer sender)))
   (awhen (gethash (sort-keyseq (event-key-to-keyseq event-key))
                   (table-of dispatch-table)
                   (default-of dispatch-table))
     (funcall it)
     t)))

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
        else if (keywordp x)
               collect x
        else if (= #.(gdk:keyval-from-name "Return") x)
               collect #\Return
        else
          collect (gdk:keyval-to-char x)))

(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*)))
    (dispatch-event dispatch-table buffer-text-view event-key)))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*)))
    (dispatch-event dispatch-table command-text-view event-key)))


(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)
