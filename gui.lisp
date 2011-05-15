(in-package :info.read-eval-print.editor)

(defvar *editor*)
(defvar *view*)


(defclass* view (source-view)
  ((box)
   (status-view))
  (:metaclass gobject-class))

(defmethod buffer-of ((view view))
  (text-view-buffer view))

(defmethod (setf buffer-of) (buffer (view view))
  (setf (view-of buffer) view
        (text-view-buffer view) buffer))

(defmethod status-text ((view view))
  (text-buffer-text (text-view-buffer (status-view-of view))))

(defmethod (setf status-text) (value (view view))
  (setf (text-buffer-text (text-view-buffer (status-view-of view)))
        value))

(defmethod update-status ((view view))
  (setf (status-text view) (name-of (buffer-of view))))



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

(defvar *normal-y-dispatch-table*
  (make-instance 'temporary-dispatch-table
                 :mode :normal
                 :dispatch-table-to-restore *normal-dispatch-table*
                 :default (constantly t)))


(defclass* editor ()
  ((window)
   (view-box)
   (views nil)
   (current-view nil)
   (buffers nil)
   (buffer-key-bindings)
   (current-buffer)
   (command-buffer)
   (command-view)
   (dispatch-tables `((:normal . ,*normal-dispatch-table*)
                      (:insert . ,*insert-dispatch-table*)
                      (:command . ,*command-dispatch-table*)))
   (command-key-bindings)
   (mode :normal :type (member :normal :insert :command))))

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


(defun event-key-to-keyseq (event-key)
  (loop for x in (list* (event-key-keyval event-key)
                        (event-key-state event-key))
        if (eq :control-mask x)
          collect :control
        else if (numberp x)
               collect (cond ((= #.(gdk:keyval-from-name "Return") x)
                              #\Return)
                             ((= #.(gdk:keyval-from-name "Escape") x)
                              #\Esc)
                             (t (gdk:keyval-to-char x)))))

(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*view* buffer-text-view)
        (*buffer* (current-buffer-of *editor*)))
    (dispatch-event dispatch-table buffer-text-view event-key)))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*view* command-text-view)
        (*buffer* (command-buffer-of *editor*)))
    (dispatch-event dispatch-table command-text-view event-key)))


(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)

(defparameter *default-buffer-style-scheme* "oblivion")
(defparameter *default-status-style-scheme* "classic")

(defun make-view ()
  (let-ui (v-box
           :var view-box
           (scrolled-window
            :hscrollbar-policy :automatic
            :vscrollbar-policy :automatic
            :shadow-type :etched-in
            (view :var view :show-line-numbers t :wrap-mode :char))
           :expand t
           :fill t
           (source-view :var status-view
                        :buffer (make-instance 'buffer)
                        :wrap-mode :char)
           :expand nil)
    (make-instance 'source-view )
    (let ((source-buffer (make-instance 'buffer))
          (status-buffer (source-view-buffer status-view)))
      (setf (buffer-of view) source-buffer
            (box-of view) view-box
            (status-view-of view) status-view
            (view-of status-buffer) status-view
            (style-scheme source-buffer) *default-buffer-style-scheme*
            (style-scheme status-buffer) *default-status-style-scheme*)
      view)))

(defmethod window-split ((editor editor) view)
  )


(defun main ()
  (with-main-loop
    (let ((buffer-view (make-view)))
      (let-ui (gtk-window
               :type :toplevel
               :position :center
               :title "Editor"
               :default-width 300
               :default-height 400
               :var window
               (v-box
                :var view-box
                (:expr (box-of buffer-view))
                :expand t
                :fill t
                (source-view :var command-view
                             :buffer (make-instance 'buffer :name "command buffer")
                             :wrap-mode :char)
                :expand nil))
        (let ((cb (source-view-buffer command-view)))
          (setf (view-of cb) command-view
                (style-scheme cb) *default-buffer-style-scheme*)
          (setf *editor* (make-instance 'editor
                                        :window window
                                        :view-box view-box
                                        :views (list buffer-view)
                                        :current-view buffer-view
                                        :buffers (list (buffer-of buffer-view))
                                        :current-buffer (source-view-buffer buffer-view)
                                        :command-view command-view
                                        :command-buffer (source-view-buffer command-view))))

        (connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
        (connect-signal buffer-view "key-press-event" 'buffer-text-view-key-press-event-cb)
        (connect-signal buffer-view "key-release-event" 'buffer-text-view-key-release-event-cb)
        (connect-signal command-view "key-press-event" 'command-text-view-key-press-event-cb)
        (connect-signal command-view "key-release-event" 'command-text-view-key-release-event-cb)

        (widget-show window)))))
