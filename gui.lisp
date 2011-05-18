(in-package :info.read-eval-print.editor)

(defvar *editor*)
(defvar *view*)

(defclass* view (v-box)
  ((buffer-view :reader t)
   (status-view :reader t))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((view view) &rest args)
  (declare (ignore args))
  (connect-signal (buffer-view-of view) "key-press-event" 'buffer-text-view-key-press-event-cb)
  (connect-signal (buffer-view-of view) "key-release-event" 'buffer-text-view-key-release-event-cb)
  (connect-signal (buffer-view-of view) "grab-focus" 'view-grab-focus-cb))

(defmethod buffer-view-of ((text-view text-view))
  text-view)

(defmethod buffer-of ((view view))
  (text-view-buffer (buffer-view-of view)))

(defmethod (setf buffer-of) (buffer (view view))
  (setf (view-of buffer) (buffer-view-of view)
        (text-view-buffer (buffer-view-of view)) buffer))

(defmethod status-text ((view view))
  (text-buffer-text (text-view-buffer (status-view-of view))))

(defmethod (setf status-text) (value (view view))
  (setf (text-buffer-text (text-view-buffer (status-view-of view)))
        (or value "")))

(defmethod update-status ((view view))
  (setf (status-text view) (name-of (buffer-of view))))

(defmethod focus ((view view))
  (widget-grab-focus (buffer-view-of view)))

(defun make-view (&key (buffer (make-instance 'buffer)))
  (let ((buffer-view (make-instance 'source-view
                                    :show-line-numbers t
                                    :wrap-mode :char))
        (status-view (make-instance 'source-view
                                    :buffer (make-instance 'buffer)
                                    :wrap-mode :char)))
    (let-ui (view
             :buffer-view buffer-view
             :status-view status-view
             :var view
             (scrolled-window
              :hscrollbar-policy :automatic
              :vscrollbar-policy :automatic
              :shadow-type :etched-in
              (:expr buffer-view))
             :expand t
             :fill t
             (:expr status-view)
             :expand nil)
      (let ((status-buffer (source-view-buffer status-view)))
        (setf (buffer-of view) buffer
              (view-of status-buffer) status-view
              (style-scheme buffer) *default-buffer-style-scheme*
              (style-scheme status-buffer) *default-status-style-scheme*)
        (update-status view)
        view))))



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
   (views)
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

(defun view-grab-focus-cb (view)
  (let ((view (widget-parent (widget-parent view))))
    (setf (current-view-of *editor*) view
          (current-buffer-of *editor*) (buffer-of view))))

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*view* command-text-view)
        (*buffer* (command-buffer-of *editor*)))
    (dispatch-event dispatch-table command-text-view event-key)))


(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)

(defparameter *default-status-style-scheme* "classic")


(defmethod window-split ((editor editor) view &optional (buffer (buffer-of view)))
  (let ((views (views-of editor))
        (new-view (make-view :buffer buffer)))
    (%window-split views view new-view 'v-box)))

(defmethod window-vsplit ((editor editor) view &optional (buffer (buffer-of view)))
  (let ((views (views-of editor))
        (new-view (make-view :buffer buffer)))
    (%window-split views view new-view 'h-box)))

(defgeneric %window-split (wiews view new-view box-class)
  (:method ((views container) view new-view box-class)
    (labels ((f (fun other)
               (container-remove views view)
               (when other (container-remove views other))
               (let ((new-box (make-instance box-class)))
                 (box-pack-start new-box view)
                 (box-pack-start new-box new-view)
                 (funcall fun views new-box)
                 (when other (funcall fun views other))
                 (widget-show new-box))
               t))
      (let ((children (container-children views)))
        ;; view は car か cdr にいるはず。
        (cond ((eq view (car children))
               (print 'car)
               (f #'box-pack-start (cadr children)))
              ((eq view (cadr children))
               (f #'box-pack-end (car children)))
              (t
               (loop for x in children
                     thereis (%window-split x view new-view box-class)))))))
  (:method (views view new-view box-class)
    nil))


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
                (v-box
                 :var views
                 (:expr buffer-view))
                :expand t
                :fill t
                (source-view :var command-view
                             :buffer (make-instance 'buffer :name "command buffer")
                             :wrap-mode :char)
                :expand nil))
        (let ((cb (source-view-buffer command-view)))
          (setf (view-of cb) command-view)
          (setf *editor* (make-instance 'editor
                                        :window window
                                        :views views
                                        :current-view buffer-view
                                        :buffers (list (buffer-of buffer-view))
                                        :current-buffer (buffer-of buffer-view)
                                        :command-view command-view
                                        :command-buffer (source-view-buffer command-view))))

        (connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
        (connect-signal command-view "key-press-event" 'command-text-view-key-press-event-cb)
        (connect-signal command-view "key-release-event" 'command-text-view-key-release-event-cb)

        (widget-show window)))))

#+nil
(gtk-source-completion-get-providers (source-view-completion (make-instance 'source-view)))
