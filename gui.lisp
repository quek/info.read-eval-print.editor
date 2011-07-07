(in-package :info.read-eval-print.editor)

(defgeneric view-of (frame))

(defgeneric buffer-of (frame))


(defmethod initialize-instance :after ((frame frame) &rest args &key buffer)
  (declare (ignore args))
  (when buffer
    (setf (frame-of buffer) frame))
  (gobject:connect-signal (view-of frame) "key-press-event" 'buffer-text-view-key-press-event-cb)
  (gobject:connect-signal (view-of frame) "key-release-event" 'buffer-text-view-key-release-event-cb)
  (gobject:connect-signal (view-of frame) "grab-focus" 'view-grab-focus-cb))

(defmethod view-of ((text-view gtk:text-view))
  text-view)

(defmethod buffer-of ((frame frame))
  (gtk:text-view-buffer (view-of frame)))

(defmethod (setf buffer-of) (buffer (frame frame))
  (setf (frame-of buffer) frame
        (gtk:text-view-buffer (view-of frame)) buffer))

(defmethod status-text ((frame frame))
  (gtk:text-buffer-text (gtk:text-view-buffer (status-view-of frame))))

(defmethod (setf status-text) (value (frame frame))
  (setf (gtk:text-buffer-text (gtk:text-view-buffer (status-view-of frame)))
        (or value "")))

(defmethod update-status ((frame frame))
  (setf (status-text frame)
        (format nil "~a  ~(~a ~{~a~^ ~}~)"
                (name-of (buffer-of frame))
                (external-format-of (buffer-of frame))
                (enabled-modes-of (mode-of (buffer-of frame))))))

(defmethod focus ((frame frame))
  (gtk:widget-grab-focus (view-of frame)))

(defun make-frame (&key (buffer (make-instance 'buffer))
                     (show-line-numbers t))
  (let ((view (make-instance 'gtk:source-view
                             :show-line-numbers show-line-numbers
                             :wrap-mode :char))
        (status-view (make-instance 'gtk:source-view
                                    :buffer (make-instance 'buffer)
                                    :wrap-mode :char)))
    (iterate ((x (scan (list view status-view))))
      (gtk::widget-modify-font x (pango::pango-font-description-from-string *defualt-font*)))
    (gtk:let-ui (frame
             :var f
             :view view
             :status-view status-view
             (gtk:scrolled-window
              :hscrollbar-policy :automatic
              :vscrollbar-policy :automatic
              :shadow-type :etched-in
              (:expr view))
             :expand t
             :fill t
             (:expr status-view)
             :expand nil)
      (let ((status-buffer (gtk:source-view-buffer status-view)))
        (setf (buffer-of f) buffer
              (frame-of status-buffer) status-view
              (style-scheme buffer) *default-buffer-style-scheme*
              (style-scheme status-buffer) *default-status-style-scheme*)
        (update-status f)
        f))))



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


(defmethod (setf dispatch-table) (dispatch-table editor mode)
  (setf (cdr (assoc mode (dispatch-tables-of editor)))
        dispatch-table))

(defmethod dispatch-table (editor &optional (mode (mode-of editor)))
  (cdr (assoc mode (dispatch-tables-of editor))))

(defmethod dispatch-event ((dispatch-table dispatch-table) sender event-key)
  (let ((key-seq (sort-keyseq (event-key-to-keyseq event-key))))
    (awhen (gethash key-seq
                    (table-of dispatch-table)
                    (default-of dispatch-table))
      (funcall it)
      t)))

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
  (loop for x in (list* (gdk:event-key-keyval event-key)
                        (gdk:event-key-state event-key))
        if (eq :control-mask x)
          collect :control
        else if (eq :mod1-mask x)
               collect :meta
        else if (eq :super-mask x)
               collect :super
        else if (eq :hyper-mask x)
               collect :hyper
        else if (numberp x)
               collect (cond ((= #.(gdk:keyval-from-name "Return") x)
                              #\Return)
                             ((= #.(gdk:keyval-from-name "Escape") x)
                              #\Esc)
                             ((= #.(gdk:keyval-from-name "Tab") x)
                              #\Tab)
                             (t (gdk:keyval-to-char x)))))


(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let* ((keyseq (sort-keyseq (event-key-to-keyseq event-key)))
         (*frame* buffer-text-view)
         (*buffer* (current-buffer-of *editor*))
         (mode (mode-of *buffer*)))
    (aif (get-key-binding mode keyseq (mode-of *editor*))
         (multiple-value-bind (a b c) (funcall-with-mode mode it)
           (declare (ignore a b))
           (not c))
         (if (eq :normal (mode-of *editor*))
             ;; :normal モードでバインディングがないキーは無視。
             (constantly t)
             ;; :insert モードでバインディングがないキーは gtk のデフォルトの動作
             nil))))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

(defun view-grab-focus-cb (view)
  (let ((frame (gtk:widget-parent (gtk:widget-parent view))))
    (setf (current-frame-of *editor*) frame
          (current-buffer-of *editor*) (buffer-of frame))))

;; event は nil を返すとデフォルトのイベントが実行される。
(defun command-text-view-key-press-event-cb (command-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*frame* command-text-view)
        (*buffer* (command-buffer-of *editor*)))
    (dispatch-event dispatch-table command-text-view event-key)))


(defun command-text-view-key-release-event-cb (command-text-view event-key)
  (declare (ignore command-text-view event-key))
  nil)


(defun map-frame (editor function)
  (labels ((f (x)
             (typecase x
               (frame (list (funcall function x)))
               (t (loop for x in (gtk:container-children x)
                        nconc (f x))))))
    (f (top-frame-of editor))))

(defmethod editor-window-close ((editor editor) (frame frame))
  (when (closable-p editor)
    (let ((parent (gtk:widget-parent frame)))
      (gtk:container-remove parent frame)
      (setf (current-frame-of editor) (first-frame editor))
      (when (null (gtk:container-children parent))
        (gtk:container-remove (gtk:widget-parent parent) parent)))))

(defun closable-p (editor)
  (let ((count 0))
    (labels ((f (x)
               (typecase x
                 (frame
                    (when (< 1 (incf count))
                      (return-from closable-p t)))
                 (t
                    (loop for x in (gtk:container-children x)
                          thereis (f x))))))
      (f (top-frame-of editor)))))

(defun first-frame (editor)
  (labels ((f (x)
             (typecase x
               (frame x)
               (t
                  (loop for x in (gtk:container-children x)
                        thereis (f x))))))
    (f (top-frame-of editor))))

(defmethod editor-window-split ((editor editor) frame &optional (buffer (buffer-of frame)))
  (let ((views (top-frame-of editor))
        (new-frame (make-frame :buffer buffer)))
    (%editor-window-split views frame new-frame 'gtk:v-box)))

(defmethod editor-window-vsplit ((editor editor) frame &optional (buffer (buffer-of frame)))
  (let ((views (top-frame-of editor))
        (new-frame (make-frame :buffer buffer)))
    (%editor-window-split views frame new-frame 'gtk:h-box)))

(defgeneric %editor-window-split (wiews frame new-frame box-class)
  (:method ((views gtk:container) frame new-frame box-class)
    (labels ((f (fun other)
               (gtk:container-remove views frame)
               (when other (gtk:container-remove views other))
               (let ((new-box (make-instance box-class)))
                 (gtk:box-pack-start new-box frame)
                 (gtk:box-pack-start new-box new-frame)
                 (funcall fun views new-box)
                 (when other (funcall fun views other))
                 (gtk:widget-show new-box))
               t))
      (let ((children (gtk:container-children views)))
        ;; frame は car か cdr にいるはず。
        (cond ((eq frame (car children))
               (f #'gtk:box-pack-start (cadr children)))
              ((eq frame (cadr children))
               (f #'gtk:box-pack-end (car children)))
              (t
               (loop for x in children
                     thereis (%editor-window-split x frame new-frame box-class)))))))
  (:method (views frame new-frame box-class)
    nil))


(defun window-j (editor)
  (%window-j (current-frame-of editor)))

(defun %window-j (current)
  (let ((parent (gtk:widget-parent current)))
    (typecase parent
      (gtk:v-box (let* ((children (gtk:container-children parent))
                        (car (car children))
                        (cadr (cadr children)))
                   (if (eq car current)
                       (typecase cadr
                         (frame (focus cadr))
                         ((or gtk:v-box gtk:h-box)
                          (focus (find-depth-first-frame (gtk:container-children cadr)))))
                       (%window-j parent))))
      (gtk:h-box (%window-j parent)))))


(defun window-h (editor)
  (%window-h (current-frame-of editor)))

(defun %window-h (current)
  (let ((parent (gtk:widget-parent current)))
    (typecase parent
      (gtk:h-box (let* ((children (gtk:container-children parent))
                        (car (car children))
                        (cadr (cadr children)))
                   (if (eq cadr current)
                       (typecase car
                         (frame (focus car))
                         ((or gtk:v-box gtk:h-box)
                          (focus (rfind-depth-first-frame (gtk:container-children car)))))
                       (%window-h parent))))
      (gtk:v-box (%window-h parent)))))

(defun window-k (editor)
  (%window-k (current-frame-of editor)))

(defun %window-k (current)
  (let ((parent (gtk:widget-parent current)))
    (typecase parent
      (gtk:v-box (let* ((children (gtk:container-children parent))
                        (car (car children))
                        (cadr (cadr children)))
                   (if (eq cadr current)
                       (typecase car
                         (frame (focus car))
                         ((or gtk:v-box gtk:h-box)
                          (focus (rfind-depth-first-frame (gtk:container-children car)))))
                       (%window-k parent))))
      (gtk:h-box (%window-k parent)))))

(defun window-l (editor)
  (%window-l (current-frame-of editor)))

(defun %window-l (current)
  (let ((parent (gtk:widget-parent current)))
    (typecase parent
      (gtk:h-box (let* ((children (gtk:container-children parent))
                        (car (car children))
                        (cadr (cadr children)))
                   (if (eq car current)
                       (typecase cadr
                         (frame (focus cadr))
                         ((or gtk:v-box gtk:h-box)
                          (focus (find-depth-first-frame (gtk:container-children cadr)))))
                       (%window-l parent))))
      (gtk:v-box (%window-l parent)))))

(defun find-depth-first-frame (list)
  (loop for i in list
        thereis (and (typep i 'frame) i)
        thereis (find-depth-first-frame (gtk:container-children i))))

(defun rfind-depth-first-frame (list)
  (loop for i in (reverse list)
        thereis (and (typep i 'frame) i)
        thereis (find-depth-first-frame (gtk:container-children i))))

(defun open-info-frame (&optional (text ""))
  (with-slots (info-frame) *editor*
    (gtk:widget-show info-frame)
    (setf (text-of (buffer-of info-frame)) text)))

(defun close-info-frame ()
  (with-slots (info-frame) *editor*
    (gtk:widget-hide info-frame)))

(defun message (message)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer (command-view-of *editor*)))
        (princ-to-string message)))

(defvar *command-dispatch-table* (make-instance 'dispatch-table))
;; command
(loop for (keyseq command)
      in `(((:control #\c) normal-mode)
           ((:control #\[) normal-mode)
           ((#\Esc) normal-mode)
           ((:control #\m) run-command)
           ((#\Return) run-command)
           ((:control #\i) simple-completion)
           ((#\Tab) simple-completion))
      do (set-command *command-dispatch-table* keyseq command))


(defun main ()
  (gtk:with-main-loop
    (let ((frame (make-frame))
          (info-frame (make-frame :show-line-numbers nil)))
      (gtk:let-ui (gtk:gtk-window
                   :type :toplevel
                   :position :center
                   :title "Editor"
                   :default-width 300
                   :default-height 400
                   :var window
                   (gtk:v-box
                    (gtk:v-box
                     :var top-frame
                     (:expr frame))
                    :expand t
                    :fill t
                    (:expr info-frame)
                    :expand t
                    :fill t
                    (gtk:source-view :var command-view
                                     :buffer (make-instance 'buffer :name "command buffer")
                                     :wrap-mode :char)
                    :expand nil))
        (let ((cb (gtk:source-view-buffer command-view)))
          (setf (frame-of cb) command-view)
          (setf *editor* (make-instance 'editor
                                        :window window
                                        :info-frame info-frame
                                        :current-frame frame
                                        :top-frame top-frame
                                        :buffers (list (buffer-of frame))
                                        :current-buffer (buffer-of frame)
                                        :command-view command-view
                                        :command-buffer (gtk:source-view-buffer command-view))))

        (gobject:connect-signal window "destroy"
                                (lambda (w) (declare (ignore w)) (gtk:leave-gtk-main)))
        (gobject:connect-signal command-view "key-press-event" 'command-text-view-key-press-event-cb)
        (gobject:connect-signal command-view "key-release-event" 'command-text-view-key-release-event-cb)

        (gtk::widget-modify-font command-view (pango::pango-font-description-from-string *defualt-font*))

        (gtk:widget-show window)
        (gtk:widget-hide info-frame)))))

(defmacro with-editor (&body body)
  `(let* ((*buffer* (current-buffer-of *editor*))
          (*frame* (frame-of *buffer*)))
     ,@body))