(in-package :info.read-eval-print.editor)

(defvar *editor*)
(defvar *frame*)
(defparameter *default-status-style-scheme* "classic")

(defgeneric view-of (frame))

(defgeneric buffer-of (frame))

(defclass* frame (v-box)
  ((view :reader t)
   (status-view :reader t))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((frame frame) &rest args &key buffer)
  (declare (ignore args))
  (when buffer
    (setf (frame-of buffer) frame))
  (connect-signal (view-of frame) "key-press-event" 'buffer-text-view-key-press-event-cb)
  (connect-signal (view-of frame) "key-release-event" 'buffer-text-view-key-release-event-cb)
  (connect-signal (view-of frame) "grab-focus" 'view-grab-focus-cb))

(defmethod view-of ((text-view text-view))
  text-view)

(defmethod buffer-of ((frame frame))
  (text-view-buffer (view-of frame)))

(defmethod (setf buffer-of) (buffer (frame frame))
  (setf (frame-of buffer) (view-of frame)
        (text-view-buffer (view-of frame)) buffer))

(defmethod status-text ((frame frame))
  (text-buffer-text (text-view-buffer (status-view-of frame))))

(defmethod (setf status-text) (value (frame frame))
  (setf (text-buffer-text (text-view-buffer (status-view-of frame)))
        (or value "")))

(defmethod update-status ((frame frame))
  (setf (status-text frame)
        (format nil "~a  ~a"
                (name-of (buffer-of frame))
                (string-downcase (external-format-of (buffer-of frame))))))

(defmethod focus ((frame frame))
  (widget-grab-focus (view-of frame)))

(defun make-frame (&key (buffer (make-instance 'buffer))
                     (show-line-numbers t))
  (let ((view (make-instance 'source-view
                             :show-line-numbers show-line-numbers
                             :wrap-mode :char))
        (status-view (make-instance 'source-view
                                    :buffer (make-instance 'buffer)
                                    :wrap-mode :char)))
    (let-ui (frame
             :var f
             :view view
             :status-view status-view
             (scrolled-window
              :hscrollbar-policy :automatic
              :vscrollbar-policy :automatic
              :shadow-type :etched-in
              (:expr view))
             :expand t
             :fill t
             (:expr status-view)
             :expand nil)
      (let ((status-buffer (source-view-buffer status-view)))
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

(defvar *normal-ctl-w-dispatch-table*
  (make-instance 'temporary-dispatch-table
                 :mode :normal
                 :dispatch-table-to-restore *normal-dispatch-table*
                 :default (constantly t)))


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
  (let ((key-seq (sort-keyseq (event-key-to-keyseq event-key))))
    (p key-seq)
    (awhen (gethash key-seq
                    (table-of dispatch-table)
                    (default-of dispatch-table))
      (p it)
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
  (loop for x in (list* (event-key-keyval event-key)
                        (event-key-state event-key))
        if (eq :control-mask x)
          collect :control
        else if (numberp x)
               collect (cond ((= #.(gdk:keyval-from-name "Return") x)
                              #\Return)
                             ((= #.(gdk:keyval-from-name "Escape") x)
                              #\Esc)
                             ((= #.(gdk:keyval-from-name "Tab") x)
                              #\Tab)
                             (t (gdk:keyval-to-char x)))))

(defun buffer-text-view-key-press-event-cb (buffer-text-view event-key)
  (let ((dispatch-table (dispatch-table *editor*))
        (*frame* buffer-text-view)
        (*buffer* (current-buffer-of *editor*)))
    (dispatch-event dispatch-table buffer-text-view event-key)))


(defun buffer-text-view-key-release-event-cb (buffer-text-view event-key)
  (declare (ignore buffer-text-view event-key))
  nil)

(defun view-grab-focus-cb (view)
  (let ((frame (widget-parent (widget-parent view))))
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
               (t (loop for x in (container-children x)
                        nconc (f x))))))
    (f (top-frame-of editor))))

(defmethod window-close ((editor editor) (frame frame))
  (when (closable-p editor)
    (let ((parent (widget-parent frame)))
      (container-remove parent frame)
      (setf (current-frame-of editor) (first-frame editor))
      (when (null (container-children parent))
        (container-remove (widget-parent parent) parent)))))

(defun closable-p (editor)
  (let ((count 0))
    (labels ((f (x)
               (typecase x
                 (frame
                    (when (< 1 (incf count))
                      (return-from closable-p t)))
                 (t
                    (loop for x in (container-children x)
                          thereis (f x))))))
      (f (top-frame-of editor)))))

(defun first-frame (editor)
  (labels ((f (x)
             (typecase x
               (frame x)
               (t
                  (loop for x in (container-children x)
                        thereis (f x))))))
    (f (top-frame-of editor))))

(defmethod window-split ((editor editor) frame &optional (buffer (buffer-of frame)))
  (let ((views (top-frame-of editor))
        (new-frame (make-frame :buffer buffer)))
    (%window-split views frame new-frame 'v-box)))

(defmethod window-vsplit ((editor editor) frame &optional (buffer (buffer-of frame)))
  (let ((views (top-frame-of editor))
        (new-frame (make-frame :buffer buffer)))
    (%window-split views frame new-frame 'h-box)))

(defgeneric %window-split (wiews frame new-frame box-class)
  (:method ((views container) frame new-frame box-class)
    (labels ((f (fun other)
               (container-remove views frame)
               (when other (container-remove views other))
               (let ((new-box (make-instance box-class)))
                 (box-pack-start new-box frame)
                 (box-pack-start new-box new-frame)
                 (funcall fun views new-box)
                 (when other (funcall fun views other))
                 (widget-show new-box))
               t))
      (let ((children (container-children views)))
        ;; frame は car か cdr にいるはず。
        (cond ((eq frame (car children))
               (f #'box-pack-start (cadr children)))
              ((eq frame (cadr children))
               (f #'box-pack-end (car children)))
              (t
               (loop for x in children
                     thereis (%window-split x frame new-frame box-class)))))))
  (:method (views frame new-frame box-class)
    nil))


(defun window-j (editor)
  (%window-j (current-frame-of editor)))

(defun %window-j (current)
  (let ((parent (widget-parent current)))
    (typecase parent
      (v-box (let* ((children (container-children parent))
                    (car (car children))
                    (cadr (cadr children)))
               (if (eq car current)
                   (typecase cadr
                     (frame (focus cadr))
                     ((or v-box h-box) (focus (find-depth-first-frame (container-children cadr)))))
                   (%window-j parent))))
      (h-box (%window-j parent)))))


(defun window-h (editor)
  (%window-h (current-frame-of editor)))

(defun %window-h (current)
  (let ((parent (widget-parent current)))
    (typecase parent
      (h-box (let* ((children (container-children parent))
                    (car (car children))
                    (cadr (cadr children)))
               (if (eq cadr current)
                   (typecase car
                     (frame (focus car))
                     ((or v-box h-box) (focus (rfind-depth-first-frame (container-children car)))))
                   (%window-h parent))))
      (v-box (%window-h parent)))))

(defun window-k (editor)
  (%window-k (current-frame-of editor)))

(defun %window-k (current)
  (let ((parent (widget-parent current)))
    (typecase parent
      (v-box (let* ((children (container-children parent))
                    (car (car children))
                    (cadr (cadr children)))
               (if (eq cadr current)
                   (typecase car
                     (frame (focus car))
                     ((or v-box h-box) (focus (rfind-depth-first-frame (container-children car)))))
                   (%window-k parent))))
      (h-box (%window-k parent)))))

(defun window-l (editor)
  (%window-l (current-frame-of editor)))

(defun %window-l (current)
  (let ((parent (widget-parent current)))
    (typecase parent
      (h-box (let* ((children (container-children parent))
                    (car (car children))
                    (cadr (cadr children)))
               (if (eq car current)
                   (typecase cadr
                     (frame (focus cadr))
                     ((or v-box h-box) (focus (find-depth-first-frame (container-children cadr)))))
                   (%window-l parent))))
      (v-box (%window-l parent)))))

(defun find-depth-first-frame (list)
  (loop for i in list
        thereis (and (typep i 'frame) i)
        thereis (find-depth-first-frame (container-children i))))

(defun rfind-depth-first-frame (list)
  (loop for i in (reverse list)
        thereis (and (typep i 'frame) i)
        thereis (find-depth-first-frame (container-children i))))

(defun open-info-frame ()
  (with-slots (info-frame) *editor*
    (widget-show info-frame)))

(defun close-info-frame ()
  (with-slots (info-frame) *editor*
    (widget-hide info-frame)))

(defun main ()
  (with-main-loop
    (let ((frame (make-frame))
          (info-frame (make-frame :show-line-numbers nil)))
      (let-ui (gtk-window
               :type :toplevel
               :position :center
               :title "Editor"
               :default-width 300
               :default-height 400
               :var window
               (v-box
                (v-box
                 :var top-frame
                 (:expr frame))
                :expand t
                :fill t
                (:expr info-frame)
                :expand t
                :fill t
                (source-view :var command-view
                             :buffer (make-instance 'buffer :name "command buffer")
                             :wrap-mode :char)
                :expand nil))
        (let ((cb (source-view-buffer command-view)))
          (setf (frame-of cb) command-view)
          (setf *editor* (make-instance 'editor
                                        :window window
                                        :info-frame info-frame
                                        :current-frame frame
                                        :top-frame top-frame
                                        :buffers (list (buffer-of frame))
                                        :current-buffer (buffer-of frame)
                                        :command-view command-view
                                        :command-buffer (source-view-buffer command-view))))

        (connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
        (connect-signal command-view "key-press-event" 'command-text-view-key-press-event-cb)
        (connect-signal command-view "key-release-event" 'command-text-view-key-release-event-cb)

        (widget-show window)
        (widget-hide info-frame)))))
