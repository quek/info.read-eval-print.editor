(in-package :info.read-eval-print.editor)


(define-symbol-macro *digit-argument*
    (or (digit-argument-of *buffer*) 1))


(defmacro define-command (&whole form name &body body)
  (multiple-value-bind (layer-arg layer qualifiers args method-body)
      (contextl::parse-method-body form body)
    (declare (ignore layer-arg layer qualifiers method-body))
    `(progn
       (unless (ignore-errors (layered-function-definer ',name))
         (eval-always
           (import ',name :info.read-eval-print.editor.command)
           (export ',name :info.read-eval-print.editor.command))
         (define-layered-function ,name ,(let ((x (scan args)))
                                           (collect (if (consp x)
                                                        (car x)
                                                        x)))))
       (define-layered-method ,name ,@body))))

(defmacro define-command-alias (command &rest aliases)
  `(progn
     ,@(let ((x (scan aliases)))
         (collect `(progn
                     (setf (fdefinition ',x)
                           (fdefinition ',command))
                     (import ',x :info.read-eval-print.editor.command)
                     (export ',x :info.read-eval-print.editor.command))))))

(define-command command-mode ()
  (setf (mode-of *editor*) :command)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer (command-view-of *editor*))) ":")
  (gtk:widget-grab-focus (command-view-of *editor*)))

(define-command normal-mode ()
  (setf (mode-of *editor*) :normal)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer (command-view-of *editor*))) "")
  (focus (current-frame-of *editor*)))

(define-command i ()
  (setf (mode-of *editor*) :insert)
  (gtk:widget-grab-focus (current-frame-of *editor*)))

(define-command a ()
  (forward-char)
  (i))

(define-command o ()
  (end-of-line)
  (buffer-insert *buffer* (string #\Newline))
  (i))

(define-command e (path)
  (let* ((*frame* (current-frame-of *editor*))
         (*buffer* (make-instance 'buffer :frame *frame*)))
    (find-file *buffer* path)
    (setf (buffer-of *frame*) *buffer*)
    (update-status *frame*)
    (focus *frame*)))
(define-command-alias e edit)

(define-command q ()
  (gtk:object-destroy (window-of *editor*)))
(define-command-alias q quit)

(define-command run-command ()
  ":e /tmp/a.txt"
  (let* ((input (text-of *buffer*))
         (splited (ppcre:split "\\s" input :start 1 :limit 2)))
    (awhen (find-symbol (string-upcase (car splited))
                        :info.read-eval-print.editor.command)
      (normal-mode)
      (close-info-frame)
      (apply it (cdr splited)))))


(define-command digit-argument-n (n)
  (setf (digit-argument-of *buffer*) n))

(loop for i from 0 to 9
      do (eval `(defun ,(sym 'digit-argument- i) ()
                  (digit-argument-n ,i))))


(define-command info.read-eval-print.editor.command::split ()
  (editor-window-split *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(define-command info.read-eval-print.editor.command::vsplit ()
  (editor-window-vsplit *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(define-command info.read-eval-print.editor.command::close ()
  (editor-window-close *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(defun directory* (path)
  (remove-duplicates
   (sort (append (directory (str path "*"))
                 (directory (str path "*.*")))
         #'string< :key #'namestring)
   :test #'equal))

(define-command simple-completion ()
  (let* ((input (text-of *buffer*))
         (splited (ppcre:split "\\s" input :start 1)))
    (if (len=1 splited)
        ;; コマンドの補完
        (let ((commands (car (swank:simple-completions (car splited) "info.read-eval-print.editor.command"))))
          (if (len=1 commands)
              (setf (text-of *buffer*) (str ":" (car commands)))
              (open-info-frame (collect-append 'string
                                               (format nil "~a~%" (scan commands))))))
        ;; きっとファイルの補完
        (let* ((butlast (butlast splited))
               (path (car (last splited)))
               (files (directory* path)))
          (cond ((len=1 files)
                 (setf (text-of *buffer*)
                       (format nil ":~{~a~^ ~}" `(,@butlast ,(car files)))))
                ((len>1 files)
                 (progn
                   (open-info-frame (collect-append 'string
                                                    (format nil "~a~%" (scan files))))
                   (let ((file (with-output-to-string (out)
                                 (block nil
                                   (apply #'map nil (lambda (&rest xs)
                                                      (if (apply #'char= xs)
                                                          (write-char (car xs) out)
                                                          (return)))
                                          (mapcar #'namestring files))))))
                     (when (< (length path) (length file))
                       (progn (setf (text-of *buffer*)
                                    (format nil ":~{~a~^ ~}" `(,@butlast ,file))))))))
                (t nil))))))
