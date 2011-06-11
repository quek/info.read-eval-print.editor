(in-package :info.read-eval-print.editor)


(defmacro define-command (&whole form name &body body)
  (multiple-value-bind (layer-arg layer qualifiers args method-body)
      (contextl::parse-method-body form body)
    (declare (ignore layer-arg layer qualifiers method-body))
    (let ((name (intern (symbol-name name) :info.read-eval-print.editor.command)))
      `(progn
         (define-layered-function ,name ,args)
         (define-layered-method ,name ,@body)))))

(defmacro define-command-alias (command &rest aliases)
  (let ((command (intern (symbol-name command) :info.read-eval-print.editor.command)))
    `(progn
       ,@(let ((x (scan aliases)))
           (collect `(setf (fdefinition ',(intern (symbol-name x)
                                                  :info.read-eval-print.editor.command))
                           (fdefinition ',command)))))))


(define-command command-mode ()
  (setf (mode-of *editor*) :command)
  (setf (text-buffer-text (text-view-buffer (command-view-of *editor*))) ":")
  (widget-grab-focus (command-view-of *editor*)))

(define-command normal-mode ()
  (setf (mode-of *editor*) :normal)
  (setf (text-buffer-text (text-view-buffer (command-view-of *editor*))) "")
  (focus (current-frame-of *editor*)))

(define-command i ()
  (setf (mode-of *editor*) :insert)
  (widget-grab-focus (current-frame-of *editor*)))

(define-command a ()
  (info.read-eval-print.editor.command::forward-char)
  (info.read-eval-print.editor.command::i))

(define-command o ()
  (info.read-eval-print.editor.command::end-of-line)
  (insert *buffer* (string #\Newline))
  (info.read-eval-print.editor.command::i))

(define-command e (path)
  (let* ((*frame* (current-frame-of *editor*))
         (*buffer* (make-instance 'buffer :frame *frame*)))
    (find-file *buffer* path)
    (setf (buffer-of *frame*) *buffer*)
    (update-status *frame*)
    (focus *frame*)))
(define-command-alias e edit)

(define-command q ()
  (object-destroy (window-of *editor*)))

(define-command run-command ()
  ":e /tmp/a.txt"
  (let* ((input (text-of *buffer*))
         (splited (ppcre:split "\\s" input :start 1 :limit 2)))
    (awhen (find-symbol (string-upcase (car splited))
                        :info.read-eval-print.editor.command)
      (info.read-eval-print.editor.command::normal-mode)
      (close-info-frame)
      (apply it (cdr splited)))))

(define-command digit-argument-n (n)
  (setf (digit-argument-of *buffer*) n))

(let ((*package* (find-package :info.read-eval-print.editor.command)))
  (loop for i from 0 to 9
        do (eval `(defun ,(sym 'info.read-eval-print.editor.command::digit-argument- i) ()
                    (info.read-eval-print.editor.command::digit-argument-n ,i)))))


(define-command split ()
  (window-split *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(define-command vsplit ()
  (window-vsplit *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(define-command close ()
  (window-close *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

#+nil
(define-command command-mode-completion ()
  (let* ((input (text-of *buffer*))
         (pos (text-iter-offset (iter-at-mark *buffer*)))
         (splited (ppcre:split "\\s" input :start 1 :limit 2))
         (cmd (car splited))
         (path (cadr splited)))
    (let ((files (directory (str path "*"))))
      (if (= 1 (length files))
          (setf (text-of *buffer*) (str ":" cmd " " (car files)))
          (progn
            (open-info-frame)
            (setf (text-of (buffer-of (info-frame-of *editor*)))
                  (with-output-to-string (out)
                    (iterate ((file (scan files)))
                      (format out "~a~%" file))))
            ;; (setf (text-of *buffer*) (str ":" cmd " " path " " files))
            (let ((iter (iter-at-mark *buffer*)))
              (setf (text-iter-offset iter) pos)
              (update-cursor *buffer* iter)))))))

(define-command simple-completion ()
  (let* ((input (text-of *buffer*))
         (pos (text-iter-offset (iter-at-mark *buffer*)))
         (splited (ppcre:split "\\s" input :start 1)))
    (if (len=1 splited)
        ;; コマンドの補完
        (let ((commands (car (swank:simple-completions (car splited) "info.read-eval-print.editor.command"))))
          (if (len=1 commands)
              (setf (text-of *buffer*) (str ":" (car commands)))
              (open-info-frame (with-output-to-string (out)
                                 (iterate ((x (scan commands)))
                                   (format out "~a~%" x))))))
        ;; きっとファイルの補完
        (let* ((butlast (butlast splited))
               (path (car (last splited)))
               (files (directory (str path "*"))))
          (if (len=1 files)
              (setf (text-of *buffer*)
                    (format nil "~{~a~^ ~}" `(":" ,@butlast ,(car files))))
              (progn
                (open-info-frame (with-output-to-string (out)
                                   (iterate ((file (scan files)))
                                     (format out "~a~%" file))))
                (let ((iter (iter-at-mark *buffer*)))
                  (setf (text-iter-offset iter) pos)
                  (update-cursor *buffer* iter))))))))


;; command
(loop for (keyseq command)
      in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
           ((:control #\[) info.read-eval-print.editor.command::normal-mode)
           ((#\Esc) info.read-eval-print.editor.command::normal-mode)
           ((:control #\m) info.read-eval-print.editor.command::run-command)
           ((#\Return) info.read-eval-print.editor.command::run-command)
           ((:control #\i) info.read-eval-print.editor.command::simple-completion)
           ((#\Tab) info.read-eval-print.editor.command::simple-completion))
      do (set-command *command-dispatch-table* keyseq command))
