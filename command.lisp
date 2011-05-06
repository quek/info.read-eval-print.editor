(in-package :info.read-eval-print.editor)


(defun info.read-eval-print.editor.command::command-mode ()
  (setf (mode-of *editor*) :command)
  (setf (text-buffer-text (text-view-buffer (command-text-view-of *editor*))) ":")
  (widget-grab-focus (command-text-view-of *editor*)))

(defun info.read-eval-print.editor.command::normal-mode ()
  (setf (mode-of *editor*) :normal)
  (setf (text-buffer-text (text-view-buffer (command-text-view-of *editor*))) "")
  (widget-grab-focus (buffer-text-view-of *editor*)))

(defun info.read-eval-print.editor.command::insert-mode ()
  (setf (mode-of *editor*) :insert)
  (widget-grab-focus (buffer-text-view-of *editor*)))


(defun info.read-eval-print.editor.command::e (path)
  (let ((*buffer* (current-buffer-of *editor*)))
    (find-file *buffer* path)
    (widget-grab-focus (view-of *buffer*))))


(defun info.read-eval-print.editor.command::q ()
  (object-destroy (window-of *editor*)))

(defun info.read-eval-print.editor.command::run-command ()
  ":open /tmp/a.txt"
  (let* ((input (text-of *buffer*))
         (splited (ppcre:split "\\s" input :start 1 :limit 2)))
    (awhen (find-symbol (string-upcase (car splited))
                        :info.read-eval-print.editor.command)
      (info.read-eval-print.editor.command::normal-mode)
      (apply it (cdr splited)))))


(loop for (keyseq command)
        in `(((#\;) info.read-eval-print.editor.command::command-mode)
             ((#\i) info.read-eval-print.editor.command::insert-mode)
             ((#\d) info.read-eval-print.editor.command::backward-char)
             ((#\h) info.read-eval-print.editor.command::next-line)
             ((#\t) info.read-eval-print.editor.command::previous-line)
             ((#\n) info.read-eval-print.editor.command::forward-char)
             ((#\w) info.read-eval-print.editor.command::forward-sexp)
             ((#\b) info.read-eval-print.editor.command::backward-sexp)
             ((#\G) info.read-eval-print.editor.command::end-of-buffer)
             ((#\g) ,(lambda () (setf (dispatch-table *editor* :normal) *normal-g-dispatch-table*)))
             ((#\e) info.read-eval-print.editor.command::eval-last-sexp))
      do (set-command *normal-dispatch-table* keyseq command))

(loop for (keyseq command)
        in `(((:control #\c) info.read-eval-print.editor.command::normal-mode))
      do (set-command *insert-dispatch-table* keyseq command))

(loop for (keyseq command)
        in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
             ((#\Esc) info.read-eval-print.editor.command::normal-mode)
             ((:control #\m) info.read-eval-print.editor.command::run-command)
             ((#\Return) info.read-eval-print.editor.command::run-command))
      do (set-command *command-dispatch-table* keyseq command))

(loop for (keyseq command)
      in `(((#\g) info.read-eval-print.editor.command::beginning-of-buffer))
      do (set-command *normal-g-dispatch-table* keyseq command))
