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


(defun info.read-eval-print.editor.command::backward-char (&optional (n 1))
  (let ((iter (text-buffer-get-iter-at-mark *buffer* (text-buffer-insertion-mark *buffer*))))
    (text-iter-move iter :count n :direction :backward)
    (text-buffer-place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::forward-char (&optional (n 1))
  (let ((iter (text-buffer-get-iter-at-mark *buffer* (text-buffer-insertion-mark *buffer*))))
    (text-iter-move iter :count n :direction :forward)
    (text-buffer-place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::next-line (&optional (n 1))
  (let* ((iter (text-buffer-get-iter-at-mark *buffer* (text-buffer-insertion-mark *buffer*)))
         (line-offset (text-iter-line-offset iter)))
    (loop repeat n do (text-view-forward-display-line *view* iter))
    (setf (text-iter-line-offset iter) line-offset)
    (text-buffer-place-cursor *buffer* iter)
    (print (list line-offset (text-iter-line-offset iter)))
    (when (/= line-offset (text-iter-line-offset iter))
      (text-view-forward-display-line-end *view* iter)
      (text-buffer-place-cursor *buffer* iter))))

(defun info.read-eval-print.editor.command::previous-line (&optional (n 1))
  (let* ((iter (text-buffer-get-iter-at-mark *buffer* (text-buffer-insertion-mark *buffer*)))
         (line-offset (text-iter-line-offset iter)))
    (loop repeat n do (text-view-backward-display-line *view* iter))
    (setf (text-iter-line-offset iter) line-offset)
    (text-buffer-place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::beginning-of-buffer ()
  (let ((iter (text-buffer-get-start-iter *buffer*)))
    (text-buffer-place-cursor *buffer* iter)))

(defun info.read-eval-print.editor.command::end-of-buffer ()
  (let ((iter (text-buffer-get-end-iter *buffer*)))
    (text-buffer-place-cursor *buffer* iter)))


(defun info.read-eval-print.editor.command::open (path)
  (let ((buffer (text-view-buffer (buffer-text-view-of *editor*))))
    (setf (text-buffer-text buffer)
          (collect 'string
            (scan-file path #'read-char)))
    (widget-grab-focus (buffer-text-view-of *editor*))
    (let ((*buffer* buffer))
      (info.read-eval-print.editor.command::beginning-of-buffer))))


(defun info.read-eval-print.editor.command::quit ()
  (object-destroy (window-of *editor*)))

(defun info.read-eval-print.editor.command::run-command ()
  ":open /tmp/a.txt"
  (let* ((input (text-buffer-text *buffer*))
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
             ((#\G) info.read-eval-print.editor.command::end-of-buffer)
             ((#\g) ,(lambda () (setf (dispatch-table *editor* :normal) *normal-g-dispatch-table*))))
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

