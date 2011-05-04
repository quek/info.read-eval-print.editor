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


(defun info.read-eval-print.editor.command::open (path)
  (setf (text-buffer-text (text-view-buffer (buffer-text-view-of *editor*)))
        (collect 'string
          (scan-file path #'read-char)))
  (widget-grab-focus (buffer-text-view-of *editor*)))

