(in-package :info.read-eval-print.editor)


(defun info.read-eval-print.editor.command::command-mode ()
  (setf (mode-of *editor*) :command)
  (setf (text-buffer-text (text-view-buffer (command-text-view-of *editor*))) ":")
  (widget-grab-focus (command-text-view-of *editor*)))

(defun info.read-eval-print.editor.command::open (path)
  (setf (text-buffer-text (text-view-buffer (buffer-text-view-of *editor*)))
        (collect 'string
          (scan-file path #'read-char)))
  (widget-grab-focus (buffer-text-view-of *editor*)))
