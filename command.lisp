(in-package :info.read-eval-print.editor)


(defun info.read-eval-print.editor.command::command-mode ()
  (setf (mode-of *editor*) :command)
  (setf (text-buffer-text (text-view-buffer (command-view-of *editor*))) ":")
  (widget-grab-focus (command-view-of *editor*)))

(defun info.read-eval-print.editor.command::normal-mode ()
  (setf (mode-of *editor*) :normal)
  (setf (text-buffer-text (text-view-buffer (command-view-of *editor*))) "")
  (widget-grab-focus (current-frame-of *editor*)))

(defun info.read-eval-print.editor.command::i ()
  (setf (mode-of *editor*) :insert)
  (widget-grab-focus (current-frame-of *editor*)))

(defun info.read-eval-print.editor.command::a ()
  (info.read-eval-print.editor.command::forward-char)
  (info.read-eval-print.editor.command::i))

(defun info.read-eval-print.editor.command::o ()
  (info.read-eval-print.editor.command::end-of-line)
  (insert *buffer* (string #\Newline))
  (info.read-eval-print.editor.command::i))

(defun info.read-eval-print.editor.command::e (path)
  (let* ((*frame* (current-frame-of *editor*))
         (*buffer* (make-instance 'buffer :frame *frame*)))
    (find-file *buffer* path)
    (setf (buffer-of *frame*) *buffer*)
    (update-status *frame*)
    (focus *frame*)))


(defun info.read-eval-print.editor.command::q ()
  (object-destroy (window-of *editor*)))

(defun info.read-eval-print.editor.command::run-command ()
  ":e /tmp/a.txt"
  (let* ((input (text-of *buffer*))
         (splited (ppcre:split "\\s" input :start 1 :limit 2)))
    (awhen (find-symbol (string-upcase (car splited))
                        :info.read-eval-print.editor.command)
      (info.read-eval-print.editor.command::normal-mode)
      (apply it (cdr splited)))))

(defun info.read-eval-print.editor.command::digit-argument-n (n)
  (setf (digit-argument-of *buffer*) n))

(let ((*package* (find-package :info.read-eval-print.editor.command)))
  (loop for i from 0 to 9
        do (eval `(defun ,(sym 'info.read-eval-print.editor.command::digit-argument- i) ()
                    (info.read-eval-print.editor.command::digit-argument-n ,i)))))


(defun info.read-eval-print.editor.command::split ()
  (window-split *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(defun info.read-eval-print.editor.command::vsplit ()
  (window-vsplit *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))

(defun info.read-eval-print.editor.command::close ()
  (window-close *editor* (current-frame-of *editor*))
  (focus (current-frame-of *editor*)))



;; normal
(loop for (keyseq command)
        in `(((#\:) info.read-eval-print.editor.command::command-mode)
             ((#\i) info.read-eval-print.editor.command::i)
             ((#\a) info.read-eval-print.editor.command::a)
             ((#\o) info.read-eval-print.editor.command::o)
             ((#\h) info.read-eval-print.editor.command::backward-char)
             ((#\j) info.read-eval-print.editor.command::next-line)
             ((#\k) info.read-eval-print.editor.command::previous-line)
             ((#\l) info.read-eval-print.editor.command::forward-char)
             ((#\w) info.read-eval-print.editor.command::forward-sexp)
             ((#\b) info.read-eval-print.editor.command::backward-sexp)
             ((#\G) info.read-eval-print.editor.command::end-of-buffer)
             ((#\$) info.read-eval-print.editor.command::end-of-line)
             ((#\x) info.read-eval-print.editor.command::delete-char)
             ((#\X) info.read-eval-print.editor.command::backward-delete-char)
             ((#\u) info.read-eval-print.editor.command::undo)
             ((:control #\r) info.read-eval-print.editor.command::redo)
             ((#\0) info.read-eval-print.editor.command::digit-argument-0)
             ((#\1) info.read-eval-print.editor.command::digit-argument-1)
             ((#\2) info.read-eval-print.editor.command::digit-argument-2)
             ((#\3) info.read-eval-print.editor.command::digit-argument-3)
             ((#\4) info.read-eval-print.editor.command::digit-argument-4)
             ((#\5) info.read-eval-print.editor.command::digit-argument-5)
             ((#\6) info.read-eval-print.editor.command::digit-argument-6)
             ((#\7) info.read-eval-print.editor.command::digit-argument-7)
             ((#\8) info.read-eval-print.editor.command::digit-argument-8)
             ((#\9) info.read-eval-print.editor.command::digit-argument-9)
             ((#\p) info.read-eval-print.editor.command::paste-below-cursor)
             ((#\e) info.read-eval-print.editor.command::eval-last-sexp)
             ((#\g) ,(lambda () (setf (dispatch-table *editor* :normal) *normal-g-dispatch-table*)))
             ((#\y) ,(lambda () (setf (dispatch-table *editor* :normal) *normal-y-dispatch-table*))))
      do (set-command *normal-dispatch-table* keyseq command))

;; insert
(loop for (keyseq command)
        in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
             ((:control #\[) info.read-eval-print.editor.command::normal-mode)
             ((#\Esc) info.read-eval-print.editor.command::normal-mode))
      do (set-command *insert-dispatch-table* keyseq command))

;; command
(loop for (keyseq command)
        in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
             ((:control #\[) info.read-eval-print.editor.command::normal-mode)
             ((#\Esc) info.read-eval-print.editor.command::normal-mode)
             ((:control #\m) info.read-eval-print.editor.command::run-command)
             ((#\Return) info.read-eval-print.editor.command::run-command))
      do (set-command *command-dispatch-table* keyseq command))

;; normal - g
(loop for (keyseq command)
        in `(((#\g) info.read-eval-print.editor.command::beginning-of-buffer))
      do (set-command *normal-g-dispatch-table* keyseq command))

;; normal - y
(loop for (keyseq command)
        in `(((#\y) info.read-eval-print.editor.command::yank-current-line))
      do (set-command *normal-y-dispatch-table* keyseq command))
