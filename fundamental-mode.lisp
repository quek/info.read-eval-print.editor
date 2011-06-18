(in-package :info.read-eval-print.editor)

(define-mode fundamental-mode () ())

(defvar *digit-argument-map* (make-instance 'key-map))

(define-command indent ()
  (info.read-eval-print.editor.command::insert "indent"))

(define-command newline-and-indent ()
  (insert *buffer* (format nil "~%"))
  (info.read-eval-print.editor.command::indent))


(loop for (keyseq command)
      in `(((#\0) info.read-eval-print.editor.command::digit-argument-0)
           ((#\1) info.read-eval-print.editor.command::digit-argument-1)
           ((#\2) info.read-eval-print.editor.command::digit-argument-2)
           ((#\3) info.read-eval-print.editor.command::digit-argument-3)
           ((#\4) info.read-eval-print.editor.command::digit-argument-4)
           ((#\5) info.read-eval-print.editor.command::digit-argument-5)
           ((#\6) info.read-eval-print.editor.command::digit-argument-6)
           ((#\7) info.read-eval-print.editor.command::digit-argument-7)
           ((#\8) info.read-eval-print.editor.command::digit-argument-8)
           ((#\9) info.read-eval-print.editor.command::digit-argument-9))
      do (set-key *fundamental-mode-map* :normal keyseq command nil))

(pushnew *digit-argument-map* (inherits-of *fundamental-mode-map*))

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
           ((#\p) info.read-eval-print.editor.command::paste-below-cursor)
           ((#\d) ,(^ push-temp-key-map *fundamental-mode-map* *d-key-map*))
           ((#\g) ,(^ push-temp-key-map *fundamental-mode-map* *g-key-map*))
           ((#\y) ,(^ push-temp-key-map *fundamental-mode-map* *y-key-map*))
           ((:control #\w) ,(^ push-temp-key-map *fundamental-mode-map* *ctl-w-key-map*))
           ((#\=) info.read-eval-print.editor.command::indent))
      do (set-key *fundamental-mode-map* :normal keyseq command))

(loop for (keyseq command)
      in `(((:control #\c) info.read-eval-print.editor.command::normal-mode)
           ((:control #\[) info.read-eval-print.editor.command::normal-mode)
           ((#\Esc) info.read-eval-print.editor.command::normal-mode))
      do (set-key *fundamental-mode-map* :insert keyseq command))

(defvar *d-key-map* (make-instance 'key-map :inherits (list *digit-argument-map*)))

(loop for (keyseq command)
      in `(((#\d) info.read-eval-print.editor.command::delete-line))
      do (set-key *d-key-map* :normal keyseq command))

(defvar *g-key-map* (make-instance 'key-map :inherits (list *digit-argument-map*)))

(loop for (keyseq command)
      in `(((#\g) info.read-eval-print.editor.command::beginning-of-buffer))
      do (set-key *g-key-map* :normal keyseq command))

(defvar *y-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\y) info.read-eval-print.editor.command::yank-current-line))
      do (set-key *y-key-map* :normal keyseq command))

(defvar *ctl-w-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\h) ,(lambda () (window-h *editor*)))
           ((#\j) ,(lambda () (window-j *editor*)))
           ((#\k) ,(lambda () (window-k *editor*)))
           ((#\l) ,(lambda () (window-l *editor*))))
      do (set-key *ctl-w-key-map* :normal keyseq command))