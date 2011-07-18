(in-package :info.read-eval-print.editor)

(define-mode fundamental-mode () ())

(defvar *digit-argument-map* (make-instance 'key-map))

(define-command self-insert-command ()
  (values t t t))

(define-layered-method get-key-binding :in fundamental-mode :around
  (mode keyseq (editor-mode (eql :insert)))
  (aif (call-next-layered-method)
       it
       #'self-insert-command))

(define-command indent ()
  (insert "indent"))

(define-command newline-and-indent ()
  (buffer-insert *buffer* (format nil "~%"))
  (indent))

(define-command hide-info-frame ()
  (close-info-frame))

(loop for (keyseq command)
      in `(((#\0) digit-argument-0)
           ((#\1) digit-argument-1)
           ((#\2) digit-argument-2)
           ((#\3) digit-argument-3)
           ((#\4) digit-argument-4)
           ((#\5) digit-argument-5)
           ((#\6) digit-argument-6)
           ((#\7) digit-argument-7)
           ((#\8) digit-argument-8)
           ((#\9) digit-argument-9))
      do (set-key *digit-argument-map* :normal keyseq command nil))

(pushnew *digit-argument-map* (inherits-of *fundamental-mode-map*))

(loop for (keyseq command)
      in `(((#\:) command-mode)
           ((#\i) i)
           ((#\a) a)
           ((#\o) o)
           ((#\h) backward-char)
           ((#\j) next-line)
           ((#\k) previous-line)
           ((#\l) forward-char)
           ((#\w) forward-word)
           ((#\W) forward-word*)
           ((#\b) backward-word)
           ((#\B) backward-word*)
           ((#\e) end-of-word)
           ((#\E) end-of-word*)
           ((#\G) end-of-buffer)
           ((#\0) beginning-of-line-or-digit-0)
           ((#\^) back-to-indentation)
           ((#\$) end-of-line)
           ((#\x) delete-char)
           ((#\X) backward-delete-char)
           ((#\u) undo)
           ((:control #\r) redo)
           ((#\p) paste-below-cursor)
           ((#\d) ,(^ push-temp-key-map *fundamental-mode-map* *d-key-map*))
           ((#\g) ,(^ push-temp-key-map *fundamental-mode-map* *g-key-map*))
           ((#\y) ,(^ push-temp-key-map *fundamental-mode-map* *y-key-map*))
           ((:control #\w) ,(^ push-temp-key-map *fundamental-mode-map* *ctl-w-key-map*))
           ((#\") ,(^ push-temp-key-map *fundamental-mode-map* *register-key-map*))
           ((#\=) indent)
           ((:meta #\q) close-info-frame))
      do (set-key *fundamental-mode-map* :normal keyseq command))

(loop for (keyseq command)
      in `(((:control #\c) normal-mode)
           ((:control #\[) normal-mode)
           ((#\Esc) normal-mode)
           ((:meta #\q) close-info-frame))
      do (set-key *fundamental-mode-map* :insert keyseq command))

(defvar *d-key-map* (make-instance 'key-map :inherits (list *digit-argument-map*)))

(loop for (keyseq command)
      in `(((#\d) delete-line)
           ((#\w) delete-word))
      do (set-key *d-key-map* :normal keyseq command))

(defvar *g-key-map* (make-instance 'key-map :inherits (list *digit-argument-map*)))

(loop for (keyseq command)
      in `(((#\g) beginning-of-buffer))
      do (set-key *g-key-map* :normal keyseq command))

(defvar *y-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\y) yank-current-line))
      do (set-key *y-key-map* :normal keyseq command))

(defvar *ctl-w-key-map* (make-instance 'key-map))

(loop for (keyseq command)
      in `(((#\h) ,(lambda () (window-h *editor*)))
           ((#\j) ,(lambda () (window-j *editor*)))
           ((#\k) ,(lambda () (window-k *editor*)))
           ((#\l) ,(lambda () (window-l *editor*))))
      do (set-key *ctl-w-key-map* :normal keyseq command))

(defvar *register-key-map* (make-instance 'key-map))

(iterate ((c (scan-char-range #\a #\z))
          (cc (scan-char-range #\A #\Z)))
  (set-key *register-key-map* :normal (list c) (^ setf *register* c))
  (set-key *register-key-map* :normal (list cc) (^ setf *register* cc)))