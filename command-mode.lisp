(in-package :info.read-eval-print.editor)

(define-mode command-mode () ())

(loop for (keyseq command)
      in `(((:control #\c) normal-mode)
           ((:control #\[) normal-mode)
           ((#\Esc) normal-mode)
           ((:control #\m) run-command)
           ((#\Return) run-command)
           ((:control #\i) simple-completion)
           ((#\Tab) simple-completion))
      do (set-key *command-mode-map* :insert keyseq command))

