(in-package :info.read-eval-print.editor)

(define-mode fundamental-command-mode () ())

(define-mode command-mode (fundamental-command-mode) ())

(define-mode interactive-command-mode (fundamental-command-mode) ())


(loop for (keyseq command)
      in `(((:control #\c) normal-mode)
           ((:control #\[) normal-mode)
           ((#\Esc) normal-mode))
      do (set-key *fundamental-command-mode-map* :insert keyseq command))

(loop for (keyseq command)
      in `(((:control #\m) run-command)
           ((#\Return) run-command)
           ((:control #\i) simple-completion)
           ((#\Tab) simple-completion))
      do (set-key *command-mode-map* :insert keyseq command))

(loop for (keyseq command)
      in `(((:control #\m) run-command)
           ((#\Return) run-command))
      do (set-key *interactive-command-mode-map* :insert keyseq command))
