;;;; package.lisp

(quek:sdefpackage :info.read-eval-print.editor
                  (:use :cl :quek :anaphora :contextl)
                  (:import-from :hu.dwim.defclass-star #:defclass*)
                  (:export #:main))


(defpackage :info.read-eval-print.editor.command
  (:use)
  (:import-from :cl #:setf))

