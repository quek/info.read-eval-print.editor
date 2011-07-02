;;;; package.lisp

(quek:sdefpackage :info.read-eval-print.editor
                  (:use :cl :gtk :gdk :gobject :quek :anaphora :contextl)
                  (:import-from :hu.dwim.defclass-star #:defclass*)
                  (:shadow #:point                     ; gdk:point
                           #:frame)                    ; gtk:frame
                  (:export #:main))


(defpackage :info.read-eval-print.editor.command
  (:use)
  (:import-from :cl #:setf))

