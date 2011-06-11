;;;; package.lisp

(macrolet
    ((m ()
       `(defpackage :info.read-eval-print.editor
          (:use :cl :gtk :gdk :gobject :series :quek :anaphora :contextl)
          (:shadowing-import-from :series ,@series::/series-forms/)
          (:import-from :hu.dwim.defclass-star #:defclass*)
          (:shadow #:frame)             ; gtk:frame
          (:export #:main))))
  (m))

(series::install :pkg :info.read-eval-print.editor :implicit-map t)


(defpackage :info.read-eval-print.editor.command)

