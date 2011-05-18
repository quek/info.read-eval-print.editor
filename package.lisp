;;;; package.lisp

(macrolet
    ((m ()
       `(defpackage :info.read-eval-print.editor
          (:use :cl :gtk :gdk :gobject :series :quek :anaphora)
          (:shadowing-import-from :series ,@series::/series-forms/)
          (:import-from :hu.dwim.defclass-star #:defclass*)
          (:export #:main))))
  (m))

(series::install :pkg :info.read-eval-print.editor :implicit-map t)


(defpackage :info.read-eval-print.editor.command)

