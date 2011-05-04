;;;; info.read-eval-print.editor.asd

(asdf:defsystem :info.read-eval-print.editor
  :serial t
  :components ((:file "package")
               (:file "info.read-eval-print.editor")
               (:file "command"))
  :depends-on (:cl-gtk2-gtk :cl-ppcre :hu.dwim.defclass-star :quek))
