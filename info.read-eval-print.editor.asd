;;;; info.read-eval-print.editor.asd

(asdf:defsystem :info.read-eval-print.editor
  :serial t
  :components ((:file "gtk-source-view")
               (:file "package")
               (:file "util")
               (:file "config")
               (:file "buffer")
               (:file "gui")
               (:file "command"))
  :depends-on (:cl-gtk2-gtk :cl-ppcre :hu.dwim.defclass-star :quek))
