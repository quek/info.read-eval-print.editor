;;;; info.read-eval-print.editor.asd

(asdf:defsystem :info.read-eval-print.editor
  :serial t
  :components ((:file "gtk-source-view")
               (:file "package")
               (:file "util")
               (:file "special-variable")
               (:file "class")
               (:file "config")
               (:file "command")
               (:file "buffer")
               (:file "mode")
               (:file "gui")
               (:file "common-lisp")
               (:file "cl-indent"))
  :depends-on (:cl-gtk2-gtk :cl-ppcre :hu.dwim.defclass-star :jp :swank :closer-mop :contextl :quek))
