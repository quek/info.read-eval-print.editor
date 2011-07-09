;;;; info.read-eval-print.editor.asd

(asdf:defsystem :info.read-eval-print.editor
  :serial t
  :components ((:file "gtk-source-view")
               (:file "package")
               (:file "start")
               (:file "util")
               (:file "special-variable")
               (:file "class")
               (:file "config")
               (:file "font")
               (:file "command")
               (:file "register")
               (:file "buffer")
               (:file "mode")
               (:file "gui")
               (:file "command-mode")
               (:file "fundamental-mode")
               (:file "common-lisp")
               (:file "common-lisp-indent")
               (:file "end"))
  :depends-on (:cl-gtk2-gtk
               :cl-ppcre
               :cl-unicode
               :hu.dwim.defclass-star
               :jp
               :swank
               :closer-mop
               :contextl :quek))
