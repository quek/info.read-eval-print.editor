(in-package :info.read-eval-print.editor)

(defparameter *default-buffer-style-scheme* "oblivion")
(defparameter *default-status-style-scheme* "classic")

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :info.read-eval-print.editor)))

;; ./language-specs を追加して Common Lisp にも対応。
(glib:at-init ()
  (let* ((lm (gtk-source-language-manager-get-default))
         (path (gtk-source-language-manager-get-search-path lm))
         (x (namestring (merge-pathnames "language-specs" *src-location*))))
    (unless (member x path :test #'equal)
      (gtk-source-language-manager-set-search-path lm (cons x path)))))
;;(gtk-source-language-manager-get-search-path (gtk-source-language-manager-get-default))
