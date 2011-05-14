(defpackage :gtk-source-view-generation
  (:use :cl :gobject :cffi :glib)
  (:export #:gtk-generate
           #:gtk-generate-child-properties))

(in-package :gtk-source-view-generation)

(load-foreign-library "libgtksourceview-2.0.so")

(defcfun gtk-init-check :boolean
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

(defun gtk-init ()
  (gtk-init-check (foreign-alloc :int :initial-element 0)
                  (foreign-alloc :string :initial-contents '("/usr/bin/sbcl"))))

(gtk-init)

(defcfun gtk-test-register-all-types :void)

(gtk-test-register-all-types)

(defun gtk-generate (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (generate-types-hierarchy-to-file
     stream
     "GtkObject"
     :include-referenced t
     :prefix "Gtk"
     :package (or (find-package :gtk) (make-package :gtk))
     :prologue (format nil "(in-package :gtk)")
;;     :interfaces '("GtkSourceCompletionProvider")
     :objects '("GtkSourceBuffer"
                "GtkSourceCompletion"
                "GtkSourceCompletionContext"
                ;; "GtkSourceCompletionInfo"
                ;; "GtkSourceCompletionItem"
                ;; "GtkSourceCompletionProposal"
                ;; "GtkSourceIter"
                "GtkSourceGutter"
                ;; "GtkSourceMark"
                "GtkSourceView"
                "GtkSourceLanguage"
                ;; "GtkSourceLanguageManager"
                ;; "GtkSourcePrintCompositor"
                ;; "GtkSourceStyle"
                "GtkSourceStyleScheme"
                ;; "GtkSourceStyleSchemeManager"
                "GtkSourceUndoManager"))))

(defun gtk-generate-child-properties (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((*package* (find-package :gtk))
          (*print-case* :downcase))
     (write-string "(in-package :gtk)" stream)
     (terpri stream)
     (format stream "~{~S~%~%~}" (gtk:generate-child-properties)))))