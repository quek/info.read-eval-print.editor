(in-package :gtk)

(glib:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)

    (define-foreign-library libgtksourceview
      ((:and :unix (:not :darwin)) (:or "libgtksourceview-2.0.so.0" "libgtksourceview-2.0.so"))
      (:darwin (:or "libgtksourceview-2.0.0.dylib" "libgtksourceview-2.0.dylib"))
      (:windows (:or "libgtksourceview-2.0-0.dll" "libgtksourceview-win32-2.0-0.dll"))
      (t "libgtksourceview-2.0"))

    (use-foreign-library libgtksourceview)))


(define-g-enum "GtkSourceSmartHomeEndType"
    source-smart-home-end-type
    (:export t :type-initializer "gtk_source_smart_home_end_type_get_type")
  (:disabled 0)
  (:before 1)
  (:after 2)
  (:always 3))

(define-g-flags "GtkSourceCompletionActivation"
    source-completion-activation
    (:export t :type-initializer "gtk_source_completion_activation_get_type")
  (:none 0)
  (:interactive 1)
  (:user-requested 2))

(define-g-flags "GtkSourceDrawSpacesFlags"
    source-draw-spaces-flags
    (:export t :type-initializer "gtk_source_draw_spaces_flags_get_type")
  (:space 1)
  (:tab 2)
  (:newline 4)
  (:nbsp 8)
  (:leading 16)
  (:text 32)
  (:trailing 64)
  (:all 127))

(define-g-object-class "GtkSourceView" source-view
  (:superclass text-view :export t :interfaces ("AtkImplementorIface" "GtkBuildable") :type-initializer "gtk_source_view_get_type")
  ((buffer source-view-buffer "buffer" "GtkSourceBuffer" t t)
   (auto-indent source-view-auto-indent "auto-indent" "gboolean" t t)
   (completion source-view-completion "completion" "GtkSourceCompletion" t nil)
   (draw-spaces source-view-draw-spaces "draw-spaces" "GtkSourceDrawSpacesFlags" t t)
   (highlight-current-line source-view-highlight-current-line "highlight-current-line" "gboolean" t t)
   (indent-on-tab source-view-indent-on-tab "indent-on-tab" "gboolean" t t)
   (indent-width source-view-indent-width "indent-width" "gint" t t)
   (insert-spaces-instead-of-tabs source-view-insert-spaces-instead-of-tabs "insert-spaces-instead-of-tabs" "gboolean" t t)
   (right-margin-position source-view-right-margin-position "right-margin-position" "guint" t t)
   (show-line-marks source-view-show-line-marks "show-line-marks" "gboolean" t t)
   (show-line-numbers source-view-show-line-numbers "show-line-numbers" "gboolean" t t)
   (show-right-margin source-view-show-right-margin "show-right-margin" "gboolean" t t)
   (smart-home-end source-view-smart-home-end "smart-home-end" "GtkSourceSmartHomeEndType" t t)
   (tab-width source-view-tab-width "tab-width" "guint" t t)))

(define-g-object-class "GtkSourceBuffer" source-buffer (:superclass text-buffer :export t :interfaces nil :type-initializer "gtk_source_buffer_get_type")
  ((can-redo source-buffer-can-redo "can-redo" "gboolean" t nil) (can-undo source-buffer-can-undo "can-undo" "gboolean" t nil)
   (highlight-matching-brackets source-buffer-highlight-matching-brackets "highlight-matching-brackets" "gboolean" t t)
   (highlight-syntax source-buffer-highlight-syntax "highlight-syntax" "gboolean" t t)
   (language source-buffer-language "language" "GtkSourceLanguage" t t)
   (max-undo-levels source-buffer-max-undo-levels "max-undo-levels" "gint" t t)
   (style-scheme source-buffer-style-scheme "style-scheme" "GtkSourceStyleScheme" t t)
   (undo-manager source-buffer-undo-manager "undo-manager" "GtkSourceUndoManager" t t)))

(define-g-object-class "GtkSourceCompletionContext" source-completion-context
  (:superclass g-initially-unowned :export t :interfaces nil :type-initializer "gtk_source_completion_context_get_type")
  ((activation source-completion-context-activation "activation" "GtkSourceCompletionActivation" t t)
   (completion source-completion-context-completion "completion" "GtkSourceCompletion" t nil)
   (iter
     source-completion-context-iter
     "iter"
     "GtkTextIter"
     t
     t)))

(define-g-object-class "GtkSourceGutter" source-gutter (:superclass g-object :export t :interfaces nil :type-initializer "gtk_source_gutter_get_type")
  ((view source-gutter-view "view" "GtkSourceView" t nil) (window-type source-gutter-window-type "window-type" "GtkTextWindowType" t nil)))

(define-g-object-class "GtkSourceLanguage" source-language (:superclass g-object :export t :interfaces nil :type-initializer "gtk_source_language_get_type")
  ((hidden source-language-hidden "hidden" "gboolean" t nil) (id source-language-id "id" "gchararray" t nil)
   (name source-language-name "name" "gchararray" t nil) (section source-language-section "section" "gchararray" t nil)))

(define-g-object-class "GtkSourceStyleScheme" source-style-scheme
  (:superclass g-object :export t :interfaces nil :type-initializer "gtk_source_style_scheme_get_type")
  ((description source-style-scheme-description "description" "gchararray" t nil)
   (filename source-style-scheme-filename "filename" "gchararray" t nil) (id source-style-scheme-id "id" "gchararray" t nil)
   (name source-style-scheme-name "name" "gchararray" t nil)))

(define-g-object-class "GtkSourceUndoManager" source-undo-manager
  (:superclass g-interface :export t :interfaces nil :type-initializer "gtk_source_undo_manager_get_type") nil)



(defcfun (source-view-new "gtk_source_view_new") (g-object source-buffer)
  (source-view (g-object source-view)))
(export 'source-view-new)

(defcfun (source-buffer-undo "gtk_source_buffer_undo") :void
  (buffer (g-object source-buffer)))
(export 'source-buffer-undo)

(defcfun (source-buffer-redo "gtk_source_buffer_redo") :void
  (buffer (g-object source-buffer)))
(export 'source-buffer-redo)

