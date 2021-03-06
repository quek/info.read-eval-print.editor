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

(define-g-interface "GtkSourceCompletionProvider"
    source-completion-provider
    (:export t :type-initializer "gtk_source_completion_provider_get_type"))

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

(define-g-object-class "GtkSourceCompletion" source-completion
  (:superclass g-object :export t :interfaces nil :type-initializer "gtk_source_completion_get_type")
  ((accelerators source-completion-accelerators "accelerators" "guint" t t)
   (auto-complete-delay source-completion-auto-complete-delay "auto-complete-delay" "guint" t t)
   (proposal-page-size source-completion-proposal-page-size "proposal-page-size" "guint" t t)
   (provider-page-size source-completion-provider-page-size "provider-page-size" "guint" t t)
   (remember-info-visibility source-completion-remember-info-visibility "remember-info-visibility" "gboolean" t t)
   (select-on-show source-completion-select-on-show "select-on-show" "gboolean" t t)
   (show-headers source-completion-show-headers "show-headers" "gboolean" t t)
   (show-icons source-completion-show-icons "show-icons" "gboolean" t t)
   (view source-completion-view "view" "GtkSourceView*" t nil)))

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

(defcfun (show-line-numbers "gtk_source_view_get_show_line_numbers") :boolean
   (view (g-object source-view)))
(defun (setf show-line-numbers) (value view)
  (foreign-funcall "gtk_source_view_set_show_line_numbers"
                   (g-object source-view) view :boolean value :void))
(export 'show-line-numbers)

(defcfun gtk-source-language-manager-get-default :pointer)
(export 'gtk-source-language-manager-get-default)

(defcfun gtk-source-language-manager-get-search-path (gstrv :free-from-foreign nil)
  (lm :pointer))
(export 'gtk-source-language-manager-get-search-path)

(defcfun gtk-source-language-manager-set-search-path :void
  (lm :pointer)
  (dirs gstrv))
(export 'gtk-source-language-manager-set-search-path)

(defcfun gtk-source-language-manager-guess-language (g-object source-language)
  (lm :pointer)
  (file-name (:string :free-to-foreign t))
  (contet-type (:string :free-to-foreign t)))
(export 'gtk-source-language-manager-guess-language)


(defcfun gtk-source-style-scheme-manager-get-default :pointer)
(export 'gtk-source-style-scheme-manager-get-default)

(defcfun gtk-source-style-scheme-manager-get-scheme (g-object source-style-scheme)
  (manager :pointer)
  (scheme-id :string))
(export 'gtk-source-style-scheme-manager-get-scheme)


(defcfun gtk-source-completion-get-providers (glist (g-object source-completion-provider) :free-from-foreign nil)
  (completion (g-object source-completion)))
(export 'gtk-source-completion-get-providers)
