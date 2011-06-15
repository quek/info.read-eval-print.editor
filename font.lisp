(in-package :pango)

;; PangoFontDescription *pango_font_description_from_string (const char *str);
(defcfun pango-font-description-from-string :pointer
  (str :string))


(in-package :gtk)

(defcfun (widget-modify-font "gtk_widget_modify_font") :void
  (widget (g-object widget))
  (font-desc :pointer))
