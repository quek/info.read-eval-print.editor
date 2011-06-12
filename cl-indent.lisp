;;; cl-indent.el --- enhanced lisp-indent mode

;; Copyright (C) 1987, 2000-2011 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Created: July 1987
;; Maintainer: FSF
;; Keywords: lisp, tools
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies a single entry point, common-lisp-indent-function,
;; which performs indentation in the preferred style for Common Lisp code.
;; To enable it:
;;
;; (setq lisp-indent-function 'common-lisp-indent-function)

;;; Code:


(cl:defpackage :info.read-eval-print.editor.cl-indent
  (:use :cl :info.read-eval-print.editor.command)
  (:export #:indent-line)
  (:shadow #:if)
  (:shadowing-import-from :cl #:close)
  (:import-from :info.read-eval-print.editor #:save-excursion))

(cl:in-package :info.read-eval-print.editor.cl-indent)

(defmacro while (test &body body)
  `(loop while ,test
         do (progn ,@body)))

(defmacro if (test then &rest else)
  `(cl:if ,test ,then (progn ,@else)))

(defmacro put (symbol indicator value)
  `(setf (get ,symbol ,indicator) ,value))

(defun intern-soft (name)
  (find-symbol name))

(defvar *match* nil)
(defun string-match (regexp string &optional (start 0))
  (setf *match* (multiple-value-list (ppcre:scan regexp string :start start)))
  (apply #'values *match*))
(defun match-beginning (subexp)
  (if (zerop subexp)
      (car *match*)
      (ignore-errors (aref (nth (1+ subexp) *match*) 0))))
(defun match-end (subexp)
  (if (zerop subexp)
      (cadr *match*)
      (ignore-errors (aref (nth (1+ subexp) *match*) 1))))

(defun substring (string from &optional to)
  (subseq string from to))




(defvar *lisp-body-indent* 2
  "Number of columns to indent the second line of a `(def...)' form.")






(defvar *lisp-indent-maximum-backtracking* 3
  "Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is 0, no backtracking will occur and forms such as `flet'
may not be correctly indented.")

(defvar *lisp-tag-indentation* 1
  "Indentation of tags relative to containing list.
This variable is used by the function `lisp-indent-tagbody'.")

(defvar *lisp-tag-body-indentation* 3
  "Indentation of non-tagged lines relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by `*lisp-body-indent*'.")

(defvar *lisp-backquote-indentation* t
  "Whether or not to indent backquoted lists as code.
If nil, indent backquoted lists as data, i.e., like quoted lists.")

(defvar *lisp-loop-indent-subclauses* t
  "Whether or not to indent loop subclauses.")

(defvar *lisp-simple-loop-indentation* 2
  "Indentation of forms in simple loop forms.")

(defvar *lisp-loop-indent-forms-like-keywords* nil
  "Whether or not to indent loop subforms just like
loop keywords. Only matters when `*lisp-loop-indent-subclauses*'
is nil.")

(defvar *lisp-align-keywords-in-calls* t
  "Whether to align keyword arguments vertically or not.
If t (the default), keywords in contexts where no other
indentation rule takes precedence are aligned like this:

\(make-instance 'foo :bar t
                    :quux 42)

If nil, they are indented like any other function
call arguments:

\(make-instance 'foo :bar t
               :quux 42)")

(defvar *lisp-lambda-list-indentation* t
  "Whether to indent lambda-lists specially. Defaults to t. Setting this to
nil makes `lisp-lambda-list-keyword-alignment',
`*lisp-lambda-list-keyword-parameter-alignment*', and
`lisp-lambda-list-keyword-parameter-indentation' meaningless, causing
lambda-lists to be indented as if they were data:

\(defun example (a b &optional o1 o2
                o3 o4
                &rest r
                &key k1 k2
                k3 k4)
  #|...|#)")

(defvar *lisp-lambda-list-keyword-alignment* nil
  "Whether to vertically align lambda-list keywords together.
If nil (the default), keyworded lambda-list parts are aligned
with the initial mandatory arguments, like this:

\(defun foo (arg1 arg2 &rest rest
            &key key1 key2)
  #|...|#)

If non-nil, alignment is done with the first keyword
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &rest rest
                      &key key1 key2)
  #|...|#)")

(defvar *lisp-lambda-list-keyword-parameter-indentation* 2
  "Indentation of lambda list keyword parameters.
See `*lisp-lambda-list-keyword-parameter-alignment*'
for more information.")

(defvar *lisp-lambda-list-keyword-parameter-alignment* nil
  "Whether to vertically align lambda-list keyword parameters together.
If nil (the default), the parameters are aligned
with their corresponding keyword, plus the value of
`*lisp-lambda-list-keyword-parameter-indentation*', like this:

\(defun foo (arg1 arg2 &key key1 key2
                        key3 key4)
  #|...|#)

If non-nil, alignment is done with the first parameter
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &key key1 key2
                            key3 key4)
  #|...|#)")


(defvar *lisp-indent-defun-method* '(4 &lambda &body)
        "Defun-like indentation method.
This applies when the value of the `common-lisp-indent-function' property
is set to `defun'.")

;;;; LOOP indentation, the simple version

(defun common-lisp-loop-type (loop-start)
  "Returns the type of the loop form at LOOP-START.
Possible types are SIMPLE, EXTENDED, and EXTENDED/SPLIT.
EXTENDED/SPLIT refers to extended loops whose body does
not start on the same line as the opening parenthesis of
the loop."
  (handler-case
      (save-excursion
        (goto-char loop-start)
        (let ((line (line-number-at-pos)))
          (forward-char 1)
          (forward-sexp 2)
          (backward-sexp 1)
          (if (looking-at "\\sw")
              (if (= line (line-number-at-pos))
                  'extended
                  'extended/split)
              'simple)))
    (error (e) (print e) 'simple)))

(defun common-lisp-loop-part-indentation (indent-point state)
  "Compute the indentation of loop form constituents."
  (let* ((loop-start (elt state 1))
         (type (common-lisp-loop-type loop-start))
         (loop-indentation (save-excursion
                             (goto-char loop-start)
                             (if (eq 'extended/split type)
                                 (- (current-column) 4)
                                 (current-column))))
         (indent nil)
         (re "\\(:?\\sw+\\|;\\|)\\|\n\\)"))
    (goto-char indent-point)
    (back-to-indentation)
    (cond ((eq 'simple type)
           (+ loop-indentation *lisp-simple-loop-indentation*))
          ;; We are already in a body, with forms in it.
          ((and (not (looking-at re))
                (save-excursion
                  (while (and (ignore-errors (backward-sexp) t)
                              (not (looking-at re)))
                    (setq indent (current-column)))
                  (when (and indent
                             (looking-at common-lisp-indent-body-introducing-loop-macro-keyword))
                    t)))
           (list indent loop-start))
          ;; Keyword-style
          ((or *lisp-loop-indent-forms-like-keywords* (looking-at re))
           (list (+ loop-indentation 6) loop-start))
          ;; Form-style
          (t
           (list (+ loop-indentation 9) loop-start)))))

;;;###autoload
(defun common-lisp-indent-function (indent-point state)
  "Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's common-lisp-indent-function property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `*lisp-indent-defun-method*';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list.  The list element in position M specifies how to indent the Mth
  function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted
  list elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest *lisp-body-indent*, i.e., indent
    all remaining elements by `*lisp-body-indent*'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3."
  (common-lisp-indent-function-1 indent-point state))


(defun common-lisp-indent-function-1 (indent-point state)
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to containing lists.
          ;; `foo' has a path of (0 3 1) in `((a b c (d foo) f) g)'.
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          ;; If non-nil, this is an indentation to use
          ;; if nothing else specifies it more firmly.
          tentative-calculated
          (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (elt state 1))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))

      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth *lisp-indent-maximum-backtracking*))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem function method tentative-defun)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil)
                (setq tem (point))
                (forward-sexp 1)
                (setq function (string-upcase (buffer-substring-no-properties
                                               tem (point))))
                (goto-char tem)
                (setq tem (intern-soft function)
                      method (get tem 'common-lisp-indent-function))
                (cond ((and (null method)
                            (string-match ":[^:]+" function))
                       ;; The pleblisp package feature
                       (setq function (substring function
                                                 (1+ (match-beginning 0)))
                             method (get (intern-soft function)
                                         'common-lisp-indent-function)))
                      ((and (null method))
                       ;; backwards compatibility
                       (setq method (get tem 'lisp-indent-function)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (ignore-errors
                           (forward-sexp 1)
                           (if (>= (point) indent-point)
                               nil
                               (parse-partial-sexp (point)
                                                   indent-point 1 t)
                               (setq n (1+ n))
                               t))))
              (setq path (cons n path)))

            ;; backwards compatibility.
            (cond ((null function))
                  ((null method)
                   (when (null (cdr path))
                     ;; (package prefix was stripped off above)
                     (cond ((and (string-match "\\`def" function)
                                 (not (string-match "\\`default" function)))
                            (setq tentative-defun t))
                           ((string-match "\\`(with|without|do)-" function)
                            (setq method '(&lambda &body))))))
                  ;; backwards compatibility.  Bletch.
                  ((eq method 'defun)
                   (setq method *lisp-indent-defun-method*)))

            (cond ((and (or (eq (char-after (1- containing-sexp)) #\')
                            (and (not *lisp-backquote-indentation*)
                                 (eq (char-after (1- containing-sexp)) #\`)))
                        (not (eq (char-after (- containing-sexp 2)) #\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((save-excursion
                     (goto-char indent-point)
                     (backward-sexp)
                     (let ((re "#!?\\(+\\|-\\)"))
                       (if (or (looking-at re)
                               (ignore-errors
                                 (backward-sexp)
                                 (looking-at re)))
                           (setq calculated (current-column))))))
                  ((eq (char-after (1- containing-sexp)) #\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method)
                   ;; If this looks like a call to a `def...' form,
                   ;; think about indenting it as one, but do it
                   ;; tentatively for cases like
                   ;; (flet ((defunp ()
                   ;;          nil)))
                   ;; Set both normal-indent and tentative-calculated.
                   ;; The latter ensures this value gets used
                   ;; if there are no relevant containing constructs.
                   ;; The former ensures this value gets used
                   ;; if there is a relevant containing construct
                   ;; but we are nested within the structure levels
                   ;; that it specifies indentation for.
                   (if tentative-defun
                       (setq tentative-calculated
                             (common-lisp-indent-call-method
                              function *lisp-indent-defun-method*
                              path state indent-point
                              sexp-column normal-indent)
                             normal-indent tentative-calculated)
                       (when *lisp-align-keywords-in-calls*
                         ;; No method so far. If we're looking at a keyword,
                         ;; align with the first keyword in this expression.
                         ;; This gives a reasonable indentation to most things
                         ;; with keyword arguments.
                         (save-excursion
                           (goto-char indent-point)
                           (back-to-indentation)
                           (when (looking-at ":")
                             (while (ignore-errors (backward-sexp 2) t)
                               (when (looking-at ":")
                                 (setq calculated (current-column)))))))))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-function)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path)
                                           normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column *lisp-body-indent*))
                                          (t
                                           ;; other body form
                                           normal-indent))))
                  (t
                   (setq calculated
                         (common-lisp-indent-call-method
                          function method path state indent-point
                          sexp-column normal-indent)))))
          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (unless calculated
            (or (ignore-errors (backward-up-list 1)
                               (setq depth (1+ depth)))
                (setq depth *lisp-indent-maximum-backtracking*)))))
      (or calculated tentative-calculated))))


(defun common-lisp-indent-call-method (function method path state indent-point
                                       sexp-column normal-indent)
  (let ((lisp-indent-error-function function))
    (if (symbolp method)
        (funcall method
                 path state indent-point
                 sexp-column normal-indent)
        (lisp-indent-259 method path state indent-point
                         sexp-column normal-indent))))

;; Dynamically bound in common-lisp-indent-call-method.
(defvar lisp-indent-error-function)

(defun lisp-indent-report-bad-format (m)
  (error "~a has a badly-formed ~a property: ~a"
         ;; Love those free variable references!!
         lisp-indent-error-function 'common-lisp-indent-function m))


;; Lambda-list indentation is now done in LISP-INDENT-LAMBDA-LIST.
;; See also `*lisp-lambda-list-keyword-alignment*',
;; `*lisp-lambda-list-keyword-parameter-alignment*' and
;; `*lisp-lambda-list-keyword-parameter-indentation*' -- dvl

(defvar lisp-indent-lambda-list-keywords-regexp
  "&\\(\
optional\\|rest\\|key\\|allow-other-keys\\|aux\\|whole\\|body\\|environment\\|more\
\\)\\([ \t]\\|$\\)"
  "Regular expression matching lambda-list keywords.")

(defun lisp-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (if (not *lisp-lambda-list-indentation*)
      (1+ sexp-column)
      (let (limit)
        (cond ((save-excursion
                 (goto-char indent-point)
                 (back-to-indentation)
                 (setq limit (point))
                 (looking-at lisp-indent-lambda-list-keywords-regexp))
               ;; We're facing a lambda-list keyword.
               (if *lisp-lambda-list-keyword-alignment*
                   ;; Align to the first keyword if any, or to the beginning of
                   ;; the lambda-list.
                   (save-excursion
                     (goto-char containing-form-start)
                     (down-list)
                     (let ((key-indent nil)
                           (next t))
                       (while (and next (< (point) indent-point))
                         (if (looking-at lisp-indent-lambda-list-keywords-regexp)
                             (setq key-indent (current-column)
                                   next nil)
                             (setq next (ignore-errors (forward-sexp) t))
                             (if next
                                 (ignore-errors
                                   (forward-sexp)
                                   (backward-sexp)))))
                       (or key-indent
                           (1+ sexp-column))))
                   ;; Align to the beginning of the lambda-list.
                   (1+ sexp-column)))
              (t
               ;; Otherwise, align to the first argument of the last lambda-list
               ;; keyword, the keyword itself, or the beginning of the
               ;; lambda-list.
               (save-excursion
                 (goto-char indent-point)
                 (let ((indent nil)
                       (next t))
                   (while (and next (> (point) containing-form-start))
                     (setq next (ignore-errors (backward-sexp) t))
                     (let* ((col (current-column))
                            (pos
                             (save-excursion
                               (ignore-errors (forward-sexp))
                               (skip-chars-forward "[ \\t]")
                               (if (eolp)
                                   (+ col *lisp-lambda-list-keyword-parameter-indentation*)
                                   col))))
                       (if (looking-at lisp-indent-lambda-list-keywords-regexp)
                           (setq indent (if *lisp-lambda-list-keyword-parameter-alignment*
                                            (or indent pos)
                                            (+ col
                                               *lisp-lambda-list-keyword-parameter-indentation*))
                                 next nil)
                           (setq indent col))))
                   (or indent (1+ sexp-column)))))))))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun lisp-indent-259
    (method path state indent-point sexp-column normal-indent)
  (catch 'exit
    (let ((p path)
          (containing-form-start (elt state 1))
          n tem tail)
      ;; Isn't tail-recursion wonderful?
      (while p
        ;; This while loop is for destructuring.
        ;; p is set to (cdr p) each iteration.
        (if (not (consp method)) (lisp-indent-report-bad-format method))
        (setq n (1- (car p))
              p (cdr p)
              tail nil)
        (while n
          ;; This while loop is for advancing along a method
          ;; until the relevant (possibly &rest/&body) pattern
          ;; is reached.
          ;; n is set to (1- n) and method to (cdr method)
          ;; each iteration.
          (setq tem (car method))

          (or (eq tem 'nil)             ;default indentation
              (eq tem '&lambda)         ;lambda list
              (and (eq tem '&body) (null (cdr method)))
              (and (eq tem '&rest)
                   (consp (cdr method))
                   (null (cddr method)))
              (integerp tem)            ;explicit indentation specified
              (and (consp tem)          ;destructuring
                   (eq (car tem) '&whole)
                   (or (symbolp (cadr tem))
                       (integerp (cadr tem))))
              (and (symbolp tem)        ;a function to call to do the work.
                   (null (cdr method)))
              (lisp-indent-report-bad-format method))

          (cond ((eq tem '&body)
                 ;; &body means (&rest <*lisp-body-indent*>)
                 (throw 'exit
                   (if (and (= n 0)     ;first body form
                            (null p))   ;not in subforms
                       (+ sexp-column
                          *lisp-body-indent*)
                       normal-indent)))
                ((eq tem '&rest)
                 ;; this pattern holds for all remaining forms
                 (setq tail (> n 0)
                       n 0
                       method (cdr method)))
                ((> n 0)
                 ;; try next element of pattern
                 (setq n (1- n)
                       method (cdr method))
                 (if (< n 0)
                     ;; Too few elements in pattern.
                     (throw 'exit normal-indent)))
                ((eq tem 'nil)
                 (throw 'exit (if (consp normal-indent)
                                  normal-indent
                                  (list normal-indent containing-form-start))))
                ((eq tem '&lambda)
                 (throw 'exit
                   (cond ((null p)
                          (list (+ sexp-column 4) containing-form-start))
                         (t
                          ;; Indentation within a lambda-list. -- dvl
                          (list (lisp-indent-lambda-list
                                 indent-point
                                 sexp-column
                                 containing-form-start)
                                containing-form-start)))))
                ((integerp tem)
                 (throw 'exit
                   (if (null p)         ;not in subforms
                       (list (+ sexp-column tem) containing-form-start)
                       normal-indent)))
                ((symbolp tem)          ;a function to call
                 (throw 'exit
                   (funcall tem path state indent-point
                            sexp-column normal-indent)))
                (t
                 ;; must be a destructing frob
                 (if (not (null p))
                     ;; descend
                     (setq method (cddr tem)
                           n nil)
                     (setq tem (cadr tem))
                     (throw 'exit
                       (cond (tail
                              normal-indent)
                             ((eq tem 'nil)
                              (list normal-indent
                                    containing-form-start))
                             ((integerp tem)
                              (list (+ sexp-column tem)
                                    containing-form-start))
                             (t
                              (funcall tem path state indent-point
                                       sexp-column normal-indent))))))))))))

(defun lisp-indent-tagbody (path state indent-point sexp-column normal-indent)
  (if (not (null (cdr path)))
      normal-indent
      (save-excursion
        (goto-char indent-point)
        (back-to-indentation)
        (list (cond ((looking-at "\\sw\\|\\s_")
                     ;; a tagbody tag
                     (+ sexp-column *lisp-tag-indentation*))
                    ((integerp *lisp-tag-body-indentation*)
                     (+ sexp-column *lisp-tag-body-indentation*))
                    ((eq *lisp-tag-body-indentation* 't)
                     (handler-case
                         (progn (backward-sexp 1) (current-column))
                       (error () (1+ sexp-column))))
                    (t (+ sexp-column *lisp-body-indent*)))
                                        ;            (cond ((integerp *lisp-tag-body-indentation*)
                                        ;                   (+ sexp-column *lisp-tag-body-indentation*))
                                        ;                  ((eq *lisp-tag-body-indentation* 't)
                                        ;                   normal-indent)
                                        ;                  (t
                                        ;                   (+ sexp-column *lisp-body-indent*)))
              (elt state 1)
              ))))

(defun lisp-indent-do (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 3)
      (let ((*lisp-tag-body-indentation* *lisp-body-indent*))
        (funcall (function lisp-indent-tagbody)
                 path state indent-point sexp-column normal-indent))
      (funcall (function lisp-indent-259)
               '((&whole nil &rest
                  ;; the following causes weird indentation
                  ;;(&whole 1 1 2 nil)
                  )
                 (&whole nil &rest 1))
               path state indent-point sexp-column normal-indent)))

(defun lisp-indent-defsetf (path state indent-point sexp-column normal-indent)
  (declare (ignore normal-indent))
  (list
   (cond
     ;; Inside the lambda-list in a long-form defsetf.
     ((and (eql 2 (car path)) (cdr path))
      (lisp-indent-lambda-list indent-point sexp-column (elt state 1)))
     ;; Long form: has a lambda-list.
     ((or (cdr path)
          (save-excursion
            (goto-char (elt state 1))
            (ignore-errors
              (down-list)
              (forward-sexp 3)
              (backward-sexp)
              (looking-at "nil\\|("))))
      (+ sexp-column
         (case (car path)
           ((1 3) 4)
           (2 4)
           (t 2))))
     ;; Short form.
     (t
      (+ sexp-column
         (case (car path)
           (1 4)
           (2 4)
           (t 2)))))
   (elt state 1)))

;; LISP-INDENT-DEFMETHOD now supports the presence of more than one method
;; qualifier and indents the method's lambda list properly. -- dvl
(defun lisp-indent-defmethod
    (path state indent-point sexp-column normal-indent)
  (lisp-indent-259
   (let ((nqual 0))
     (if (and (>= (car path) 3)
              (save-excursion
                (beginning-of-defun)
                (forward-char 1)
                (forward-sexp 2)
                (skip-chars-forward "[ \\t\\n]")
                (while (looking-at "\\sw\\|\\s_")
                  (incf nqual)
                  (forward-sexp)
                  (skip-chars-forward "[ \\t\\n]"))
                (> nqual 0)))
         (append '(4) (make-list nqual :initial-element 4) '(&lambda &body))
         (get 'defun 'common-lisp-indent-function)))
   path state indent-point sexp-column normal-indent))


(defun lisp-indent-function-lambda-hack (path state indent-point
                                         sexp-column normal-indent)
  (declare (ignore state indent-point))
  ;; indent (function (lambda () <newline> <body-forms>)) kludgily.
  (if (or (cdr path) ; wtf?
          (> (car path) 3))
      ;; line up under previous body form
      normal-indent
      ;; line up under function rather than under lambda in order to
      ;;  conserve horizontal space.  (Which is what #' is for.)
      (handler-case
          (save-excursion
            (backward-up-list 2)
            (forward-char 1)
            (if (looking-at "\\(lisp:+\\)?function\\(\\Sw\\|\\S_\\)")
                (+ *lisp-body-indent* -1 (current-column))
                (+ sexp-column *lisp-body-indent*)))
        (error () (+ sexp-column *lisp-body-indent*)))))

(defun lisp-indent-loop (path state indent-point sexp-column normal-indent)
  (declare (ignore sexp-column))
  (cond ((not (null (cdr path)))
         normal-indent)
        (*lisp-loop-indent-subclauses*
         (list (common-lisp-indent-loop-macro-1 state indent-point)
               (common-lisp-indent-parse-state-start state)))
        (t
         (common-lisp-loop-part-indentation indent-point state))))

;;;; LOOP indentation, the complex version -- handles subclause indentation

;; Regexps matching various varieties of loop macro keyword ...
(defvar common-lisp-indent-body-introducing-loop-macro-keyword
  "do\\|finally\\|initially"
  "Regexp matching loop macro keywords which introduce body-forms.")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar common-lisp-indent-prefix-loop-macro-keyword
  "and\\|else"
  "Regexp matching loop macro keywords which are prefixes.")

(defvar common-lisp-indent-clause-joining-loop-macro-keyword
  "and"
  "Regexp matching 'and', and anything else there ever comes to be like it.")

;; This is handled right, but it's incomplete ...
;; (It could probably get arbitrarily long if I did *every* iteration-path)
(defvar common-lisp-indent-indented-loop-macro-keyword
  "into\\|by\\|upto\\|downto\\|above\\|below\\|on\\|being\\|=\\|first\\|then\\|from\\|to"
  "Regexp matching keywords introducing loop subclauses.
Always indented two.")

(defvar common-lisp-indent-indenting-loop-macro-keyword
  "when\\|unless\\|if"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented.")

(defvar common-lisp-indent-loop-macro-else-keyword "else")

;;; Attempt to indent the loop macro ...

(defun common-lisp-indent-parse-state-depth (parse-state)
  (car parse-state))

(defun common-lisp-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun common-lisp-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

(defun common-lisp-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      (goto-char (common-lisp-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
        (common-lisp-indent-loop-advance-past-keyword-on-line)

        (when (eolp)
          (forward-line 1)
          (end-of-line)
          ;; If indenting first line after "(loop <newline>"
          ;; cop out ...
          (if (<= indent-point (point))
              (throw 'return-indentation (+ 2 loop-start-column)))
          (back-to-indentation))

        (let* ((case-fold-search t)
               (loop-macro-first-clause (point))
               (previous-expression-start (common-lisp-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (loop-body-p nil)
               (loop-body-indentation nil)
               (indented-clause-indentation (+ 2 default-value)))
          (declare (ignore case-fold-search))
          ;; Determine context of this loop clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)

          ;; Handle a body-introducing-clause which ends a line specially.
          (if (looking-at common-lisp-indent-body-introducing-loop-macro-keyword)
              (let ((keyword-position (current-column)))
                (setq loop-body-p t)
                (setq loop-body-indentation
                      (if (common-lisp-indent-loop-advance-past-keyword-on-line)
                          (current-column)
                          (back-to-indentation)
                          (if (/= (current-column) keyword-position)
                              (+ 2 (current-column))
                              (+ keyword-position 3)))))

              (back-to-indentation)
              (if (< (point) loop-macro-first-clause)
                  (goto-char loop-macro-first-clause))
              ;; If there's an "and" or "else," advance over it.
              ;; If it is alone on the line, the next "cond" will treat it
              ;; as if there were a "when" and indent under it ...
              (let ((exit nil))
                (while (and (null exit)
                            (looking-at common-lisp-indent-prefix-loop-macro-keyword))
                  (if (null (common-lisp-indent-loop-advance-past-keyword-on-line))
                      (progn (setq exit t)
                             (back-to-indentation)))))

              ;; Found start of loop clause preceding the one we're trying to indent.
              ;; Glean context ...
              (cond
                ((looking-at "(")
                 ;; We're in the middle of a clause body ...
                 (setq loop-body-p t)
                 (setq loop-body-indentation (current-column)))
                ((looking-at common-lisp-indent-body-introducing-loop-macro-keyword)
                 (setq loop-body-p t)
                 ;; Know there's something else on the line (or would
                 ;; have been caught above)
                 (common-lisp-indent-loop-advance-past-keyword-on-line)
                 (setq loop-body-indentation (current-column)))
                (t
                 (setq loop-body-p nil)
                 (if (or (looking-at common-lisp-indent-indenting-loop-macro-keyword)
                         (looking-at common-lisp-indent-prefix-loop-macro-keyword))
                     (setq default-value (+ 2 (current-column))))
                 (setq indented-clause-indentation (+ 2 (current-column)))
                 ;; We still need loop-body-indentation for "syntax errors" ...
                 (goto-char previous-expression-start)
                 (setq loop-body-indentation (current-column)))))

          ;; Go to first non-blank character of the line we're trying to indent.
          ;; (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
            ((looking-at "(")
             ;; Clause body ...
             loop-body-indentation)
            ((or (eolp) (looking-at ";"))
             ;; Blank line.  If body-p, indent as body, else indent as
             ;; vanilla clause.
             (if loop-body-p
                 loop-body-indentation
                 default-value))
            ((looking-at common-lisp-indent-indented-loop-macro-keyword)
             indented-clause-indentation)
            ((looking-at common-lisp-indent-clause-joining-loop-macro-keyword)
             (let ((stolen-indent-column nil))
               (forward-line -1)
               (while (and (null stolen-indent-column)
                           (> (point) loop-macro-first-clause))
                 (back-to-indentation)
                 (if (and (< (current-column) loop-body-indentation)
                          (looking-at "\\sw"))
                     (progn
                       (if (looking-at common-lisp-indent-loop-macro-else-keyword)
                           (common-lisp-indent-loop-advance-past-keyword-on-line))
                       (setq stolen-indent-column
                             (current-column)))
                     (forward-line -1)))
               (if stolen-indent-column
                   stolen-indent-column
                   default-value)))
            (t default-value)))))))

(defun common-lisp-indent-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (if (eolp)
      nil
      (current-column)))

;;;; IF* is not standard, but a plague upon the land
;;;; ...let's at least try to indent it.

(defvar common-lisp-indent-if*-keyword
  "threnret\\|elseif\\|then\\|else"
  "Regexp matching if* keywords")

(defun common-lisp-indent-if* (path parse-state indent-point sexp-column normal-indent)
  (declare (ignore path sexp-column normal-indent))
  (list (common-lisp-indent-if*-1 parse-state indent-point)
	(common-lisp-indent-parse-state-start parse-state)))

(defun common-lisp-indent-if*-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of if* macro, and use it to establish
      ;; base column for indentation
      (goto-char (common-lisp-indent-parse-state-start parse-state))
      (let ((if*-start-column (current-column)))
	(common-lisp-indent-if*-advance-past-keyword-on-line)
	(let* ((case-fold-search t)
	       (if*-first-clause (point))
	       (previous-expression-start
                (common-lisp-indent-parse-state-prev parse-state))
	       (default-value (current-column))
	       (if*-body-p nil)
	       (if*-body-indentation nil))
          (declare (ignore case-fold-search))
	  ;; Determine context of this if* clause, starting with the
	  ;; expression immediately preceding the line we're trying to indent
	  (goto-char previous-expression-start)
	  ;; Handle a body-introducing-clause which ends a line specially.
	  (back-to-indentation)
          (if (< (point) if*-first-clause)
              (goto-char if*-first-clause))
          ;; Found start of if* clause preceding the one we're trying to indent.
          ;; Glean context ...
          (cond
            ((looking-at common-lisp-indent-if*-keyword)
             (setq if*-body-p t)
             ;; Know there's something else on the line (or would
             ;; have been caught above)
             (common-lisp-indent-if*-advance-past-keyword-on-line)
             (setq if*-body-indentation (current-column)))
            ((looking-at "#'\\|'\\|(")
             ;; We're in the middle of a clause body ...
             (setq if*-body-p t)
             (setq if*-body-indentation (current-column)))
            (t
             (setq if*-body-p nil)
             ;; We still need if*-body-indentation for "syntax errors" ...
             (goto-char previous-expression-start)
             (setq if*-body-indentation (current-column))))

          ;; Go to first non-blank character of the line we're trying to indent.
          ;; (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
            ((or (eolp) (looking-at ";"))
             ;; Blank line.  If body-p, indent as body, else indent as
             ;; vanilla clause.
             (if if*-body-p
                 if*-body-indentation
                 default-value))
            ((not (looking-at common-lisp-indent-if*-keyword))
             ;; Clause body ...
             if*-body-indentation)
            (t
             (- (+ 7 if*-start-column)
                (- (match-end 0) (match-beginning 0))))))))))

(defun common-lisp-indent-if*-advance-past-keyword-on-line ()
  (forward-word 1)
  (block move-forward
    (while (and (looking-at "\\s-") (not (eolp)))
      (forward-char 1)))
  (if (eolp)
      nil
      (current-column)))


;;;; Indentation specs for standard symbols, and a few semistandard ones.
(let ((l '((block 1)
           (case        (4 &rest (&whole 2 &rest 1)))
           (ccase . case)
           (ecase . case)
           (typecase . case)
           (etypecase . case)
           (ctypecase . case)
           (catch 1)
           (cond        (&rest (&whole 2 &rest 1)))
           (defvar      (4 2 2))
           (defclass    (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
           (defconstant . defvar)
           (defcustom   (4 2 2 2))
           (defparameter . defvar)
           (defconst     . defcustom)
           (define-condition  . defclass)
           (define-modify-macro (4 &lambda &body))
           (defsetf      lisp-indent-defsetf)
           (defun       (4 &lambda &body))
           (defgeneric  (4 &lambda &body))
           (define-setf-method . defun)
           (define-setf-expander . defun)
           (defmacro . defun)
           (defsubst . defun)
           (deftype . defun)
           (defmethod   lisp-indent-defmethod)
           (defpackage  (4 2))
           (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                          &rest (&whole 2 &rest 1)))
           (destructuring-bind
                 ((&whole 6 &rest 1) 4 &body))
           (do          lisp-indent-do)
           (do* . do)
           (dolist      ((&whole 4 2 1) &body))
           (dotimes . dolist)
           (eval-when   1)
           (flet        ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
           (labels . flet)
           (macrolet . flet)
           (generic-flet . flet)
           (generic-labels . flet)
           (handler-case (4 &rest (&whole 2 &lambda &body)))
           (restart-case . handler-case)
           ;; single-else style (then and else equally indented)
           (if          (&rest nil))
           (if*         common-lisp-indent-if*)
           (lambda      (&lambda &rest lisp-indent-function-lambda-hack))
           (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
           (let* . let)
           (compiler-let . let) ;barf
           (handler-bind . let)
           (restart-bind . let)
           (locally 1)
           (loop           lisp-indent-loop)
           (:method (&lambda &body)) ; in `defgeneric'
           (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
           (multiple-value-call (4 &body))
           (multiple-value-prog1 1)
           (multiple-value-setq (4 2))
           (multiple-value-setf . multiple-value-setq)
           (pprint-logical-block (4 2))
           (print-unreadable-object ((&whole 4 1 &rest 1) &body))
           ;; Combines the worst features of BLOCK, LET and TAGBODY
           (prog        (&lambda &rest lisp-indent-tagbody))
           (prog* . prog)
           (prog1 1)
           (prog2 2)
           (progn 0)
           (progv       (4 4 &body))
           (return 0)
           (return-from (nil &body))
           (symbol-macrolet . let)
           (tagbody     lisp-indent-tagbody)
           (throw 1)
           (unless 1)
           (unwind-protect (5 &body))
           (when 1)
           (with-accessors . multiple-value-bind)
           (with-condition-restarts . multiple-value-bind)
           (with-output-to-string (4 2))
           (with-slots . multiple-value-bind)
           (with-standard-io-syntax (2)))))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
             (car (cdr el))))))


(defun parse-partial-sexp (from to &optional target-depth stop-before old-state comment-stop)
  from to  target-depth stop-before old-state comment-stop)


(defun indent-line ()
  (save-excursion
    (beginning-of-line)))









#+nil 
(defun test-lisp-indent (tests)
  (let ((ok 0))
    (dolist (test tests)
      (with-temp-buffer
        (lisp-mode)
        (setq indent-tabs-mode nil)
        (when (consp test)
          (when (cddr test)
            (error "Malformed test: %s" test))
          (dolist (bind (first test))
            (make-variable-buffer-local (first bind))
            (set (first bind) (second bind)))
          (setf test (second test)))
        (insert test)
        (goto-char 0)
        (skip-chars-forward "[ \\t\\n]")
        ;; Mess up the indentation so we know reindentation works
        (let ((mess nil))
          (save-excursion
            (while (not (eobp))
              (forward-line 1)
              (ignore-errors (delete-char 1) (setf mess t))))
          (if (or (not mess) (equal (buffer-string) test))
              (error "Couldn't mess up indentation?")))
        (indent-sexp)
        (if (equal (buffer-string) test)
            (incf ok)
            (error "Bad indentation.\nWanted: %s\nGot: %s"
                   test
                   (buffer-string)))))
    ok))

;; (run-lisp-indent-tests)

#+nil
(defun run-lisp-indent-tests ()
  (test-lisp-indent
   '("
 (defun foo ()
   t)"
     (((*lisp-lambda-list-keyword-parameter-alignment* nil)
       (*lisp-lambda-list-keyword-alignment* nil))
      "
 (defun foo (foo &optional opt1
                   opt2
             &rest rest)
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* t)
       (*lisp-lambda-list-keyword-alignment* nil))
      "
 (defun foo (foo &optional opt1
                           opt2
             &rest rest)
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* nil)
       (*lisp-lambda-list-keyword-alignment* t))
      "
 (defun foo (foo &optional opt1
                   opt2
                 &rest rest)
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* t)
       (*lisp-lambda-list-keyword-alignment* t))
      "
 (defun foo (foo &optional opt1
                           opt2
                 &rest rest)
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* nil)
       (*lisp-lambda-list-keyword-alignment* nil))
      "
 (defmacro foo ((foo &optional opt1
                       opt2
                 &rest rest))
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* t)
       (*lisp-lambda-list-keyword-alignment* nil))
      "
 (defmacro foo ((foo &optional opt1
                               opt2
                 &rest rest))
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* nil)
       (*lisp-lambda-list-keyword-alignment* t))
      "
 (defmacro foo ((foo &optional opt1
                       opt2
                     &rest rest))
   (list foo opt1 opt2
         rest))")
     (((*lisp-lambda-list-keyword-parameter-alignment* t)
       (*lisp-lambda-list-keyword-alignment* t))
      "
 (defmacro foo ((foo &optional opt1
                               opt2
                     &rest rest))
   (list foo opt1 opt2
         rest))")
     "
  (let ((x y)
        (foo #-foo (no-foo)
             #+foo (yes-foo))
        (bar #-bar
             (no-bar)
             #+bar
             (yes-bar)))
    (list foo bar
          x))"
     (((*lisp-loop-indent-subclauses* t))
      "
  (loop for i from 0 below 2
        for j from 0 below 2
        when foo
          do (fubar)
             (bar)
             (moo)
          and collect cash
                into honduras
        else do ;; this is the body of the first else
                ;; the body is ...
                (indented to the above comment)
                (ZMACS gets this wrong)
             and do this
             and do that
             and when foo
                   do the-other
                   and cry
        when this-is-a-short-condition do
          (body code of the when)
        when here's something I used to botch do (here is a body)
                                                 (rest of body indented same)
        do
           (exdented loop body)
           (I'm not sure I like this but it's compatible)
        when funny-predicate do ;; Here's a comment
                                (body filled to comment))")
     "
  (defun foo (x)
    (tagbody
     foo
       (bar)
     baz
       (when (losing)
         (with-big-loser
             (yow)
           ((lambda ()
              foo)
            big)))
       (flet ((foo (bar baz zap)
                (zip))
              (zot ()
                quux))
         (do ()
             ((lose)
              (foo 1))
           (quux)
          foo
           (lose))
         (cond ((x)
                (win 1 2
                     (foo)))
               (t
                (lose
                 3))))))"
     "
  (if* (eq t nil)
     then ()
          ()
   elseif (dsf)
     thenret x
     else (balbkj)
          (sdf))"
     "
   (list foo #+foo (foo)
             #-foo (no-foo))"
     (((*lisp-loop-indent-subclauses* t))
      "
    (loop for x in foo1
          for y in quux1
          )")
     (((*lisp-loop-indent-subclauses* nil))
      "
    (loop for x in foo
          for y in quux
          )")
     (((*lisp-loop-indent-subclauses* nil)
       (*lisp-loop-indent-forms-like-keywords* t))
      "
   (loop for x in foo
         for y in quux
         finally (foo)
                 (fo)
                 (zoo)
         do
         (print x)
         (print y)
         (print 'ok!))")
     (((*lisp-loop-indent-subclauses* nil)
       (*lisp-loop-indent-forms-like-keywords* nil))
      "
   (loop for x in foo
         for y in quux
         finally (foo)
                 (fo)
                 (zoo)
         do
            (print x)
            (print y)
            (print 'ok!))")
     (((*lisp-loop-indent-subclauses* t)
       (*lisp-loop-indent-forms-like-keywords* nil))
      "
   (loop for x in foo
         for y in quux
         finally (foo)
                 (fo)
         do
            (print x)
            (print y)
            (print 'ok!))")
     (((*lisp-loop-indent-subclauses* nil)
       (*lisp-loop-indent-forms-like-keywords* nil))
      "
   (loop for f in files
         collect (open f
                       :direction :output)
         do (foo) (bar)
            (quux))")
     (((*lisp-loop-indent-subclauses* t))
      "
   (loop for f in files
         collect (open f
                       :direction :output)
         do (foo) (bar)
            (quux))")
     "
   (defsetf foo bar
     \"the doc string\")"
     "
   (defsetf foo
       bar
     \"the doc string\")"
     (((*lisp-lambda-list-keyword-parameter-alignment*n t))
      "
   (defsetf foo (x y &optional a
                               z)
       (a b c)
     stuff)")
     (((*lisp-align-keywords-in-calls* t))
      "
    (make-instance 'foo :bar t quux t
                        :zot t)")
     (((*lisp-align-keywords-in-calls* nil))
      "
    (make-instance 'foo :bar t quux t
                   :zot t)")
     (((*lisp-lambda-list-indentation* nil))
      "
      (defun example (a b &optional o1 o2
                      o3 o4
                      &rest r
                      &key k1 k2
                      k3 k4)
        'hello)"))))



;;(put 'while    'common-lisp-indent-function 1)
;;(put 'defwrapper'common-lisp-indent-function ...)
;;(put 'def 'common-lisp-indent-function ...)
;;(put 'defflavor        'common-lisp-indent-function ...)
;;(put 'defsubst 'common-lisp-indent-function ...)

;;(put 'with-restart 'common-lisp-indent-function '((1 4 ((* 1))) (2 &body)))
;;(put 'restart-case 'common-lisp-indent-function '((1 4) (* 2 ((0 1) (* 1)))))
;;(put 'define-condition 'common-lisp-indent-function '((1 6) (2 6 ((&whole 1))) (3 4 ((&whole 1))) (4 &body)))
;;(put 'with-condition-handler 'common-lisp-indent-function '((1 4 ((* 1))) (2 &body)))
;;(put 'condition-case 'common-lisp-indent-function '((1 4) (* 2 ((0 1) (1 3) (2 &body)))))
;;(put 'defclass 'common-lisp-indent-function '((&whole 2 &rest (&whole 2 &rest 1) &rest (&whole 2 &rest 1)))
;;(put 'defgeneric 'common-lisp-indent-function 'defun)

;;; cl-indent.el ends here
