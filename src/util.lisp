(in-package :command-line-parse)

(deftype or-null (type) `(or ,type null))

;; Useful mixins

(defclass has-metavar ()
  ((meta :reader        meta
         :initarg       :meta
         :initform      (error "Specify METAVAR")
         :documentation "Meta variable to show in the help")
   (fn   :reader        meta-fn
         :initarg       :fn
         :initform      #'identity
         :documentation "Function which is applied to the metavar to produce the result"))
  (:documentation "Mixin for stuff which has a metavar"))

(defclass has-name ()
  ((name        :reader        name
                :initarg       :name
                :initform      (error "Specify the name")
                :type          symbol
                :documentation "The name of an option, flag or argument")
   (description :reader        description
                :initarg       :description
                :initform      nil
                :type          (or-null string)
                :documentation "Description of a flag / an option / an argument"))
  (:documentation "Mixin for stuff which has a name and description"))

(defclass is-key ()
  ((short :reader        short
          :initarg       :short
          :initform      nil
          :type          (or-null character)
          :documentation "The short form")
   (long  :reader        long
          :initarg       :long
          :initform      nil
          :type          (or-null string)
          :documentation "The long form"))
  (:documentation "Mixing for stuff which has a key (-f/--foo) part,
like switches and options"))

(defclass has-children ()
  ((children :initarg  :children
             :initform nil
             :type     list
             :reader   children))
  (:documentation "Mixin for parsers with children"))

(defclass has-child ()
  ((child :initarg  :child
          :initform (error "Specify a child")
          :reader   child))
  (:documentation "Mixin for parsers with exactly one child"))

(define-condition cmd-line-parse-error (simple-error)
  ()
  (:documentation "Error signaled if the parser fails"))

;; Helper functions

(serapeum:-> long-str (string)
             (values string &optional))
(defun long-str (string)
  "Prepend STRING with '--'."
  (concatenate 'string "--" string))

(serapeum:-> short-str (character)
             (values string &optional))
(defun short-str (char)
  "Prepend CHAR with '-'."
  (concatenate 'string "-" (list char)))

(serapeum:-> peek-flag ((or-null string)
                        (or-null character)
                        list)
             (values boolean &optional))
(defun peek-flag (long short input)
  "Tell if the next input is a flag."
  (cond
    ((null input) nil)
    ((and long  (string= (car input) (long-str long)))   t)
    ((and short (string= (car input) (short-str short))) t)
    (t nil)))

(serapeum:-> show-flag ((or-null string)
                        (or-null character))
             (values string &optional))
(defun show-flag (long short)
  "Pring flag usage"
  (cond
    ((and short long)
     (concatenate 'string (short-str short) "|" (long-str long)))
    (short
     (short-str short))
    (long
     (long-str long))
    ;; Cannot happen
    (t "")))

(serapeum:-> forbid-option (string)
             (values string &optional))
(defun forbid-option (string)
  "Return a string if it does not begin with '-', otherwise signal
CMD-LINE-PARSE-ERROR."
  (if (char= (aref string 0) #\-)
      (error 'cmd-line-parse-error
             :format-control   "Value expected, got ~a"
             :format-arguments (list string))
      string))
