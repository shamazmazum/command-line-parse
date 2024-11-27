(in-package :command-line-parse)

;;;; Interface

(defgeneric parse-input (parser input &optional acc)
  (:documentation "Apply a parser to an input. Return three values:
new accumulator, the rest of input and a boolean variable indicating
the success. Used by PARSE."))

(defgeneric show-parser (parser) (:documentation "Text representation
of a parser. Used by SHOW-USAGE."))

;;;; Parsers

;; Flag

(defclass flag (has-name is-key)
  ()
  (:documentation "A parser for an optional flag in the form '-f' or '--foo'"))

(defun flag (name &key short long description)
  "Make a parser for a boolean flag in the short form (like '-f')
and/or the long form (like '--foo')."
  (unless (or short long)
    (error "Specify either :SHORT or :LONG keys."))
  (make-instance 'flag
                 :name        name
                 :short       short
                 :long        long
                 :description description))

;; Option

(defclass option (has-name is-key has-metavar)
  ()
  (:documentation "A parser for an option in the form '-f VAR' or '--foo VAR'"))

(defun option (name metavar &key short long description (fn #'identity))
  "Make a parser for an option in the short form (like '-f VAR')
and/or the long form (like '--foo VAR')."
  (unless (or short long)
    (error "Specify either :SHORT or :LONG keys."))
  (make-instance 'option
                 :name        name
                 :short       short
                 :long        long
                 :meta        metavar
                 :fn          fn
                 :description description))

;; Argument

(defclass argument (has-name has-metavar)
  ()
  (:documentation "A parser for positional argument"))

(defun argument (name meta &key description (fn #'identity))
  "Make a parser for a positional argument."
  (make-instance 'argument
                 :name        name
                 :meta        meta
                 :fn          fn
                 :description description))

;; Arguments
(defclass arguments (has-name)
  ()
  (:documentation "A parser for the rest of an input"))

(defun arguments (name &key description)
  "Make a parser for the rest of an input, treating it as a list of
positional arguments."
  (make-instance 'arguments
                 :name        name
                 :description description))

;; Command

(defclass command (has-name)
  ((command :initarg  :command
            :initform (error "Specify a command")
            :reader   command-command)
   (value   :initarg  :value
            :initform (error "Specify a value")
            :reader   command-value))
  (:documentation "A parser for subcommands"))

(defun command (name command value &key description)
  "Make a parser for a subcommand. Produces a cons pair (NAME . VALUE)
if COMMAND is seen in the input."
  (make-instance 'command
                 :name        name
                 :command     command
                 :value       value
                 :description description))

;; Choice

(defclass choice (has-children)
  ()
  (:documentation "Returns what the first working parser returns"))

(defun choice (&rest children)
  "Make a choice parser. Returns what the first successful parser
returns."
  (make-instance 'choice :children children))

;; Repeat

(defclass rep (has-child)
  ()
  (:documentation "A repeating parser"))

(defun rep (child)
  (make-instance 'rep :child child))

;; Sequence

(defclass seq (has-children)
  ()
  (:documentation "A sequence of parsers"))

(defun seq (&rest children)
  "Create a sequence of parsers. The sequence succeeds only if all
parsers succeed when applied in sequential order."
  (make-instance 'seq :children children))

;; End of options guard

(defclass end-of-options-guard (has-child)
  ()
  (:documentation "A parser which fails when its child fails or when
it consumes the end of options mark ('--')"))

(defun end-of-options-guard (child)
  "Make END-OF-OPTIONS-GUARD parser."
  (make-instance 'end-of-options-guard :child child))

;; Optional

(defclass optional (has-child)
  ()
  (:documentation "A wrapper parser for options and flags"))

(defun optional (&rest children)
  "Create a parser for options and flags. This is (almost) a usual
CHOICE which is repeated until it fails."
  (make-instance 'optional
                 :child (rep (end-of-options-guard (apply #'choice children)))))
