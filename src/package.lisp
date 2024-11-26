(defpackage command-line-parse
  (:use #:cl)
  (:export #:flag
           #:option
           #:argument
           #:arguments
           #:command
           #:choice
           #:optional
           #:seq
           #:parse
           #:show-usage
           #:cmd-line-parse-error

           #:parse-impure
           #:show-usage-impure))
