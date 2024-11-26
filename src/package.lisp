(defpackage command-line-parse
  (:use #:cl)
  (:export #:flag
           #:option
           #:argument
           #:command
           #:choice
           #:optional
           #:seq
           #:parse
           #:show-usage
           
           #:parse-impure
           #:show-usage-impure))
