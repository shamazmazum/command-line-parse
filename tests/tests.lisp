(in-package :command-line-parse/tests)

(def-suite suite-1 :description "Parser variant 1")
(def-suite suite-2 :description "Parser variant 2")
(def-suite usage   :description "Usage")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(suite-1 suite-2 usage))))

(defun split-args (args)
  (split-sequence #\Space args))

(defparameter *parser-1*
  (choice
   (seq
    (command 'what "show-progress" 'show
             :description "Show progress")
    (optional
     (flag 'verbose
           :short       #\v
           :long        "verbose"
           :description "Be verbose")
     (flag 'human
           :long        "human-readable"
           :description "Human readable output")))
   (seq
    (command 'what "format" 'format
             :description "Format disk")
    (optional
     (option 'level       "LEVEL"
             :short       #\l
             :long        "level"
             :fn          #'parse-number
             :description "Format level (1-5)")
     (flag   'force
             :long        "force"
             :description "Format even if already formatted"))
    (argument 'disk "DISK" :description "Disk to format"))
   (seq
    (command 'what "process" 'process)
    (choice
     (flag 'quick :short #\q :description "Quick processing")
     (flag 'slow  :short #\s :description "Slow processing"))
    (arguments 'args))))

(defparameter *parser-2*
  (seq
   (optional
    (flag 'foo :short #\f :long "foo")
    (flag 'bar :short #\b :long "bar"))
   (argument 'arg0 "ARG0")
   (argument 'arg1 "ARG1")))

(defparameter *usage-1*
"Usage: program show-progress [-v|--verbose] [--human-readable] |
               format [-l|--level LEVEL] [--force] DISK |
               process -q | -s ARGUMENTS*

Description of commands
format            Format disk
show-progress     Show progress

Description of arguments
DISK     Disk to format

Description of flags and options
-s                   Slow processing
-q                   Quick processing
--force              Format even if already formatted
-l|--level LEVEL     Format level (1-5)
--human-readable     Human readable output
-v|--verbose         Be verbose
")

(defparameter *usage-2*
"Usage: program [-f|--foo] [-b|--bar] ARG0 ARG1
")

(in-suite suite-1)

(test parser1-success
  (let ((list (parse *parser-1* (split-args "show-progress --human-readable"))))
    (is (eq (%assoc 'verbose list) nil))
    (is (eq (%assoc 'human   list) t))
    (is (eq (%assoc 'what    list) 'show)))

  (let ((list (parse *parser-1* (split-args "format -l 5 disk0"))))
    (is (=       (%assoc 'level list) 5))
    (is (string= (%assoc 'disk  list) "disk0"))
    (is (eq      (%assoc 'what  list) 'format))
    (is (eq      (%assoc 'force list) nil)))

  (let ((list (parse *parser-1* (split-args "process -q foo bar"))))
    (is (eq     (%assoc 'quick list) t))
    (is (eq     (%assoc 'slow  list) nil))
    (is (equalp (%assoc 'args  list) '("foo" "bar")))))

(test parser1-failure
  (signals cmd-line-parse-error
    (parse *parser-1* (split-args "show-progress --human-readable foo")))

  (signals cmd-line-parse-error
    (parse *parser-1* (split-args "show-progress --force")))

  (signals cmd-line-parse-error
    (parse *parser-1* (split-args "format --force --level")))

  (signals cmd-line-parse-error
    (parse *parser-1* (split-args "format --level 3 foo bar")))

  (signals cmd-line-parse-error
    (parse *parser-1* (split-args "process foo bar"))))

(in-suite suite-2)

(test parser2-success
  (let ((list (parse *parser-2* (split-args "--foo foo bar"))))
    (is (eq (%assoc 'foo list) t))
    (is (eq (%assoc 'bar list) nil))
    (is (string= (%assoc 'arg0 list) "foo"))
    (is (string= (%assoc 'arg1 list) "bar"))))

(test parser2-failure
  (signals cmd-line-parse-error
    (parse *parser-2* (split-args "--foo --bar foo")))
  (signals cmd-line-parse-error
    (parse *parser-2* (split-args "--baz foo bar"))))

(in-suite usage)

(test usage-1
  (is (string= (show-usage *parser-1* "program") *usage-1*)))

(test usage-2
  (is (string= (show-usage *parser-2* "program") *usage-2*)))
