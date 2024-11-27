(in-package :command-line-parse)

(defun parse-argv (parser)
  "Parse command line arguments. This function relies on UIOP to get
arguments and the name of your program. Signals CMD-LINE-PARSE-ERROR
on failure."
  (parse parser (uiop:command-line-arguments)))

(defun print-usage (parser &optional (program-name (uiop:argv0)))
  "Print a usage help to error output stream"
  (princ (show-usage parser program-name) *error-output*))
