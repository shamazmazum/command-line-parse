(defun do-all()
  (ql:quickload :command-line-parse/tests)
  (uiop:quit
   (if (uiop:call-function "command-line-parse/tests:run-tests")
       0 1)))

(do-all)
