(defsystem :command-line-parse
  :name :command-line-parse
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Wavelet transform library"
  :license "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "util")
               (:file "parsers")
               (:file "parse")
               (:file "usage")
               (:file "impure"))
  :depends-on (:serapeum)
  :in-order-to ((test-op (load-op "command-line-parse/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :command-line-parse/tests '#:run-tests)))

(defsystem :command-line-parse/tests
  :name :command-line-parse/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:command-line-parse :fiveam :split-sequence :parse-number))
