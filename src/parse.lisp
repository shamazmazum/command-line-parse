(in-package :command-line-parse)

(defmethod parse-input ((flag flag) input &optional acc)
  (if (peek-flag (long flag) (short flag) input)
      (values (acons (name flag) t acc) (cdr input) t)
      (values acc input nil)))

(defmethod parse-input ((option option) input &optional acc)
  (let ((startp (peek-flag (long option) (short option) input)))
    (cond
      ((and (cdr input) startp)
       (values (acons (name option)
                      (funcall (meta-fn option) (forbid-option (cadr input)))
                      acc)
               (cddr input) t))
      (startp
       (error 'cmd-line-parse-error
              :format-control   "Abrupt ending of the input while parsing ~a"
              :format-arguments (list option)))
      (t
       (values acc input nil)))))

(defmethod parse-input ((argument argument) input &optional acc)
  (if input
      (values (acons (name argument) (car input) acc) (cdr input) t)
      (error 'cmd-line-parse-error
             :format-control   "Abrupt ending of the input: argument ~a missing"
             :format-arguments (list argument))))

(defmethod parse-input ((command command) input &optional acc)
  (if (and input (string= (car input) (command-command command)))
      (values (acons (name command) (command-value command) acc)
              (cdr input) t)
      (values acc input nil)))

(defmethod parse-input ((choice choice) input &optional acc)
  (labels ((%go (parsers)
             (if (null parsers) (values acc input nil)
                 (multiple-value-bind (result rest successp)
                     (parse-input (car parsers) input acc)
                   (if successp
                       (values result rest t)
                       (%go (cdr parsers)))))))
    (%go (children choice))))

(defmethod parse-input ((rep rep) input &optional acc)
  (labels ((%go (acc input)
             (multiple-value-bind (acc rest successp)
                 (parse-input (child rep) input acc)
               (if successp
                   (%go acc rest)
                   ;; REP always succeeds
                   (values acc rest t)))))
    (%go acc input)))

(defmethod parse-input ((seq seq) input &optional acc)
  (labels ((%go (%acc %input parsers)
             (if (null parsers)
                 (values %acc %input t)
                 (multiple-value-bind (%acc rest successp)
                     (parse-input (car parsers) %input %acc)
                   (if successp
                       (%go %acc rest (cdr parsers))
                       (values acc input nil))))))
    (%go acc input (children seq))))

(defmethod parse-input ((eoog end-of-options-guard) input &optional acc)
  (multiple-value-bind (acc input successp)
      (parse-input (child eoog) input acc)
    (values
     acc (if (and (not successp)
                  input
                  (string= (car input) "--"))
             (cdr input) input)
     successp)))

(defmethod parse-input ((optional optional) input &optional acc)
  (parse-input (child optional) input acc))

;; Final parsing function

(defun parse (parser input)
  "Parse a list of command line arguments INPUT using a PARSER."
  (multiple-value-bind (result rest)
      (parse-input parser input)
    (when rest
      (error 'cmd-line-parse-error
             :format-control   "Unparsed input remians: ~s"
             :format-arguments (list rest)))
    result))
