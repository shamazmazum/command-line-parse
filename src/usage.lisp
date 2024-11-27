(in-package :command-line-parse)

(defmethod print-parser ((flag flag) stream)
  (princ (show-flag (long flag) (short flag)) stream))

(defmethod print-parser ((option option) stream)
  (princ (show-flag (long option) (short option)) stream)
  (princ " " stream)
  (princ (meta option) stream))

(defmethod print-parser ((command command) stream)
  (princ (command-command command) stream))

(defmethod print-parser ((argument argument) stream)
  (princ (meta argument) stream))

(defmethod print-parser ((arguments arguments) stream)
  (princ "ARGUMENTS*" stream))

(defun print-children (children stream &key prefix suffix separator)
  (pprint-logical-block (stream children)
    (loop
     (pprint-exit-if-list-exhausted)
     (when prefix
       (write-char prefix stream))
     (print-parser (pprint-pop) stream)
     (when suffix
       (write-char suffix stream))
     (pprint-exit-if-list-exhausted)
     (princ separator stream)
     (pprint-newline :fill stream)
     (pprint-indent :current 0))))

(defmethod print-parser ((optional optional) stream)
  (print-children (children (child (child (child optional)))) stream
                  :prefix #\[ :suffix #\] :separator #\Space))

(defmethod print-parser ((seq seq) stream)
  (print-children (children seq) stream
                  :separator #\Space))

(defmethod print-parser ((choice choice) stream)
  (print-children (children choice) stream
                  :separator " | "))

;; Description printing

(serapeum:-> show-parser (parser)
             (values string &optional))
(defun show-parser (parser)
  (with-output-to-string (out)
    (print-parser parser out)))

(serapeum:-> collect-descriptions (parser)
             (values list &optional))
(defun collect-descriptions (parser)
  (labels ((%go (acc parser)
             (let ((acc
                    (if (and (typep parser 'has-name)
                             (description parser))
                        (acons (show-parser parser)
                               (description parser)
                               acc)
                        acc)))
               (cond
                 ((typep parser 'has-children)
                  (reduce #'%go (children parser)
                          :initial-value acc))
                 ((typep parser 'has-child)
                  (%go acc (child parser)))
                 (t acc)))))
    (%go nil parser)))

(serapeum:-> show-descriptions (parser)
             (values (or-null string) &optional))
(defun show-descriptions (parser)
  (let ((descriptions (collect-descriptions parser)))
    (if descriptions
        (let ((position
               (+ (reduce #'max descriptions :key (alexandria:compose #'length #'car)) 5)))
          (with-output-to-string (out)
            (loop for description in descriptions do
                  (princ (car description) out)
                  (loop repeat (- position (length (car description))) do
                        (write-char #\Space out))
                  (princ (cdr description) out)
                  (write-char #\NewLine out)))))))

;; Final usage function

(serapeum:-> show-usage (parser string)
             (values string &optional))
(defun show-usage (parser program-name)
  "Get a usage help string for a parser"
  (with-output-to-string (out)
    (format out "Usage: ~a " program-name)
    (print-parser parser out)
    (terpri out)
    (let ((descriptions (show-descriptions parser)))
      (when descriptions
        (terpri out)
        (princ "Options and arguments description:" out)
        (terpri out)
        (princ descriptions out)))))
