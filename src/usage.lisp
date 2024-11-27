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

(defmethod print-parser ((optional optional) stream)
  (let ((children (children (child (child (child optional))))))
    (pprint-logical-block (stream children)
      (loop for tail on children
            for (child . rest) = tail do
            (write-char #\[ stream)
            (print-parser child stream)
            (write-char #\] stream)
            (when rest
              (write-char #\Space stream))
            (pprint-newline :fill stream)
            (pprint-indent :current 0)))))

(defmethod print-parser ((seq seq) stream)
  (let ((children (children seq)))
    (pprint-logical-block (stream children)
      (loop for tail on children
            for (child . rest) = tail do
            (print-parser child stream)
            (when rest
              (write-char #\Space stream))
            (pprint-newline :fill stream)
            (pprint-indent :current 0)))))

(defmethod print-parser ((choice choice) stream)
  (let ((children (children choice)))
    (pprint-logical-block (stream children)
      (loop for tail on children
            for (child . rest) = tail do
            (print-parser child stream)
            (when rest
              (princ " | " stream))
            (pprint-newline :fill stream)
            (pprint-indent :current 0)))))

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

(serapeum:-> show-usage (string parser)
             (values string &optional))
(defun show-usage (program-name parser)
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
