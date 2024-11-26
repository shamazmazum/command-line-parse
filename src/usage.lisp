(in-package :command-line-parse)

(defmethod show-parser ((flag flag))
  (show-flag (long flag) (short flag)))

(defmethod show-parser ((option option))
  (concatenate
   'string
   (show-flag (long option) (short option))
   " "
   (meta option)))

(defmethod show-parser ((command command))
  (command-command command))

(defmethod show-parser ((argument argument))
  (meta argument))

(defmethod show-parser ((arguments arguments))
  "ARGUMENTS*")

(defmethod show-parser ((choice choice))
  (format
   nil "~{(~a)~^ | ~}"
   (mapcar #'show-parser (children choice))))

(defmethod show-parser ((optional optional))
  (format
   nil "~{[~a]~^ ~}"
   (mapcar #'show-parser
           (children (child (child (child optional)))))))

(defmethod show-parser ((seq seq))
  (format
   nil "~{~a~^ ~}"
   (mapcar #'show-parser (children seq))))

(defun show-usage (program-name parser)
  "Get a usage help string for a parser"
  (format nil "Usage: ~a ~a" program-name (show-parser parser)))
