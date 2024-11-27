(in-package :command-line-parse)

;; Usage printing

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

;; Description printing

(serapeum:-> collect-descriptions (parser)
             (values list &optional))
(defun collect-descriptions (parser)
  (declare (optimize (speed 3)))
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
          (apply #'concatenate 'string
                 (mapcar (lambda (description)
                           (concatenate
                            'string
                            (car description)
                            (loop repeat (- position (length (car description)))
                                  collect #\Space)
                            (cdr description)
                            '(#\NewLine)))
                         descriptions))))))

;; Final usage function

(serapeum:-> show-usage (string parser)
             (values string &optional))
(defun show-usage (program-name parser)
  "Get a usage help string for a parser"
  (concatenate
   'string
   (format nil "Usage: ~a ~a" program-name (show-parser parser))
   (let ((descriptions (show-descriptions parser)))
     (if descriptions
         (concatenate
          'string
          '(#\NewLine #\NewLine)
          "Options and arguments description:"
          '(#\NewLine)
          descriptions)))))
