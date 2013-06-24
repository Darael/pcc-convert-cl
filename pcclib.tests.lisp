;;Testsuite for pcclib.lisp

(in-package #:org.tpchq.pcclib)

(defun test-negative-years ()
  (cycle-years -108 -1))

(defun encdec (date month year)
  "Decodes the result of encoding a date.  Should return the date, in list form"
  (declare (type (integer 1 28) date))
  (decode-universal-time (test-encode-universal-time :date date
                                                     :month month
                                                     :year year)))

(defun all-dates (start-year end-year)
  "Generates a list of all dates in the range start-year through end-year in the
  form (:DATE date :MONTH month :YEAR year)"
  (declare (type (integer) start-year end-year))
  (loop for year from start-year to end-year append
       (loop for month from 1 to 14 append
            (loop for date from 1 to (if (= month 14)
                                         (if (leap-year-p year) 2 1)
                                         28)
               collecting `(:date ,date :month ,month :year ,year)))))

(defun cycle-date (&key date month year)
  "Takes a date in the PCC, and returns it with either NIL or the incorrect
  result of doing a decode-encode cycle on it."
  (declare (type (integer 1 28) date)
           (type (integer 1 14) month)
           (type (integer) year))
  `(:date ,date :month ,month :year ,year
    :wrong-output ,(let ((cycled (encdec date month year)))
                        (if (equal cycled `(0 0 0 ,date ,month ,year))
                            cycled))))

(defun cycle-years (start-year end-year)
  "Tests that a year-range in the PCC encodes and decodes symmetrically.
  Returns T if all values are as expected, else NIL and the list of failures."
  (declare (type (integer) start-year end-year))
  (assert (<= start-year end-year))
  (let ((errors
         (remove-if-not #'(lambda (x) (getf x :wrong-output))
                        (mapcar #'(lambda (date) (apply #'cycle-date date))
                                (all-dates start-year end-year)))))
    (if errors (values nil errors) t)))

(defun test-encode-universal-time (&key date month year)
  (declare (type (integer 1 28) date)
           (type (integer 1 14) month)
           (type (integer) year))
  (encode-universal-time 0 0 0 date month year))

(defun confirm-years (start-year end-year)
  "Given that start-year/01/01 PCC encodes to the correct universal-time,
  check that all dates in the given range do so also."
  (declare (type (integer) start-year end-year))
  (assert (<= start-year end-year))
  (loop for date in (all-dates start-year end-year)
       for current = (encode-universal-time 0 0 0 1 1 start-year)
     then (+ current seconds-in-day) always (= (apply #'test-encode-universal-time
                                                      date)
                                               current)))
