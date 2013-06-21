;;;PCClib - a library for dealing with dates and times in the Planning
;;;Committee Calendar, as used be TPC.  Copyright 2011 Gregorian,
;;;002PCC The Planning Committee.  Released under the Plan Standard Licence.
;;;Version 1.0
;;
;;At the very least, the following functions are provably correct as of
;;2011-06-29: LEAP-YEARS, LEAP-YEAR-P, ENCODE-UNIVERSAL-TIME,
;;DECODE-UNIVERSAL-TIME, PCC-SECONDCOUNT, and the constants they use.
;;Helper functions for each of these are also correct.
;;
;;Any other code in the file has either not been mathematically proved or
;;not been fully tested.  Either way, don't depend on it.  Even the functions
;;mentioned above may not be consistent with each other in the order in which
;;they return date-components, but they all call each other correctly.

(in-package :org.tpchq.pcclib)

(defconstant universal-offset (cl:encode-universal-time 0 0 0 22 12 2008)
  "The value of universal-time at 000/01/01 00:00:00 PCC")
(defconstant days-in-week 7)
(defconstant weeks-in-month 4)
(defconstant seconds-in-minute 60)
(defconstant minutes-in-hour 60)
(defconstant hours-in-day 24)
(defconstant minutes-in-day (* minutes-in-hour hours-in-day))
(defconstant seconds-in-hour (* seconds-in-minute minutes-in-hour))
(defconstant seconds-in-day (* minutes-in-day seconds-in-minute))
(defconstant seconds-in-week (* seconds-in-day days-in-week))
(defconstant days-in-month (* weeks-in-month days-in-week))
(defconstant seconds-in-month (* seconds-in-day days-in-month))
(defconstant days-in-year 365)
(defconstant days-in-4y (1+ (* days-in-year 4)))
(defconstant days-in-century (1- (* days-in-4y 25)))
(defconstant days-in-400y (1+ (* days-in-century 4)))

(defun leap-years (year)
  "Returns the number of leap years after 0PCC up to and including year.
  This will be negative if year is."
  (let ((lys 0))
    (multiple-value-bind (q r) (truncate year 400)
      (incf lys q)
      (if (>= r 391) (incf lys))
      (if (<= r -9) (decf lys))
      (multiple-value-bind (q r) (truncate year 100)
        (decf lys q)
        (if (>= r 91) (decf lys))
        (if (<= r -9) (incf lys))
        (multiple-value-bind (q r) (truncate year 4)
          (incf lys q)
          (if (>= r 3) (incf lys))
          (if (<= r -1) (decf lys)))))
    (if (leap-year-p year) (if (< year 0) (1+ lys) (1- lys)) lys)))

(defun leap-year-p (year)
  "Returns T if year is a leap year in the PCC, NIL else"
  (or
   (= (mod year 400) 391)
   (and (= (mod year 4) 3)
        (/= (mod year 100) 91))))

(defun encode-universal-time (second minute hour date month year)
  "The time values specified in decoded format (pcc) are converted
  to universal time, which is returned"
  (let ((encoded-time (+ (secondcount second minute hour date month year)
                         universal-offset)))
    ;next line is necessary iff universal-time must be >=0
    (assert (typep encoded-time '(integer 0)))
    encoded-time))

(defun secondcount (second minute hour date month year)
  "The time values specified in decoded format (pcc) are converted to a
  number of seconds since 0 PCC, which is returned.  For use with other
  CL time-handling forms, use encode-universal-time instead, which
  wraps this with an offset from CL's universal-time"
  (declare (type (integer 1 14) month)
           (type (integer 1 28) date)
           (type (mod 60) second)
           (type (mod 60) minute)
           (type (mod 24) hour))
  (if (= month 14) (if (leap-year-p year) (assert (typep date '(integer 1 2))) (assert (= date 1))))
  (let* ((days (+ (1- date)
                  (* days-in-month (1- month))
                  (* year 365)
                  (leap-years year)))
         (encoded-time (+ (* days seconds-in-day)
                          (* hour seconds-in-hour)
                          (* minute seconds-in-minute)
                          second)))
    encoded-time))

(defun truncate-for-date (number &optional (divisor 1))
  "Acts as a cross between TRUNCATE and FLOOR: Return number (or number/divisor)
  as an integer, rounded toward 0.  The second returned value is
  (mod number divisor)"
  (multiple-value-bind (quotient remainder) (truncate number divisor)
    (values quotient (mod remainder divisor))))

(defun month-day-to-week-and-day (monthday)
  "Takes a day of the month in the PCC and returns the week of the month and
  day of the week in same"
  (multiple-value-bind (weeks days) (truncate (1- monthday) 7)
    (values (1+ weeks) (1+ days))))

(defun decode-day-of-year (yearday)
  "Takes a day of year (zero-based) and returns month and day-of-month"
  (declare (type (integer 0 365) yearday))
  (multiple-value-bind (month monthday)
      (truncate yearday days-in-month)
    (values (1+ month) (1+ monthday))))

(defun decode-day-of-fouryear (fouryearday)
  "Takes a day-of-four-year-cycle (zero-based) and returns year, month, day.
  Considers a cycle to start on year zero."
  (declare (type (integer 0 1460) fouryearday));1460 is (1- days-in-4y)
  (if (= fouryearday (* days-in-year 4))
      (values 3 14 2)
      (multiple-value-bind (year yearday)
          (floor fouryearday days-in-year)
        (multiple-value-bind (month date)
            (decode-day-of-year yearday)
          (values year month date)))))

(defun decode-day-of-century (centuryday &optional fourth-century-p)
  "Takes a day of a century and returns year, month, day.  Treats
  as the fourth of a four-century cycle (with an extra leap year)
  if fourth-century-p is set"
  (declare (type (integer 0 36524 ) centuryday); 36524 is days-in-century
           (type (boolean) fourth-century-p))
  (if (and (not fourth-century-p) (>= centuryday (* days-in-4y 23)))
      (incf centuryday))
  (multiple-value-bind (fouryear fouryearday)
      (truncate centuryday days-in-4y)
    (multiple-value-bind (year4 month date)
        (decode-day-of-fouryear fouryearday)
      (let ((year (+ (* fouryear 4) year4)))
        (values year month date)))))

(defun decode-day-of-400y (400yday)
  "Takes a day of a four-hundred-year cycle, and returns the year of the cycle,
  month, and day-of-month."
  (declare (type (integer 0 146096) 400yday)); 146096 is (1- days-in-400y)
  (multiple-value-bind (century centuryday)
      (truncate 400yday days-in-century)
    (if (= century 4) (progn (decf century) (setf centuryday days-in-century)))
    (multiple-value-bind (century-year month date)
        (decode-day-of-century centuryday (= century 3))
      (let ((year (+ (* 100 century) century-year)))
        (values year month date)))))

(defun decode-universal-time (universal-time)
  "Converts a universal-time to decoded format for the PCC, returning
   the following six values: second, minute, hour, day of month, month, year.
   Month 14 is used to refer to the holiday(s) at the end of the year."
  (let ((universal-pcc (- universal-time universal-offset)))
    (multiple-value-bind (day-count remainder)
        (truncate universal-pcc seconds-in-day)
      (setf remainder (mod remainder seconds-in-day))
      (let ((hour (truncate remainder seconds-in-hour))
            (minute (truncate (mod remainder seconds-in-hour) seconds-in-minute))
            (second (mod remainder seconds-in-minute)))
        (declare (type (mod 60) second)
                 (type (mod 60) minute)
                 (type (mod 24) hour))
        (multiple-value-bind (400y-count day400)
            (truncate day-count days-in-400y)
          (setf day400 (mod day400 days-in-400y))
          (if (< 400y-count 0) (decf 400y-count))
          (multiple-value-bind (year400 month date)
              (decode-day-of-400y day400)
            (let ((year (+ (* 400 400y-count) year400)))
              (values second minute hour date month year))))))))
