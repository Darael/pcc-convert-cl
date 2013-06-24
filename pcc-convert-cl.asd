;;;; pcc-convert-cl.asd

(asdf:defsystem #:pcc-convert-cl
  :serial t
  :description "A system for converting between PCC and Gregorian calendars"
  :author "Vinothan Shankar"
  :license "Plan Standard Licence 1.0 or GPL 3.0"
  :components ((:file "pcclib-packages.lisp")
               (:file "pcclib.lisp" :depends-on ("package"))
               (:file "pcclib.tests.lisp" :depends-on ("pcclib.lisp"))))
