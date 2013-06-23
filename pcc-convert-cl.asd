;;;; pcc-convert-cl.asd

(asdf:defsystem #:pcc-convert-cl
  :serial t
  :description "A system for converting between PCC and Gregorian calendars"
  :author "Vinothan Shankar"
  :license "Plan Standard Licence 1.0 or GPL 3.0"
  :components ((:file "package")
               (:file "pcc-convert-cl")))
