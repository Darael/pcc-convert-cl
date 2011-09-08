;;;PCCLib, package definition.  See copyright in pcclib.lisp.
(defpackage :org.tpchq.pcclib
  (:nicknames 
   :org.tpchq.libpcc
   :pcclib
   :pcc)
  (:use :cl)
  (:export :encode-universal-time
	   :decode-universal-time
	   :leap-year-p)
  (:shadow :encode-universal-time
	   :decode-universal-time)
  (:documentation """PCClib provides versions of the time functions for use with the
   PCC instead of the Gregorian calendar"""))