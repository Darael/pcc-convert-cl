;;;; package.lisp

(defpackage #:org.tpchq.pcclib
  (:nicknames
   #:org.tpchq.libpcc
   #:libpcc
   #:pcclib
   #:pcc)
  (:use #:cl)
  (:shadow
   #:encode-universal-time
   #:decode-universal-time))
