(in-package :cl-user)

(defpackage :swframes
  (:use :cl :utils :clos*)
  (:nicknames :sw)
  (:import-from :wb "HTML")
  (:import-from :knewos "RUN-SPARQL")
  (:import-from :mt "DELETEF")
  ;; exports in the code for now
  )

