(in-package :cl-user)

(defpackage :swframes
  (:use :cl :utils);was :knewos
  (:import-from :wb "HTML")
  (:import-from :knewos "LXML-ATTRIBUTE" "LXML-SUBELEMENTS")
  (:import-from :knewos "RUN-SPARQL")
  )			
  
