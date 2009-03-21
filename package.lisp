(in-package :cl-user)

(defpackage :swframes
  (:use :cl :utils);was :knewos
  (:import-from :wb "HTML")
  (:import-from :knewos "LXML-ATTRIBUTE" "LXML-SUBELEMENTS" "LXML-ALL-SUBELEMENTS" "LXML-TAG")
  (:import-from :knewos "RUN-SPARQL")
  )			
  
