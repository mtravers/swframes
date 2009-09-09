(in-package :cl-user)

;;; Clean these up so we can use-package :swframes
(unexport '(FRAMES:DESCRIBE-FRAME
	    FRAMES:RENAME-FRAME
	    FRAMES:DF
	    FRAMES:SLOTV
	    FRAMES:FOR-ALL-FRAMES)
	  :biolisp)

(unexport '(WEBLISTENER:PARSE-XML) 
	  :biolisp)


(defpackage :swframes
  (:use :cl :utils :clos*)
  (:nicknames :sw)
  (:import-from :wb "HTML")
;  (:import-from :knewos "RUN-SPARQL")
  ;; exports in the code for now
  )

