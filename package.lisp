(in-package :cl-user)

;;; Clean these up so we can use-package :swframes
(unexport '(FRAMES:DESCRIBE-FRAME
	    WEBLISTENER:PARSE-XML
	    FRAMES:RENAME-FRAME
	    FRAMES:DF
	    FRAMES:SLOTV
	    FRAMES:FOR-ALL-FRAMES))

(defpackage :swframes
  (:use :cl :utils :clos*)
  (:nicknames :sw)
  (:import-from :wb "HTML")
  (:import-from :knewos "RUN-SPARQL")
  ;; exports in the code for now
  )

