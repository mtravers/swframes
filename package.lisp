(in-package :cl-user)

;;; Clean these up so we can use-package :swframes
(unexport '(frames:describe-frame
	    frames:rename-frame
	    frames:df
	    frames:slotv
	    frames:for-all-frames)
	  :biolisp)

(unexport '(weblistener:parse-xml) 
	  :biolisp)

(defpackage :swframes
  (:use :cl :mt :clos*)
  (:nicknames :sw)
  (:import-from :wb "HTML")
  ;; exports are in the code
  )

