(in-package :cl-user)

#+BIOLISP
;;; Clean these up so we can use-package :swframes
(unexport '(frames:describe-frame
	    frames:rename-frame
	    frames:df
	    frames:slotv
	    frames:for-all-frames)
	  :biolisp)

#+BIOLISP
(unexport '(weblistener:parse-xml) 
	  :biolisp)

(defpackage :swframes
  (:use :blisp :mt :clos* :3utils)
  (:nicknames :sw)
  (:import-from :wb "HTML")
  ;; exports are in the code
  )

