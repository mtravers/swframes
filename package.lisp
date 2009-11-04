(in-package :cl-user)

;;; Clean these up so we can use-package :swframes
#+BIOLISP
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
  (:import-from #+BIOLISP :wb "HTML")
  ;; exports are in the code
  )

