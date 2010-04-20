(in-package :cl-user)

(defpackage :swframes
  (:use :blisp :mt :clos* :lisp-unit :3utils)
  (:nicknames :sw)
  (:import-from #+BIOLISP :wb "HTML")
  (:shadowing-import-from :mt "SET-EQUAL")
  ;; exports are in the code
  )

