(in-package :cl-user)

(defpackage :swframes
  (:use :blisp :mt :clos* :lisp-unit :3utils)
  (:nicknames :sw)
  #+BIOLISP (:import-from :wb "HTML")
  (:shadowing-import-from :mt "SET-EQUAL")
  ;; exports are in the code
  )

