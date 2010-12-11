(in-package :cl-user)

(defpackage :swframes
  (:use :blisp :lxml :mt :clos* :lisp-unit)
  (:nicknames :sw)
  #+BIOLISP (:import-from :wb "HTML")
  (:shadowing-import-from :mt "SET-EQUAL")
  ;; exports are in the code
  )

