(in-package :sw)

(export '(in-background-thread))

;;; needs some error handling +++
(defmacro in-background-thread (&body body)
  `(#+ACL 
    mp:process-run-function
    #-ACL
    acl-compat.mp:process-run-function
    (string (gensym "THREAD"))
    #'(lambda () ,@body)))
